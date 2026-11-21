"""Start nagi's two T3 services in a private home and network namespace."""
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import time
from urllib.parse import urlsplit

# Use Guix Home's service extensions and Shepherd's real start procedures.
# Only these two services belong in the test supervisor.
SERVICE_CONFIG = """
(use-modules (gnu home) (gnu services) (gnu services shepherd) (gnu home services shepherd)
             (roquix home services t3code) (roquix packages t3code)
             (guix store) (guix derivations) (guix gexp) (json))
(define home (primitive-load "guix-system/config/nagi-home.scm"))
(define all
  (service-value (fold-services (home-environment-services home)
                                #:target-type home-shepherd-service-type)))
(define config
  (home-shepherd-configuration
   (inherit all) (daemonize? #f)
   (services (filter (lambda (s) (or (memq 't3code (shepherd-service-provision s))
                                     (memq 't3code-local (shepherd-service-provision s))))
                     (home-shepherd-configuration-services all)))))
(with-store store
  (define (realize obj)
    (let ((drv (run-with-store store (lower-object obj))))
      (build-derivations store (list drv))
      (derivation->output-path drv)))
  (display (scm->json-string
            (vector (realize ((@@ (gnu home services shepherd) home-shepherd-configuration-file) config))
                    (realize (home-shepherd-configuration-shepherd config))
                    (realize t3code-cli-rocktakey)))))
"""


READ_PROVIDERS = """
import {readFileSync} from 'node:fs';
const origin = process.argv[1];
const response = await fetch(origin + '/api/auth/websocket-ticket/rpc', {
  method: 'POST', headers: {Authorization: 'Bearer ' + readFileSync(0, 'utf8')}
});
if (!response.ok) throw Error('WebSocket ticket: ' + response.status);
const {ticket} = await response.json();
const url = new URL('/ws', origin.replace('http:', 'ws:'));
url.search = new URLSearchParams({wsTicket: ticket, orchestrationProtocol: '2'});
const socket = new WebSocket(url);
socket.onopen = () => socket.send(JSON.stringify({
  _tag: 'Request', id: '1', tag: 'server.getSettings', payload: {}, headers: []
}));
socket.onmessage = ({data}) => {
  const reply = JSON.parse(data);
  if (reply._tag !== 'Exit' || reply.requestId !== '1') return;
  if (reply.exit._tag !== 'Success') throw Error('Settings RPC failed');
  console.log(JSON.stringify(Object.keys(reply.exit.value.providerInstances)));
  socket.close();
};
socket.onerror = () => { throw Error('Settings connection failed'); };
"""


def check_instances(config, supervisor, package):
    home = Path.home()
    instances = [
        ("t3code", home / ".t3", home / "project-a"),
        ("t3code-local", home / ".t3-local", home / "project-b"),
    ]
    for name, state, project in instances:
        project.mkdir()
        settings = state / "userdata/settings.json"
        settings.parent.mkdir(parents=True)
        settings.write_text(json.dumps({"providerInstances": {
            "fixture-" + name: {"driver": "codex", "displayName": name, "enabled": False}
        }}))
    (home / "run").mkdir(mode=0o700)
    sock = str(home / "shepherd.sock")
    herd = [supervisor + "/bin/herd", "-s", sock]
    cli = package + "/bin/t3"

    def projects_command(state, *args):
        return subprocess.run([cli, *args, "--base-dir", state],
                              capture_output=True, text=True, timeout=15)

    def verify():
        for name, state, own in instances:
            # Read the running server through the same settings RPC as its client.
            token = projects_command(state, "auth", "session", "issue", "--ttl", "2m", "--token-only")
            token.check_returncode()
            origin = json.loads((state / "userdata/server-runtime.json").read_text())["origin"]
            assert urlsplit(origin).hostname == "127.0.0.1", origin
            rpc = subprocess.run(["node", "--input-type=module", "-e", READ_PROVIDERS, origin],
                                 input=token.stdout.strip(), capture_output=True, text=True, timeout=15)
            if rpc.returncode:
                raise RuntimeError(rpc.stderr)
            configured = {key for key in json.loads(rpc.stdout) if key.startswith("fixture-")}
            assert configured == {"fixture-" + name}, configured
            for _, _, project in instances:
                result = projects_command(state, "thread", "list", "--project", project, "--json")
                if project == own:
                    result.check_returncode()
                    assert json.loads(result.stdout) is not None
                elif result.returncode == 0 or f"No T3 Code project matches '{project}'" not in result.stderr:
                    raise AssertionError((state, project, result.stdout, result.stderr))

    def wait_for_server(state, project, *, restored):
        deadline = time.monotonic() + 60
        while time.monotonic() < deadline:
            if daemon.poll() is not None:
                raise RuntimeError("Shepherd exited before the service was ready")
            if (state / "userdata/server-runtime.json").is_file():
                result = projects_command(state, "thread", "list", "--project", project, "--json")
                if result.returncode == 0 or (
                    not restored and f"No T3 Code project matches '{project}'" in result.stderr
                ):
                    return
            time.sleep(0.3)
        raise RuntimeError(f"T3 did not become ready for {project}")

    with subprocess.Popen([supervisor + "/bin/shepherd", "-c", config, "-s", sock]) as daemon:
        try:
            for _, state, project in instances:
                wait_for_server(state, project, restored=False)
                projects_command(state, "project", "add", project).check_returncode()
            verify()
            for name, _, _ in instances:
                subprocess.run([*herd, "stop", name], check=True, timeout=30)
            for name, state, own in reversed(instances):
                subprocess.run([*herd, "start", name], check=True, timeout=30)
                wait_for_server(state, own, restored=True)
            verify()
        finally:
            if daemon.poll() is None:
                daemon.terminate()
                daemon.wait(timeout=30)


if __name__ == "__main__":
    if sys.argv[1:]:
        check_instances(*sys.argv[1:])
    else:
        artifacts = json.loads(subprocess.check_output(
            ["guix", "repl", "--", "/dev/stdin"], input=SERVICE_CONFIG, text=True))
        with tempfile.TemporaryDirectory() as temp:
            home = "/home/rocktakey"
            subprocess.run([
                "bwrap", "--unshare-all", "--die-with-parent", "--new-session",
                "--ro-bind", "/gnu/store", "/gnu/store", "--ro-bind", "/etc", "/etc",
                "--ro-bind", "/bin", "/bin", "--proc", "/proc", "--dev", "/dev",
                "--tmpfs", "/tmp", "--bind", temp, home,
                "--ro-bind", str(Path(__file__).resolve()), "/test.py",
                "--clearenv", "--setenv", "HOME", home, "--setenv", "PATH", os.environ["PATH"],
                "--setenv", "XDG_RUNTIME_DIR", home + "/run", "--chdir", home,
                sys.executable, "/test.py", *artifacts], check=True, timeout=200)
