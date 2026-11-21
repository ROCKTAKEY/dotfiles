"""Exercise the repository's shell definitions with disposable host and project state."""
import os
from pathlib import Path
import pwd
import shutil
import socket
import subprocess
import sys
import tempfile
from concurrent.futures import ThreadPoolExecutor


T3_VARIABLES = (
    "T3_MCP_BEARER_TOKEN", "T3_MCP_URL", "T3_ACP_MCP_AUTHORIZATION",
    "T3_ACP_MCP_ENDPOINT", "T3_ACP_MCP_NODE", "T3_ACP_MCP_ENTRYPOINT",
    "T3CODE_HOME", "T3CODE_LOG_WS_EVENTS",
)


def exercise_profiles(guix, fj):
    home = Path.home()
    runtime = Path(os.environ["XDG_RUNTIME_DIR"])
    runtime.mkdir(parents=True, exist_ok=True)
    for directory in [".codex", ".agents", ".config/gh", ".config/glab-cli", "project-a", "project-b"]:
        (home / directory).mkdir(parents=True)
    for file in [".ssh/known_hosts", ".ssh/ai", ".ssh/ai.pub", ".config/git/config",
                 ".config/git/ignore", ".config/git/config.codex.local", ".gnupg/pubring.kbx",
                 ".gnupg/private-keys-v1.d/BC20FD0C1F13E0C63A98552620D00C487E3370C2.key"]:
        path = home / file
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(path.name)

    def run(profiles, command, project="project-a"):
        return subprocess.check_output(
            [guix, "extra-profile", "shell", *profiles, "--", "--", *command],
            cwd=home / project, text=True, timeout=90)

    run(["codex"], [sys.executable, "-c", """
import errno
from pathlib import Path
for name in ('known_hosts', 'ai', 'ai.pub'):
    path = Path.home() / '.ssh' / name
    assert path.read_text() == name
    try:
        path.open('ab').close()
    except OSError as error:
        assert error.errno == errno.EROFS, error
    else:
        raise AssertionError(f'{path} is writable')
"""])
    run(["codex", "guix"], ["guix", "extra-profile", "--help"])

    # Separate projects have separate private homes. Only shared host state
    # can carry the dummy Forgejo login between them.
    for writer, reader, address in [("codex", "opencode", "writer-a.invalid"),
                                    ("opencode", "codex", "writer-b.invalid")]:
        run([writer], [fj, "-H", "https://" + address, "auth", "add-token", "fixture"])
        addresses = run([reader], [fj, "auth", "list"], project="project-b")
        assert address in addresses.splitlines(), addresses

    run(["codex", "t3code"], [sys.executable, "-c", """
import os
import sys
for name in sys.argv[1:]:
    assert os.environ[name] == name
assert os.environ['COLORTERM'] == 'truecolor'
assert 'NOT_T3_MCP_BEARER_TOKEN' not in os.environ
assert 'T3CODEX_HOME' not in os.environ
assert 'OTHER_SECRET' not in os.environ
""", *T3_VARIABLES])

    def identify_socket(listener, identity):
        listener.settimeout(90)
        with listener.accept()[0] as connection:
            connection.settimeout(5)
            assert connection.recv(1) == b"?"
            connection.sendall(identity)

    with socket.socket(socket.AF_UNIX) as bus, socket.socket(socket.AF_UNIX) as wayland:
        for listener, name in [(bus, "bus"), (wayland, "wayland-1")]:
            listener.bind(str(runtime / name))
            listener.listen()
        with ThreadPoolExecutor(max_workers=2) as pool:
            responses = [pool.submit(identify_socket, bus, b"B"),
                         pool.submit(identify_socket, wayland, b"W")]
            run(["codex", "virt"], [sys.executable, "-c", """
import os, socket
from pathlib import Path
bus = os.environ['DBUS_SESSION_BUS_ADDRESS'].removeprefix('unix:path=')
wayland = Path(os.environ['XDG_RUNTIME_DIR']) / os.environ['WAYLAND_DISPLAY']
for path, identity in [(bus, b'B'), (str(wayland), b'W')]:
    with socket.socket(socket.AF_UNIX) as client:
        client.settimeout(5)
        client.connect(path)
        client.sendall(b'?')
        assert client.recv(1) == identity
"""])
            for response in responses:
                response.result()


def main():
    home = pwd.getpwuid(os.getuid()).pw_dir
    root = Path(__file__).resolve().parents[3]
    with tempfile.TemporaryDirectory() as temp:
        fixture = Path(temp)
        definitions = fixture / "dotfiles/guix/.config/guix/extra-profiles"
        # The probes are store executables. Only the shell definitions, not the
        # agents' package manifests, are needed in the fixture profile registry.
        shutil.copytree(root / "guix/.config/guix/extra-profiles", definitions,
                        ignore=shutil.ignore_patterns("manifest.scm"))
        (fixture / ".config/guix").mkdir(parents=True)
        (fixture / ".config/guix/extra-profiles").symlink_to(
            Path(home) / definitions.relative_to(fixture))
        guix = shutil.which("guix")
        fj = shutil.which("fj")
        if not guix or not fj:
            raise RuntimeError("guix and fj are required")
        (fixture / ".config/guix/current").symlink_to(Path(guix).parent.parent)
        etc = fixture / "etc"
        etc.mkdir()
        for name in ("passwd", "group", "nsswitch.conf", "services"):
            if (Path("/etc") / name).is_file():
                shutil.copyfile(Path("/etc") / name, etc / name)
        (etc / "resolv.conf").touch()
        (etc / "hosts").write_text("127.0.0.1 localhost\n::1 localhost\n")
        runtime = f"/run/user/{os.getuid()}"
        subprocess.run([
            "bwrap", "--unshare-all", "--die-with-parent", "--new-session",
            "--ro-bind", "/gnu/store", "/gnu/store", "--ro-bind", str(etc), "/etc",
            "--ro-bind", "/bin", "/bin",
            "--ro-bind", "/var/guix", "/var/guix",
            "--tmpfs", "/var/guix/profiles/per-user/" + pwd.getpwuid(os.getuid()).pw_name,
            "--dir", "/var/run/libvirt", "--dir", runtime,
            "--proc", "/proc", "--dev", "/dev", "--tmpfs", "/tmp",
            "--bind", temp, home, "--chdir", home,
            "--ro-bind", str(Path(__file__).resolve()), "/test.py", "--clearenv",
            "--setenv", "HOME", home, "--setenv", "PATH", os.environ["PATH"],
            "--setenv", "XDG_RUNTIME_DIR", runtime, "--setenv", "WAYLAND_DISPLAY", "wayland-1",
            "--setenv", "DBUS_SESSION_BUS_ADDRESS", "unix:path=" + runtime + "/bus",
            *(argument for name in T3_VARIABLES for argument in ("--setenv", name, name)),
            "--setenv", "OTHER_SECRET", "excluded",
            "--setenv", "COLORTERM", "truecolor", "--setenv", "T3CODEX_HOME", "excluded",
            "--setenv", "NOT_T3_MCP_BEARER_TOKEN", "excluded",
            sys.executable, "/test.py", guix, fj], check=True)


if __name__ == "__main__":
    if len(sys.argv) == 3:
        exercise_profiles(*sys.argv[1:])
    else:
        main()
