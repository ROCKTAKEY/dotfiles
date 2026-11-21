"""Send IPv4 and IPv6 traffic through nagi's rules in private network namespaces."""
import json
from pathlib import Path
import socket
import subprocess
import sys

RULES = """
(use-modules (gnu services) (gnu services networking) (gnu system) (guix gexp)
             (srfi srfi-1) (json))
(define os (primitive-load "guix-system/config/nagi.scm"))
(define config (service-value (find (lambda (s) (eq? iptables-service-type (service-kind s)))
                                    (operating-system-services os))))
(display (scm->json-string
          (vector (plain-file-content (iptables-configuration-ipv4-rules config))
                  (plain-file-content (iptables-configuration-ipv6-rules config)))))
"""


def run(*args, **kwargs):
    subprocess.run(args, check=True, timeout=30, **kwargs)


def exercise(rules, iproute, inetutils, utils, iptables):
    ip = iproute + "/sbin/ip"
    peer = [utils + "/bin/nsenter", "--net=/run/netns/peer"]
    run(ip, "netns", "add", "peer")
    run(ip, "link", "add", "host0", "type", "veth", "peer", "name", "client0")
    run(ip, "link", "set", "client0", "netns", "peer")
    for prefix, device, ipv4, ipv6 in [
        ([], "host0", "192.168.11.1/24", "fd00::1/64"),
        (peer, "client0", "192.168.11.16/24", "fd00::2/64"),
    ]:
        run(*prefix, ip, "link", "set", device, "up")
        run(*prefix, ip, "addr", "add", ipv4, "dev", device)
        run(*prefix, ip, "-6", "addr", "add", ipv6, "dev", device, "nodad")
    for restore, content in zip(("iptables-restore", "ip6tables-restore"), rules):
        run(iptables + "/sbin/" + restore, input=content, text=True)

    for family, address, ping in [(socket.AF_INET, "192.168.11.1", "ping"),
                                  (socket.AF_INET6, "fd00::1", "ping6")]:
        run(*peer, inetutils + "/bin/" + ping, "-n", "-c", "1", address)
        # An unlisted UDP port is the negative control: a missing firewall
        # would otherwise satisfy every allow-case.
        for port in (5353, 5354):
            with socket.socket(family, socket.SOCK_DGRAM) as listener:
                listener.bind((address, port))
                listener.settimeout(2)
                run(*peer, sys.executable, "-c",
                    "import socket,sys; s=socket.socket(int(sys.argv[1]), socket.SOCK_DGRAM); "
                    "s.sendto(b'probe', (sys.argv[2], int(sys.argv[3])))",
                    str(int(family)), address, str(port))
                try:
                    delivered = listener.recv(16)
                except TimeoutError:
                    delivered = None
                expected = b"probe" if port == 5353 else None
                assert delivered == expected, (address, port, delivered)


if __name__ == "__main__":
    if sys.argv[1:]:
        exercise(json.load(sys.stdin), *sys.argv[1:])
    else:
        rules = subprocess.check_output(["guix", "repl", "--", "/dev/stdin"], input=RULES, text=True)
        paths = subprocess.check_output(["guix", "build", "iproute2", "inetutils", "util-linux",
                                         "iptables"], text=True).splitlines()
        paths = [next(p for p in paths if (Path(p) / tool).is_file())
                 for tool in ("sbin/ip", "bin/ping6", "bin/nsenter", "sbin/iptables-restore")]
        run("bwrap", "--unshare-all", "--die-with-parent", "--uid", "0", "--gid", "0",
            "--cap-add", "CAP_NET_ADMIN", "--cap-add", "CAP_NET_RAW", "--cap-add", "CAP_SYS_ADMIN",
            "--ro-bind", "/gnu/store", "/gnu/store", "--ro-bind", "/etc", "/etc",
            "--tmpfs", "/run", "--dir", "/var", "--symlink", "/run", "/var/run",
            "--proc", "/proc", "--dev", "/dev",
            "--ro-bind", str(Path(__file__).resolve()), "/test.py",
            sys.executable, "/test.py", *paths, input=rules, text=True)
