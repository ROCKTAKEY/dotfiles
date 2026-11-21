"""Check the deployed YAML with process-exporter's own /proc parser.

Usage: python3 nagi-process-classification-test.py /path/to/process-exporter
"""

import pathlib
import re
import subprocess
import sys
import tempfile

config = pathlib.Path('guix-system/config/nagi-process-exporter.yml')
cases = [
    ('strata-host', ['/opt/strata-host'], 'strata-engine'),
    ('python3', ['/bin/python3', '/opt/strata/serve/server.py'], 'strata-server'),
    ('node', ['/bin/node', '/opt/t3code-cli/dist/bin.mjs', '--port', '3773'], 't3code-3773'),
    ('node', ['/bin/node', '/opt/t3code-cli/dist/bin.mjs', '--port', '3774'], 't3code-3774'),
    ('python3', ['/bin/python3', 'other.py'], 'python3'),
    ('node', ['/bin/node', 'other.js'], 'node'),
    ('kworker', [], 'kworker'),
]

with tempfile.TemporaryDirectory(prefix='process-exporter-classification-') as tmp:
    root = pathlib.Path(tmp)
    (root / 'stat').write_bytes(pathlib.Path('/proc/stat').read_bytes())
    for pid, (comm, args, expected) in enumerate(cases, 1000):
        proc = root / str(pid)
        proc.mkdir()
        (proc / 'fd').mkdir()
        # Real kernel records keep the fixture compatible with procfs parsing;
        # only the process identity and command used for grouping are synthetic.
        for name in ('status', 'io', 'limits', 'smaps', 'cgroup', 'stat'):
            (proc / name).write_bytes(pathlib.Path('/proc/self', name).read_bytes())
        stat = (proc / 'stat').read_text()
        (proc / 'stat').write_text(re.sub(r'^\d+ \(.*?\)', f'{pid} ({comm})', stat))
        (proc / 'cmdline').write_bytes(
            b'\0'.join(arg.encode() for arg in args) + (b'\0' if args else b''))

    output = subprocess.check_output([
        sys.argv[1], '-procfs', tmp, '-config.path', str(config),
        '-threads=false', '-children=false', '-gather-smaps=true',
        '-once-to-stdout-delay=1s',
    ], text=True)
    groups = dict(re.findall(
        r'namedprocess_namegroup_num_procs\{groupname="([^"]+)"\} (\d+)', output))
    assert groups == {expected: '1' for _, _, expected in cases}, groups
    print('PASS: Strata, T3 ports, comm fallback, and empty command line')
