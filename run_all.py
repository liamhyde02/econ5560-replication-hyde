"""Master script: runs the whole replication from raw data to final outputs.

Usage, from the repo root:
    python run_all.py > logs/run_log.txt 2>&1
    pip freeze > logs/requirements.txt

Runs the scripts in code/ in order. Each one reads data/raw/ or an earlier
output and writes to outputs/. The run stops at the first error.
"""
import pathlib
import platform
import subprocess
import sys
import time
from importlib.metadata import version

ROOT = pathlib.Path(__file__).resolve().parent
STEPS = [
    "code/01_check_data.py",
    "code/02_table2_descriptives.py",
    "code/03_mixed_logit_tables3_4.py",
    "code/04_figure1_wtp.py",
]

(ROOT / "outputs").mkdir(exist_ok=True)
start = time.time()
for step in STEPS:
    t0 = time.time()
    print(f"\n===== {step} =====", flush=True)
    subprocess.run([sys.executable, str(ROOT / step)], cwd=ROOT, check=True)
    print(f"----- {step} finished in {time.time() - t0:.1f} s", flush=True)

print(f"\nAll steps finished. Total run time: {time.time() - start:.1f} s")

# Python's equivalent of R's sessionInfo(): interpreter, platform and package versions.
print("\nSession info")
print(f"Python {sys.version.split()[0]} on {platform.platform()}")
for pkg in ["numpy", "pandas", "scipy", "openpyxl", "xlogit", "matplotlib"]:
    print(f"{pkg} {version(pkg)}")
