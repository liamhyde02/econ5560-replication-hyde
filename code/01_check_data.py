"""Step 1: check that the raw data are the files the team received, unchanged.

Checks the SHA-256 of both raw files against the values recorded when they
were obtained, checks the panel structure, and checks that the observed-choice
spreadsheet matches the Dataverse file on all 19 shared columns.
Writes outputs/data_checks.csv. Stops with an error if any check fails.
"""
import hashlib
import pathlib

import pandas as pd

ROOT = pathlib.Path(__file__).resolve().parents[1]
RAW = ROOT / "data" / "raw"
OUT = ROOT / "outputs"
OUT.mkdir(exist_ok=True)

DTA = RAW / "data_impact_community_based_mgt_on_MPAs.dta"
XLSX = RAW / "MPA_journal_data_with_choice_for_CalPolyFA2026.xlsx"
EXPECTED_SHA = {
    DTA.name: "c6715751197eab29d541627ec853d1a4ec8fb49b6f2d003a36989463af91f53c",
    XLSX.name: "a6126ee32f170633f26fc5e468db3cdc0437c740c4d6f654d1bc4039ae96780d",
}

rows = []


def check(name, ok, detail):
    rows.append(dict(check=name, passed=bool(ok), detail=detail))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}: {detail}")
    if not ok:
        raise SystemExit(f"Data check failed: {name}")


for f in (DTA, XLSX):
    sha = hashlib.sha256(f.read_bytes()).hexdigest()
    check(f"sha256 {f.name}", sha == EXPECTED_SHA[f.name], sha)

d = pd.read_stata(DTA, convert_categoricals=False)
x = pd.read_excel(XLSX, sheet_name="Data")

check("dta rows", len(d) == 14832, f"{len(d)} rows (respondent x choice set x alternative)")
check("dta columns", d.shape[1] == 19, f"{d.shape[1]} columns; no chosen-alternative variable")
check("respondents", d.record_id.nunique() == 824, f"{d.record_id.nunique()} respondents (paper reports 827)")
check("choice sets", d.Cid.nunique() == 4944, f"{d.Cid.nunique()} choice sets, 6 per respondent")
check("3 alternatives per set", (d.groupby("Cid").size() == 3).all(), "two MPA options plus a status quo")
check("one status quo per set", (d.groupby("Cid").ASC.apply(lambda s: (s == 0).sum()) == 1).all(), "ASC = 0 once per set")

check("xlsx rows", len(x) == 14832, f"{len(x)} rows")
check("one choice per set", (x.groupby("Cid").Choice.sum() == 1).all(), "exactly one Choice = 1 per Cid")
check("status quo is Alt 3", (x.loc[x.Alt == 3, "ASC"] == 0).all(), "Alt 3 always has ASC = 0")

shared = list(d.columns)
m = d.merge(x[shared], on="record_iidd", suffixes=("_dta", "_xlsx"))
same = all(
    (pd.to_numeric(m[f"{c}_dta"], errors="coerce") == pd.to_numeric(m[f"{c}_xlsx"], errors="coerce")).all()
    for c in shared if c != "record_iidd"
)
check("xlsx matches dta", len(m) == 14832 and same, "all 19 shared columns identical after merging on record_iidd")

pd.DataFrame(rows).to_csv(OUT / "data_checks.csv", index=False)
print(f"Wrote {OUT / 'data_checks.csv'}")
