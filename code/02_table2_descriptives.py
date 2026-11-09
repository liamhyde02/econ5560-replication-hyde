"""Step 2: reproduce Table 2 (respondent characteristics by survey version).

Follows the team's R code (Zoey Miles's MeansTable.qmd and code/week7_outputs.R
in the team repo): one row per respondent, gender code 2 = female and birth
code 1 = Okinawa (both confirmed by the provider's data dictionary), income
category codes 1-6 used literally, as the paper appears to do.
Writes outputs/table2_descriptives.csv with the published values beside ours.
"""
import pathlib

import numpy as np
import pandas as pd

ROOT = pathlib.Path(__file__).resolve().parents[1]
OUT = ROOT / "outputs"
OUT.mkdir(exist_ok=True)

d = pd.read_stata(ROOT / "data/raw/data_impact_community_based_mgt_on_MPAs.dta", convert_categoricals=False)
dem = ["record_id", "survey_type", "gender", "age", "annual_income", "birth_location", "children_HH"]
p = d[dem].drop_duplicates()
assert p.record_id.is_unique, "demographics vary within a respondent"
p = p.assign(
    age=pd.to_numeric(p.age),
    children_HH=pd.to_numeric(p.children_HH),
    female=(p.gender == 2).astype(int),
    born_okinawa=(p.birth_location == 1).astype(int),
    income_code=pd.to_numeric(p.annual_income),
)

VARS = [
    ("female", "Female share"),
    ("age", "Age (years; top answer is 56 and over)"),
    ("income_code", "Income category code (1-6)"),
    ("born_okinawa", "Born in Okinawa share"),
    ("children_HH", "Children under 18 in household"),
]
# Shah et al. (2019), Table 2: (mean, sd) for national, then community. Birthplace SD not printed.
PAPER = {
    "female": ((0.50, 0.50), (0.52, 0.50)),
    "age": ((41.25, 9.78), (41.95, 9.36)),
    "income_code": ((3.63, 1.43), (3.70, 1.41)),
    "born_okinawa": ((0.68, np.nan), (0.70, np.nan)),
    "children_HH": ((0.83, 1.13), (0.75, 1.03)),
}

rows = []
for g, gname in [(1, "National (top-down)"), (2, "Community (bottom-up)")]:
    sub = p[p.survey_type == g]
    for v, label in VARS:
        x = sub[v]
        pm, ps = PAPER[v][g - 1]
        rows.append(dict(
            group=gname, variable=label, n=len(x),
            mean=round(x.mean(), 4), sd_sample=round(x.std(ddof=1), 4), sd_population=round(x.std(ddof=0), 4),
            paper_mean=pm, paper_sd=ps,
            mean_matches_2dp=round(x.mean(), 2) == pm,
        ))
t2 = pd.DataFrame(rows)
t2.to_csv(OUT / "table2_descriptives.csv", index=False)

inc = []
for g, gname in [(1, "National (top-down)"), (2, "Community (bottom-up)")]:
    x = p.loc[p.survey_type == g, "income_code"]
    inc.append(dict(group=gname, codes_used="1-6 (as in paper)", n=len(x), mean=round(x.mean(), 4)))
    inc.append(dict(group=gname, codes_used="1-5 (code 6 = no answer dropped)", n=int((x != 6).sum()), mean=round(x[x != 6].mean(), 4)))
pd.DataFrame(inc).to_csv(OUT / "table2_income_sensitivity.csv", index=False)

print(t2.to_string(index=False))
print(f"Respondents: {len(p)} ({(p.survey_type == 1).sum()} national, {(p.survey_type == 2).sum()} community)")
print(f"Means matching the paper to 2 decimals: {int(t2.mean_matches_2dp.sum())} of {len(t2)}")
