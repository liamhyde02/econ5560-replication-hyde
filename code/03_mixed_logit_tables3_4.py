"""Step 3: Models 1 and 2 (Table 3) and willingness to pay (Table 4), Python xlogit.

Taken from the team repo (code/estimate_mixed_logit_observed_choice.py), with
output paths changed for this repo. It is the Python version of Zoey Miles's
mlogit specification (ReplicationRevised.qmd); with the same 500 Halton draws
the team found the two agree on every coefficient to within 1e-7 of a SE.

Specification (paper Eqs. 1-2, Table 3):
  * Model 1: fish_catch, coral_coverage, coastal_conditions, contribution (yen),
    ASC; all independent normal random parameters.
  * Model 2: Model 1 plus each term interacted with community = (survey_type == 2),
    also normal random parameters.
  * Panel by respondent (record_id), choice occasions by Cid, 500 Halton draws,
    simulated maximum likelihood.
  * PAPER_TABLE3 holds the published mean coefficients and significance stars,
    and PAPER_TABLE4 the published WTP and standard errors. Standard deviations
    are not compared because their signs are not identified.
  * Start values: Model 1 from the fixed-coefficient logit with small SDs;
    Model 2 from Model 1 (interactions at 0). mlogit-style defaults (SDs 0.1)
    fail to converge (the team checked this in its repo).
  * Marginal WTP = -b_k / b_pay (Eq. 3 with its minus sign restored), yen per
    month per percentage point; delta-method standard errors. For the community
    group, WTP = -(b_k + g_k) / (b_pay + g_pay).

Writes outputs/table3_coefficients.csv, outputs/table4_wtp.csv and
outputs/model_summary.json.
"""
import json
from importlib.metadata import version
import pathlib
import sys

import numpy as np
import pandas as pd
from scipy.stats import norm
from xlogit import MixedLogit, MultinomialLogit

ROOT = pathlib.Path(__file__).resolve().parents[1]
DATA = ROOT / "data/raw/MPA_journal_data_with_choice_for_CalPolyFA2026.xlsx"
OUT = ROOT / "outputs"
OUT.mkdir(parents=True, exist_ok=True)
N_DRAWS = 500

d = pd.read_excel(DATA, sheet_name="Data")
d = d.sort_values(["record_id", "Cid", "Alt"]).reset_index(drop=True)
assert (d.groupby("Cid").size() == 3).all()
assert (d.groupby("Cid").Choice.sum() == 1).all()
assert (d.loc[d.Alt == 3, "ASC"] == 0).all()

d["community"] = (d.survey_type == 2).astype(int)
base = ["fish_catch", "coral_coverage", "coastal_conditions", "contribution", "ASC"]
inter = [f"{v}_comm" for v in base]
for v, w in zip(base, inter):
    d[w] = d[v] * d["community"]

common = dict(y=d.Choice.values, ids=d.Cid.values, alts=d.Alt.values)

PAPER_TABLE3 = {
    ("Model 1", "fish_catch"): (0.0069, "**"), ("Model 1", "coral_coverage"): (0.0360, "***"),
    ("Model 1", "coastal_conditions"): (0.0096, "***"), ("Model 1", "contribution"): (-0.0055, "***"),
    ("Model 1", "ASC"): (8.220, "***"),
    ("Model 2", "fish_catch"): (0.0035, ""), ("Model 2", "coral_coverage"): (0.0299, "***"),
    ("Model 2", "coastal_conditions"): (0.005, ""), ("Model 2", "contribution"): (-0.0052, "***"),
    ("Model 2", "ASC"): (8.461, "***"),
    ("Model 2", "fish_catch_comm"): (0.00758, ""), ("Model 2", "coral_coverage_comm"): (0.0168, "**"),
    ("Model 2", "coastal_conditions_comm"): (0.0105, "*"),
    ("Model 2", "contribution_comm"): (-0.0011, "**"), ("Model 2", "ASC_comm"): (0.225, ""),
}
PAPER_TABLE4 = {
    ("Model 1 (pooled)", "fish_catch"): (1.244, 0.503), ("Model 1 (pooled)", "coral_coverage"): (6.524, 0.616),
    ("Model 1 (pooled)", "coastal_conditions"): (1.744, 0.530), ("Model 1 (pooled)", "ASC"): (1489.0, 147.8),
    ("Model 2 national", "fish_catch"): (0.677, 0.680), ("Model 2 national", "coral_coverage"): (5.766, 0.821),
    ("Model 2 national", "coastal_conditions"): (0.958, 0.734), ("Model 2 national", "ASC"): (1631.7, 161.8),
    ("Model 2 community", "fish_catch"): (1.754, 0.745), ("Model 2 community", "coral_coverage"): (7.383, 0.920),
    ("Model 2 community", "coastal_conditions"): (2.447, 0.766), ("Model 2 community", "ASC"): (1373.8, 137.5),
}


def fit_mxl(names, start):
    m = MixedLogit()
    m.fit(X=d[names].values, varnames=names, panels=d.record_id.values,
          randvars={v: "n" for v in names}, n_draws=N_DRAWS, halton=True,
          init_coeff=start, maxiter=5000, verbose=0, **common)
    return m


mnl = MultinomialLogit()
mnl.fit(X=d[base].values, varnames=base, verbose=0, **common)
m1 = fit_mxl(base, np.r_[mnl.coeff_, np.abs(mnl.coeff_) / 2])
k = len(base)
m2 = fit_mxl(base + inter, np.r_[m1.coeff_[:k], np.zeros(k),
                                 np.abs(m1.coeff_[k:]), np.abs(m1.coeff_[k:]) / 2])
for name, m in [("Model 1", m1), ("Model 2", m2)]:
    if not m.convergence:
        sys.exit(f"{name} did not converge")
assert m1.loglikelihood >= mnl.loglikelihood - 1e-6
assert m2.loglikelihood >= m1.loglikelihood - 1e-6


def stars(p):
    return "***" if p < 0.01 else "**" if p < 0.05 else "*" if p < 0.10 else ""


def coef_table(m, label):
    rows = []
    for n, b, se, p in zip(m.coeff_names, m.coeff_, m.stderr, m.pvalues):
        rows.append(dict(model=label, term=n, estimate=b, std_err=se, z=b / se,
                         p_value=p, sig=stars(p)))
    return pd.DataFrame(rows)


coefs = pd.concat([coef_table(m1, "Model 1"), coef_table(m2, "Model 2")])
coefs["paper_estimate"] = [PAPER_TABLE3.get((m, t), (np.nan, ""))[0] for m, t in zip(coefs.model, coefs.term)]
coefs["paper_sig"] = [PAPER_TABLE3.get((m, t), (np.nan, ""))[1] for m, t in zip(coefs.model, coefs.term)]
coefs.to_csv(OUT / "table3_coefficients.csv", index=False)


def wtp(m, num, den):
    """-sum(b[num]) / sum(b[den]) with delta-method SE."""
    names = list(m.coeff_names)
    b, V = np.asarray(m.coeff_), np.asarray(m.covariance)
    gi = [names.index(n) for n in num]
    pi = [names.index(n) for n in den]
    a, c = b[gi].sum(), b[pi].sum()
    grad = np.zeros_like(b)
    grad[gi] = -1 / c
    grad[pi] += a / c**2
    est, se = -a / c, float(np.sqrt(grad @ V @ grad))
    return est, se


rows = []
for attr in ["fish_catch", "coral_coverage", "coastal_conditions", "ASC"]:
    for label, m, num, den in [
        ("Model 1 (pooled)", m1, [attr], ["contribution"]),
        ("Model 2 national", m2, [attr], ["contribution"]),
        ("Model 2 community", m2, [attr, f"{attr}_comm"], ["contribution", "contribution_comm"]),
    ]:
        est, se = wtp(m, num, den)
        p = 2 * norm.sf(abs(est / se))
        paper_wtp, paper_se = PAPER_TABLE4[(label, attr)]
        rows.append(dict(series=label, attribute=attr, wtp_yen=est, std_err=se,
                         p_value=p, sig=stars(p), paper_wtp=paper_wtp, paper_se=paper_se))
wtps = pd.DataFrame(rows)
wtps.to_csv(OUT / "table4_wtp.csv", index=False)

summary = {
    "data": DATA.name,
    "respondents": int(d.record_id.nunique()),
    "choice_occasions": int(d.Cid.nunique()),
    "halton_draws": N_DRAWS,
    "loglik": {"logit": mnl.loglikelihood, "model1": m1.loglikelihood,
               "model2": m2.loglikelihood},
    "converged": {"model1": bool(m1.convergence), "model2": bool(m2.convergence)},
    "software": {"xlogit": version("xlogit"), "numpy": np.__version__, "pandas": pd.__version__},
}
(OUT / "model_summary.json").write_text(json.dumps(summary, indent=2))
print(json.dumps(summary, indent=2))
print(coefs.round(4).to_string(index=False))
print(wtps.round(3).to_string(index=False))
