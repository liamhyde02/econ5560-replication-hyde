"""Step 4: Figure 1, attribute WTP by management regime, ours next to the paper's.

Reads outputs/table4_wtp.csv (written by step 3) and draws the three attribute
WTPs for Model 2 national and community, with 95% intervals, beside the
published Table 4 values. Writes outputs/figure1_wtp.png and .pdf.
"""
import pathlib

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

ROOT = pathlib.Path(__file__).resolve().parents[1]
OUT = ROOT / "outputs"
w = pd.read_csv(OUT / "table4_wtp.csv")
w = w[w.attribute != "ASC"]

attrs = ["fish_catch", "coral_coverage", "coastal_conditions"]
labels = ["Fish catch", "Coral coverage", "Coastal protection"]
series = [("Model 2 national", "National (top-down)", "#176E73"),
          ("Model 2 community", "Community (bottom-up)", "#B56540")]

fig, ax = plt.subplots(figsize=(8, 4.8))
x = np.arange(len(attrs))
width = 0.2
for i, (s, name, color) in enumerate(series):
    sub = w[w.series == s].set_index("attribute").loc[attrs]
    off = (i - 0.5) * 2 * width
    ax.bar(x + off - width / 2, sub.wtp_yen, width, yerr=1.96 * sub.std_err, color=color,
           capsize=3, label=f"{name}, this replication")
    ax.bar(x + off + width / 2, sub.paper_wtp, width, yerr=1.96 * sub.paper_se, color=color,
           alpha=0.4, hatch="//", capsize=3, label=f"{name}, paper Table 4")
ax.set_xticks(x, labels)
ax.set_ylabel("WTP (yen per month per percentage point)")
ax.set_title("Marginal WTP by management regime (Model 2), 95% intervals")
ax.axhline(0, color="black", linewidth=0.8)
ax.legend(fontsize=8, frameon=False)
fig.tight_layout()
fig.savefig(OUT / "figure1_wtp.png", dpi=150)
fig.savefig(OUT / "figure1_wtp.pdf", metadata={"CreationDate": None})
print(f"Wrote {OUT / 'figure1_wtp.png'} and .pdf")
