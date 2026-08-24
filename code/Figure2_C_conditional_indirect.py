"""
Figure C: Conditional Indirect Effects (Moderated Mediation)
=============================================================
Source data: STEP5_moderated_mediation_BC_results.csv
             Rows: total_ind_school_*, total_ind_internet_*, total_ind_injury_*

Plots the indirect effect (a*b) at three levels of W (family connectedness):
  - Low  (-1 SD)
  - Mid  (mean)
  - High (+1 SD)

Error bars = 95% Bias-Corrected Bootstrap CI
"""
import os
import numpy as np
import matplotlib.pyplot as plt
from matplotlib import rcParams

# ---------------------------------------------------------------
# 1. Data (BC Bootstrap from STEP5 — n_boot = 10000)
# ---------------------------------------------------------------
# x-axis positions
w_levels = ["Low\n(-1 SD)", "Mid\n(mean)", "High\n(+1 SD)"]
x_pos = np.array([0, 1, 2])

# X1: Academic-disengagement (School)
x1_est  = np.array([1.175, 0.336, -0.233])
x1_lo   = np.array([0.44, -0.20, -0.95])
x1_hi   = np.array([1.92,  0.79,  0.27])
x1_sig  = ["*", "", ""]

# X2: Bullying-victimization (Injury)
x2_est  = np.array([1.221, 0.592, 0.140])
x2_lo   = np.array([0.48,  0.11, -0.66])
x2_hi   = np.array([1.87,  2.64,  3.07])
x2_sig  = ["*", "*", ""]

# X3: Internet-related-impairment (Internet)
x3_est  = np.array([0.430, 0.221, 0.069])
x3_lo   = np.array([-0.30, -0.22, -0.36])
x3_hi   = np.array([ 1.31,  0.68,  0.52])
x3_sig  = ["", "", ""]

# ---------------------------------------------------------------
# 2. Plot styling
# ---------------------------------------------------------------
rcParams["font.family"]      = "Arial"
rcParams["font.size"]        = 17
rcParams["axes.linewidth"]   = 1.0
rcParams["axes.spines.top"]  = False
rcParams["axes.spines.right"]= False
rcParams["xtick.major.width"]= 1.0
rcParams["ytick.major.width"]= 1.0

fig, ax = plt.subplots(figsize=(6.5, 5.0))

# Black-and-white palette — distinguish by line style + marker + fill
c_all = "black"

def plot_line(x, est, marker, ls, fill, label):
    """Plot one series with black line, no error bars."""
    ax.plot(
        x, est,
        marker=marker,
        color=c_all,
        markerfacecolor=fill,
        markeredgecolor="black",
        markeredgewidth=1.2,
        markersize=10,
        linewidth=1.8,
        linestyle=ls,
        label=label,
    )

# X1: solid line, filled circle
plot_line(x_pos, x1_est, "o", "-", "black",
          r"$X_1$: Academic-disengagement")
# X2: dashed line, filled square
plot_line(x_pos, x2_est, "s", "--", "black",
          r"$X_2$: Bullying-victimization")
# X3: dotted line, open (white) triangle
plot_line(x_pos, x3_est, "^", ":", "white",
          r"$X_3$: Internet-related-impairment")

# Reference horizontal line at y = 0
ax.axhline(0, color="gray", linewidth=0.8, linestyle="-", alpha=0.6)

# ---------------------------------------------------------------
# 3. Axes
# ---------------------------------------------------------------
ax.set_xticks(x_pos)
ax.set_xticklabels(w_levels, fontsize=16)
ax.set_xlim(-0.4, 2.4)

ax.set_ylim(-0.4, 1.6)
ax.set_yticks(np.arange(-0.4, 1.7, 0.4))
ax.tick_params(axis="y", labelsize=16)

ax.set_xlabel("Perceived family connectedness (Z)", fontsize=16, labelpad=10)
ax.set_ylabel(r"Indirect effect  ($a \times b$ conditional on $Z$)", fontsize=16, labelpad=10)

# ---------------------------------------------------------------
# 4. No annotation box (removed per user request)
# ---------------------------------------------------------------

# ---------------------------------------------------------------
# 5. No title (label placed in LaTeX layout)
# ---------------------------------------------------------------

# ---------------------------------------------------------------
# 6. Legend (inside plot, top-right)
# ---------------------------------------------------------------
leg = ax.legend(
    loc="upper right",
    fontsize=14,
    frameon=False,
    handlelength=2.0,
    borderpad=0.4,
    labelspacing=0.5,
)

# ---------------------------------------------------------------
# 7. Save
# ---------------------------------------------------------------
out_dir = r"d:\Study on Youth Suicide\output"
os.makedirs(out_dir, exist_ok=True)
out_path = os.path.join(out_dir, "figure_C_conditional_indirect.pdf")

plt.tight_layout()
plt.savefig(out_path, bbox_inches="tight", facecolor="white")
plt.close()

print(f"Figure C saved: {out_path}")
print(f"X1 (School): {x1_est.tolist()}, CI_lo {x1_lo.tolist()}, CI_hi {x1_hi.tolist()}")
print(f"X2 (Injury): {x2_est.tolist()}, CI_lo {x2_lo.tolist()}, CI_hi {x2_hi.tolist()}")
print(f"X3 (Internet): {x3_est.tolist()}, CI_lo {x3_lo.tolist()}, CI_hi {x3_hi.tolist()}")
