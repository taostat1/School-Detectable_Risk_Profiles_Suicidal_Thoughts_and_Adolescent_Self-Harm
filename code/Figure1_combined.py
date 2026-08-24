"""
Figure 1: Combined 3×2 panel (B/W journal style)
  Left column (A, B, C): LPA profile line plots — grayscale + line style + marker
    A = Academic disengagement, B = Internet dependency, C = Bullying victimization
  Right column (D, E, F): Outcome bar plots — reverse-coded scores (higher = greater risk)
    D = Academic disengagement, E = Internet dependency, F = Bullying victimization
All labels in English.
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.font_manager as fm
import chardet
import itertools
import os
import os.path as osp
from statsmodels.stats.multicomp import pairwise_tukeyhsd

# ── paths ──
base_dir = osp.dirname(osp.dirname(osp.abspath(__file__)))
out_dir  = osp.join(base_dir, "output")
fig_dir  = osp.join(out_dir, "figure")
if not osp.exists(fig_dir):
    os.makedirs(fig_dir)

data_path = osp.join(out_dir, "画图所用数据.csv")
with open(data_path, 'rb') as f:
    enc = chardet.detect(f.read())['encoding']
data = pd.read_csv(data_path, encoding=enc)

# ── outcome scores are already reverse-coded in the dataset (higher = greater risk) ──
suicide_col = "我从没有过自杀的想法和准备"
harm_col    = "我从没做过故意弄伤自己的行为"

# ── fonts ──
arial_prop = fm.FontProperties(family='arial')
plt.rcParams.update({
    "font.family": "arial",
    "font.size": 9,
    "axes.labelsize": 9,
    "axes.titlesize": 9,
    "xtick.labelsize": 8,
    "ytick.labelsize": 8,
    "axes.unicode_minus": False,
})

# ── profile labels (English) ──
school_labels = ["Low Disengagement", "Moderate Strain",
                 "School Aversion", "Pervasive Disengagement"]
internet_labels = ["Low Impairment", "Moderate Impairment",
                   "Offline Distress", "Multidomain Impairment"]
bullying_labels = ["Low Exposure", "Verbal Victimization",
                   "Physical Victimization", "Mixed Verbal-Property"]

# ── indicator labels (English) ──
school_indicators = {
    "我不喜欢上学":                    "School Dislike",
    "我觉得学习很累很烦":              "Study Exhaustion",
    "我希望可以不用学习":              "Desire to Avoid Studying",
}
internet_indicators = {
    "我只要有一段时间没有上网、看手机，就会莫名地情绪低落": "Mood Without Internet",
    "长时间网游，使我的身体健康状况越来越不如以前了":     "Health Impairment",
    "由于上网使我与周围其他人的关系没以前好了，但我无法减少上网时间": "Interpersonal/Control Impairment",
}
bullying_indicators = {
    "别人给我起难听的外号，骂我，或取笑、讽刺我":       "Verbal Victimization",
    "别人强迫向我要钱，或者拿走、损坏我的东西":          "Property Victimization",
    "某些同学采用打、踢、推、撞等方式欺负我":            "Physical Victimization",
}

outcome_labels = {
    "suicide": "Suicidal Thoughts/Preparation",
    "harm":    "Self-Harm Behavior",
}

# ── B/W palette for line plots ──
bw_colors   = ["#000000", "#555555", "#999999", "#CCCCCC"]
line_styles = ['solid', 'dashed', 'dotted', 'dashdot']
markers     = ['o', 's', '^', 'D']

# ── B/W palette for bar plots ──
bar_colors  = ["#E0E0E0", "#606060"]
bar_hatches = ["", "//"]

# ── helper: draw significance bracket ──
def add_sig(ax, x1, x2, y, h, text, direction="up"):
    fs = 9 if text != "NS" else 7
    if direction == "up":
        ax.plot([x1, x1, x2, x2], [y, y+h, y+h, y], lw=0.9, c='black')
        ax.text((x1+x2)/2, y+h-0.15, text, ha='center', va='bottom', fontsize=fs,
                fontproperties=arial_prop)
    else:
        ax.plot([x1, x1, x2, x2], [y, y-h, y-h, y], lw=0.9, c='black')
        ax.text((x1+x2)/2, y-h-0.10, text, ha='center', va='top', fontsize=fs,
                fontproperties=arial_prop)


# ══════════════════════════════════════════════════════════
# 1.  Profile line-plot helper  (B/W style)
# ══════════════════════════════════════════════════════════
def draw_profile_line(ax, data, group_col, indicator_map, class_labels):
    """Draw B/W line plot of indicator means per LPA profile."""
    tmp = data.rename(columns=indicator_map)
    indicator_names = list(indicator_map.values())
    class_means = tmp.groupby(group_col)[indicator_names].mean()

    class_means_long = class_means.reset_index().melt(
        id_vars=group_col, var_name='Indicator', value_name='Mean')

    lines = []
    for i, class_id in enumerate(class_means.index):
        subset = class_means_long[class_means_long[group_col] == class_id]
        line, = ax.plot(
            subset['Indicator'], subset['Mean'],
            color=bw_colors[i], linestyle=line_styles[i],
            linewidth=1.6, marker=markers[i], markersize=4)
        lines.append(line)

    ax.set_ylabel("Indicator Mean", fontproperties=arial_prop)
    ax.grid(axis="y", linestyle="--", alpha=0.35)
    ax.spines["top"].set_visible(True)
    ax.spines["right"].set_visible(True)

    # legend: above the plotting area, no overlap with axes frame
    ax.legend(lines, class_labels, loc='upper center',
              bbox_to_anchor=(0.5, 1.22), ncol=2, fontsize=7,
              handlelength=1.8, framealpha=0.9, borderpad=0.4)

    for t in ax.get_yticklabels():
        t.set_fontproperties(arial_prop)
    for t in ax.get_xticklabels():
        t.set_fontproperties(arial_prop)
        t.set_rotation(8)
        t.set_ha('right')


# ══════════════════════════════════════════════════════════
# 2.  Outcome bar-plot helper  (reverse-coded scores, B/W)
# ══════════════════════════════════════════════════════════
def draw_outcome_bar(ax, data, group_col, class_labels):
    """Draw grouped bar chart for reverse-coded suicidal thoughts & self-harm scores,
    with Tukey HSD brackets above (suicidal thoughts) and below (self-harm)."""
    sex_col = "sex"

    sub = data.dropna(subset=[group_col, sex_col, suicide_col, harm_col]).copy()
    sub[group_col] = sub[group_col].astype('category')
    sub[sex_col]   = sub[sex_col].astype('category')

    groups = sorted(sub[group_col].unique())
    positions = np.arange(len(groups))
    bw = 0.32

    # means & SDs (already reverse-coded in dataset: higher = greater risk)
    means_su, stds_su, means_ha, stds_ha, ns = [], [], [], [], []
    for g in groups:
        gd = sub[sub[group_col] == g]
        ns.append(len(gd))
        means_su.append(gd[suicide_col].mean())
        stds_su.append(gd[suicide_col].std())
        means_ha.append(gd[harm_col].mean())
        stds_ha.append(gd[harm_col].std())

    # bars
    ax.bar(positions - bw/2, means_su, bw, yerr=stds_su, capsize=3,
           color=bar_colors[0], edgecolor="black", linewidth=0.6,
           hatch=bar_hatches[0], label=outcome_labels["suicide"])
    ax.bar(positions + bw/2, means_ha, bw, yerr=stds_ha, capsize=3,
           color=bar_colors[1], edgecolor="black", linewidth=0.6,
           hatch=bar_hatches[1], label=outcome_labels["harm"])

    # x-ticks
    ax.set_xticks(positions)
    xtick_labels = []
    for g in groups:
        idx = int(g) - 1
        xtick_labels.append(class_labels[idx])
    ax.set_xticklabels(xtick_labels, fontproperties=arial_prop, fontsize=6.5,
                        rotation=12, ha='right')

    ax.set_ylabel("Mean Score (higher = greater risk)", fontproperties=arial_prop)
    ax.grid(axis="y", linestyle="--", alpha=0.35)
    ax.spines["top"].set_visible(True)
    ax.spines["right"].set_visible(True)

    # sample sizes above bars
    for i, pos in enumerate(positions):
        max_h = max(means_su[i]+stds_su[i], means_ha[i]+stds_ha[i])
        ax.text(pos, max_h+0.15, f'n={ns[i]}', ha='center', va='bottom',
                fontsize=7, fontproperties=arial_prop)

    # ── Tukey HSD brackets: suicidal thoughts on top ──
    mc_su = pairwise_tukeyhsd(sub[suicide_col], sub[group_col], alpha=0.05)
    sig_su = []
    pairs_su = list(itertools.combinations(sorted(mc_su.groupsunique), 2))
    for idx, (ig, jg) in enumerate(pairs_su):
        if mc_su.reject[idx]:
            sig_su.append((int(ig), int(jg), mc_su.pvalues[idx]))

    unique_pairs_su = list(set((p[0], p[1]) for p in sig_su))
    unique_pairs_su.sort(key=lambda x: (x[0], -x[1]))
    unique_pairs_su.reverse()
    if unique_pairs_su:
        y_top = max(m+s for m,s in zip(means_su, stds_su)) + 0.6
        for i_g, j_g in unique_pairs_su:
            pv = next(p[2] for p in sig_su if p[0]==i_g and p[1]==j_g)
            sig = "***" if pv<0.001 else "**" if pv<0.01 else "*" if pv<0.05 else ""
            if sig:
                add_sig(ax,
                        positions[i_g-1] - bw/2,
                        positions[j_g-1] - bw/2,
                        y_top, 0.08, sig, "up")
                y_top += 0.5
        ax.set_ylim(top=y_top+0.2)

    # ── Tukey HSD brackets: self-harm below the x-axis ──
    mc_ha = pairwise_tukeyhsd(sub[harm_col], sub[group_col], alpha=0.05)
    sig_ha = []
    pairs_ha = list(itertools.combinations(sorted(mc_ha.groupsunique), 2))
    for idx, (ig, jg) in enumerate(pairs_ha):
        if mc_ha.reject[idx]:
            sig_ha.append((int(ig), int(jg), mc_ha.pvalues[idx]))

    unique_pairs_ha = list(set((p[0], p[1]) for p in sig_ha))
    unique_pairs_ha.sort(key=lambda x: (x[0], -x[1]))
    unique_pairs_ha.reverse()
    if unique_pairs_ha:
        current_y = -0.5
        for i_g, j_g in unique_pairs_ha:
            pv = next(p[2] for p in sig_ha if p[0]==i_g and p[1]==j_g)
            sig = "***" if pv<0.001 else "**" if pv<0.01 else "*" if pv<0.05 else ""
            if sig:
                add_sig(ax,
                        positions[i_g-1] + bw/2,
                        positions[j_g-1] + bw/2,
                        current_y, 0.08, sig, "down")
                current_y -= 0.5
        ax.set_ylim(bottom=current_y-0.2)

    # legend: above the plotting area, no overlap with axes frame
    ax.legend(loc='upper center', bbox_to_anchor=(0.5, 1.16), ncol=2,
              fontsize=7, framealpha=0.9, borderpad=0.4, handlelength=1.8)

    # y ticks arial
    for t in ax.get_yticklabels():
        t.set_fontproperties(arial_prop)


# ══════════════════════════════════════════════════════════
# 3.  Build the 3×2 figure
# ══════════════════════════════════════════════════════════
fig, axes = plt.subplots(3, 2, figsize=(8, 12))

# Row 1: School disengagement
draw_profile_line(axes[0, 0], data, "school_class",
                   school_indicators, school_labels)
draw_outcome_bar(axes[0, 1], data, "school_class",
                  school_labels)

# Row 2: Internet dependency
draw_profile_line(axes[1, 0], data, "internet_class",
                   internet_indicators, internet_labels)
draw_outcome_bar(axes[1, 1], data, "internet_class",
                  internet_labels)

# Row 3: Bullying victimization
draw_profile_line(axes[2, 0], data, "injury_class",
                   bullying_indicators, bullying_labels)
draw_outcome_bar(axes[2, 1], data, "injury_class",
                  bullying_labels)

# ── panel labels: column-major numbering (first column A–C, second column D–F) ──
# axes.flat is row-major: [(0,0),(0,1),(1,0),(1,1),(2,0),(2,1)]
# column-major labels:    (0,0)→A (1,0)→B (2,0)→C (0,1)→D (1,1)→E (2,1)→F
col_major_labels = ["A", "B", "C", "D", "E", "F"]
label_grid = [col_major_labels[0:3], col_major_labels[3:6]]  # [col0 rows, col1 rows]
for r in range(3):
    for c in range(2):
        ax = axes[r, c]
        ax.text(-0.10, 1.34, label_grid[c][r], transform=ax.transAxes,
                fontsize=12, fontweight='bold', fontproperties=arial_prop,
                va='bottom', ha='left')

plt.tight_layout(h_pad=3.0, w_pad=4.5, rect=[0, 0, 1, 0.96])

# ── save ──
fig.savefig(osp.join(fig_dir, "Figure1_combined.pdf"), bbox_inches="tight")
fig.savefig(osp.join(fig_dir, "Figure1_combined.tiff"), dpi=600, bbox_inches="tight")
fig.savefig(osp.join(fig_dir, "Figure1_combined.png"), dpi=500, bbox_inches="tight")

print(f"Figure 1 saved to {fig_dir}/")
print("  Figure1_combined.pdf")
print("  Figure1_combined.tiff")
print("  Figure1_combined.png")
