"""
Figure 1: Combined 3×2 panel (B/W journal style)
  Left column (A, B, C): LPA profile line plots — grayscale + line style + marker
    A = Academic disengagement, B = Internet dependency, C = Bullying victimization
  Right column (D, E, F): Outcome bar plots — age- and sex-adjusted means with clustered pointwise 95% CIs
    D = Academic disengagement, E = Internet dependency, F = Bullying victimization
Run "python code/revision/Figure1.py" from the repository root.
The aggregate inputs are produced by code/revision/run_models.R.
"""

import argparse
import json
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.font_manager as fm
import numpy as np
import pandas as pd

source_dir = Path(__file__).resolve().parent

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
    "pdf.fonttype": 42,
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
    "我只要有一段时间没有上网、看手机，就会莫名地情绪低落": "Low Mood When Offline",
    "长时间网游，使我的身体健康状况越来越不如以前了":     "Health Impairment",
    "由于上网使我与周围其他人的关系没以前好了，但我无法减少上网时间": "Interpersonal/Control\nImpairment",
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

# ══════════════════════════════════════════════════════════
# 1.  Profile line-plot helper  (B/W style)
# ══════════════════════════════════════════════════════════
def draw_profile_line(ax, profiles, domain, indicator_map, class_labels):
    """Draw B/W line plot of indicator means per LPA profile."""
    rows = profiles.loc[profiles.domain == domain]
    class_means = rows.pivot(index="group", columns="indicator", values="est")
    if list(class_means.index) != [1, 2, 3, 4] or list(class_means.columns) != [1, 2, 3]:
        raise ValueError(f"Incomplete profile means for {domain}")

    lines = []
    for i, class_id in enumerate(class_means.index):
        line, = ax.plot(
            list(indicator_map.values()), class_means.loc[class_id].to_numpy(),
            color=bw_colors[i], linestyle=line_styles[i],
            linewidth=1.6, marker=markers[i], markersize=4, clip_on=False)
        lines.append(line)

    ax.set_ylabel("Indicator Mean", fontproperties=arial_prop)
    ax.grid(axis="y", linestyle="--", alpha=0.35)
    ax.spines["top"].set_visible(True)
    ax.spines["right"].set_visible(True)

    # legend: above the plotting area, no overlap with axes frame
    ax.legend(lines, class_labels, loc='lower center',
              bbox_to_anchor=(0.5, 1.025), ncol=2, fontsize=7,
              handlelength=1.8, framealpha=0.9, borderpad=0.4)

    for t in ax.get_yticklabels():
        t.set_fontproperties(arial_prop)
    for t in ax.get_xticklabels():
        t.set_fontproperties(arial_prop)
        t.set_fontsize(7.8)
        t.set_rotation(8)
        t.set_ha('right')
        t.set_rotation_mode("anchor")


# ══════════════════════════════════════════════════════════
# 2.  Outcome bar-plot helper  (reverse-coded scores, B/W)
# ══════════════════════════════════════════════════════════
def draw_outcome_bar(ax, profiles, domain, class_labels, means):
    """Plot marginal means from the age- and sex-adjusted ANCOVAs."""
    groups = np.arange(1, 5)
    positions = groups - 1
    counts = profiles.loc[(profiles.domain == domain) & (profiles.indicator == 1)].sort_values("group").n
    if counts.sum() != 508:
        raise ValueError(f"Incomplete class counts for {domain}")
    endpoints = []
    width = 0.32
    for j, (outcome, label) in enumerate(zip(("thoughts", "self_harm"), outcome_labels.values())):
        rows = means.loc[(means.domain == domain) & (means.outcome == outcome)].sort_values("group")
        if not np.array_equal(rows.group, groups) or not np.array_equal(rows.n, counts):
            raise ValueError(f"Profile counts do not match the model for {domain}/{outcome}")
        estimate = rows.est.to_numpy()
        lower = rows.lower.to_numpy()
        upper = rows.upper.to_numpy()
        ax.bar(positions + (j - 0.5) * width, estimate - 1, width, bottom=1,
               yerr=[estimate - lower, upper - estimate], capsize=3,
               color=bar_colors[j], edgecolor="black", linewidth=0.6,
               hatch=bar_hatches[j], label=label)
        endpoints.append(upper)
    for pos, top, count in zip(positions, np.max(endpoints, axis=0), counts):
        ax.text(pos, top + 0.10, f"n={count}", ha="center", va="bottom", fontsize=7)
    ax.set_xticks(positions, class_labels, ha="right", rotation=12,
                  rotation_mode="anchor", fontsize=7.5)
    ax.set_xlim(-0.5, 3.5)
    ax.set_ylabel("Age- and sex-adjusted mean (95% CI)")
    ax.grid(axis="y", linestyle="--", alpha=0.35)
    ax.legend(loc="lower center", bbox_to_anchor=(0.5, 1.035), ncol=2,
              fontsize=7, framealpha=0.9, borderpad=0.4, handlelength=1.8)


# ══════════════════════════════════════════════════════════
# 3.  Build the 3×2 figure
# ══════════════════════════════════════════════════════════
def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--data", type=Path, default=source_dir / "Figure1_values.json")
    parser.add_argument("--output", type=Path,
                        default=source_dir.parents[1] / "output" / "Figure" / "Figure1_combined.pdf")
    args = parser.parse_args()
    summary = json.loads(args.data.read_text(encoding="utf-8-sig"))
    profiles = pd.DataFrame(summary["profiles"])
    means = pd.DataFrame(summary["figure1"])
    if summary["n"] != 508 or set(summary["covariates"]) != {"age", "sex"} or len(means) != 24 or len(profiles) != 36:
        raise ValueError("Figure 1 requires the complete sample and all 24 adjusted means.")

    domains = (("school", school_indicators, school_labels),
               ("internet", internet_indicators, internet_labels),
               ("injury", bullying_indicators, bullying_labels))
    fig, axes = plt.subplots(3, 2, figsize=(8.7, 10.6))
    fig.subplots_adjust(left=0.09, right=0.985, top=0.91, bottom=0.105,
                        hspace=0.80, wspace=0.33)
    for row, (domain, indicators, labels) in enumerate(domains):
        left, right = axes[row]
        draw_profile_line(left, profiles, domain, indicators, labels)
        draw_outcome_bar(right, profiles, domain, labels, means)
        left.set_ylim(1, 5)
        right.set_ylim(min(1, means.lower.min()), max(5, means.upper.max() + 0.31))
        for ax, letter in zip((left, right), ("ABC"[row], "DEF"[row])):
            ax.set_yticks(range(1, 6))
            ax.set_axisbelow(True)
            ax.text(-0.10, 1.34, letter, transform=ax.transAxes, fontsize=12,
                    fontweight="bold", va="bottom", ha="left")
    args.output.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(args.output, bbox_inches="tight", pad_inches=0.075)
    plt.close(fig)


if __name__ == "__main__":
    main()
