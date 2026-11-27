"""Render Figure 2 from summary statistics. Run: python Figure2.py
Requires matplotlib, XeLaTeX (with TikZ/standalone), and the Arial font.
The input is the four-profile model using the mean of Q4 (sharing happy
events) and Q36 (seeking family help), adjusted for both age and sex.
"""

import argparse
import json
import math
import shutil
import subprocess
import tempfile
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.ticker import MaxNLocator

HERE = Path(__file__).resolve().parent
TEMPLATE = HERE / "Figure2.tex"
DOMAINS = (
    ("injury", "Bullying victimization", ("in2", "in3", "in4")),
    ("school", "Academic disengagement", ("s1", "s2", "s4")),
    ("internet", "Internet-related impairment", ("i2", "i3", "i4")),
)
PROBES = ("low", "mean", "high")
STYLES = (("o", "-", "black"), ("s", "--", "black"), ("^", ":", "white"))
NUMBERS = ("One", "Two", "Three")


def panel_c(estimates, destination):
    """Plot the STEP5 sum of the three relative indirect contrasts per domain."""
    plt.rcParams.update({"font.family": "Arial", "font.size": 9, "pdf.fonttype": 42,
                         "axes.linewidth": 0.8, "axes.spines.top": False,
                         "axes.spines.right": False, "xtick.major.width": 0.8,
                         "ytick.major.width": 0.8})
    fig, ax = plt.subplots(figsize=(3.80, 3.26))
    fig.subplots_adjust(left=0.158, right=0.983, bottom=0.218, top=0.978)
    observed = [0.0]
    for i, ((domain, title, codes), (marker, line, face)) in enumerate(zip(DOMAINS, STYLES), 1):
        series = [math.fsum(estimates[f"ind_{code}_{probe}"]["est"] for code in codes)
                  for probe in PROBES]
        ax.plot(range(3), series, marker=marker, linestyle=line, color="black",
                markerfacecolor=face, markeredgecolor="black", markeredgewidth=0.8,
                markersize=5.0, linewidth=1.0, label=rf"$X_{i}$: {title}")
        observed.extend(series)
    span = max(observed) - min(observed) or 1.0
    ax.set_xlim(-0.40, 2.40)
    ax.set_ylim(min(observed)-0.14*span, max(observed)+0.40*span)
    ax.yaxis.set_major_locator(MaxNLocator(nbins=6, steps=[1, 2, 2.5, 5, 10]))
    ax.axhline(0, color="0.72", linewidth=0.65, zorder=0)
    ax.set_xticks(range(3), ["Low\n(−1 SD)", "Mid\n(mean)", "High\n(+1 SD)"])
    ax.tick_params(axis="both", labelsize=8.1, length=3, pad=2.5)
    ax.set_xlabel(r"Family support ($Z$)", fontsize=8.8, labelpad=3)
    ax.set_ylabel("Sum of conditional indirect contrasts", fontsize=8.6, labelpad=2.4)
    ax.legend(loc="upper right", fontsize=7.2, frameon=False, handlelength=1.9,
              handletextpad=0.55, borderpad=0.3, labelspacing=0.30)
    fig.savefig(destination, bbox_inches="tight", pad_inches=0.022)
    plt.close(fig)


def compact_number(number):
    number_text = f"{number:.3f}"
    return number_text.replace("-0.", "-.").removeprefix("0") if abs(number) < 1 else number_text


def symbol(i, path):
    if path == "b":
        return "b"
    if path == "b_by_family":
        return "b_z"
    letter = "a" if path.startswith("a") else "c'"
    suffix = "z" if path.endswith("by_family") else ""
    return rf"{letter}_{{{i}{suffix}}}"


def p_value(p):
    if not 0 <= p <= 1:
        raise ValueError(f"Invalid p value {p}")
    if p < .001:
        return "<.001"
    return (f"{p:.4f}" if p < .005 else f"{p:.3f}").lstrip("0")


def direction(values, p):
    """Summarize signs of all contrasts from a significant joint path test."""
    if p >= 0.05:
        return "n.s."
    if all(value > 0 for value in values):
        return "Positive"
    if all(value < 0 for value in values):
        return "Negative"
    return "Mixed"


def tex_values(estimates, tests):
    # Use the same test for each arrow style and its table entry.
    sections = (
        (r"$a$-path", ("a",)),
        (r"$b$-path", ("b",)),
        (r"Direct paths (bypassing $M$)", ("c_prime",)),
        ("Moderation effects", ("a_by_family", "b_by_family", "c_prime_by_family")),
    )
    rows, styles = [], []
    for heading, paths in sections:
        rows.append(r"\midrule" + "\n" + r"\multicolumn{5}{@{}l}{\itshape " + heading + r"}\\")
        for path in paths:
            scalar = path.startswith("b")
            interaction = path.endswith("by_family")
            domains = ["all"] if scalar else [domain[0] for domain in DOMAINS]
            for i, domain in enumerate(domains, 1):
                test = tests[(domain, path)]
                p = test["p"]
                if scalar:
                    entry = estimates["b_z" if interaction else "b"]
                    est, lower, upper = (compact_number(entry[k]) for k in ("est", "lo", "hi"))
                    stat = rf"{est}\,[{lower},{upper}]"
                    connector = r"Z\times b" if interaction else r"M\to Y"
                    style = "second"
                    contrast_values = [entry["est"]]
                else:
                    prefix = "a" if path.startswith("a") else "c"
                    stat = rf"\chi^2({int(test['df'])})={test['chi2']:.2f}"
                    outcome = "M" if prefix == "a" else "Y"
                    codes = DOMAINS[i - 1][2]
                    labels = [code[:-1] + "h" + code[-1] if interaction else code for code in codes]
                    contrast_values = [estimates[f"{prefix}_{label}"]["est"] for label in labels]
                    connector = (rf"Z\times {symbol(i, 'a' if prefix == 'a' else 'c_prime')}"
                                 if interaction else rf"X_{i}\to {outcome}")
                    style = ("first" if prefix == "a" else "direct") + NUMBERS[i-1]
                if interaction:
                    style += "Z"
                styles.append(style + "/.style={" + ("sigline" if p < 0.05 else "nsline") + "}")
                stars = sum(p < threshold for threshold in (0.05, 0.01, 0.001))
                printed_p = p_value(p) + ("^{" + "*"*stars + "}" if stars else "")
                cells = ["$" + entry + "$" for entry in (symbol(i,path),connector,stat,printed_p)]
                cells.append(direction(contrast_values, p))
                rows.append(" & ".join(cells) + r"\\")
    return (r"\newcommand{\ResultPathStyles}{\tikzset{" + ",\n".join(styles) + "}}\n"
            + r"\newcommand{\ResultTable}{" + "\n" + "\n".join(rows) + "\n}\n")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--data", type=Path, default=HERE / "Figure2_data.json")
    parser.add_argument("--output", type=Path,
                        default=HERE.parents[1] / "output" / "Figure" / "Figure_2.pdf")
    args = parser.parse_args()
    data = json.loads(args.data.read_text(encoding="utf-8-sig"))
    estimates = data["estimates"]
    tests = {(row["domain"], row["path"]): row for row in data["tests"]}
    if (data["family_items"] != [4, 36] or data["covariates"] != ["age", "sex"]
            or data["n"] != 508 or data["profiles"] != 4 or len(tests) != 14):
        raise ValueError("Expected the age- and sex-adjusted four-profile, two-item model.")

    with tempfile.TemporaryDirectory() as tmp:
        build = Path(tmp)
        panel_c(estimates, build / "Figure2C.pdf")
        (build / "Figure2_values.tex").write_text(tex_values(estimates, tests), encoding="utf-8")
        shutil.copyfile(TEMPLATE, build / "Figure2.tex")
        run = subprocess.run(["xelatex", "-interaction=nonstopmode", "-halt-on-error", "Figure2.tex"],
                             cwd=build, capture_output=True)
        log = run.stdout.decode(errors="replace")
        if run.returncode or "Overfull " in log:
            raise RuntimeError(log[-6500:])
        args.output.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(build / "Figure2.pdf", args.output)


if __name__ == "__main__":
    main()
