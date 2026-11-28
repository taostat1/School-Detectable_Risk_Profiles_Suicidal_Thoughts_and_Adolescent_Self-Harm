# School-Detectable Risk Profiles and Adolescent Self-Harm: The Role of Suicidal Ideation and Family Support

The revised analyses use 508 questionnaires and adjust for age and sex. Family support is the mean of items 4 (sharing happy events with family) and 36 (seeking family help when facing difficulties). The main model uses four bullying profiles and the sensitivity analysis uses three.

## Revised figures and supplementary tables

- [Figure 1](output/Figure/Figure1_combined.pdf) shows profile means and outcomes adjusted for age and sex.
- [Figure 2](output/Figure/Figure_2.pdf) shows the fitted pathways, tests, and conditional indirect contrasts.
- [Supplementary Tables S1–S5](output/supplementary_materials.xlsx) include the unmoderated model (S4) and family-support moderation model (S5).
- [Download the two PDF figures and Supplementary Tables S1–S5 together](output/Supplementary_Tables_and_Figures.zip).

## Multiple testing

The revision uses the Benjamini–Hochberg (BH) procedure at a 5% false discovery rate within each family of related outcome tests. The three sex comparisons, six ANCOVAs, seven main path tests, and seven interaction tests form separate families. The four-profile analysis includes nine relative indirect contrasts and nine high-minus-low contrasts, adjusted separately. Each model and bullying-profile solution is adjusted separately. Intervals are pointwise. The BLRT p values in S1–S3 are unadjusted model-selection diagnostics.

The [aggregate test results](output/BH_results.csv) give each raw and adjusted p value, test family, and corresponding table row. To reproduce them from the published [aggregate inputs](code/revision/BH_inputs.csv), run the following command from the repository root.

```sh
Rscript --vanilla code/revision/apply_BH.R
```

## Reproducing the figures and models

The [figure scripts](code/revision) use aggregate plot inputs supplied with the repository. From the repository root, run

```sh
python code/revision/Figure1.py
python code/revision/Figure2.py
```

These commands require NumPy, pandas, Matplotlib, and Arial. Figure 2 also uses XeLaTeX with TikZ and the standalone class. Both scripts accept `--data` and `--output` to select a different aggregate input or output PDF.

Researchers with authorized study files can reproduce the adjusted ANCOVAs and the four- and three-profile STEP4/STEP5 models using R. Supply the private study directory and a directory for the aggregate results.

```sh
Rscript --vanilla code/revision/run_models.R "/path/to/private-study" "/path/to/aggregate-results"
```

The private study directory supplies the five source files for STEP1 under `data/`, the four-profile LPA workspace under `github_upload/output/` or `output/`, and a three-profile workspace ending in `_3class.RData` under `敏感性分析/output/` or `output/`. The script writes `Figure1_values.json`, `Figure2_data.json`, `supplement_models.json`, and `BH_results.csv` to the requested output directory. It derives the raw p values from the fitted models and screening comparisons before applying BH. It uses student-clustered HC1 Wald tests and 10,000 student-level bootstrap draws (seed 2026) for pointwise bias-corrected intervals. Tables S4 and S5 report both solutions side by side.

## Prerequisites

Open RStudio and install **renv** by running the following command in the console:

```r
install.packages("renv")
```

renv is the virtual environment tool officially recommended by RStudio.

Next, starting from the project containing `Study on Youth Suicide.Rproj` and `renv.lock`, restore the virtual environment as follows:

1. Open the `Study on Youth Suicide.Rproj` project in RStudio (via the `.Rproj` file).

2. Run the following command in the console:

```r
renv::restore()
```

This command automatically installs all packages and the exact versions recorded in `renv.lock`, ensuring that your environment matches the original one.

## Data Availability

Access to the raw data (emotion-sonar records, physical-fitness tests, and psychological assessments) and participant-level analysis files requires permission from the data rights holder and applicable ethics and data-governance approvals. The public figures and BH results can be reproduced using the aggregate files supplied here.

## Original analysis scripts

The STEP1–STEP5 scripts directly under `code/` and the STEP4/STEP5 CSV files directly under `output/` document the earlier analysis. The revised model and figure entry points are listed above.

1. **STEP1_data_integration.R**: integrates the emotion-sonar, physical-fitness, Baoshan psychological assessment, and main psychological assessment data, and performs data cleaning (requires the restricted raw data);

2. **STEP2_demographic_differences.R**: examines demographic differences in suicidal thoughts/preparation and self-harm, corresponding to Section 2.1 of the manuscript;

3. **STEP3_latent_profile_analysis.R**: estimates the latent profile models, corresponding to Sections 2.2–2.4 of the manuscript;

4. **STEP4_mediation_model.R**: the mediation model without moderation, corresponding to the first regression step of Section 2.5 (reads `output/LPA_analysis_data.RData`);

5. **STEP5_moderated_mediation_model.R**: the original two-stage moderated mediation model (reads `output/LPA_analysis_data.RData`).
