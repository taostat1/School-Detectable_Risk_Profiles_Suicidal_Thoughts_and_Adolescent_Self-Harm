# School-Detectable Risk Profiles, Suicidal Thoughts, and Adolescent Self-Harm

The revised analyses use 508 questionnaires, adjust for age and sex, and report unadjusted p values. Family support is the mean of items 4 (sharing happy events with family) and 36 (seeking family help when facing difficulties). The main model uses four bullying profiles and the sensitivity analysis uses three.

## Revised figures and supplementary tables

- [Figure 1](output/Figure/Figure1_combined.pdf) shows profile means and outcomes adjusted for age and sex.
- [Figure 2](output/Figure/Figure_2.pdf) shows the fitted pathways, tests, and conditional indirect contrasts.
- [Supplementary Tables S1–S5](output/supplementary_materials.xlsx) include the unmoderated model (S4) and family-support moderation model (S5).

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

The private study directory supplies the five source files for STEP1 under `data/`, the four-profile LPA workspace under `github_upload/output/` or `output/`, and a three-profile workspace ending in `_3class.RData` under `敏感性分析/output/` or `output/`. The script writes `Figure1_values.json`, `Figure2_data.json`, and `supplement_models.json`. It uses student-clustered HC1 Wald tests and 10,000 student-level bootstrap draws (seed 2026) for pointwise bias-corrected intervals. Tables S4 and S5 report both solutions side by side.

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

The raw data (emotion-sonar records, physical-fitness tests, and psychological assessments) are **not publicly available due to ethical restrictions**. The participant-level analysis data used for STEP4–STEP5 and Figure 1 are restricted. Running these analyses requires an authorized local copy. STEP1–STEP3 require access to the raw data and are provided for full transparency of the data-integration and latent-profile-analysis workflow.

## Original analysis scripts

The STEP1–STEP5 scripts directly under `code/` and the STEP4/STEP5 CSV files directly under `output/` document the earlier analysis. The revised model and figure entry points are listed above.

1. **STEP1_data_integration.R**: integrates the emotion-sonar, physical-fitness, Baoshan psychological assessment, and main psychological assessment data, and performs data cleaning (requires the restricted raw data);

2. **STEP2_demographic_differences.R**: examines demographic differences in suicidal thoughts/preparation and self-harm, corresponding to Section 2.1 of the manuscript;

3. **STEP3_latent_profile_analysis.R**: estimates the latent profile models, corresponding to Sections 2.2–2.4 of the manuscript;

4. **STEP4_mediation_model.R**: the mediation model without moderation, corresponding to the first regression step of Section 2.5 (reads `output/LPA_analysis_data.RData`);

5. **STEP5_moderated_mediation_model.R**: the original two-stage moderated mediation model (reads `output/LPA_analysis_data.RData`).
