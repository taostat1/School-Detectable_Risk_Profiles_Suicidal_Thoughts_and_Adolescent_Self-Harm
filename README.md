# School-Detectable Risk Profiles, Suicidal Thoughts, and Adolescent Self-Harm

This document accompanies the supplementary code for the manuscript. Please read the following instructions carefully before running the code.

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

The raw data (emotion-sonar records, physical-fitness tests, and psychological assessments) are **not publicly available due to ethical restrictions**. The cleaned analysis dataset (`output/LPA_analysis_data.RData`) is provided, so that the mediation and moderated-mediation analyses (STEP4–STEP5) and both figures can be fully reproduced. STEP1–STEP3 require access to the raw data and are provided for full transparency of the data-integration and latent-profile-analysis workflow.

## About the Supplementary Files

All code is stored in the `code` folder and all output in the `output` folder. The supplementary tables (Tables S1–S5) are provided in `output/supplementary_materials.xlsx`.

Run the scripts in numerical order (STEP1 through STEP5):

1. **STEP1_data_integration.R**: integrates the emotion-sonar, physical-fitness, Baoshan psychological assessment, and main psychological assessment data, and performs data cleaning (requires the restricted raw data);

2. **STEP2_demographic_differences.R**: examines demographic differences in suicidal thoughts/preparation and self-harm, corresponding to Section 2.1 of the manuscript;

3. **STEP3_latent_profile_analysis.R**: estimates the latent profile models, corresponding to Sections 2.2–2.4 of the manuscript;

4. **STEP4_mediation_model.R**: the mediation model without moderation, corresponding to the first regression step of Section 2.5 (reads `output/LPA_analysis_data.RData`);

5. **STEP5_moderated_mediation_model.R**: the two-stage moderated mediation model, corresponding to the second regression step of Section 2.5 (reads `output/LPA_analysis_data.RData`);

6. Figures 1 and 2 in the manuscript were produced in Python with `code/Figure1_combined.py` and `code/create_figure2_v8.py`, respectively; `Figure1_combined.py` reads `output/figure_data.csv`, and `create_figure2_v8.py` reads the STEP4/STEP5 output tables in the `output` folder.
