# Methodological approaches to reducing misclassification bias in exclusive breastfeeding measurement

Code for:

> Khoury SM, Fink G, Grünig M, Joseph A-C, Wallenborn JT. Methodological approaches to reducing misclassification bias in exclusive breastfeeding measurement. *American Journal of Epidemiology*. 2026:kwag197. https://doi.org/10.1093/aje/kwag197

The analysis uses Demographic and Health Survey (DHS) data (2000–2022) to quantify how methodological choices change estimates of exclusive breastfeeding (EBF) prevalence: handling of missing feeding data (DHS, UNICEF/WHO, complete case, multiple imputation), prelacteal feeding, age weighting, and estimating EBF *under* six months versus *at* six months (and at 183 days).

## Repository structure

```
DHS_Methods_EBF/
├── 01_data_preparation/          Stata
│   ├── 01_KR_BF_workfile_construction_22_June_2026.do   Step 01: loops over the DHS KR files, extracts variables, appends
│   │                                                  → wf_breastfeeding_dhs_22Jun2026.dta
│   ├── 01a_KR_variable_selection_22_June_2026.do         called by step 01: variables kept from each KR file
│   ├── 01b_KR_country_codes_29_Sept_2021.do              called by step 01: country names and codes
│   ├── 02_DHS-data-research.do        Step 02: harmonizes the appended DHS Children's Recode files → DHS_harmonized.dta
│   └── 03_Methods_Paper_Final.do         Step 03: analysis samples → DHS_methods_analysis_2000-2022.dta (main)
│                                              and DHS_methods_analysis.dta (2015–2022, age in days)
├── 02_analysis/                  R
│   ├── 04_DHS_Methods_Paper_10072026_simple_av.R          Step 04: main analysis (Table 1, Figs 3–7, Supplementary Fig 1)
│   ├── 05_DHS_Methods_Paper_vf_10072026_sensitivity.R     Step 05: sensitivity analysis (model-based under-6-month estimates)
│   └── 06_DHS_Methods_Paper_vf_supp.analysis_23062026.R   Step 06: supplementary analysis, 2015–2022 surveys, age in days (Supplementary Fig 2)
├── data/
│   └── restricted/               DHS microdata — NOT included (see data/README.md)
└── results/                      Survey-level and pooled estimates (aggregate, shareable)
```

## How to reproduce

1. **Obtain DHS microdata** (free, on registration) from the [DHS Program](https://dhsprogram.com): the Children's Recode (KR) Stata files listed in `01_KR_BF_workfile_construction_22_June_2026.do`. Put them in one folder.
2. **Stata:** at the top of each do-file, set `global root` to the repository folder (an absolute path) and, in step 01, `global dhs_raw` to the folder with the KR files. Then run, in order: `01_KR_BF_workfile_construction_22_June_2026.do`, `02_DHS-data-research.do`, `03_Methods_Paper_Final.do`. All intermediate datasets are written to `data/restricted/`.
3. **R:** with the working directory set to `02_analysis/`, run the three R scripts. Each writes its tables (Excel) and figures to `02_analysis/` (the main script writes to `02_analysis/EBF_methods_outputs/`).

## What each step does

- **Harmonization (`02_DHS-data-research.do`):** keeps living children who live with their mother (last-born child), derives age in months and, for surveys from 2015 onward, age in days; harmonizes current breastfeeding status, 24-hour recall of liquids and foods, and prelacteal feeding across survey rounds; and constructs the EBF definitions.
- **Analysis samples (`03_Methods_Paper_Final.do`):** applies survey-level exclusions and creates the 2000–2022 and 2015–2022 analysis files.
- **Analysis (R):** survey-weighted estimates per survey; multiple imputation by chained equations (m = 20, seed 1452) for missing feeding data; survey-weighted logistic regression with a quartic in age to estimate prevalence at exactly six months (or 183 days); Rubin's rules across imputations; pooled averages across surveys.

## Software

- Stata (64-bit)
- R 4.5.1 with `survey`, `mice`, `dplyr`, `tidyr`, `tibble`, `haven`, `ggplot2`, `openxlsx`, `broom`, `tidyverse`

## Data availability

DHS microdata are third-party data and cannot be redistributed; they are freely available on registration from the DHS Program. The authors had no special access privileges. Aggregate survey-level and pooled estimates are provided in `results/` (CC BY 4.0).

## License

Code: MIT License (see `LICENSE`). Results files: [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).

## Contact

Stephanie M. Khoury — stephanie.khoury@swisstph.ch
Swiss Tropical and Public Health Institute, Allschwil, Switzerland
