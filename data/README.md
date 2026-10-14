# Data

## `restricted/` (not shared)
DHS microdata cannot be redistributed under the DHS Program terms of use. To reproduce the analysis, register with the DHS Program (https://dhsprogram.com), download the Children's Recode (KR) files, and place the following here:

| File | Created by | Description |
|---|---|---|
| `wf_breastfeeding_dhs_22Jun2026.dta` | `01_KR_BF_workfile_construction_22_June_2026.do` | Appended DHS KR files (input to `02_DHS-data-research.do`) |
| `DHS_harmonized.dta` | `02_DHS-data-research.do` | Harmonized child-level dataset |
| `DHS_methods_analysis_2000-2022.dta` | `03_Methods_Paper_Final.do` | Main analysis sample (2000–2022) |
| `DHS_methods_analysis.dta` | `03_Methods_Paper_Final.do` | Supplementary sample (2015–2022, age in days) |
