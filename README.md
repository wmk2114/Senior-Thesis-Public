# We Don't Do That: State Identity and Nuclear Non-Use
This repository contains the analysis code, supporting files, and documentation for a senior thesis project in political science examining whether nuclear weapons use is treated as a taboo and how national identity alters public support for nuclear non-use.

## Project Summary
This project explores the relationship between nuclear non-use norms and state identity in shaping attitudes toward nuclear attack. It combines an original online survey experiment with historical public opinion evidence to answer three core research questions:

- Is there a normative taboo against nuclear weapons use?
- How do appeals to national identity affect support for nuclear violence?
- Are identity and non-use frames associated with different treatment effects across political groups?

## Links
- Final thesis PDF: [Senior Thesis — Wyatt King](https://wmk2114.github.io/my-site/pdfs/Senior%20Thesis%20Wyatt%20King.pdf)
- Charles A. Beard Prize information: [Charles A. Beard Prize](https://www.oah.org/awards/dissertation-awards/charles-a-beard-prize/)

## Repository Structure
The analysis folder contains the following files and directories:

```text
├── Data Visualizations/
├── Scripts/
│   ├── Data Preprocessing.R
│   ├── Data Visualizations.R
│   ├── Main Analysis.R
│   └── Robustness Tests.R
├── TEX/
├── Thesis Data.sav
├── USIA Survey/
└── DDW-0000C-14.xls
```

## File Descriptions
- `Data Preprocessing.R`
  - Cleans and prepares raw survey and archival data.
  - Creates the analysis-ready objects used by later scripts.
- `Main Analysis.R`
  - Runs the primary models reported in the main thesis text.
  - Estimates OLS regressions, heterogeneous treatment effects, and manipulation / treatment checks.
- `Robustness Tests.R`
  - Runs supplementary robustness checks and supplemental analyses for the appendix.
- `Data Visualizations.R`
  - Generates figures for the project, including response proportion bar charts and linear probability model plots.
- `Data Visualizations/`
  - Folder intended for exported figure files produced by `Data Visualizations.R`.
- `TEX/`
  - Contains LaTeX source files for the final thesis and supporting materials.
- `Thesis Data.sav`
  - SAV-format dataset used in the project.
- `USIA Survey/`
  - Archival survey materials from the US Information Agency dataset.
- `DDW-0000C-14.xls`
  - Auxiliary data file used in replication and documentation.

## Data Sources
The analysis uses two primary data sources:

1. Original survey experiment
   - Fielded via Qualtrics and recruited through Prolific.
   - The raw respondent-level dataset is not included in this public repository.
2. USIA 1994 survey data
   - Historical public opinion evidence from the US Information Agency.
   - Documented in the `USIA Survey/` folder and collected via Roper iPoll.

> Note: Raw data is not publicly distributed here. The repository provides analysis scripts and derived files needed to reproduce the reported results.

## Dependencies
The analysis requires the following R packages:

- `tidyverse`
- `haven`
- `survey`
- `readxl`
- `speedycode`
- `readroper`
- `modelsummary`
- `glmnet`
- `estimatr`
- `ggplot2`
- `showtext`

### Install dependencies
Run the following command in R to install required packages:

```r
install.packages(c(
  "tidyverse",
  "haven",
  "survey",
  "readxl",
  "speedycode",
  "readroper",
  "modelsummary",
  "glmnet",
  "estimatr",
  "ggplot2",
  "showtext"
))
```

## Reproducing the Analysis
To reproduce the analysis:

1. Open R or RStudio.
2. Set the working directory to this folder:
   - `~/Desktop/Columbia 2025/Senior Thesis/Data Analysis`
3. Source the preprocessing script first, then run the analysis and visualization scripts in order:
   - `source("Scripts/Data Preprocessing.R")`
   - `source("Scripts/Main Analysis.R")`
   - `source("Scripts/Robustness Tests.R")`
   - `source("Scripts/Data Visualizations.R")`

## Notes
- `Data Preprocessing.R` creates the cleaned data objects used by later scripts.
- `Main Analysis.R` contains the main regression tables and treatment effect estimates.
- `Robustness Tests.R` contains supplemental checks for the appendix.
- `Data Visualizations.R` produces figures and table-ready plots.
- The main analyses use midpoint imputation and robust HC2 standard errors.
- Respondents who fail manipulation or comprehension checks are retained unless otherwise noted in the scripts.

## Acknowledgments
This project was completed as a senior thesis in the Columbia University Department of Political Science.

