# We Don't Do That: State Identity and Nuclear Non-Use
Is the use of nuclear weapons taboo, and if it is not, how can people's aversion to it be strengthened?

## Project Summary
This thesis entertains three questions commonly debated in international relations scholarship:

- Is there a taboo against nuclear weapons use?
- How do conceptions of state identity impact people's support for norm-breaking actions?
- How does the institutionalization of non-use affect the public's opposition to nuclear use?

You can find the final version of my thesis [here](https://wmk2114.github.io/my-site/pdfs/Senior%20Thesis%20Wyatt%20King.pdf]).

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
2. Change your working directory accordingly. Currently set as:
   - `~/Desktop/Columbia 2025/Senior Thesis/Data Analysis`
3. Source the preprocessing script first, then run the analysis and visualization scripts in order:
   - `source("Scripts/Data Preprocessing.R")`
   - `source("Scripts/Main Analysis.R")`
   - `source("Scripts/Robustness Tests.R")`
   - `source("Scripts/Data Visualizations.R")`