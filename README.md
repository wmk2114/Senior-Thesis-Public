# We Don't Do That: State Identity and Nuclear Non-Use
Is the use of nuclear weapons taboo, and if it is not, how can people's aversion to it be strengthened?

## Overview
This thesis entertains three questions commonly debated in international relations scholarship:

- Is there a taboo against nuclear weapons use?
- How do conceptions of state identity impact people's support for norm-breaking actions?
- How does the institutionalization of non-use affect the public's opposition to nuclear use?

You can find the final version of my thesis [here](https://wmk2114.github.io/my-site/pdfs/Senior%20Thesis%20Wyatt%20King.pdf]).

## Data Sources
To collect my data, I fielded an online survey experiment using Qualtrics, with respondents being recruited via Prolific. **I do not include my data in this public repository.** As part of my thesis, I also include data from a 1994 survey conducted by the US Information Agency. This data was found through [Roper iPoll](https://ropercenter.cornell.edu/ipoll/study/31084587).

## Reproduction
To reproduce my data analysis and visualization, use the .R scripts included in this repository. I broke up my data analysis across several .R files, so that I could keep my environment clean across each. **Data Preprocessing.R** is the file in which I do my data cleaning, both for the Qualtrics data and the 1994 survey data. **Main Analysis.R** hosts the analyses that I included in-text. **Robustness Tests.R** includes all of the analyses included in my appendix. **Data Visualizations.R**, true to its name, includes all of my figures. To run each of the latter three files, you will have to source specific objects from **Data Preprocessing.R** into your environment.

## AI Disclosure
In coding my analysis, I used AI in four capacities. In the order of least intensive to most:

- ChatGPT & Claude were used to line edit code if there were syntax issues
- ChatGPT was used to generate pseudocode in each of my .R files
- ChatGPT was used to turn certain lines of code I wrote into functions that could be implemented via `apply()` functions
- ChatGPT was used to create functions that implemented inverse covariance weighting and principal component analysis. I reviewed the output manually and asked Claude to do the same.

