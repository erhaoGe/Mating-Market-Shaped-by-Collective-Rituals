# Mating Markets Shaped by Collective Religious Rituals

This repository contains the data and analysis scripts used to reproduce the results
reported in:

**Ge et al., _Mating Markets Shaped by Collective Religious Rituals_**

The repository is intended to support transparency and reproducibility of the analyses
presented in both the main text and the Supplementary Information.


## Repository contents

### 1. Analysis scripts

- `Main_text.R`  
  Scripts reproducing all analyses and figures reported in the main text.

- `SI.R`  
  Scripts reproducing all analyses and figures reported in the Supplementary Information.

**Execution order is important**:  
The scripts in `Main_text.R` must be run **before** running `SI.R`, as the SI scripts
depend on objects generated in the main-text analyses.

### 2. Data

The dataset included in this repository contains individual-level information from a
Tibetan community in western China, including:

- Social and demographic variables (e.g. age, gender, education, household background)
- Detailed participation histories in four types of collective religious rituals  
  (mouth-piercing, back-piercing, forehead-cutting, and statue-carrying)
- Marital outcomes and marriage timing
- Information on spouses’ geographic origins

The data are provided in a cleaned and processed format suitable for direct replication
of the analyses reported in the paper.


## Software requirements

All analyses were conducted in R. Required R packages are listed and installed
within the analysis scripts. Users are advised to use a recent version of R.


## Data availability and enquiries

For additional information regarding the data, please contact the first author:
Email: ucsaege@ucl.ac.uk
