## Developmental Instability in Wild Nigerian Olive Baboons

This study analyzes dental fluctuating asymmetry (FA) in a natural death sample of olive baboons (*Papio anubis*) from Gashaka Gumti National Park, Nigeria -- an ecological outlier population living in the wettest habitat of any studied baboon group. Using the FA10b index across permanent premolar and molar dentition, the analysis tests whether baboons show human-like FA trends by tooth dimension and class, and whether weaning or reproductive onset represent periods of elevated developmental instability. Lengths carry significantly greater FA than breadths in both sexes, mirroring the human pattern, and premolar lengths show the highest values overall. Neither weaning nor reproductive onset produces elevated FA; instead, the juvenile period between these life history stages -- indexed by fourth premolar FA -- emerges as the most developmentally demanding phase.

### Published Paper

Hoover, K.C., Gelipter, E., Sommer, V., & Kovarovic, K. (2021). Developmental instability in wild Nigerian olive baboons (*Papio anubis*). *PeerJ*, 9, e11832. DOI: [10.7717/peerj.11832](https://doi.org/10.7717/peerj.11832)

### Repository Contents

This repo contains two sets of files. All files prefixed `revised-` are revised scripts and figures from the original publication. Revisions offer a fully reproducible end-to-end pipeline from raw data through ETL to results - including data cleaning and wrangling scripts (revised from originals or created where none existed), code modernization, and the addition of environment management to future-proof the revised scripts. All other files are associated with the original publication. Occasionally, an original script may be updated with annotations for clarity; the commit history will note any such changes.

#### Original files

Original publication scripts are organized in `paper-scripts/` and original data files in `paper-data/`. The original pipeline followed the Palmer and Strobeck (2003) step-by-step protocol using a combination of R scripts and manual data entry into the PS Excel worksheet. Scripts are batched by leading numbers: Script1 tests for normality; Script2 batches assess ME per measurement set; Script3 tests for trait size asymmetry via scatterplots and Rosner's ESD; Script4 examines trait size differences for FA; Script5 generates descriptive and univariate values for the PS worksheet; Script6 reassesses ME on the cleaned dataset; Script7 tests trait size dependency via correlation; Script8 explores human FA trends and tests hypotheses; Script9 generates plots. Raw data files are suffixed `EG data`; processed data files are prefixed by step number.

#### Revised files

The revised pipeline replaces the original Excel-dependent workflow with a fully automated R pipeline. Scripts are in `scripts-revised/`:

- `revised1-wrangling.R` -- converts raw Excel replicate sheets to pipeline-ready long-format CSV
- `revised2-inspection.R` -- ME assessment, outlier removal, DA and antisymmetry tests, trait size dependency, and FA10b calculation; uses `fa-functions.R` and `fa-constants.R`
- `revised3-analysis.R` -- FA10b trends, human trends exploration, hypothesis testing, life history, and figure generation
- `revised-analysis-paper-data.R` -- replicates the published paper scripts with modernized code and environment capture
- `fa-functions.R` -- shared functions used across inspection and analysis scripts
- `fa-constants.R` -- shared file path constants and pipeline decision parameters

### Portfolio Page

The [portfolio page](https://kchoover14.github.io/baboon-stress) includes a full project narrative, key findings, and figures.

### Tools & Technologies

**Languages** | R

**Tools** | RStudio | GitHub

**Packages** | dplyr | tidyr | readr | readxl | openxlsx | ggplot2 | ggridges | gridExtra | cowplot | plotly | htmlwidgets | car | outliers | DescTools | Hmisc | coin

**Environment**

- `renv.lock` and `renv/` -- restore with `renv::restore()`

### Expertise

Variance-based statistical methods applied to biological signal extraction in noisy field data, including multi-stage measurement error assessment, data quality screening, and reproducible pipeline design.

### License

- Code and scripts are licensed under the [MIT License](LICENSE).
- Data, figures, and written content © Kara C. Hoover, licensed under [CC BY-NC-ND 4.0](https://creativecommons.org/licenses/by-nc-nd/4.0/).