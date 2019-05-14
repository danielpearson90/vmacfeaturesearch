
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vmacfeaturesearch

[![Travis-CI Build
Status](https://travis-ci.org/danielpearson90/vmacfeaturesearch.svg?branch=master)](https://travis-ci.org/danielpearson90/vmacfeaturesearch)
[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/danielpearson90/vmacfeaturesearch/master/?urlpath=rstudio)

This repository contains the data and analysis code for our project
investigating whether overt attention to reward-related distractors can
be suppressed under feature search conditions.

Our pre-print is online here:

> Pearson, D., Watson, P., Cheng, P., & Le Pelley, M. (2019). *Overt
> attentional capture by reward-related stimuli overcomes inhibitory
> suppression*. PsyArXiv, Accessed 14 May 2019. Online at
> <https://doi.org/10.31234/osf.io/db9hg>

### How to cite

Please cite this compendium as:

> Pearson, D., Watson, P., Cheng, P., & Le Pelley, M. (2019).
> Investigating attentional suppression of reward-related distractors.
> Retrieved from <https://osf.io/yrdzv>

### Contents

  - [:file\_folder: analysis](/analysis): Contains analysis report in R
    Markdown format, as well as R script format.
  - [:file\_folder: analysis/paper](/analysis/paper): R markdown source
    document for the manuscript.
  - [:file\_folder: analysis/data](/analysis/data): Data files that have
    been dericed from the raw eye-tracker data. The raw eye-gaze files
    are very large and so are not stored here. They can be found at:
    <https://osf.io/yrdzv/>.
  - [:file\_folder: analysis/figures](/analysis/figures): Figures
    generated in the analysis.

### Reproducibility

This repository is organized as a reproducible research compendium.
Click the ![Binder](https://mybinder.org/badge_logo.svg) button above to
explore in an interactive RStudio session (this takes approximate 15-20
mins to build). Binder uses
[rocker-project.org](https://rocker-project.org) Docker images to ensure
a consistent and reproducible computational environment. These Docker
images can also be used locally.

To explore the code locally, clone or download this repository into
RStudio or your perferred environment and install the compendium by
running `devtools::install(dep=TRUE)`.

This compendium is checked by Travis-CI continuous integration. Click
the ![Build
Status](https://travis-ci.org/cboettig/noise-phenomena.svg?branch=master)
button above for details.

### Licenses

**Text and figures :**
[CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/), Copyright (c)
2019 Daniel Pearson.

**Code :** See the [DESCRIPTION](DESCRIPTION) file

**Data :** [CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/),
Copyright (c) 2019 Daniel Pearson.
