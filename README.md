# mwshiny: Multi-Window Shiny

## Overview

mwshiny, or Multi-Window Shiny, is an R package that extends Shiny apps over multiple connected windows. This a development version of the package. For a stable release, please download from CRAN (more information about that [here](https://CRAN.R-project.org/package=mwshiny)).

## Install

To install a stable release from CRAN, enter the following in your R console:

```{r}
install.packages("mwshiny")
```

To directly download this unstable release, use the devtools package to install from GitHub:

```{r}
# if you don't have devtools installed, uncomment the following line:
# install.packages("devtools")
devtools::install_github("delosh653/mwshiny")
```

## Examples

For examples with a simple two-window system, please check out the vignette "mwshiny: Creating a Multi-Window Shiny App", which can be found in the vignettes folder or [here](https://cran.r-project.org/web/packages/mwshiny/vignettes/mws-vignette.html). Information on how to find and specify JS and CSS package dependencies can be found in the vignette "Specifying Package JS and CSS Dependencies with mwshiny", again found in the vignettes folder or [here](https://cran.r-project.org/web/packages/mwshiny/vignettes/mws-dependencies.html).

Further examples were developed for useR! 2019 in Toulouse, and can be found in the repository [delosh653/mwshiny-examples](https://github.com/delosh653/mwshiny-examples), along with the slides presented at that conference. These examples include the a two monitor system, a controller-driving system, and an example in the [Rensselaer Campfire](https://empac.rpi.edu/program/research/campfire).

## Contact

Hannah De los Santos /
email: delosh@rpi.edu /
Rensselaer Polytechnic Institute
