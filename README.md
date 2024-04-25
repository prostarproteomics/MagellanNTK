<!-- badges: start -->
  [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R-CMD-check](https://github.com/prostarproteomics/MagellanNTK/workflows/R-CMD-check/badge.svg)](https://github.com/prostarproteomics/MagellanNTK/actions)
<!-- badges: end -->
  
# Magellan

Magellan is a R package which proposes a framework to navigate between steps of a complex data processing tool when the succession of processes is mostly chronological.

For example, if a process is composed of three steps, then it is very easy to run the first steps, then the second and finaly the last one. It is like a dataflow manager.

Moreover, this navigation system, which is at the core of Magellan, can by used at several levels. It can then be possible to define, for example, a super-process (i.e. a pipeline) in which each step is a whole process containing itself several steps.


## Install

devtools::install_github('prostarproteomics/MagellanNTK')


# List of articles

* In the menu 'Get started', xxxx
* 'Articles > Create a pipeline': for developers
* 'Articles > Create a process': for developers
* 



# Future developments

* Generalize Magellan for more than two levels,
* implements a shiny app to help users to develop a module process
* Integrate synctatic analyzers for modules
