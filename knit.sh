#!/bin/bash

Rscript -e "library(knitr); knit('$1')"
Rscript -e "library(knitr); purl('$1')"
