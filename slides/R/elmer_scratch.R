# created November 04, 2024

# https://github.com/tidyverse/elmer

# check if 'librarian' is installed and if not, install it
if (! "librarian" %in% rownames(installed.packages()) ){
  install.packages("librarian")
}

# load packages if not already loaded
librarian::shelf(
  magrittr, tidyverse, malcolmbarrett/causalworkshop, ggplot2, patchwork, tidyverse/elmer
)

# source("R/ggdag-mask.R")
# source("R/setup.R")

# theme_set(theme_bw(base_size = 18) + theme(legend.position = "top"))

utils::vignette(package = "elmer")