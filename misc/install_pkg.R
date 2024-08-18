


if (! "librarian" %in% rownames(installed.packages()) ){
  install.packages("librarian")
}

# load packages if not already loaded
librarian::shelf(
  tidyverse, magrittr, tidymodels, modeldata
  , update_all = TRUE
)

tibble::tibble(classes = installed.packages()[,1] )

tmp <- installed.packages()[,1]
length(tmp)


for (idx in seq(1,length(tmp),10)){
  cat(
    paste(tmp[idx:(idx+9)], collapse=", "),",\n", sep=""
  )
}

# ============================

# Install most recent R version (4.4.1) - https://cran.r-project.org/
# install most recent RStudio - https://posit.co/download/rstudio-desktop/
# install git - https://gitforwindows.org/


install.packages("cmdstanr", repos = c('https://stan-dev.r-universe.dev', getOption("repos")))

if (! "librarian" %in% rownames(installed.packages()) ){
  install.packages("librarian")
}

# load packages if not already loaded
librarian::shelf(
  abind, airports, anytime, askpass, backports, base, base64enc, bayestestR, BH, bigD,
  BiocManager, bit, bit64, bitops, blob, boot, BradleyTerry2, brglm, broom, bslib,
  cachem, callr, cards, caret, cellranger, checkmate, cherryblossom, class, cli, clipr,
  clock, cluster, cmdstanr, codetools, colorspace, commonmark, compiler, conflicted, correlation, countdown,
  cpp11, crayon, crosstalk, curl, dagitty, data.table, DataExplorer, datasets, datawizard, DBI,
  dbplyr, Deriv, desc, diagram, dials, DiceDesign, digest, discrim, distributional, doFuture,
  doParallel, dplyr, dtplyr, dygraphs, e1071, effectsize, evaluate, extraDistr, fansi, farver,
  fastmap, fivethirtyeight, flexclust, fontawesome, forcats, foreach, forecast, foreign, fracdiff, fs,
  furrr, future, future.apply, gapminder, gargle, generics, ggdag, ggforce, ggfortify, ggokabeito,
  ggplot2, ggraph, ggrepel, glmnet, globals, glue, googledrive, googlesheets4, gower, GPfit,
  graphics, graphlayouts, grDevices, grid, gridExtra, gt, gtable, gtExtras, gtools, gtsummary,
  halfmoon, hardhat, haven, here, highr, hms, htmltools, htmlwidgets, httr, ids,
  igraph, infer, inline, insight, ipred, ISLR, ISLR2, isoband, iterators, janitor,
  jquerylib, jsonlite, juicyjuice, KernSmooth, knitr, labeling, Lahman, later, lattice, lava,
  lazyeval, lhs, librarian, lifecycle, listenv, lme4, lmtest, loo, lubridate, magrittr,
  markdown, MASS, Matrix, matrixStats, memoise, methods, mgcv, mime, minqa, mitools,
  mlbench, modelbased, modeldata, modelenv, ModelMetrics, modelr, modeltime, modeltools, munsell, networkD3,
  neuralnet, nlme, nloptr, nnet, numDeriv, openintro, openssl, padr, paletteer, palmerpenguins,
  parallel, parallelly, parameters, parsnip, patchwork, performance, pillar, pkgbuild, pkgconfig, plogr,
  plotly, plyr, polyclip, posterior, prettyunits, prismatic, pROC, processx, prodlim, profileModel,
  progress, progressr, promises, prophet, proxy, ps, purrr, quadprog, quantmod, QuickJSR,
  qvcalc, R6, ragg, ranger, rappdirs, RColorBrewer, Rcpp, RcppArmadillo, RcppEigen, RcppGSL,
  RcppParallel, RcppRoll, RcppZiggurat, reactable, reactR, readr, readxl, recipes, rematch, rematch2,
  remotes, repr, reprex, reshape2, Rfast, rlang, rmarkdown, rpart, rprojroot, rsample,
  RSQLite, rstan, rstantools, rstudioapi, rvest, sass, scales, see, selectr, sensemakr,
  sfd, shape, skimr, slider, smd, snakecase, spatial, splines, SQUAREM, StanHeaders,
  stats, stats4, stringi, stringr, survey, survival, svglite, sys, systemfonts, tcltk,
  tensorA, textshaping, tibble, tidyclust, tidygraph, tidymodels, tidyr, tidyselect, tidysmd, tidyverse,
  timechange, timeDate, timetk, tinytex, tipr, tools, tseries, tsfeatures, TTR, tune,
  tweenr, tzdb, urca, usdata, utf8, utils, uuid, V8, vctrs, viridis,
  viridisLite, vroom, warp, whisker, withr, workflows, workflowsets, xfun, xgboost, xml2,
  xts, yaml, yardstick
  , update_all = TRUE
)

