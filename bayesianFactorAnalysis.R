#' ---
#' title: "Bayesian design analysis"
#' date: "`r Sys.Date()`"
#' output:
#'    html_document:
#'       toc: true
#'       toc_float: true
#'       code_folding: show
#'       fig_retina: 2
#' always_allow_html: yes
#' ---

#'# Simulation settings
# Number of simulations
nSim <- 10000
# Bayes factor boundary
bfBoundary <- 10

# Prior distribution
priorDistribution <- "Cauchy"
# Distribution location
priorLocation <- 0
# Distribution scale
priorScale <- sqrt(2)/2

# Hedged number of labs
nLabsExpected <- 30 # The number of labs that committed to data collection was 41.
# Minimum required N per lab
labMinN <- 70
# Stopping N for labs
labMaxN <- 120 
# Number of experimental conditions
nConditions <- 5
# Distribution of plausible, yet conservatively small effect sizes
effectSize <- rnorm(1e6, .2, .05) # Centered at .2 with 95% CI between .1 and .3

#+ setup, include = FALSE
knitr::opts_chunk$set(echo=FALSE, warning = FALSE)
# Libraries
library(BFDA)
library(parallel)
# simResults <- readRDS("simResults.RDS")

minN <- (nLabsExpected*labMinN/nConditions)
maxN <- (nLabsExpected*labMaxN/nConditions)

simH1 <- BFDA.sim(expected.ES = effectSize, type = "t.between", design = "sequential",
                   prior = list(priorDistribution, list(prior.location = priorLocation, prior.scale = priorScale)),
                   n.min = minN, n.max = maxN, alternative = "two.sided", boundary = bfBoundary, B = nSim,
                   verbose = TRUE, cores = detectCores(), stepsize = 10)

simH0 <- BFDA.sim(expected.ES = 0, type = "t.between", design = "sequential",
                   prior = list(priorDistribution, list(prior.location = priorLocation, prior.scale = priorScale)),
                   n.min = minN, n.max = maxN, alternative = "two.sided", boundary = bfBoundary, B = nSim,
                   verbose = TRUE, cores = detectCores(), stepsize = 10)

simResults <- list(
  simObjectH1 = simH1,
  simObjectH0 = simH0,
  resultsH1 = BFDA.analyze(simH1, design = "sequential", boundary = 10),
  resultsH0 = BFDA.analyze(simH0, design = "sequential", boundary = 10))

#'# Bayesian design analysis results

#+ include = TRUE
#'## H1 results
simResults$simObjectH1
simResults$resultsH1
#'### Sequential design analysis plot for H1
plot(simResults$simObjectH1, boundary=c(1/10, 10))
plotH1 <- recordPlot()

#'## H0 results
simResults$simObjectH0
simResults$resultsH0
#'### Sequential design analysis plot for H0
plot(simResults$simObjectH0, boundary=c(1/10, 10))
plotH0 <- recordPlot()

saveRDS(simResults, "simResults.RDS")