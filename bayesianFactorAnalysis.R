---
  title: "Simulation for the multi-site"
output:
  html_document: default
pdf_document: default
---

# Bayesian design analysis settings
nLabsExpected <- 42 # The number of labs that committed to data collection
labMinN <- 70 # Minimum required N per lab
labMaxN <- 130 # Stopping N for labs
nConditions <- 5 # Number of experimental conditions
effectSize <- rnorm(1e6, .2, .05) # Distribution of plausible, yet conservatively small effect sizes, centered at .2 with 95% CI between .1 and .3

# Libraries
library(BFDA)

minN <- (nLabs*labMinN/nConditions)
maxN <- (nLabs*labMaxN/nConditions)

simH1 <- BFDA.sim(expected.ES = effectSize, type = "t.between", design = "sequential",
                   prior = list("Cauchy", list(prior.location = 0, prior.scale = sqrt(2)/2)),
                   n.min = minN, n.max = maxN, alternative = "two.sided", boundary = 10, B = 1000,
                   verbose = TRUE, cores = 4, stepsize = 10)

simH0 <- BFDA.sim(expected.ES = 0, type = "t.between", design = "sequential",
                   prior = list("Cauchy", list(prior.location = 0, prior.scale = sqrt(2)/2)),
                   n.min = minN, n.max = maxN, alternative = "two.sided", boundary = 10, B = 1000,
                   verbose = TRUE, cores = 4, stepsize = 10)

(simResults <- list(
  resultsH1 = BFDA.analyze(simH1, design = "sequential", boundary = 10),
  resultsH0 = BFDA.analyze(simH0, design = "sequential", boundary = 10)))

plot(simH1, boundary=c(1/10, 10))
plotH1 <- recordPlot()
plot(simH0, boundary=c(1/10, 10))
plotH0 <- recordPlot()