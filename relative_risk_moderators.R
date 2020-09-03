#########################################
# Code for relative risk meta-analysis
#   with moderators
# 
# Adapted from:
#   https://stat.ethz.ch/pipermail/r-sig-meta-analysis/2018-July/000917.html
#   www.theanalysisfactor.com/the-difference-between-relative-risk-and-odds-ratios
#   https://www.wikihow.com/Calculate-Relative-Risk
#
#########################################

################
# Load library
################
library(metafor)

################
# Load data
################
dat <- get(data("dat.bcg"))
head(dat)
dat_rr <- metafor::escalc(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = dat.bcg)
head(dat_rr)

# do mixed effects meta-analysis
rr_rma <- metafor::rma(yi = yi, vi = vi, method = "REML", data = dat_rr, mods = ~factor(alloc))
# rr_rma <- metafor::rma(yi = yi, vi = vi, method = "REML", data = dat_rr)

# get confidence intervals
summary(rr_rma)
metafor::confint.rma.uni(object = rr_rma)

# forest plot
metafor::forest.rma(x = rr_rma)

# get z-value for testing the null hypothesis
rr_rma$zval
rr_rma$pval

# get I^2 and tau^2
# tau^2 is the variance in true effect sizes
# I^2 statistic tells us what proportion of the observed variance in effect size
#   reflects differences in true effect size rather than sampling error
rr_rma$I2
rr_rma$tau2

# Q-value : the Q-statistic gives a test of the null hypothesis that all studies 
#     share a common effect size
# Video by Sergei Bronstein
#   Meta-analysis | basic lecture 03
#   https://www.youtube.com/watch?v=O6qDlov5-ls
rr_rma$QE
summary(rr_rma)
cat("The Q-value is greater than the degrees of freedom. Hence we can reject the null hypothesis that all studies share a common effect size.")
cat("The observed variation in effect size is greater than what we would expect by chance")

#############################################
# model selection ?
#############################################
# try a simpler model without moderators
rr_rma_reduced <- metafor::rma(yi = yi, vi = vi, method = "REML", data = dat_rr)

# get confidence intervals and AIC score
summary(rr_rma_reduced)

#############################################
# model selection
#############################################
anova(rr_rma, rr_rma_reduced)
# look at AICc and p-value in summary

# also do permutation test
metafor::permutest(x = rr_rma)
metafor::permutest(x = rr_rma_reduced)
