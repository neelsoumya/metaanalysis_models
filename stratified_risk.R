#############################################
# Epidemiological stratified risk models
#   in metafor
#   1. Risk difference (RD)
#   2. Relative risk (RR)
#   3. Odds ratio (OR)
#
# Adapted from:
#   www.metafor-project.org/doku.php/analyses:rothman2008
#
#############################################

#################
# Load library
#################
library(metafor)


#################
# Load data
#################
dat <- data.frame( age = c("age < 55", "age >= 55"),
                   ai  = c(8,22),
                   bi  = c(98,76),
                   ci  = c(5,16),
                   di  = c(115,69)
                  )

head(dat)

metafor::to.table(ai = ai,
                  bi = bi,
                  ci = ci,
                  di = di,
                  measure = "OR",
                  slab = age,
                  rows = c("tolbutamide","placebo"),
                  cols = c("dead","survived"),
                  data = dat
                  )


#######################################################
# Calculate the stratum-specific risk difference (RD)
#######################################################
metafor::escalc(ai = ai, bi = bi, ci = ci, di = di, measure = "RD", data = dat)


##################################################
# Fit a fixed-effects model for risk difference
#   Mantel-Haenszel method
##################################################
rd_model <- metafor::rma.mh(ai = ai, bi = bi, ci = ci, di = di, data = dat, measure = "RD")

summary(rd_model)

confint(rd_model)

metafor::forest(x = rd_model)


############################################################
# Fit a fixed-effects model for risk ratio/relative risk
#   Mantel-Haenszel method
############################################################
rr_model <- metafor::rma.mh(ai = ai, bi = bi, ci = ci, di = di, data = dat, measure = "RR")

summary(rr_model)

confint(rr_model)

metafor::forest(x = rr_model)


############################################################
# Fit a fixed-effects model for odds ratio (OR)
#   Cochran-Mantel-Haenszel method
############################################################
or_model <- metafor::rma.mh(ai = ai, bi = bi, ci = ci, di = di, data = dat, measure = "OR")

summary(or_model)

confint(object = or_model)

metafor::forest(x = or_model)
