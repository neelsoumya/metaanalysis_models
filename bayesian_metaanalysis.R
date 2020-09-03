###############################################
# Example script for Bayesian meta-analysis
# 
# Adapted from:
#   http://www.stats.bris.ac.uk/R/web/packages/bayesmeta/vignettes/bayesmeta.html
#
# Installation:
#   install.packages("bayesmeta")
#
#############################################


##################
# Load libraries
##################
library(metafor)
library(bayesmeta)

##################
# Load data
##################
# The data set summarizes the results from a study
# on the effects of coaching programs at eight different schools.
# In order to compare the schools' success, students filled out 
# standardized questionnaires before and after the coaching, and 
# each school's mean increase in the score (the “effect” column) along
# with the standard error (the “stderr” column) was recorded. 
# You may also consult the help (via help("Rubin1981")) for more details.
data("Rubin1981")
print(Rubin1981)

#########################
# Computing effect size
#########################
data("CrinsEtAl2014")
print(CrinsEtAl2014[,c(1,10,12,13,15)])

crins.es <- escalc(measure="OR",
                   ai=exp.AR.events,  n1i=exp.total,
                   ci=cont.AR.events, n2i=cont.total,
                   slab=publication, data=CrinsEtAl2014)
print(crins.es[,c("publication", "yi", "vi")])


##########################
# Random effects model
##########################

taupriordensity <- function(t){dhalfcauchy(t, scale=25)}
schools_example_1 <- bayesmeta(y     = Rubin1981[,"effect"],
                               sigma = Rubin1981[,"stderr"],
                               label = Rubin1981[,"school"],
                               mu.prior.mean=0, mu.prior.sd=50,
                               tau.prior=taupriordensity)

print(schools_example_1)

# forest plot
bayesmeta::forestplot.bayesmeta(x = schools_example_1)

# plot the posterior distribution
bayesmeta::plot.bayesmeta(x = schools_example_1)

# posterior probability of mu
# say you were interested in the posterior probability that the mean effect mu is actually positive.
1 - schools_example_1$pposterior(mu = 0)

# The quantile function (inverse CDF) is given by the ...$qposterior() element;
# if you are interested in a 95% posterior upper limit on the probable effect magnitude mu, you may call
schools_example_1$qposterior(mu.p = 0.95)

# 95% posterior upper limit on the heterogeneity tau
schools_example_1$qposterior(tau.p = 0.95)

# 95% credible intervals for the effect mu
schools_example_1$post.interval(mu.level = 0.95)

# 95% credible intervals for the hetergeneity tau
schools_example_1$post.interval(tau.level = 0.95)
# You can see the difference by explicitly requesting a “central” interval
# that is determined via 2.5% and 97.5% quantiles;
schools_example_1$post.interval(tau.level = 0.95, method = "central")


#################################################
# Look at estimates of parameters in each study
#################################################
schools_example_1$theta[,c("A", "G")]
