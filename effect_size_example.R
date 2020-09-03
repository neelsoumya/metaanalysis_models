#############################################################
# Example of standardized mean differences
#
# Adapted from:
#    http://www.deeplytrivial.com/search?q=meta-analysis
#
# Tutorial:
#   https://www.youtube.com/watch?v=O6qDlov5-Is
#
#############################################################

######################
# Load library
######################
library(metafor)

###########################################
# Look at standardised mean differences
###########################################
smd_meta<-data.frame(
   id = c("005","005","029","031","038","041","041","058","058","067","067"),
    study = c(1,2,3,1,1,1,2,1,2,1,2),
    author_year = c("Ruva 2007","Ruva 2007","Chrzanowski 2006","Studebaker 2000",
                  "Ruva 2008","Bradshaw 2007","Bradshaw 2007","Wilson 1998",
                  "Wilson 1998","Locatelli 2011","Locatelli 2011"),
    n1 = c(138,140,144,21,54,78,92,31,29,90,181),
    n2 = c(138,142,234,21,52,20,18,15,13,29,53),
    m1 = c(5.29,5.05,1.97,5.95,5.07,6.22,5.47,6.13,5.69,4.81,4.83),
    m2 = c(4.08,3.89,2.45,3.67,3.96,5.75,4.89,3.80,3.61,4.61,4.51),
    sd1 = c(1.65,1.50,1.08,1.02,1.65,2.53,2.31,2.51,2.51,1.20,1.19),
    sd2 = c(1.67,1.61,1.22,1.20,1.76,2.17,2.59,2.68,2.78,1.39,1.34)
)

smd_meta <- escalc(measure="SMD", m1i=m1, m2i=m2, sd1i=sd1, sd2i=sd2, n1i=n1, n2i=n2,
                   data=smd_meta)

head(smd_meta)


###########################################
# Look at odds ratios
###########################################

or_meta<-data.frame(
  id = c("001","003","005","005","011","016","025","025","035","039","045","064","064"),
  study = c(1,5,1,2,1,1,1,2,1,1,1,1,2),
  author_year = c("Bruschke 1999","Finkelstein 1995","Ruva 2007","Ruva 2007",
                  "Freedman 1996","Keelen 1979","Davis 1986","Davis 1986",
                  "Padawer-Singer 1974","Eimermann 1971","Jacquin 2001",
                  "Ruva 2006","Ruva 2006"),
  tg = c(58,26,67,90,36,37,17,17,47,15,133,68,53),
  cg = c(49,39,22,50,12,33,19,17,33,11,207,29,44),
  tn = c(72,60,138,140,99,120,60,55,60,40,136,87,74),
  cn = c(62,90,138,142,54,120,52,57,60,44,228,83,73)
)

or_meta <- escalc(measure="OR", ai=tg, bi=(tn-tg), ci=cg, di=(cn-cg), data=or_meta)

head(or_meta)


# There are two overall methods of meta-analysis - 
# fixed effects and random effects.
# Fixed effects means that you suspect there is only
# one underlying effect size that your studies are trying to estimate.
# You can access this by setting method="FE" (for fixed effects) in the rma function.

smd.rma<-rma(yi,vi,method="FE",data=smd_meta)
summary(smd.rma)

# we'll look at the Model Results, which includes the estimate (the overall effect size).
smd.rma$beta

# The overall Cohen's d for this set of studies is 0.349, a moderate effect.
# In plain English, the average guilt rating among people who saw pretrial
# publicity is about 0.349 standard deviations higher than people in the control group. 
# There's an associated Z-value and p-value, which tests whether that metric is significantly different from 0.

options(scipen=999)
smd.rma$zval
smd.rma$pval

# The Z-test confirms that this value is significantly different from 0.
# I'll go into conducting a random effects model (and how to tell if you should) on Sunday.

# forest plot
metafor::forest.rma(x = smd.rma)

# test residuals
rstudent(model = smd.rma)
#metafor::rstudent(smd.rma)

inf <- influence(model = smd.rma)
inf
plot(inf, plotdfb=TRUE)

# cross validation (leave 1 one out)
# res <- rma(yi, vi, data = dat)
leave1out(smd.rma, transf = exp, digits = 3)

# Q-Q plots
# qqplot(smd.rma, main="Random effects model")

# regtest
regtest(x = smd.rma, model = "lm")

# funnel plot
rtf <- trimfill(x = smd.rma)
rtf
funnel(x = rtf)




####################################
# Now, let's run the meta-analysis 
# for the log odds ratio dataset.
####################################
or.rma<-rma(yi,vi,method="FE",data=or_meta)
summary(or.rma)

or.rma$beta
or.rma$zval
or.rma$pval

# Our estimated log odds ratio for this dataset is 0.7456, which is significantly different from 0.
# This number is difficult to interpret in isolation, so we want to convert it back to odds ratio, 
# which is easier to interpret.

exp(or.rma$beta)

cat("Overall, people who saw pretrial publicity were over 2 times as likely to convict as people in the control group.")

########################
# Confidence intervals
#   for I^2, tau
########################
# Works with random-effects rma() model
# confint(object = or.rma)

#########################
# forest plot
#########################
metafor::forest.rma(x = or.rma)

# test residuals
rstudent(model = or.rma)
#metafor::rstudent(smd.rma)

inf <- influence(model = or.rma)
inf
plot(inf, plotdfb=TRUE)

# cross validation (leave 1 one out)
# res <- rma(yi, vi, data = dat)
leave1out(or.rma, transf = exp, digits = 3)

# Q-Q plots
# qqplot(smd.rma, main="Random effects model")
metafor::qqnorm.rma.uni(y = or.rma)

# regtest
metafor::regtest(x = or.rma, model = "lm")

# funnel plot
rtf <- trimfill(x = or.rma)
rtf
funnel(x = rtf)




##############################
# Model with random-effects
##############################
or_rma <- metafor::rma(yi = yi, vi = vi, method = "REML", data = or_meta) 
# mods = moderators
# for example metafor::rma(yi ~ vi + age + gender, vi, method = "REML", data = or_meta)

# NOTE: other options are metafor::rma.glmm()

summary(or_rma)

##############################
# Confidence intervals
##############################
metafor::confint.rma.uni(object = or_rma)

##############################
# Forest plot
##############################
metafor::forest.rma(x = or_rma)
