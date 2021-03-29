## Project Name: OI_EYE_1
## Pre-analysis of only the condition in which the actions pairs contained two
## action of the same action type (it was never an identical actions, but a different
## action of the same action type).
## Here we checked whether the looking preferences within these action pairs varied for
## action pairs of different action types.

## Written by: Hanna Schleihauf
## Date: 20 December 2020

## Coding information: child.adult: 1 = child, 2 = adult

# load necessary packages
library("parameters")
library("lme4")
library("glmmTMB")
library("DHARMa")
library("effects")

source("./functions/diagnostic_fcns.r")
source("./functions/boot_glmmTMB.r")
source("./functions/glmmTMB_stability.r")
source("./functions/drop1_para_glmmtmb.r")

# load data
xdata <- read.table(file = "./data_processed/data_processed.txt", header = T, sep = "\t")
str(xdata)

# subset for conditions with the same action type (control)
propdiff.contr <- subset(propdiff, propdiff$comb.sumsum == "Same")
# trail number for only the "Same"- conditions
propdiff.contr$trial.same <- ave(propdiff.contr$single.id, propdiff.contr$single.id, FUN = seq_along)

# prepare model fitting (dummy coding and centering)
xx.fe.re <- fe.re.tab(
  fe.model = "aoi.prop ~ comb.sum*child.adult + box",
  re = "(1|id) + (1|single.id)", data = propdiff.contr,
  other.vars = c("time.screen", "trial.same")
)
xx.fe.re$summary
t.data.same <- xx.fe.re$data
str(t.data.same)
levels(t.data.same$comb.sum)

# center all  variables included in random slope
t.data.same$z.trial.same <- as.vector(scale(as.numeric(t.data.same$trial.same)))
t.data.same$child.adult.code <- t.data.same$child.adult.2 - mean(t.data.same$child.adult.2)
t.data.same$comb.sum.PI.PI.code <- t.data.same$comb.sum.PI.PI - mean(t.data.same$comb.sum.PI.PI)
t.data.same$comb.sum.R.R.code <- t.data.same$comb.sum.R.R - mean(t.data.same$comb.sum.R.R)

# we have values 0 and 1 in the response variable, but for a beta model the values
# need to be a little bit below 1 and a little above 0, therefore, we move the values
# a little bit away from 0 and 1
range(t.data.same$aoi.prop)
hist(t.data.same$aoi.prop)
t.data.same$t.aoi.prop <- t.data.same$aoi.prop
t.data.same$t.aoi.prop <- (t.data.same$t.aoi.prop * (length(t.data.same$t.aoi.prop) - 1) + 0.5) / length(t.data.same$t.aoi.prop)
hist(t.data.same$t.aoi.prop)
range(t.data.same$t.aoi.prop)
# checking whether it worked
xx <- t.data.same$aoi.prop - t.data.same$t.aoi.prop
head(round(xx, 5))

# starting with the model
contr <-
  glmmTMBControl(
    optCtrl = list(iter.max = 200000, eval.max = 200000),
    profile = TRUE, collect = FALSE
  )
# other control function:
# contr  <-  glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS"))

full1 <- glmmTMB(t.aoi.prop ~
comb.sum +
  (1 + child.adult.code || id) +
  (1 + (comb.sum.PI.PI.code + comb.sum.R.R.code) || single.id) +
  (1 + (comb.sum.PI.PI.code + comb.sum.R.R.code) || box),
data = t.data.same, family = beta_family(link = "logit"), control = contr,
weights = time.screen, REML = FALSE
)

# checking assumptions
warnings()
summary(full1)$varcor # random effects
ranef.diagn.plot(full1) # distribution of random effects
overdisp.test(full1) # overdispersion
oo <- simulateResiduals(full1, plot = T) # residuals diagnostics
# model stability (exclude data of one individual at the time and fit model again)
full.stab <- glmmTMB.stab(
  model.res = full1, contr = contr,
  ind.cases = F, para = T, data = t.data.same,
  use = "single.id", n.cores = c("all-1"),
  save.path = NULL, load.lib = T, lib.loc = .libPaths()
)
full.stab$detailed$warnings
xx <- round(full.stab$summary[, -1], 3)
dev.off()
m.stab.plot(full.stab$summary[, -1])
xx ## model is stable

# significance tests
tests1 <- drop1p(
  model.res = full1, para = F, data = t.data.same, n.cores = c("all-1", "all"),
  to.del = NULL, return.model.results = F, contr = contr
)
tests1$drop1.res

# look at estimates
summary(full1)
round(summary(full1)$coefficients$cond, 3)
plot(effect("comb.sum", full1))

## bootstraps for plotmodel
boot.plot.same <- boot.glmmTMB(
  model.res = full1, data = t.data.same,
  excl.non.conv = T, nboots = 1000, para = T,
  level = 0.95, use = c("comb.sum"),
  circ.var.name = NULL, circ.var = NULL,
  n.cores = c("all-1"), save.path = NULL, load.lib = T,
  lib.loc = .libPaths(), set.all.effects.2.zero = F
)
boot.plot.same$ci.fitted
round(boot.plot.same$ci.estimates$fe, 3)

# save for plotting
save.image("./Pre-analysis_only_same_conditions/Pre-analysis_only_same_conditions.RData")
