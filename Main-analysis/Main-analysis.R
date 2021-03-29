## Project Name: OI_EYE_1
## Main-analysis of all conditions combines.

## Here we checked whether the looking preferences within these action pairs varied for
## action pairs of different action types.

## Written by: Hanna Schleihauf
## Date: 20 December 2020

## Coding information: child.adult: 1 = child, 2 = adult

library("parameters")
library("lme4")
library("glmmTMB")
library("effects")

source("./functions/diagnostic_fcns.r")
source("./functions/boot_glmmTMB.r")
source("./functions/glmmTMB_stability.r")
source("./functions/drop1_para_glmmtmb.r")

# load data
xdata <- read.table(file = "./data_processed/data_processed.txt", header = T, sep = "\t")
str(xdata)

# prepare model fitting (dummy coding and centering)
xx.fe.re <- fe.re.tab(
  fe.model = "aoi.prop ~ comb.sumsum*child.adult + trial",
  re = "(1|id) + (1|single.id) + (1|box)", data = propdiff, other.vars = c("time.screen")
)
xx.fe.re$summary
t.data <- xx.fe.re$data
levels(t.data$comb.sumsum) # Same is the reference level of t.data
str(t.data)

# center all  variables included in random slope
t.data$z.trial <- scale(as.numeric(t.data$trial))
t.data$child.adult.code <- t.data$child.adult.2 - mean(t.data$child.adult.2)
t.data$comb.sumsum.NC.PI.code <- t.data$comb.sumsum.NC.PI - mean(t.data$comb.sumsum.NC.PI)
t.data$comb.sumsum.NC.R.code <- t.data$comb.sumsum.NC.R - mean(t.data$comb.sumsum.NC.R)
t.data$comb.sumsum.PI.R.code <- t.data$comb.sumsum.PI.R - mean(t.data$comb.sumsum.PI.R)

# we have values 0 and 1 in the response variable, but for a beta model the values
# need to be a little bit below 1 and a little above 0, therefore, we move the values
# a little bit away from 0 and 1
range(t.data$aoi.prop)
hist(t.data$aoi.prop)
t.data$t.aoi.prop <- t.data$aoi.prop
t.data$t.aoi.prop <- (t.data$t.aoi.prop * (length(t.data$t.aoi.prop) - 1) + 0.5) / length(t.data$t.aoi.prop)
hist(t.data$t.aoi.prop)
range(t.data$t.aoi.prop)
# checking whether it worked
xx <- t.data$aoi.prop - t.data$t.aoi.prop
head(round(xx, 5))

# preliminary analysis to see whether trial number per block has an effect on the
# relative looking time to one of the actions
contr <-
  glmmTMBControl(
    optCtrl = list(iter.max = 200000, eval.max = 200000),
    profile = FALSE, collect = FALSE
  )
# other control function:
# contr  <-  glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS"))

pre <- glmmTMB(t.aoi.prop ~
z.trial +
  (1 + child.adult.code || id) +
  (1 + (comb.sumsum.NC.PI.code + comb.sumsum.NC.R.code + comb.sumsum.PI.R.code) + trial || single.id) +
  (1 + (comb.sumsum.NC.PI.code + comb.sumsum.NC.R.code + comb.sumsum.PI.R.code) + trial || box),
data = t.data, family = beta_family(link = "logit"), control = contr, weights = time.screen, REML = FALSE
)

# checking assumptions
warnings()
summary(pre)$varcor # random effects
ranef.diagn.plot(pre) # distribution of random effects
overdisp.test(pre) # overdispersion
# model stability (exclude data of one individual at the time and fit model again)
pre.stab <- glmmTMB.stab(
  model.res = pre, contr = contr,
  ind.cases = F, para = T, data = t.data,
  use = "single.id", n.cores = c("all"),
  save.path = NULL, load.lib = T, lib.loc = .libPaths()
)
pre.stab$detailed$warnings
xx <- round(pre.stab$summary[, -1], 3)
m.stab.plot(pre.stab$summary[, -1])
xx ## model is stable

# significance tests
tests.pre <- drop1p(model.res = pre, para = T, data = t.data, contr = contr, n.cores = c("all-1"), to.del = NULL, return.model.results = F)
tests.pre$drop1.res
plot(effect("z.trial", pre))
# look at estimates
summary(pre)
round(summary(pre)$coefficients$cond, 3)

## bootstraps for plotting
boot.plot.trial <-
  boot.glmmTMB(
    model.res = pre, data = t.data, excl.non.conv = T, nboots = 1000, para = T,
    resol = 100, level = 0.95, use = c("trial"), contr = contr, circ.var.name = NULL,
    circ.var = NULL, n.cores = c("all-1", "all"), save.path = NULL, load.lib = T,
    lib.loc = .libPaths(), set.all.effects.2.zero = F
  )
round(boot.plot.trial$ci.estimates$fe, 3)

## main analysis
full1 <- glmmTMB(t.aoi.prop ~
(comb.sumsum + child.adult)^2 +
  (1 + child.adult.code || id) +
  (1 + (comb.sumsum.NC.PI.code + comb.sumsum.NC.R.code + comb.sumsum.PI.R.code) || single.id) +
  (1 + (comb.sumsum.NC.PI.code + comb.sumsum.NC.R.code + comb.sumsum.PI.R.code) || box),
data = t.data, family = beta_family(link = "logit"), control = contr, weights = time.screen, REML = FALSE
)

# checking assumptions
summary(full1)$varcor # random effects
ranef.diagn.plot(full1) # distribution of random effects
overdisp.test(full1) # overdispersion
# check for colliniarity
xx <- lm(t.aoi.prop ~ comb.sumsum + child.adult,
  data = t.data
)
library(car)
vif(xx)
# model stability (exclude data of one individual at the time and fit model again)
full.stab <- glmmTMB.stab(model.res = full1, contr = contr, ind.cases = F, para = T, data = t.data, use = "single.id", n.cores = c("all"), save.path = NULL, load.lib = T, lib.loc = .libPaths())
m.stab$detailed$warnings
xx <- round(full.stab$summary[, -1], 3)
m.stab.plot(full.stab$summary[, -1])
xx ## model is stable

# full-null-model comparison to see whether the all predictors taken together explain more of the variance than the intercept alone
null <- glmmTMB(t.aoi.prop ~
1 +
  (1 + child.adult.code || id) +
  (1 + (comb.sumsum.NC.PI.code + comb.sumsum.NC.R.code + comb.sumsum.PI.R.code) || single.id) +
  (1 + (comb.sumsum.NC.PI.code + comb.sumsum.NC.R.code + comb.sumsum.PI.R.code) || box),
t.data,
family = beta_family(link = "logit"), control = contr, weights = time.screen, REML = FALSE
)
anova(full1, null, test = "Chisq")

# significance tests of interaction effect
tests1 <- drop1p(model.res = full1, para = T, data = t.data, contr = contr, n.cores = c("all-1"), to.del = NULL, return.model.results = F)
tests1$drop1.res
# look at estimates
summary(full1)
round(summary(full1)$coefficients$cond, 3)

# fit a reduces model without the interaction
full2 <- glmmTMB(t.aoi.prop ~
(comb.sumsum + child.adult) +
  (1 + child.adult.code || id) +
  (1 + (comb.sumsum.NC.PI.code + comb.sumsum.NC.R.code + comb.sumsum.PI.R.code) || single.id) +
  (1 + (comb.sumsum.NC.PI.code + comb.sumsum.NC.R.code + comb.sumsum.PI.R.code) || box),
data = t.data, family = beta_family(link = "logit"), control = contr, weights = time.screen, REML = FALSE
)
# significance tests of main effects
tests2 <- drop1p(model.res = full2, para = T, data = t.data, contr = contr, n.cores = c("all-1"), to.del = NULL, return.model.results = F)
tests2$drop1.res

## bootstraps for full model
boot.plot.full1 <- boot.glmmTMB(
  model.res = full1, data = t.data, excl.non.conv = T,
  nboots = 1000, para = T, level = 0.95,
  use = c("comb.sumsum", "child.adult"),
  contr = contr, circ.var.name = NULL, circ.var = NULL,
  n.cores = c("all-1"), save.path = NULL,
  load.lib = T, lib.loc = .libPaths(), set.all.effects.2.zero = F
)
round(boot.plot.full1$ci.estimates$fe, 3)

# fit plot model
plotmodel.condition <-
  glmmTMB(t.aoi.prop ~
  (comb.sumsum + child.adult.code) +
    (1 + child.adult.code || id) +
    (1 + (comb.sumsum.NC.PI.code + comb.sumsum.NC.R.code + comb.sumsum.PI.R.code) || single.id) +
    (1 + (comb.sumsum.NC.PI.code + comb.sumsum.NC.R.code + comb.sumsum.PI.R.code) || box),
  data = t.data, family = beta_family(link = "logit"), control = contr, weights = time.screen, REML = FALSE
  )

# bootstraps for plot model
boot.plot.condition <-
  boot.glmmTMB(
    model.res = plotmodel.condition, data = t.data, excl.non.conv = T,
    nboots = 1000, para = T, level = 0.95, use = c("comb.sumsum"),
    contr = contr, circ.var.name = NULL, circ.var = NULL, n.cores = c("all-1", "all"),
    save.path = NULL, load.lib = T, lib.loc = .libPaths(), set.all.effects.2.zero = F
  )
round(boot.plot.condition$ci.estimates$fe, 3)

# post-hoc pairwise comparison
library("emmeans")
emm <- emmeans(full2, ~comb.sumsum)
summary(emm, type = "response") ## values for plot
plot(emm, by = "comb.sumsum", intervals = TRUE, type = "response")
emmeans(full2, pairwise ~ comb.sumsum)
summary(pairs(emm), type = "response")
summary(pairs(regrid(emm)), type = "response")
emmip(emm, ~"comb.sumsumsum", type = "link")
emmip(emm, ~"comb.sumsumsum*child.adult", type = "response")

save.image("./Main-analysis/Main-analysis.RData")
