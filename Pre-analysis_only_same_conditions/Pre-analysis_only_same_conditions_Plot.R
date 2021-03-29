## Project Name: OI_EYE_1
## Plot of the Pre-analysis of only the condition in which the actions pairs contained two
## action of the same action type.

## Written by: Hanna Schleihauf
## Date: 20 December 2020

## Coding information: child.adult: 1 = child, 2 = adult

load("./Pre-analysis_only_same_conditions/Pre-analysis_only_same_conditions.RData")

# subset the results of the bootplot to enable plotting
boot.plot.PI.PI <- subset(boot.plot.same$ci.fitted, boot.plot.same$ci.fitted$comb.sum == "PI-PI")
boot.plot.NC.NC <- subset(boot.plot.same$ci.fitted, boot.plot.same$ci.fitted$comb.sum == "NC-NC")
boot.plot.R.R <- subset(boot.plot.same$ci.fitted, boot.plot.same$ci.fitted$comb.sum == "R-R")

# start creating the plot
# define the space around the margins of each of the plots per subject
par(mar = c(5, 3.5, 1, 0.5), mgp = c(1.8, 0.5, 0)) # set margins etc.

t.data.same$comb.sum <-
  ordered(t.data.same$comb.sum, levels = c("NC-NC", "PI-PI", "R-R"))
t.data.same$xcats <-
  as.factor(t.data.same$comb.sum)
levels(t.data.same$xcats)

# this step is redundant here, but allows flexibility when changing the plot
t.data.same$xcats <- as.numeric(t.data.same$xcats)
t.data.same$xcats[t.data.same$xcats == 1] <- 1
t.data.same$xcats[t.data.same$xcats == 2] <- 2
t.data.same$xcats[t.data.same$xcats == 3] <- 3

# create empty plot
dev.off()
plot(
  x = 1, y = 1, type = "n", xlim = c(0, 4), ylim = c(-0.1, 1.1),
  tcl = -0.25,
  xaxs = "i", xaxt = "n",
  las = 1,
  xlab = "", ylab = ""
)
# calculate jitter x values
t.data.same$xvals <- t.data.same$xcats + runif(n = nrow(t.data.same), min = -0.35, max = 0.35)
# define colors
mycolors <- "red"
mycolors.t <- adjustcolor(mycolors, alpha.f = 0.25)
# add points
points(t.data.same$xvals, t.data.same$aoi.prop, col = mycolors.t, pch = 16)
# add line between at 0.5
segments(
  x0 = -0.5, x1 = 8.5,
  y0 = 0.5, y1 = 0.5, lty = 2, lwd = 1, col = "grey15"
)
# add model line and confidence intervals
# NC-NC
pred.yvals <- boot.plot.NC.NC$fitted
segments(
  x0 = 0.8, x1 = 1.2,
  y0 = pred.yvals, y1 = pred.yvals, lty = 1, lwd = 4, col = "red4"
)
upper.cl <- boot.plot.NC.NC$upper.cl
lower.cl <- boot.plot.NC.NC$lower.cl
arrows(
  x0 = 1, x1 = 1,
  y0 = lower.cl,
  y1 = upper.cl,
  code = 3, col = "red4", lwd = 1.5, angle = 90, length = 0.1
)
text(
  x = c(1, 1), y = c(-0.08, 1.08), labels = c("NC 2", "NC 1"), adj = 0.5,
  cex = 1, col = "black"
)
# PI-PI
pred.yvals <- boot.plot.PI.PI$fitted
segments(
  x0 = 1.8, x1 = 2.2,
  y0 = pred.yvals, y1 = pred.yvals, lty = 1, lwd = 4, col = "red4"
)
upper.cl <- boot.plot.PI.PI$upper.cl
lower.cl <- boot.plot.PI.PI$lower.cl
arrows(
  x0 = 2, x1 = 2,
  y0 = lower.cl,
  y1 = upper.cl,
  code = 3, col = "red4", lwd = 1.5, angle = 90, length = 0.1
)
text(
  x = c(2, 2), y = c(-0.08, 1.08), labels = c("PI 2", "PI 1"), adj = 0.5,
  cex = 1, col = "black"
)

# R-R
pred.yvals <- boot.plot.R.R$fitted
segments(
  x0 = 2.8, x1 = 3.2,
  y0 = pred.yvals, y1 = pred.yvals, lty = 1, lwd = 4, col = "red4"
)
upper.cl <- boot.plot.R.R$upper.cl
lower.cl <- boot.plot.R.R$lower.cl
arrows(
  x0 = 3, x1 = 3,
  y0 = lower.cl,
  y1 = upper.cl,
  code = 3, col = "red4", lwd = 1.5, angle = 90, length = 0.1
)
text(
  x = c(3, 3), y = c(-0.08, 1.08), labels = c("R 2", "R 1"), adj = 0.5,
  cex = 1, col = "black"
)

# add labels at x-axis
mtext(text = c("action combinations"), side = 1, line = 0.5, at = 2)

# add labels at y-axis
mtext(text = "looking preference", side = 2, line = 2.7, at = 0.5)
