## Project Name: OI_EYE_1
## Plot Main-analysis for marginal trail effect.

## Written by: Hanna Schleihauf
## Date: 20 December 2020

## Coding information: child.adult: 1 = child, 2 = adult

# load data
load("./Main-analysis/Main-analysis.RData")

str(t.data)
xx <- summary(pre)$coefficients$cond
as.data.frame(xx)
xx <- xx[, 1]
xx

dev.off()

# start creating the plot
# define the space around the margins of each of the plots per subject
par(mar = c(3.3, 3, 2, 2), mgp = c(2.0, 0.5, 0))
# define colors
mycolors <- c("red", "blue", "blue", "blue")[as.numeric(t.data$comb.sumsum)]
mycolors.t <- adjustcolor(mycolors, alpha.f = 0.25)
# create plot with data points
plot(
  x = t.data$z.trial,
  y = t.data$aoi.prop, tcl = -0.25, pch = 20, col = mycolors.t,
  xaxt = "n", las = 1, tck = -0.01, ylab = "",
  xlab = "trial number per block"
)
# add line between at 0.5
segments(
  x0 = min(t.data$z.trial), x1 = max(t.data$z.trial),
  y0 = 0.5, y1 = 0.5, lty = 2, lwd = 1, col = "grey15"
)
# insert confidence interval
ci.y <- seq(
  from = min(t.data$z.trial),
  to = max(t.data$z.trial), length.out = 100
)
polygon(
  x = c(ci.y, rev(ci.y)),
  y = c(boot.plot.trial$ci.fitted$lower.cl, rev(boot.plot.trial$ci.fitted$upper.cl)),
  border = NA, col = adjustcolor("black", alpha.f = 0.3)
)
# add model line
xvals <- seq(from = min(t.data$z.trial), to = max(t.data$z.trial), length.out = 100)
yvals <- boot.plot.trial$ci.fitted$fitted
lines(xvals, yvals, lty = 1, lwd = 2, col = "black")
# add lables at y-axis
axis(1, at = seq(from = min(t.data$z.trial), to = max(t.data$z.trial), length.out = 30), labels = 1:30, tick = TRUE, tck = -0.01, cex = 0.5)
