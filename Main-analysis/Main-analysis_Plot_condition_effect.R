## Project Name: OI_EYE_1
## Plot Main-analysis for conditions effect.

## Written by: Hanna Schleihauf
## Date: 20 December 2020

## Coding information: child.adult: 1 = child, 2 = adult

# load data
load("./Main-analysis/Main-analysis.RData")

# subset the results of the bootplot to enable plotting
boot.plot.condition.same <- subset(boot.plot.condition$ci.fitted, boot.plot.condition$ci.fitted$comb.sumsum == "Same")
boot.plot.condition.NC.PI <- subset(boot.plot.condition$ci.fitted, boot.plot.condition$ci.fitted$comb.sumsum == "NC-PI")
boot.plot.condition.NC.R <- subset(boot.plot.condition$ci.fitted, boot.plot.condition$ci.fitted$comb.sumsum == "NC-R")
boot.plot.condition.PI.R <- subset(boot.plot.condition$ci.fitted, boot.plot.condition$ci.fitted$comb.sumsum == "PI-R")

# start creating the plot
# define the space around the margins of each of the plots per subject
par(mar = c(5, 3.5, 1, 0.5), mgp = c(1.8, 0.5, 0))
# define color groups
t.data$color.id[t.data$comb.sumsum == "Same"] <- 1
t.data$color.id[t.data$comb.sumsum != "Same"] <- 2
# order levels
t.data$comb.sumsum <- ordered(t.data$comb.sumsum, levels = c("Same", "PI-R", "NC-R", "NC-PI"))
t.data$xcats <- as.factor(t.data$comb.sumsum)
levels(t.data$xcats)
t.data$xcats <- as.numeric(t.data$xcats)

# create empty plot
dev.off()
plot(
  x = 1, y = 1, type = "n", xlim = c(0, 5), ylim = c(-0.1, 1.1),
  tcl = -0.25,
  xaxs = "i", xaxt = "n",
  las = 1,
  xlab = "", ylab = ""
)

# calculate jitter x values
t.data$xvals <- t.data$xcats + runif(n = nrow(t.data), min = -0.35, max = 0.35)
# define colors
mycolors <- c("red", "blue")[as.numeric(t.data$color.id)]
mycolors.t <- adjustcolor(mycolors, alpha.f = 0.25)
myshape <- c(16, 15)
# add points
points(t.data$xvals, t.data$aoi.prop, col = mycolors.t, pch = 16)
# add line between at 0.5
segments(
  x0 = -0.5, x1 = 8.5,
  y0 = 0.5, y1 = 0.5, lty = 2, lwd = 1, col = "grey15"
)
## add model line
# same conditions
pred.yvals <- boot.plot.condition.same$fitted
segments(
  x0 = 0.8, x1 = 1.2,
  y0 = pred.yvals, y1 = pred.yvals, lty = 1, lwd = 4, col = "red4"
)
upper.cl <- boot.plot.condition.same$upper.cl
lower.cl <- boot.plot.condition.same$lower.cl
arrows(
  x0 = 1, x1 = 1,
  y0 = lower.cl,
  y1 = upper.cl,
  code = 3, col = "red4", lwd = 1.5, angle = 90, length = 0.1
)
text(
  x = c(1, 1), y = c(-0.08, 1.08), labels = c("Same", "Same"), adj = 0.5,
  cex = 1, col = "black"
)

# PI-R conditions
pred.yvals <- boot.plot.condition.PI.R$fitted
segments(
  x0 = 1.8, x1 = 2.2,
  y0 = pred.yvals, y1 = pred.yvals, lty = 1, lwd = 4, col = "blue4"
)
upper.cl <- boot.plot.condition.PI.R$upper.cl
lower.cl <- boot.plot.condition.PI.R$lower.cl
arrows(
  x0 = 2, x1 = 2,
  y0 = lower.cl,
  y1 = upper.cl,
  code = 3, col = "blue4", lwd = 1.5, angle = 90, length = 0.1
)
text(
  x = c(2, 2), y = c(-0.08, 1.08), labels = c("PI", "R"), adj = 0.5,
  cex = 1, col = "black"
)

# NC-R conditions
pred.yvals <- boot.plot.condition.NC.R$fitted
segments(
  x0 = 2.8, x1 = 3.2,
  y0 = pred.yvals, y1 = pred.yvals, lty = 1, lwd = 4, col = "blue4"
)
upper.cl <- boot.plot.condition.NC.R$upper.cl
lower.cl <- boot.plot.condition.NC.R$lower.cl
arrows(
  x0 = 3, x1 = 3,
  y0 = lower.cl,
  y1 = upper.cl,
  code = 3, col = "blue4", lwd = 1.5, angle = 90, length = 0.1
)
text(
  x = c(3, 3), y = c(-0.08, 1.08), labels = c("NC", "R"), adj = 0.5,
  cex = 1, col = "black"
)

# NC-PI
pred.yvals <- boot.plot.condition.NC.PI$fitted
segments(
  x0 = 3.8, x1 = 4.2,
  y0 = pred.yvals, y1 = pred.yvals, lty = 1, lwd = 4, col = "blue4"
)
upper.cl <- boot.plot.condition.NC.PI$upper.cl
lower.cl <- boot.plot.condition.NC.PI$lower.cl
arrows(
  x0 = 4, x1 = 4,
  y0 = lower.cl,
  y1 = upper.cl,
  code = 3, col = "blue4", lwd = 1.5, angle = 90, length = 0.1
)
text(
  x = c(4, 4), y = c(-0.08, 1.08), labels = c("NC", "PI"), adj = 0.5,
  cex = 1, col = "black"
)

# add labels at x-axis
mtext(text = c("action combinations"), side = 1, line = 0.5, at = 2.5)

# add labels at y-axis
mtext(text = "looking preference", side = 2, line = 2.7, at = 0.5)
