## Project Name: OI_EYE_1
## Data Preparation
## The file from the data extraction gives us the looking time for each picture.
## We need one value for each pair of pictures, that indicates whether participants
## looked longer at one or the other picture. Here we create this variable.

## Written by: Hanna Schleihauf
## Date: 20 December 2020

## Coding information: child.adult: 1 = child, 2 = adult

# load necessary packages
library("nnet")

# load file that we extracted from the Tobii output
xdata <- read.table(file = "./data_processed/data_extracted.txt", header = T, sep = "\t")
str(xdata)

# making sure that factors are factors
xdata$id <- as.factor(xdata$id)
xdata$child.adult <- as.factor(xdata$child.adult)
xdata$sex <- as.factor(xdata$sex)
xdata$box <- as.factor(xdata$box)
xdata$rando.order <- as.factor(xdata$rando.order)
xdata$rando.order <- as.factor(xdata$rando.order)
xdata$side <- as.factor(xdata$side)

# create a new variable, in which the combination of actions (not the single actions) are indicated
xdata$comb.sum[xdata$comb.detail == "NC1-PI2" | xdata$comb.detail == "PI1-NC1" |
  xdata$comb.detail == "PI1-NC2" | xdata$comb.detail == "PI2-NC2"] <- "NC-PI"
xdata$comb.sum[xdata$comb.detail == "NC1-R2" | xdata$comb.detail == "R1-NC1" |
  xdata$comb.detail == "R1-NC2" | xdata$comb.detail == "R2-NC2"] <- "NC-R"
xdata$comb.sum[xdata$comb.detail == "PI1-R2" | xdata$comb.detail == "R1-PI1" |
  xdata$comb.detail == "R1-PI2" | xdata$comb.detail == "R2-PI2"] <- "PI-R"
xdata$comb.sum[xdata$comb.detail == "PI1-PI2"] <- "PI-PI"
xdata$comb.sum[xdata$comb.detail == "R1-R2"] <- "R-R"
xdata$comb.sum[xdata$comb.detail == "NC1-NC2"] <- "NC-NC"

# create a variable in which all the action combinations that include two actions of one action type are called "Same"
xdata$comb.sumsum <- xdata$comb.sum
xdata$comb.sumsum <- as.character(xdata$comb.sumsum)
xdata$comb.sumsum[xdata$comb.sumsum == "NC-NC" | xdata$comb.sumsum == "PI-PI" | xdata$comb.sumsum == "R-R"] <- "Same"
xdata$comb.sumsum <- as.factor(xdata$comb.sumsum)
levels(xdata$comb.sumsum)

# relevel so that "same"Same" is reference level
?relevel
xdata$comb.sumsum <- relevel(xdata$comb.sumsum, "Same")
levels(xdata$comb.sumsum)

# create a new variable, in which the action type of each action is indicated
xdata$action.sum[xdata$action == "R1" | xdata$action == "R2"] <- "R"
xdata$action.sum[xdata$action == "PI1" | xdata$action == "PI2"] <- "PI"
xdata$action.sum[xdata$action == "NC1" | xdata$action == "NC2"] <- "NC"

xdata$comb.sum <- as.factor(xdata$comb.sum)
xdata$action.sum <- as.factor(xdata$action.sum)

# create variable for single id (not dyad id)
xdata$single.id <- paste(xdata$id, xdata$child.adult, sep = ".")

# inspect data - where do we have missing trials
# here we can see that for Box 2 we are missing one condition completely
table(xdata$box, xdata$comb.detail)
xxdata <- subset(xdata, xdata$box != 2)
xxdata$box <- droplevels(xxdata$box)
table(xxdata$box, xxdata$comb.sum)

# Creating one score for each pair of actions, indicating the difference in
# looking time between the actions of a pair ------------------------------------

# create new data frame to store the difference scores
propdiff <- data.frame(
  xxdata$id, xxdata$single.id, xxdata$child.adult, xxdata$sex,
  xxdata$box, xxdata$rando.order, xxdata$trial, xxdata$model,
  xxdata$comb.detail, xxdata$comb.sum, xxdata$comb.sumsum, xxdata$time.screen
)
names(propdiff) <- c(
  "id", "single.id", "child.adult", "sex", "box", "rando.order",
  "trial", "model", "comb.detail", "comb.sum", "comb.sumsum", "time.screen"
)
# delete every second row, because we now have only one score for each action pair
toDelete <- seq(0, nrow(propdiff), 2)
toDelete
propdiff <- propdiff[-toDelete, ]

# calculate the difference score and insert into new data frame
# score between 0 and 1, with 0.5 meaning that the children looked equally at both videos
propdiff$aoi.prop[propdiff$comb.sum == "NC-PI"] <-
  xxdata$time.aoi[xxdata$comb.sum == "NC-PI" & xxdata$action.sum == "PI"] /
    (xxdata$time.aoi[xxdata$comb.sum == "NC-PI" & xxdata$action.sum == "PI"] +
      xxdata$time.aoi[xxdata$comb.sum == "NC-PI" & xxdata$action.sum == "NC"])

propdiff$aoi.prop[propdiff$comb.sum == "NC-R"] <-
  xxdata$time.aoi[xxdata$comb.sum == "NC-R" & xxdata$action.sum == "R"] /
    (xxdata$time.aoi[xxdata$comb.sum == "NC-R" & xxdata$action.sum == "R"] +
      xxdata$time.aoi[xxdata$comb.sum == "NC-R" & xxdata$action.sum == "NC"])

propdiff$aoi.prop[propdiff$comb.sum == "PI-R"] <-
  xxdata$time.aoi[xxdata$comb.sum == "PI-R" & xxdata$action.sum == "R"] /
    (xxdata$time.aoi[xxdata$comb.sum == "PI-R" & xxdata$action.sum == "R"] +
      xxdata$time.aoi[xxdata$comb.sum == "PI-R" & xxdata$action.sum == "PI"])

propdiff$aoi.prop[propdiff$comb.sum == "PI-PI"] <-
  xxdata$time.aoi[xxdata$comb.sum == "PI-PI" & xxdata$action == "PI2"] /
    (xxdata$time.aoi[xxdata$comb.sum == "PI-PI" & xxdata$action == "PI1"] +
      xxdata$time.aoi[xxdata$comb.sum == "PI-PI" & xxdata$action == "PI2"])

propdiff$aoi.prop[propdiff$comb.sum == "R-R"] <-
  xxdata$time.aoi[xxdata$comb.sum == "R-R" & xxdata$action == "R2"] /
    (xxdata$time.aoi[xxdata$comb.sum == "R-R" & xxdata$action == "R1"] +
      xxdata$time.aoi[xxdata$comb.sum == "R-R" & xxdata$action == "R2"])

propdiff$aoi.prop[propdiff$comb.sum == "NC-NC"] <-
  xxdata$time.aoi[xxdata$comb.sum == "NC-NC" & xxdata$action == "NC2"] /
    (xxdata$time.aoi[xxdata$comb.sum == "NC-NC" & xxdata$action == "NC1"] +
      xxdata$time.aoi[xxdata$comb.sum == "NC-NC" & xxdata$action == "NC2"])

tapply(propdiff$aoi.prop, (propdiff$comb.sumsum), mean, na.rm = T)

sum(propdiff$time.screen == 0) # 69 dropout trials
nlevels(as.factor(propdiff$single.id[propdiff$time.screen == 0])) # of 19 individuals
table(as.factor(propdiff$single.id[propdiff$time.screen == 0])) # 36 dropout trials from 14 children and 33 from 5 adults

# exclude trials in which participants are not looking at the screen
propdiff <- subset(propdiff, propdiff$time.screen != 0)

# add trial per condition as variable
propdiff$trial.per.condition <- ave(as.numeric(propdiff$single.id), list(as.numeric(propdiff$single.id), propdiff$box, propdiff$comb.sumsum), FUN = seq_along)
# add trial overall as variable
propdiff$trial.overall <- ave(propdiff$single.id, propdiff$single.id, FUN = seq_along)

save.image("data_processed.RData")
write.table(propdiff, "data_processed.txt", quote = FALSE, sep = "\t")
