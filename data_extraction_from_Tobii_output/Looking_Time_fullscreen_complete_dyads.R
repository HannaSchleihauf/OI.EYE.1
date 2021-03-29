## Project Name: OI_EYE_1
## Data extraction from Tobii
## Extract Looking time for fullscreen from complete dyads (child and parent were tested)

## Written by: Hanna Schleihauf
## Date: 20 December 2020

# load files which contain the name of the video files for which we want to extract looking times
# load files which contain the order in which the video files were presented for each counterbalancing order
file.names <- read.table("./data_raw/file.names.txt", sep = "\t", header = T)
file.orders <- read.table("./data_raw/file.orders.txt", sep = "\t", header = T)

# create a data frame and storage location for the result of the extraction
n.simus <- 6840
output <- data.frame(time = rep(x = NA, times = n.simus), a = rep(x = NA, times = n.simus), b = rep(x = NA, times = n.simus))
output.file.name <- paste("./data_raw/Output_with_filter/", "Looking_time_to_Fullscreen.txt", sep = "")

# outer loop: prepare extracting the fullscreen looking time data
for (k in 1:nrow(file.names)) {
  file.name <- paste("./data_raw/Output_with_filter/", file.names$file[k], ".tsv", sep = "")
  box.name <- file.names$box[k]
  tracking <- read.csv2(file.name, sep = "\t", header = T, dec = ",", na.strings = c("", " ", "NA"))

  # some of the video files had the same name, this needs to be changed for correct looking time extraction
  if (box.name == "box1.1") {
    tracking$StudioEventData[min(which(tracking$StudioEventData == "12. i-nc 1 - i-nc 2 r.wmv"))] <- "12a. i-nc 1 - i-nc 2 r.wmv"
    tracking$StudioEventData[min(which(tracking$StudioEventData == "12. i-nc 1 - i-nc 2 r.wmv"))] <- "12a. i-nc 1 - i-nc 2 r.wmv"
    tracking$StudioEventData[min(which(tracking$StudioEventData == "14. r 2 - i-nc 2 r.wmv"))] <- "14a. r 2 - i-nc 2 r.wmv"
    tracking$StudioEventData[min(which(tracking$StudioEventData == "14. r 2 - i-nc 2 r.wmv"))] <- "14a. r 2 - i-nc 2 r.wmv"
    tracking$StudioEventData[min(which(tracking$StudioEventData == "anna i-nc 1 - i-pi-2 - r.wmv"))] <- "11. i-nc 1 - i-pi 2 r.wmv"
    tracking$StudioEventData[min(which(tracking$StudioEventData == "anna i-nc 1 - i-pi-2 - r.wmv"))] <- "11. i-nc 1 - i-pi 2 r.wmv"
  }

  if (box.name == "box1.2") {
    tracking$StudioEventData[min(which(tracking$StudioEventData == "5. r 1 - i-nc 2 r.wmv"))] <- "5a. r 1 - i-nc 2 r.wmv"
    tracking$StudioEventData[min(which(tracking$StudioEventData == "5. r 1 - i-nc 2 r.wmv"))] <- "5a. r 1 - i-nc 2 r.wmv"
  }

  if (box.name == "box1.3") {
    tracking$StudioEventData[min(which(tracking$StudioEventData == "7. i-pi 1 - r 2 r.wmv"))] <- "7a. i-pi 1 - r 2 r.wmv"
    tracking$StudioEventData[min(which(tracking$StudioEventData == "7. i-pi 1 - r 2 r.wmv"))] <- "7a. i-pi 1 - r 2 r.wmv"
  }

  if (box.name == "box2.1") {
    tracking$StudioEventData[min(which(tracking$StudioEventData == "5. r 1 - i-nc 2.wmv"))] <- "5a. r 1 - i-nc 2.wmv"
    tracking$StudioEventData[min(which(tracking$StudioEventData == "5. r 1 - i-nc 2.wmv"))] <- "5a. r 1 - i-nc 2.wmv"
  }

  if (box.name == "box3.2") {
    tracking$StudioEventData[min(which(tracking$StudioEventData == "10. i-nc 1 - r 2 .wmv"))] <- "10a. i-nc 1 - r 2 .wmv"
    tracking$StudioEventData[min(which(tracking$StudioEventData == "10. i-nc 1 - r 2 .wmv"))] <- "10a. i-nc 1 - r 2 .wmv"
    tracking$StudioEventData[min(which(tracking$StudioEventData == "4. r 1 - i-pi 2.wmv"))] <- "4a. r 1 - i-pi 2.wmv"
    tracking$StudioEventData[min(which(tracking$StudioEventData == "4. r 1 - i-pi 2.wmv"))] <- "4a. r 1 - i-pi 2.wmv"
  }

  # filling up empty rows so that loop isn't interrupted
  tracking$ValidityLeft[is.na(tracking$ValidityLeft)] <- 0
  tracking$ValidityRight[is.na(tracking$ValidityRight)] <- 0
  tracking$GazePointLeftX..ADCSpx.[is.na(tracking$GazePointLeftX..ADCSpx.)] <- 0
  tracking$GazePointLeftY..ADCSpx.[is.na(tracking$GazePointLeftY..ADCSpx.)] <- 0
  tracking$GazePointRightX..ADCSpx.[is.na(tracking$GazePointRightX..ADCSpx.)] <- 0
  tracking$GazePointRightY..ADCSpx.[is.na(tracking$GazePointRightY..ADCSpx.)] <- 0


  # create new variables for gaze location averaged across both eyes
  tracking$GazePoint_X <- 0
  tracking$GazePoint_Y <- 0

  tracking$GazePoint_X[tracking$ValidityLeft != 0 & tracking$ValidityRight != 0] <- 9999 # if measurement was invalid (!0) then we enter a number outside the range of possible pixels
  tracking$GazePoint_Y[tracking$ValidityLeft != 0 & tracking$ValidityRight != 0] <- 9999 # if measurement was invalid (!0) then we enter a number outside the range of possible pixels

  tracking$GazePoint_X[tracking$ValidityLeft == 0 & tracking$ValidityRight == 0] <-
    (tracking$GazePointRightX..ADCSpx.[tracking$ValidityLeft == 0 & tracking$ValidityRight == 0] +
      tracking$GazePointLeftX..ADCSpx.[tracking$ValidityLeft == 0 & tracking$ValidityRight == 0]) / 2
  tracking$GazePoint_Y[tracking$ValidityLeft == 0 & tracking$ValidityRight == 0] <-
    (tracking$GazePointRightY..ADCSpx.[tracking$ValidityLeft == 0 & tracking$ValidityRight == 0] +
      tracking$GazePointLeftY..ADCSpx.[tracking$ValidityLeft == 0 & tracking$ValidityRight == 0]) / 2

  tracking$GazePoint_X[tracking$ValidityLeft == 0 & tracking$ValidityRight != 0] <-
    tracking$GazePointLeftX..ADCSpx.[tracking$ValidityLeft == 0 & tracking$ValidityRight != 0]
  tracking$GazePoint_Y[tracking$ValidityLeft == 0 & tracking$ValidityRight != 0] <-
    tracking$GazePointLeftY..ADCSpx.[tracking$ValidityLeft == 0 & tracking$ValidityRight != 0]

  tracking$GazePoint_X[tracking$ValidityLeft != 0 & tracking$ValidityRight == 0] <-
    tracking$GazePointRightX..ADCSpx.[tracking$ValidityLeft != 0 & tracking$ValidityRight == 0]
  tracking$GazePoint_Y[tracking$ValidityLeft != 0 & tracking$ValidityRight == 0] <-
    tracking$GazePointRightY..ADCSpx.[tracking$ValidityLeft != 0 & tracking$ValidityRight == 0]

  tracking$GazePoint_X[tracking$GazeEventType != "Fixation"] <- 9999 # if measurement not a fixation then we enter a number outside the range of possible pixels
  tracking$GazePoint_Y[tracking$GazeEventType != "Fixation"] <- 9999 # if measurement not a fixation then we enter a number outside the range of possible pixels

  # inner loop: start extracting the fullscreen looking time data
  for (j in 1:nrow(file.orders)) {

    # fullscrenn
    XStart <- 0
    XEnd <- 1280
    YStart <- 0
    YEnd <- 1024

    a <- which(tracking$StudioEventData == eval(parse(text = paste("file.orders$", box.name, sep = "")))[j] &
      (tracking$StudioEvent == "MovieStart"))
    output$a[min(which(is.na(output$a)))] <- a
    output$a[min(which(is.na(output$a)))] <- a

    # to merge it with the AOI data, we extract it a second time, so that the number of rows is equal
    b <- which(tracking$StudioEventData == eval(parse(text = paste("file.orders$", box.name, sep = "")))[j] &
      (tracking$StudioEvent == "MovieEnd"))
    output$b[min(which(is.na(output$b)))] <- b
    output$b[min(which(is.na(output$b)))] <- b

    counting <- 0
    for (i in a:b) {
      if (tracking$GazePoint_X[i] > XStart & tracking$GazePoint_X[i] < XEnd & tracking$GazePoint_Y[i] > YStart & tracking$GazePoint_Y[i] < YEnd) {
        counting <- counting + 1
      }
    }

    output$time[min(which(is.na(output$time)))] <- counting
    output$time[min(which(is.na(output$time)))] <- counting
  }

  print(dateiName)
}

write.table(output, file = output.file.name, sep = "\t", col.names = T, row.names = F, quote = F, dec = ".", na = "")
