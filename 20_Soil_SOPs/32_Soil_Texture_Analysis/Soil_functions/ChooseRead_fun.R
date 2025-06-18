# ChooseReadFunction
# First Select the sample.csv
# Second select the reading.csv

ChooseSample <- function(){
  sample <- read.csv(file.choose())
  return(sample)
}

ChooseReading <- function(){
  reading <- read.csv(file.choose())
  return(reading)
}

