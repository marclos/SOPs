# ChooseReadFunction
# First Select the sample.csv
# Second select the reading.csv

ChooseRead <- function(){
  reading <- read.csv(file.choose())
  sample <- read.csv(file.choose())
  return(reading)
  return(sample)
}
