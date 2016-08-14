# ChooseReadFunction
# First Select the sample.csv
# Second select the reading.csv

ChooseRead <- function(){
  sample <- read.csv(file.choose())
  reading <- read.csv(file.choose())
  return(reading)
  return(sample)
}
