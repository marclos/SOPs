# Removes sampleKEYs with missing data

RemoveMissingKEYs <- function(samplein, readingin){
  # Check sample and reading files
  library(dplyr)
  BadsampleKEYs <- filter(samplein, is.na(actualSoil) == T | is.na(totalSoil)==T) %>% select(sampleKEY); BadsampleKEYs
  BadreadingKEYs <- filter(readingin, is.na(timeUnit)==T) %>% select(sampleKEY) 
  
  # filter(sample, is.na(totalSoil)==T)
  badKEYs <- full_join(BadsampleKEYs, BadreadingKEYs); badKEYs
  reading <- readingin %>% anti_join(badKEYs, by="sampleKEY"); 
  sample <- samplein %>% anti_join(badKEYs, by="sampleKEY");
  
  # Write out Unprocessed samples
  unprocessed <- samplein %>% inner_join(badKEYs) %>% select(sampleKEY, sampleID, researcher); 
  
  write.csv(unprocessed, file = paste("/home/CAMPUS/mwl04747/github/SOPs/32_Soil_Texture_Analysis/Data/unprocessed_",Sys.Date(),".csv", sep=""))
 # return(reading)
 return(sample)
}