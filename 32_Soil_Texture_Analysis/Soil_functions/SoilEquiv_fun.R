# Function to Calculate Ws 
SoilEquiv = function(tin, airdry, ovendry, tinmass, desiredmass = 50){
  hygro = round(ovendry/airdry, 2)
  wd = (airdry - ovendry)/(ovendry-tinmass)
  WS_e = round(desiredmass * (1 + wd), 2)
  output = data.frame(
    Parameter = c("Tin ID", "Tin Tare Weight", "Mass of air-dried soil", 
                  "Mass of oven-dried soil", "Hygroscopic Correction Factor", 
                  "Desired Oven-dried Soil", "Effective Soil Weight (WSe)"),
    Value = c(tin, tinmass, airdry, ovendry, hygro, desiredmass, WS_e),
    Box = c(12, 13, 14, 15, 17, 9, 18))
  print(output)
}

# Function to Calculate Hygroscopic Water
hygro = function(airdry, ovendry, tinmass){
  hygro = round(ovendry/airdry, 2) 
}

# Function to calculate Effective Soil Weight
WS_e = function(airdry, ovendry, tinmass, actualsoilmass){
  moisture_content = (airdry - ovendry)/(ovendry-tinmass)
  round(actualsoilmass * (1 + moisture_content), 2)
}

# Functions for Readings

# Time Conversation
ElapsedTimeConversion <- function(units){
  ifelse(units == "sec", 1/60, 
  ifelse(units == "min", 1/1, 
  ifelse(units == "hour", 60/1, 
  ifelse(units == NA, NA))))
}


# Function to impute Blanks and Temp between Readings
#-----------------------------------------------------
# Align Missing with Non-Missing
Et_aligning <- function(Et){
  ifelse(Et > 800, 1440,
  ifelse(Et > 90, 120,
  ifelse(Et > 30, 60,
  ifelse(Et >= 0, 0,
  ifelse(Et == NA, NA)))))
}

# Test function
Et_aligning(c(0.5, 2, 30,60, 150, 1440))

# Function to Impute Temperature
imputeT <- function(sampleID){
  library(dplyr)
  tmp <- sampleID %>% select(Et, Rblank, Temp)
  tmp$Et_Align <- Et_aligning(Test$Et) 
  
  # non missing, keeping Rblank and Et_Align
  nonmissingR = tmp[!is.na(tmp$Rblank),c(2,4)]; nonmissingR 
  missingR = tmp[is.na(tmp$Rblank),c(2,4)]; missingR
  nonmissingT = tmp[!is.na(tmp$Temp),c(3,4)]; nonmissingT
  missingT = tmp[is.na(tmp$Temp),c(3,4)]; missingT
  
  tmpR = left_join(missingR, nonmissingR, by="Et_Align")
  tmpT = left_join(missingT, nonmissingT, by="Et_Align")
  
  tmpT$Temp.y
}

imputeR <- function(sampleID){
  library(dplyr)
  tmp <- sampleID %>% select(Et, Rblank, Temp)
  tmp$Et_Align <- Et_aligning(Test$Et) 
  
  # non missing, keeping Rblank and Et_Align
  nonmissingR = tmp[!is.na(tmp$Rblank),c(2,4)]; nonmissingR 
  missingR = tmp[is.na(tmp$Rblank),c(2,4)]; missingR
  nonmissingT = tmp[!is.na(tmp$Temp),c(3,4)]; nonmissingT
  missingT = tmp[is.na(tmp$Temp),c(3,4)]; missingT
  
  tmpR = left_join(missingR, nonmissingR, by="Et_Align")
  tmpT = left_join(missingT, nonmissingT, by="Et_Align")
  tmpR$Rblank.y
}

# Function to Select K
Kfun <- function(T=22, GSp = 2.65){
  ifelse(T < 20, NA, 
  ifelse(T  == 20, 0.01365, 
  ifelse(T  == 21, 0.01348, 
  ifelse(T  == 22, 0.01332,
  ifelse(T  == 23, 0.01317,
  ifelse(T  == 24, 0.01301,
  ifelse(T  == 25, 0.01289,
  ifelse(T > 25, NA, NA))))))))
}