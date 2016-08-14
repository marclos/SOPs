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
  tmp2 <- sampleID %>% select(Et, blank, temp)
  #tmp2 <- tmp %>% select(Et, blank, temp); tmp2
  
  missingT = tmp2[is.na(tmp2$temp),]; missingT
  nonmissingT = tmp2[!is.na(tmp2$temp),]; nonmissingT
  
  missing.mat <- matrix(missingT$Et, nrow=length(missingT$Et), ncol=length(nonmissingT$Et), byrow=F); missing.mat
  nonmissing.mat <- matrix(nonmissingT$Et, nrow=length(missingT$Et), ncol=length(nonmissingT$Et), byrow=T); nonmissing.mat
  
  W <- abs(missing.mat - nonmissing.mat)
  rownames(W) <- paste0("Missing", seq(nrow(W)))
  colnames(W) <- paste0("NonMissing", seq(ncol(W)))
  
  result <- t(sapply(seq(nrow(W)), function(i) {
    j <- which.min(W[i,])
    c(i, j)
  }))
  
  alignments <- data.frame(missing = result[,1], nonmissing=result[,2])
  #print(result)
  
return(as.vector(nonmissingT[result[,2],3]))
}

# Function to impute Reading Blanks
imputeR <- function(sampleID){
  library(dplyr)
  tmp2 <- sampleID %>% select(Et, blank, temp)
  
  missingR = tmp2[is.na(tmp2$blank),]; missingR
  nonmissingR = tmp2[!is.na(tmp2$blank),]; nonmissingR
  
  missing.mat <- matrix(missingR$Et, nrow=length(missingR$Et), ncol=length(nonmissingR$Et), byrow=F); missing.mat
  nonmissing.mat <- matrix(nonmissingR$Et, nrow=length(missingR$Et), ncol=length(nonmissingR$Et), byrow=T); nonmissing.mat
  
  W <- abs(missing.mat - nonmissing.mat)
  rownames(W) <- paste0("Missing", seq(nrow(W)))
  colnames(W) <- paste0("NonMissing", seq(ncol(W)))
  
  result <- t(sapply(seq(nrow(W)), function(i) {
    j <- which.min(W[i,])
    c(i, j)
  }))
  
  alignments <- data.frame(missingR = result[,1], nonmissingR=result[,2])
  #print(result)
  
  return(as.vector(nonmissingR[result[,2],2]))
}

# Old version!
#imputeR <- function(sampleID){
#  library(dplyr)
#  tmp <- sampleID %>% select(Et, Rblank, Temp)
#  tmp$Et_Align <- Et_aligning(Test$Et) 
  
  # non missing, keeping Rblank and Et_Align
#  nonmissingR = tmp[!is.na(tmp$Rblank),c(2,4)]; nonmissingR 
#  missingR = tmp[is.na(tmp$Rblank),c(2,4)]; missingR
#  nonmissingT = tmp[!is.na(tmp$Temp),c(3,4)]; nonmissingT
#  missingT = tmp[is.na(tmp$Temp),c(3,4)]; missingT
  
#  tmpR = left_join(missingR, nonmissingR, by="Et_Align")
#  tmpT = left_join(missingT, nonmissingT, by="Et_Align")
#  tmpR$Rblank.y
#}

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

# Function to Interpolate the Texture Class

Interpolation <- function(tmp3, texturebreak){

  De_min1 = tmp3 %>% filter(tmp3$De < texturebreak); De_min1
  De_min2 = De_min1 %>% filter(De_min1$De == max(De_min1$De)); De_min2
  
  De_max1 = tmp3 %>% filter(tmp3$De > texturebreak); De_max1
  De_max2 = De_max1 %>% filter(De_max1$De == min(De_max1$De)); De_max2
  
  x <- c(De_min2[,2], De_max2[,2]); x
  y <- c(De_min2[,3], De_max2[,3]); y
  
  m = lm(y ~ x)$coeff[2] 
  b = lm(y ~ x)$coeff[1]
  
  x1 = texturebreak
  Percent = m*x1 + b
  
  return(Percent)
}