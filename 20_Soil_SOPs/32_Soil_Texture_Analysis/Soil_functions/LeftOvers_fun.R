
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



# Function to impute Blanks and Temp between Readings
#-----------------------------------------------------




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