# Impute Missing Blank and Temperature Readings
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

imputeT <- function(sampleID){
  library(dplyr)
  tmp2 <- sampleID %>% select(Et, blank, temp)
  
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
  
  alignments <- data.frame(missingT = result[,1], nonmissingT=result[,2])
  #print(result)
  
  return(as.vector(nonmissingT[result[,2],3]))
}



impute <- function(imputeneed){
  joined <- imputeneed[!is.na(imputeneed$Et),]
  KEY <- unique(joined$sampleKEY); KEY
  
  #KEY <- KEY[-c(1, 2, 3)]; KEY
  joined$temp2 = NA
  joined$blank2 = NA
  
  #for(i in 1:length(KEY)){
  for(i in 1:length(KEY)){
    # for tesing purposes... no 22
    #i = 77
    tmp = joined[joined$sampleKEY==KEY[i],]; tmp
    
    # Temperature Imputing
    tmp$temp[is.na(tmp$temp)] <- imputeT(tmp)
    joined[joined$sampleKEY==KEY[i],]$temp2 <- tmp$temp
    
    # Reading Blank Imputing
    tmp$blank[is.na(tmp$blank)] <- imputeR(tmp)
    joined[joined$sampleKEY==KEY[i],]$blank2 <- tmp$blank
  }
  return(joined)
}