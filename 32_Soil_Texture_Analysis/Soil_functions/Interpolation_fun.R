# Function to Interpolate the Texture Class
Interpolation <- function(tmp4, texturebreak){
  # Vector of De smaller than criteria
  De_min1 = tmp4 %>% filter(tmp4$De < texturebreak); De_min1
  
  # Vector of largest remaining De
  De_min2 = De_min1 %>% filter(De_min1$De == max(De_min1$De)); De_min2
  
  # Vector of De larger than criteria
  De_max1 = tmp4 %>% filter(tmp4$De > texturebreak); De_max1
  # Vector of smallest remaining De
  De_max2 = De_max1 %>% filter(De_max1$De == min(De_max1$De)); De_max2
  
  x <- c(De_min2[,2], De_max2[,2]); x # De
  y <- c(De_min2[,3], De_max2[,3]); y # PF
  
  m = lm(y ~ x)$coeff[2] 
  b = lm(y ~ x)$coeff[1]
  
  x1 = texturebreak
  Percent = m*x1 + b
  
  return(Percent)
}