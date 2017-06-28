# Function to Select K
Kfun <- function(T=22, GSp = 2.65){
  ifelse(T < 19, NA,
  ifelse(T  == 19, 0.01382,         
  ifelse(T  == 20, 0.01365, 
  ifelse(T  == 21, 0.01348, 
  ifelse(T  == 22, 0.01332,
  ifelse(T  == 23, 0.01317,
  ifelse(T  == 24, 0.01301,
  ifelse(T  == 25, 0.01286,
  ifelse(T  == 26, 0.01272,       
  ifelse(T > 26, NA, NA))))))))))
}