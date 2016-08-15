# Time Conversation
ElapsedTimeConversion <- function(units){
  ifelse(units == "sec", 1/60, 
  ifelse(units == "min", 1/1, 
  ifelse(units == "hour", 60/1, 
  ifelse(units == NA, NA))))
}
