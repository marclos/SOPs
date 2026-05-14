# Function to calculate Effective Soil Weight
WS_e = function(airdry, ovendry, tinmass, actualsoilmass){
  moisture_content = (airdry - ovendry)/(ovendry-tinmass)
  round(actualsoilmass * (1 + moisture_content), 2)
}