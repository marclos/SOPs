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