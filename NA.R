air_narm = airquality[!is.na(airquality$Ozone) & !is.na(airquality$Solar.R),]
mean(air_narm)
