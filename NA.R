#특정 컬럼이 가지고 있는 결측 값 처리 
air_narm = airquality[!is.na(airquality$Ozone) & !is.na(airquality$Solar.R),]
mean(air_narm$Ozone)
