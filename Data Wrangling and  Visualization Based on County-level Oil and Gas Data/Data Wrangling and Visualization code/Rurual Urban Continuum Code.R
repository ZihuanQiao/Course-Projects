#Oil and Gas total gross withdrawals distribution on the Rural Urban Continuum Code
library(ggplot2)


#read tidy data and select oil and gas Rural Urban Continuum Code data in year 2011
dfnew <- read.table("oilTidyData.txt")
dfnew <- data.frame(dfnew)
dfnew.RUC <- select(dfnew, County_Name, Rural_Urban_Continuum_Code_2013, year:gaswithdraw)
dfnew.RUC2011 <- filter(dfnew.RUC, year == 2011)

#bar graphics indicating oil and gas withdraws corresponding to different Rural Urban Continuum Code
gg <- ggplot(data = dfnew.RUC2011, aes(x = Rural_Urban_Continuum_Code_2013, y = oilwithdraw))
gg + geom_bar(stat = "identity") + labs(title = "Oil withdraws with different Rural Urban Continuum Code", x = "Rural Urban Continuum Code", y = "Oil withdraws in 2011")

gg <- ggplot(data = dfnew.RUC2011, aes(x = Rural_Urban_Continuum_Code_2013, y = gaswithdraw))
gg + geom_bar(stat = "identity") + labs(title = "Gas withdraws with different Rural Urban Continuum Code", x = "Rural Urban Continuum Code", y = "Gas withdraws in 2011")
