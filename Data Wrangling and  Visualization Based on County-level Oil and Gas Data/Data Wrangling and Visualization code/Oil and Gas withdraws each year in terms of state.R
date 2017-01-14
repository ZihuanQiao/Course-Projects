#Oil and Gas withdraws each year in terms of State
library(ggplot2)
library(dplyr)



#read tidy data 
dfnew <- read.table("oilTidyData.txt")
dfnew <- data.frame(dfnew)

#select top 10000 oil/ gas withdraw county data
dfnew.oil <- arrange(dfnew, desc(oilwithdraw))
dfnew.oil <- dfnew.oil[1:10000,]
dfnew.gas <- arrange(dfnew, desc(gaswithdraw))
dfnew.gas <- dfnew.gas[1:10000,]

#draw point plot and line to indicate oil/ gas withdraws from year to year in terms of state
gg <- ggplot(dfnew.oil, aes(x = year, y = oilwithdraw, colour = factor(Stabr)))
gg + geom_point() + labs(title = "Oil Withdraw from 2000 to 2011", x = "Year", y = "Oil Withdraw") 
qq <- ggplot(dfnew.gas, aes(x = year, y = gaswithdraw, colour = factor(Stabr)))
qq + geom_point() + labs(title = "Gas Withdraw from 2000 to 2011", x = "Year", y = "Gas Withdraw") 
