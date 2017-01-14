#Oil and Gas total gross withdrawals distribution on the state level

library(ggplot2)
library(magrittr)


#read tidy data and select subset
dfnew <- read.table("oilTidyData.txt")
attach(dfnew)
dfnew.state <- dfnew %>% select(Stabr, year : gaswithdraw)


#Draw bar graghics: Oil total gross withdrawals distribution on the state level
gg <- ggplot(data = dfnew.state, aes(x = Stabr, y = oilwithdraw, fill = factor(year))) 
gg + geom_bar(stat = "identity") + 
  labs(title = "Oil total gross withdrawals from 2000 to 2011", x = "State", y = "Total Oil Withdrawals")

#Draw bar graghics: Gas total gross withdrawals distribution on the state level
gg <- ggplot(data = dfnew.state, aes(x = Stabr, y = gaswithdraw, fill = factor(year))) 
gg + geom_bar(stat = "identity") + 
  labs(title = "Gas total gross withdrawals from 2000 to 2011", x = "State", y = "Total Gas Withdrawals")
