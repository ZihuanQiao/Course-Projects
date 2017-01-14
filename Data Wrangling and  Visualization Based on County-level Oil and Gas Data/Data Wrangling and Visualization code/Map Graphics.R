#Oil and Gas total gross withdrawals distribution map and withdrawals change level map on the county level
library(ggplot2)
library(magrittr)
library(maps)
library(dplyr)
library(tidyr)


#read tidy data and basic data summary
dfnew <- read.table("oilTidyData.txt")
dfnew <- data.frame(dfnew)


#Prepare dataset for Oil and Gas total gross withdrawals distribution on the county level from 2000 to 2011
#Create new varibles to indicate mean of oil withdraws and gas withdraws during 2000-2011
dfnew.county0 <- dfnew %>% select(Stabr, County_Name, year : oil_gas_change_group )
dfnew.countyoil <- dfnew.county0 %>% group_by(County_Name) %>% summarize(averageOil = mean(oilwithdraw))
dfnew.countygas <- dfnew.county0 %>% group_by(County_Name) %>% summarize(averageGas = mean(gaswithdraw))
dfnew.county <- merge(dfnew.countyoil, dfnew.countygas)
dfnew.county <- merge(dfnew.county, dfnew.county0, by="County_Name") 
dfnew.county <- select(dfnew.county, Stabr, County_Name:averageGas) %>% distinct()

dfnew.change <- dfnew %>% select(Stabr, County_Name, oil_change_group : oil_gas_change_group)
dfnew.county <- merge(dfnew.county, dfnew.change, by= c("Stabr", "County_Name"))

#Rename county_name and state abbreviation to the standard format is shown in the map regerence
colnames(dfnew.county)[1] <- "region"
colnames(dfnew.county)[2] <- "subregion"
dfnew.county <- mutate(dfnew.county, subregion=(gsub("County", "", subregion)))
for (i in 1:length(letters))
{
  dfnew.county$subregion <- sub(LETTERS[i], letters[i], dfnew.county$subregion)
}
for (i in 1:length(state.abb)) 
{
  dfnew.county$region <- sub(state.abb[i], state.name[i], dfnew.county$region)
}
for (i in 1:length(letters))
{
  dfnew.county$region <- sub(LETTERS[i], letters[i], dfnew.county$region)
}
dfnew.county <- mutate(dfnew.county,subregion=gsub(" ","",subregion))

dfnew.change <- select(dfnew.county, region:subregion, oil_change_group:oil_gas_change_group)
dfnew.county <- select(dfnew.county, -(oil_change_group:oil_gas_change_group))

#Add new columns to show average oil/ gas level, 9 levels altogether
dfnew.county <- mutate(dfnew.county, OilLevel=0)
dfnew.county <- mutate(dfnew.county, GasLevel=0)
for (i in 1:dim(dfnew.county)[1])
{
  if(dfnew.county$averageOil[i]>=0 & dfnew.county$averageOil[i]<10) {dfnew.county$OilLevel[i] <- 0}
  if(dfnew.county$averageOil[i]>=10 & dfnew.county$averageOil[i]<10^2) {dfnew.county$OilLevel[i] <- 1}
  if(dfnew.county$averageOil[i]>=10^2 & dfnew.county$averageOil[i]<10^3) {dfnew.county$OilLevel[i] <- 2}
  if(dfnew.county$averageOil[i]>=10^3 & dfnew.county$averageOil[i]<10^4) {dfnew.county$OilLevel[i] <- 3}
  if(dfnew.county$averageOil[i]>=10^4 & dfnew.county$averageOil[i]<10^5) {dfnew.county$OilLevel[i] <- 4}
  if(dfnew.county$averageOil[i]>=10^5 & dfnew.county$averageOil[i]<10^6) {dfnew.county$OilLevel[i] <- 5}
  if(dfnew.county$averageOil[i]>=10^6 & dfnew.county$averageOil[i]<10^7) {dfnew.county$OilLevel[i] <- 6}
  if(dfnew.county$averageOil[i]>=10^7 & dfnew.county$averageOil[i]<10^8) {dfnew.county$OilLevel[i] <- 7}
  if(dfnew.county$averageOil[i]>=10^8 & dfnew.county$averageOil[i]<10^9) {dfnew.county$OilLevel[i] <- 8}
}

for (i in 1:dim(dfnew.county)[1])
{
  if(dfnew.county$averageGas[i]>=0 & dfnew.county$averageGas[i]<100) {dfnew.county$GasLevel[i] <- 0}
  if(dfnew.county$averageGas[i]>=10^2 & dfnew.county$averageGas[i]<10^3) {dfnew.county$GasLevel[i] <- 1}
  if(dfnew.county$averageGas[i]>=10^3 & dfnew.county$averageGas[i]<10^4) {dfnew.county$GasLevel[i] <- 2}
  if(dfnew.county$averageGas[i]>=10^4 & dfnew.county$averageGas[i]<10^5) {dfnew.county$GasLevel[i] <- 3}
  if(dfnew.county$averageGas[i]>=10^5 & dfnew.county$averageGas[i]<10^6) {dfnew.county$GasLevel[i] <- 4}
  if(dfnew.county$averageGas[i]>=10^6 & dfnew.county$averageGas[i]<10^7) {dfnew.county$GasLevel[i] <- 5}
  if(dfnew.county$averageGas[i]>=10^7 & dfnew.county$averageGas[i]<10^8) {dfnew.county$GasLevel[i] <- 6}
  if(dfnew.county$averageGas[i]>=10^8 & dfnew.county$averageGas[i]<10^9) {dfnew.county$GasLevel[i] <- 7}
  if(dfnew.county$averageGas[i]>=10^9 & dfnew.county$averageGas[i]<10^10) {dfnew.county$GasLevel[i] <- 8}
}

dfnew.county <- data.frame(dfnew.county)
dfnew.county <- select(dfnew.county, -averageOil, -averageGas)

head(dfnew.county)


#Draw map graphics

#extract reference data
mapcounties <- map_data("county")
mapcounties <- data.frame(mapcounties)
mapcounties <- mapcounties[,c(5,6,1:4)]
mapstates <- map_data("state")
head(mapcounties)

#merge data with ggplot county coordinates
mergedata <- merge(x=dfnew.county, y=mapcounties, by.x=c("subregion","region"), by.y=c("subregion","region"), all= TRUE)
mergedata <- arrange(mergedata, group, order)


#draw maps of oil level distribution
map <- ggplot(mergedata, aes(long,lat,group=group)) + geom_polygon(aes(fill=factor(OilLevel)))
map <- map + scale_fill_brewer(palette="PuRd") +
  coord_map(project="polyconic") 
  
#add state borders
map <- map + geom_path(data = mapstates, colour = "white", size = .075)

#add county borders
map <- map + geom_path(data = mapcounties, colour = "white", size = .05, alpha = .1)

#add title
map <- map + ggtitle("Average Oil Level Distribution Map")

map

#draw maps of gas level distribution
map <- ggplot(mergedata, aes(long,lat,group=group)) + geom_polygon(aes(fill=factor(GasLevel)))
map <- map + scale_fill_brewer() +
  coord_map(project="polyconic") 

#add state borders
map <- map + geom_path(data = mapstates, colour = "white", size = .075)

#add county borders
map <- map + geom_path(data = mapcounties, colour = "white", size = .05, alpha = .1)

#add title
map <- map + ggtitle("Average Gas Level Distribution Map")

map



#subset and merge data of oil and gas withdrawals change level from 2000 to 2011 
mergedata.c <- merge(x=dfnew.change, y=mapcounties, by.x=c("subregion","region"), by.y=c("subregion","region"), all= TRUE)
mergedata.c <- arrange(mergedata.c, group, order)

#draw maps of oil withdrawals change level from 2000 to 2011 
map <- ggplot(mergedata.c, aes(long,lat,group=group)) + geom_polygon(aes(fill=oil_change_group))
map <- map + scale_fill_brewer(palette="PuRd") +
  coord_map(project="polyconic") 

#add state borders
map <- map + geom_path(data = mapstates, colour = "white", size = .075)

#add county borders
map <- map + geom_path(data = mapcounties, colour = "white", size = .05, alpha = .1)

#add title
map <- map + ggtitle("Oil Change Level Distribution Map")

map


#draw maps of gas withdrawals change level from 2000 to 2011 
map <- ggplot(mergedata.c, aes(long,lat,group=group)) + geom_polygon(aes(fill=gas_change_group))
map <- map + scale_fill_brewer() +
  coord_map(project="polyconic") 

#add state borders
map <- map + geom_path(data = mapstates, colour = "white", size = .075)

#add county borders
map <- map + geom_path(data = mapcounties, colour = "white", size = .05, alpha = .1)

map <- map + ggtitle("Gas Change Level Distribution Map")

map


#draw maps of oil and gas withdrawals change level from 2000 to 2011 
map <- ggplot(mergedata.c, aes(long,lat,group=group)) + geom_polygon(aes(fill=oil_gas_change_group))
map <- map + scale_fill_brewer(palette = "Spectral") +
  coord_map(project="polyconic") 

#add state borders
map <- map + geom_path(data = mapstates, colour = "white", size = .075)

#add county borders
map <- map + geom_path(data = mapcounties, colour = "white", size = .05, alpha = .1)

map <- map + ggtitle("Oil and Gas Change Level Distribution Map")

map



