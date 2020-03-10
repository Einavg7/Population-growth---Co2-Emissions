#creating a map of world population growth
pop_map <- read.csv("csv\\population.csv", header = T) 

#getting rid of unwanted columns and rows 
pop_map = pop_map [-1:-2,]
pop_map = pop_map [,c(-3:-34, -58:-62)]

#setting the names of the columns to our years 1990-2012
pop_map = setNames(pop_map, c("CountryName",'Country.Code', '1990':'2012')) 
pop_map = pop_map[-1,]

#deleting the na's in the data frame
pop_map = na.omit(pop_map)
sum(is.na(pop_map)) 

pop_map$Pop.Growth <- 
  ((pop_map$`2012` - pop_map$`1990`) / pop_map$`2012`) * 100

#installing rworld map package
install.packages("rworldmap")
library(rworldmap) 

mapped_data <- joinCountryData2Map(pop_map, joinCode = "ISO3", 
                                   nameJoinColumn = "Country.Code")
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
map_pop = mapCountryData(mapped_data, nameColumnToPlot = "Pop.Growth", 
                         colourPalette = 'topo', addLegend = F, oceanCol = 'lightblue'
                         , mapTitle = 'Population Growth Between 1990-2012', missingCountryCol = 'white',
                         borderCol = 'black' )
do.call( addMapLegend, c(map_pop, legendWidth=1, legendMar = 2))

#creating a map of world co2 emission  growth rate
co2_map <- read.csv('csv\\co2.csv', header = T) 

#getting rid of unwanted columns and rows 
co2_map = co2_map [-1:-2,]
co2_map = co2_map [,c(-3:-34, -58:-62)]

#setting the names of the columns to our years 1990-2012
co2_map = setNames(co2_map, c("CountryName",'Country.Code', '1990':'2012')) 
co2_map = co2_map[-1,]

#deleting the na's in the data frame
co2_map = na.omit(co2_map)
sum(is.na(co2_map)) 

co2_map$co2.Growth <- 
  ((co2_map$`2012` - co2_map$`1990`) / co2_map$`2012`) * 100

mapped_data2 <- joinCountryData2Map(co2_map, joinCode = "ISO3", 
                                   nameJoinColumn = "Country.Code")

library(RColorBrewer)
colourPalette <- RColorBrewer::brewer.pal(9, 'BuPu')


par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
map_co2 = mapCountryData(mapped_data2, nameColumnToPlot = "co2.Growth", 
                         colourPalette = colourPalette, addLegend = F, oceanCol = 'lightblue'
                         , mapTitle = 'Co2 Emissions Growth Rate Between 1990-2012', missingCountryCol = 'White',
                         borderCol = 'black' )
do.call( addMapLegend, c(map_co2, legendWidth=1, legendMar = 2))

#creating a map of world forest area decline rate
forest_map <- read.csv("csv\\forestareasqkm.csv", header = T) 

#getting rid of unwanted columns and rows 
forest_map = forest_map [-1:-2,]
forest_map = forest_map [,c(-3:-34, -58:-62)]

#setting the names of the columns to our years 1990-2012
forest_map = setNames(forest_map, c("CountryName",'Country.Code', '1990':'2012')) 
forest_map = forest_map[-1,]

#deleting the na's in the data frame
forest_map = na.omit(forest_map)
sum(is.na(forest_map)) 

forest_map$forest.decline <- 
  ((forest_map$`2012` - forest_map$`1990`) / forest_map$`2012`) * 100

mapped_data3 <- joinCountryData2Map(forest_map, joinCode = "ISO3", 
                                    nameJoinColumn = "Country.Code")

colourPalette <- RColorBrewer::brewer.pal(9, 'YlGn')


par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
map_forest = mapCountryData(mapped_data3, nameColumnToPlot = "forest.decline", 
                         colourPalette = colourPalette, addLegend = F, oceanCol = 'lightblue'
                         , mapTitle = 'Forest Area Decline Between 1990-2012', missingCountryCol = 'White',
                         borderCol = 'black' )
do.call( addMapLegend, c(map_forest, legendWidth=1, legendMar = 2))

#installing packages for organizing and presenting the data 
install.packages('tidyr')
library(tidyr)

install.packages('dplyr')
library(dplyr)

install.packages('ggplot2')
library(ggplot2)

#agriculture
#read the agriculture csv file on r
agriculture <- read.csv("csv\\agriculturesqm.csv")

#getting rid of unwanted columns and rows 
agriculture = agriculture [-1:-2,]
agriculture = agriculture[,c(-2:-34, -58:-62)]

#setting the names of the columns to our years 1990-2012
agriculture = setNames(agriculture, c("Country_Name", "agr90", "agr91", "agr92",
                                      "agr93", 'agr94', 'agr95', ' agr96', 'agr97', 'agr98', 
                                      'agr99', 'agr00', 'agr01', 'agr02', 'agr03', 'agr04', 'agr05',
                                      'agr06', "agr07", "agr08", "agr09", "agr10", "agr11", "agr12")) 
agriculture = agriculture[-1,]

#deleting the na's in the data frame
agriculture = na.omit(agriculture)
sum(is.na(agriculture)) 

#population
#read the population csv file on r
population <- read.csv("csv\\population.csv")

#getting rid of unwanted columns and rows 
population = population [-1:-2,]
population = population [,c(-2:-34, -58:-62)]

#setting the names of the columns to our years 1990-2012
population = setNames(population, c("Country_Name", "pop90", "pop91", "pop92",
                                      "pop93", 'pop94', 'pop95', 'pop96', 'pop97', 'pop98', 
                                      'pop99', 'pop00', 'pop01', 'pop02', 'pop03', 'pop04', 'pop05',
                                      'pop06', "pop07", "pop08", "pop09", "pop10", "pop11", "pop12")) 
population = population[-1,]

#deleting the na's in the data frame
population = na.omit(population)
sum(is.na(population)) 

#co2
#read the co2 csv file on r
co2 <- read.csv('csv\\co2.csv')

#getting rid of unwanted columns and rows 
co2 = co2 [-1:-2,]
co2 = co2[,c(-2:-34, -58:-62)]

#setting the names of the columns to our years 1990-2012
co2 = setNames(co2, c("Country_Name", "co2.90", "co2.91", "co2.92",
                      "co2.93", 'co2.94', 'co2.95', 'co2.96', 'co2.97', 'co2.98', 
                      'co2.99', 'co2.00', 'co2.01', 'co2.02', 'co2.03', 'co2.04', 'co2.05',
                      'co2.06', "co2.07", "co2.08", "co2.09", "co2.10", "co2.11", "co2.12"))
                       
co2 = co2[-1,]

#deleting the na's in the data frame
co2 = na.omit(co2)
sum(is.na(co2)) 

#forest_area
#read the forestareasqkm csv file on r
forestareasqkm = read.csv('csv\\forestareasqkm.csv')

#getting rid of unwanted columns and rows 
forestareasqkm = forestareasqkm [-1:-2,]
forestareasqkm = forestareasqkm[,c(-2:-34,-58:-62)]

#setting the names of the columns to our years 1990-2012
forestareasqkm = setNames(forestareasqkm, c("Country_Name", "forest90", "forest91", "forest92",
                                            "forest93", 'forest94', 'forest95', 'forest96', 'forest97', 'forest98', 
                                            'forest99', 'forest00', 'forest01', 'forest02', 'forest03', 'forest04', 'forest05',
                                            'forest06', "forest07", "forest08", "forest09", "forest10", "forest11", "forest12")) 
                                             
forestareasqkm = forestareasqkm[-1,]

#deleting the na's in the data frame
forestareasqkm = na.omit(forestareasqkm)
sum(is.na(forestareasqkm)) 


#livestock
#read the livestock csv file on r
livestock = read.csv('csv\\livestockquan.csv')

#removing unwanted columns and rows
livestock = livestock[,c(-1, -3)]

#spreading the data so the type of produce will be the name of the column
livestock= spread(livestock, key = Item, value = 4)

#Calculating the sum of production 
sum.livestock = data.frame(Total = rowSums(livestock[,3:5]))
livestock = cbind(livestock,sum.livestock)

#deleting the unwanted data
livestock = livestock [,-3:-5]
rm(sum.livestock) 

#spreading the data so the year will be the name of the column
livestock= spread(livestock, key = Year, value = 3)

#removing na's from the data
livestock = na.omit(livestock)
sum(is.na(livestock))

#setting the names of the columns to our years 1990-2014
livestock = setNames(livestock, c('Country_Name','live90', "live91", "live92",
                                  "live93", 'live94', 'live95', 'live96', 'live97',
                                  'live98', 'live99', 'live00', 'live01', 'live02', 'live03', 'live04', 'live05',
                                  'live06', "live07", "live08", "live09", "live10", "live11", "live12"))

#temperature
#read the temperatures csv file on r
temperature <- read.csv("csv\\GlobalLandTemperaturesByCountry.csv")

#remove unwanted data
temperature = temperature[,-3]
temperature= spread(temperature, key = 3, value = 2)
temperature = temperature[c(-1:-2954,-3231:-3239),c(-2,-11)]

#changing the row names to the ID of the row
row.names(temperature) = temperature [,1]
temperature = temperature[,-1]

#deleting na's 
temperature = na.omit(temperature)
sum(is.na(temperature))

#saving the col names as a dataframe for later
x = data.frame(colnames(temperature))
x = setNames(x, 'Country_Name')

#calculating the means of each year by country
meantemp_90 <- slice(temperature, 1:12)
meantemp.90 = colMeans(meantemp_90, na.rm = T)
meantemp_90 <- rbind(meantemp_90, meantemp.90)
meantemp_90 <- meantemp_90[-1:-12,]

meantemp_91 <- slice(temperature, 13:24)
meantemp.91 = colMeans(meantemp_91, na.rm = T)
meantemp_91 <- rbind(meantemp_91, meantemp.91)
meantemp_91 <- meantemp_91[-1:-12,]

meantemp_92 <- slice(temperature, 25:36)
meantemp.92 = colMeans(meantemp_92, na.rm = T)
meantemp_92 <- rbind(meantemp_92, meantemp.92)
meantemp_92 <- meantemp_92[-1:-12,]

meantemp_93 <- slice(temperature, 37:48)
meantemp.93 = colMeans(meantemp_93, na.rm = T)
meantemp_93 <- rbind(meantemp_93, meantemp.93)
meantemp_93 <- meantemp_93[-1:-12,]
  
meantemp_94 <- slice(temperature, 49:60)
meantemp.94 = colMeans(meantemp_94, na.rm = T)
meantemp_94 <- rbind(meantemp_94, meantemp.94)
meantemp_94 <- meantemp_94[-1:-12,]

meantemp_95 <- slice(temperature, 61:72)
meantemp.95 = colMeans(meantemp_95, na.rm = T)
meantemp_95 <- rbind(meantemp_95, meantemp.95)
meantemp_95 <- meantemp_95[-1:-12,]

meantemp_96 <- slice(temperature, 73:84)
meantemp.96 = colMeans(meantemp_96, na.rm = T)
meantemp_96 <- rbind(meantemp_96, meantemp.96)
meantemp_96 <- meantemp_96[-1:-12,]

meantemp_97 <- slice(temperature, 85:96)
meantemp.97 = colMeans(meantemp_97, na.rm = T)
meantemp_97 <- rbind(meantemp_97, meantemp.97)
meantemp_97 <- meantemp_97[-1:-12,]

meantemp_98 <- slice(temperature, 97:108)
meantemp.98 = colMeans(meantemp_98, na.rm = T)
meantemp_98 <- rbind(meantemp_98, meantemp.98)
meantemp_98 <- meantemp_98[-1:-12,]

meantemp_99 <- slice(temperature, 109:120)
meantemp.99 = colMeans(meantemp_99, na.rm = T)
meantemp_99 <- rbind(meantemp_99, meantemp.99)
meantemp_99 <- meantemp_99[-1:-12,]

meantemp_00 <- slice(temperature, 121:132)
meantemp.00 = colMeans(meantemp_00, na.rm = T)
meantemp_00 <- rbind(meantemp_00, meantemp.00)
meantemp_00 <- meantemp_00[-1:-12,]

meantemp_01 <- slice(temperature, 133:144)
meantemp.01 = colMeans(meantemp_01, na.rm = T)
meantemp_01 <- rbind(meantemp_01, meantemp.01)
meantemp_01 <- meantemp_01[-1:-12,]

meantemp_02 <- slice(temperature, 145:156)
meantemp.02 = colMeans(meantemp_02, na.rm = T)
meantemp_02 <- rbind(meantemp_02, meantemp.02)
meantemp_02 <- meantemp_02[-1:-12,]

meantemp_03 <- slice(temperature, 157:168)
meantemp.03 = colMeans(meantemp_03, na.rm = T)
meantemp_03 <- rbind(meantemp_03, meantemp.03)
meantemp_03 <- meantemp_03[-1:-12,]

meantemp_04 <- slice(temperature, 169:180)
meantemp.04 = colMeans(meantemp_04, na.rm = T)
meantemp_04 <- rbind(meantemp_04, meantemp.04)
meantemp_04 <- meantemp_04[-1:-12,]

meantemp_05 <- slice(temperature, 181:192)
meantemp.05 = colMeans(meantemp_05, na.rm = T)
meantemp_05 <- rbind(meantemp_05, meantemp.05)
meantemp_05 <- meantemp_05[-1:-12,]

meantemp_06 <- slice(temperature, 193:204)
meantemp.06 = colMeans(meantemp_06, na.rm = T)
meantemp_06 <- rbind(meantemp_06, meantemp.06)
meantemp_06 <- meantemp_06[-1:-12,]

meantemp_07 <- slice(temperature, 205:216)
meantemp.07 = colMeans(meantemp_07, na.rm = T)
meantemp_07 <- rbind(meantemp_07, meantemp.07)
meantemp_07 <- meantemp_07[-1:-12,]

meantemp_08 <- slice(temperature, 217:228)
meantemp.08 = colMeans(meantemp_08, na.rm = T)
meantemp_08 <- rbind(meantemp_08, meantemp.08)
meantemp_08 <- meantemp_08[-1:-12,]

meantemp_09 <- slice(temperature, 229:240)
meantemp.09 = colMeans(meantemp_09, na.rm = T)
meantemp_09 <- rbind(meantemp_09, meantemp.09)
meantemp_09 <- meantemp_09[-1:-12,]

meantemp_10 <- slice(temperature, 241:252)
meantemp.10 = colMeans(meantemp_10, na.rm = T)
meantemp_10 <- rbind(meantemp_10, meantemp.10)
meantemp_10 <- meantemp_10[-1:-12,]

meantemp_11 <- slice(temperature, 253:264)
meantemp.11 = colMeans(meantemp_11, na.rm = T)
meantemp_11 <- rbind(meantemp_11, meantemp.11)
meantemp_11 <- meantemp_11[-1:-12,]

meantemp_12 <- slice(temperature, 265:278)
meantemp.12 = colMeans(meantemp_12, na.rm = T)
meantemp_12 <- rbind(meantemp_12, meantemp.12)
meantemp_12 <- meantemp_12[-1:-12,]

#combining the means together
temperature = data.frame(rbind(meantemp_90, meantemp_91, meantemp_92, meantemp_93, meantemp_94,
                    meantemp_95, meantemp_96, meantemp_97, meantemp_98, meantemp_99,
                    meantemp_00, meantemp_01, meantemp_02, meantemp_03, meantemp_04,
                    meantemp_05, meantemp_06, meantemp_07, meantemp_08, meantemp_09,
                    meantemp_10, meantemp_11, meantemp_12))

#switching between the rows and the columns
temperature = data.frame(t(temperature)) 


#setting the column names
temperature = setNames(temperature, c("temp90", "temp91", "temp92",
                                  "temp93", 'temp94', 'temp95', 'temp96', 'temp97',
                                  'temp98', 'temp99', 'temp00', 'temp01', 'temp02', 'temp03', 'temp04', 'temp05',
                                  'temp06', "temp07", "temp08", "temp09", "temp10", "temp11", "temp12"))

#add a country name column
temperature = bind_cols(x,temperature)

#removing all the unnecessary data
rm(list = ls()[grepl("_", ls())])
rm(x, meantemp.00, meantemp.01, meantemp.02, meantemp.03, meantemp.04, meantemp.05,
   meantemp.06, meantemp.07, meantemp.08, meantemp.09, meantemp.10, meantemp.11, 
   meantemp.12, meantemp.90, meantemp.91, meantemp.92, meantemp.93, meantemp.94,
   meantemp.95, meantemp.96, meantemp.97, meantemp.98, meantemp.99)

#joining all the dataframes in order to see witch countries have all the data
join_data <- left_join(livestock, co2, by = 'Country_Name')
join_data = na.omit(join_data)

join_data2 <- left_join(join_data, temperature, by = 'Country_Name')
join_data2 = na.omit(join_data2)

join_data3 <- left_join(join_data2, agriculture, by = 'Country_Name')
join_data3 = na.omit(join_data3)

join_data4 <- left_join(join_data3, forestareasqkm, by = 'Country_Name')
join_data4 = na.omit(join_data4)

join_final <- left_join(join_data4, population, by ='Country_Name')
join_final = na.omit(join_final)

#remove the joined data that is irrelevant
rm(join_data, join_data2, join_data3, join_data4)


#calculating the means of all of the countries by variable by year 
means <- data.frame (colMeans(join_final[,-1]))
years <- c(1990:2012, 1990:2012, 1990:2012, 1990:2012, 1990:2012, 1990:2012)
subject <- c('livestock','livestock','livestock','livestock','livestock','livestock','livestock','livestock',
             'livestock','livestock','livestock','livestock','livestock','livestock','livestock','livestock','livestock','livestock','livestock','livestock','livestock','livestock',
             'livestock',
             'co2','co2','co2','co2','co2','co2','co2','co2','co2','co2','co2','co2','co2','co2',
             'co2','co2','co2','co2','co2','co2','co2','co2','co2',
             'temp','temp','temp','temp','temp','temp','temp','temp',
             'temp','temp','temp','temp','temp','temp','temp', 'temp','temp','temp','temp','temp','temp','temp','temp',
             'agr','agr','agr','agr','agr','agr',
             'agr','agr','agr','agr','agr','agr','agr','agr','agr','agr','agr','agr','agr','agr','agr','agr','agr',
             'forest', 'forest','forest','forest','forest','forest','forest','forest',
             'forest','forest','forest','forest','forest','forest','forest','forest','forest','forest','forest','forest','forest','forest','forest',
             'pop','pop','pop','pop','pop',
             'pop','pop','pop','pop','pop','pop','pop','pop','pop','pop','pop','pop','pop','pop','pop','pop','pop','pop')
             
means = cbind(means, years, subject)%>%
  setNames(c('means', 'years','subject'))

#change the values of the means to numeric
means$means = as.numeric(as.character(means$means))
rm(years, subject)

#ploting the means of each variable
filter(means, subject == 'temp')%>%
  ggplot(aes(years, means), colour = 'grey50') +
  geom_line(colour = "red3", lwd = 2) +
  labs(x = 'Years', y = 'Temperature Means (c°)', title = 'Mean Temperatures by Country')+
  theme(plot.title = element_text(size = 15,
                                  face = 'bold',
                                  hjust = 0.5,
                                  lineheight = 1.2),
        axis.title=element_text(face="bold.italic", 
                                      size="12"))

filter(means, subject == 'co2')%>%
  ggplot(aes(years, means), colour = 'grey50') +
  geom_line(colour = "gold2", lwd = 2) +
  labs(x = 'Years', y = 'Co2 Means (kt)', title = 'Mean Co2 Emissions by Country')+
  theme(plot.title = element_text(size = 15,
                                  face = 'bold',
                                  hjust = 0.5,
                                  lineheight = 1.2),
        axis.title=element_text(face="bold.italic", 
                                size="12"))

filter(means, subject == 'livestock')%>%
  ggplot(aes(years, means), colour = 'grey50') +
  geom_line(colour = "darkmagenta", lwd = 2) +
  labs(x = 'Years', y = 'Livestock Means (tonnes)', title = 'Mean Livestock Production by Country')+
  theme(plot.title = element_text(size = 15,
                                  face = 'bold',
                                  hjust = 0.5,
                                  lineheight = 1.2),
        axis.title=element_text(face="bold.italic", 
                                size="12"))

filter(means, subject == 'agr')%>%
  ggplot(aes(years, means), colour = 'grey50') +
  geom_line(colour = "chocolate4", lwd = 2) +
  labs(x = 'Years', y = 'Agriculture Means (sqkm)', title = 'Mean Agriculture sqkm by Country')+
  theme(plot.title = element_text(size = 15,
                                  face = 'bold',
                                  hjust = 0.5,
                                  lineheight = 1.2),
        axis.title=element_text(face="bold.italic", 
                                size="12"))

filter(means, subject == 'forest')%>%
  ggplot(aes(years, means), colour = 'grey50') +
  geom_line(colour = "green", lwd = 2) +
  labs(x = 'Years', y = 'Forest Area Means (sqkm)', title = 'Mean Forest Area skqm by Country')+
  theme(plot.title = element_text(size = 15,
                                  face = 'bold',
                                  hjust = 0.5,
                                  lineheight = 1.2),
        axis.title=element_text(face="bold.italic", 
                                size="12"))

filter(means, subject == 'pop')%>%
  ggplot(aes(years, means), colour = 'grey50') +
  geom_line(colour = "deepskyblue4", lwd = 2) +
  labs(x = 'Years', y = 'Population Means', title = 'Mean Population by Country')+
  theme(plot.title = element_text(size = 15,
                                  face = 'bold',
                                  hjust = 0.5,
                                  lineheight = 1.2),
        axis.title=element_text(face="bold.italic", 
                                size="12"))

#select the data by the variable and country names and change the column title
livestock = select(join_final, 'Country_Name',contains('live'))
livestock = livestock %>% 
  setNames (c("Country_Name", 1990:2012))  %>% 
  gather("1990":"2012" ,key = "year", value = "value")

population = select(join_final, 'Country_Name',contains('pop')) 
population = population %>% 
  setNames (c("Country_Name", 1990:2012))  %>% 
  gather("1990":"2012" ,key = "year", value = "value")

co2 = select(join_final, 'Country_Name',contains('co2'))
co2 = co2 %>% 
  setNames (c("Country_Name", 1990:2012))  %>% 
  gather("1990":"2012" ,key = "year", value = "value")

forestareasqkm = select(join_final, 'Country_Name', contains('forest'))
forestareasqkm = forestareasqkm %>% 
  setNames (c("Country_Name", 1990:2012))  %>% 
  gather("1990":"2012" ,key = "year", value = "value")

temperature = select(join_final, 'Country_Name', contains('temp'))
temperature = temperature %>% 
  setNames (c("Country_Name", 1990:2012))  %>% 
  gather("1990":"2012" ,key = "year", value = "value")

agriculture = select(join_final, 'Country_Name', contains('agr'))
agriculture = agriculture %>% 
  setNames (c("Country_Name", 1990:2012))  %>% 
  gather("1990":"2012" ,key = "year", value = "value")



all_data <- data.frame(cbind(livestock$Country_Name,livestock$year,livestock$value, co2$value, agriculture$value,
                             forestareasqkm$value, population$value, temperature$value ))
all_data <- setNames(all_data, c('country_name','year', 'livestock', 'co2' , 'agr', 'forest', 'pop', 'temp'))

#change the values to numeric for statistics
all_data$livestock = as.numeric(as.character(all_data$livestock))
all_data$pop = as.numeric(as.character(all_data$pop))
all_data$co2 = as.numeric(as.character(all_data$co2))
all_data$temp = as.numeric(as.character(all_data$temp))
all_data$agr = as.numeric(as.character(all_data$agr))
all_data$forest = as.numeric(as.character(all_data$forest))

str(all_data)

#add data of the continents from github
install.packages("RCurl")
library(RCurl)
require(Rcurl)
continents <- read.csv(text = getURL('https://raw.githubusercontent.com/pdelboca/udacity-data-analysis-with-r/master/data/Countries-Continents.csv'), header = T)
continents <- setNames(continents, c('continent', 'country_name'))

#join between the countries and continents 
all_data_cont <- left_join(all_data, continents, by = 'country_name')
all_data_cont = na.omit(all_data_cont)
rm(continents)

#plot the co2 and population data grouped by continent
ggplot(data = all_data_cont) +
  geom_point(mapping = aes(x = pop, y = co2, color = continent, size = co2 ))+
  labs(x = 'Log Population (#)', y ='Log Co2 (kt)', title = "Co2 vs Population Grouped by Continents")+
  scale_x_continuous(trans = 'log')+
  scale_y_continuous(trans = 'log')

#plot the livestock and population data grouped by continent
ggplot(data = all_data_cont) +
  geom_smooth(mapping = aes(x = pop, y = livestock, color = continent, size = livestock ), method = 'loess')+
  labs(x = 'Log Population (#)', y ='Log Livestock (tonnes)', title = "Livestock vs Population Grouped by Continents")+
  scale_x_continuous(trans = 'log')+
  scale_y_continuous(trans = 'log')

#plot the livestock and co2 data grouped by continent
ggplot(data = all_data_cont) +
  geom_smooth(mapping = aes(x = co2, y = livestock, color = continent, size = livestock ), method = 'loess')+
  labs(x = 'Log Co2 (kt)', y ='Log Livestock (tonnes)', title = "Livestock vs Co2 Grouped by Continents")+
  scale_x_continuous(trans = 'log')+
  scale_y_continuous(trans = 'log')

#dividing the data to the 10 most developed and the 10 most undeveloped by HDI
hdi_top_low <- all_data %>%
  filter(country_name == 'Central African Republic' | country_name == 'Niger' |
           country_name == 'Chad' | country_name == 'Burkina Faso' |
           country_name == 'Burundi' | country_name == 'Guinea' | country_name == 'Mozambique'|
           country_name == 'Sierra Leone' | country_name == 'Liberia' | country_name == 'Mali'|
           country_name == 'Norway' | country_name == 'Australia' |
           country_name == 'Switzerland' | country_name == 'Denmark' |
           country_name == 'Netherlands' | country_name == 'Ireland' | country_name == 'Iceland'|
           country_name == 'Canada' | country_name == 'New Zealand' | country_name == 'Sweden')

hdi_top_low$category <- ifelse(hdi_top_low$country_name ==  'Central African Republic' | hdi_top_low$country_name == 'Niger' |
                                 hdi_top_low$country_name == 'Chad' |hdi_top_low$country_name == 'Burkina Faso' |
                                 hdi_top_low$country_name == 'Burundi' | hdi_top_low$country_name == 'Guinea' | hdi_top_low$country_name =='Mozambique'|
                                 hdi_top_low$country_name == 'Sierra Leone' | hdi_top_low$country_name == 'Liberia' | hdi_top_low$country_name == 'Mali',
                               "undeveloped", 'developed')

#plot of the co2 emissions developed vs undeveloped
ggplot(data = hdi_top_low) +
  geom_smooth(mapping = aes(x = year, y = (co2), color = category, group = category), method = "loess")+
  labs(x = 'Years', y = 'Co2 (kt)', title = "Co2 Emissions between Developed and Undeveloped Countries 1990-2012")+
  scale_y_continuous(trans = 'log')

#plot of the population developed vs undeveloped
ggplot(data = hdi_top_low) +
  geom_smooth(mapping = aes(x = year, y = (pop), color = category, group = category), method = "loess")+
  labs(x = 'Years', y = 'Popuation (#)', title = "Population Growth Developed and Undeveloped Countries 1990-2012")

#bar plot that shows the co2 emissions of each country over the years by developed and undeveloped countries
ggplot(hdi_top_low, aes(x = country_name, y = co2, fill = category))+
  geom_bar(stat = 'identity', width = 0.5)+
  labs(title = 'Co2 Emissions Developed and Undeveloped Countries',
       x = 'Country Name', y = 'Log Co2 Emissions')+
  theme(axis.text.x = element_text(angle=65, vjust=0.6, size = 10),plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(trans = 'log')

#statistical tests

#creating a new dataframe with columns of the means of each variable
all_means = spread(means, key = 3, value = 1)

#changing the row names 
row.names(all_means) = all_means [,1]
all_means = all_means[,-1]

#shapiro test for all Means of each variable 
shapiro.test(all_means$pop)
shapiro.test(all_means$co2)
shapiro.test(all_means$agr)
shapiro.test(all_means$forest)
shapiro.test(all_means$livestock)
shapiro.test(all_means$temp)

#correlation matrix to see the correlation of every variable
install.packages('ggcorrplot')
library(ggcorrplot)

corr <- round(cor(all_data[,3:8]), 1)

#Plot
ggcorrplot(corr, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("indianred3", "white", "springgreen3"), 
           title="Correlogram by Variable", 
           ggtheme=theme_bw)

#pearson test
#co2~pop
cor.test(all_data$co2, all_data$pop, method = "pearson")
#co2~livestock
cor.test(all_data$co2, all_data$livestock, method = "pearson")
#pop~livestock
cor.test(all_data$pop, all_data$livestock, method = "pearson")
#pop~agr
cor.test(all_data$pop, all_data$agr, method = "pearson")
#livestock~agr
cor.test(all_data$livestock, all_data$agr, method = "pearson")

#Multiple regression between co2 and all other variables
Multiple_regression= lm(co2~pop+agr+livestock+temp+forest, data=all_data)
summary(Multiple_regression)

#creating 2 different data frames for developed and undeveloped 
developed = filter(hdi_top_low, category == 'developed')
undeveloped = filter(hdi_top_low, category == 'undeveloped')

#shapiro test for developed and undeveloped countries
shapiro.test(developed$co2)
shapiro.test(undeveloped$co2)

#Independent T-Test developed vs undeveloped
t.test(developed$co2, undeveloped$co2)

  
  
