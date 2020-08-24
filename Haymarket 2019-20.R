setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymaket Jan-Mar19")
January_Data <- read.csv("Haymarket Jan Clean.csv",header = TRUE)
setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Feb")
February_Data <- read.csv("Haymarket Feb Clean.csv",header = TRUE)
setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Mar")
March_Data <- read.csv("Haymarket Mar Clean.csv",header = TRUE)
setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Apr")
April_Data <- read.csv("Haymarket Apr Clean.csv",header = TRUE)
setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_May")
May_Data <- read.csv("Haymarket May Clean.csv",header = TRUE)
setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Jun")
June_Data <- read.csv("Haymarket Jun Clean.csv",header = TRUE)
setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Jul")
July_Data <- read.csv("Haymarket Jul Clean.csv",header = TRUE)
setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Aug")
August_Data <- read.csv("Haymarket Aug Clean.csv",header = TRUE)
setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Sep")
September_Data <- read.csv("Haymarket Sep Clean.csv",header = TRUE)
setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Oct")
October_Data <- read.csv("Haymarket Oct Clean.csv",header = TRUE)
setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Nov")
November_Data <- read.csv("Haymarket Nov Clean.csv",header = TRUE)
setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Dec")
Decembet_Data <- read.csv("Haymarket Dec Clean.csv",header = TRUE)
setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Jan20")
January20_Data <- read.csv("Haymarket Jan20 Clean.csv",header = TRUE)
setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Feb20")
February20_Data <- read.csv("Haymarket Feb20 Clean.csv",header = TRUE)
setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Mar20")
March20_Data <- read.csv("Haymarket Mar20 Clean.csv",header = TRUE)
setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Apr20")
April20_Data <- read.csv("Haymarket Apr20 Clean.csv",header = TRUE)
setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_May20")
May20_Data <- read.csv("Haymarket May20 Clean.csv",header = TRUE)
class(August_Data$Temperature)
August_Data$Temperature<-as.character(August_Data$Temperature)
str(August_Data)

list_clnms = c(colnames(August_Data))
for (i in 1:length(list_clnms)){
  August_Data[, list_clnms[i]] <- ifelse(August_Data[, list_clnms[i]]>=0, August_Data[, list_clnms[i]], NA)}
str(August_Data)

as.POSIXlt.character(August_Data$Date_Time,tz="GMT",format="%Y-%m-%d %H:%M",optional=FALSE)
August_Data$Date_Time <- as.POSIXlt.character(August_Data$Date_Time,tz="GMT",format="%Y-%m-%d %H:%M",optional=FALSE)
str(July_Data)
August_Data$Temperature<-as.numeric(August_Data$Temperature)
class(August_Data$Temperature)

anyNA(August_Data)
anyNA(August_Data$Temperature)
class(April20_Data$Temperature)
April20_Data$Temperature<-as.character(April20_Data$Temperature)
str(April20_Data)

list_clnms = c(colnames(April20_Data))
for (i in 1:length(list_clnms)){
  April20_Data[, list_clnms[i]] <- ifelse(April20_Data[, list_clnms[i]]>=0, April20_Data[, list_clnms[i]], NA)}
str(April20_Data)

as.POSIXlt.character(April20_Data$Date_Time,tz="GMT",format="%Y-%m-%d %H:%M",optional=FALSE)
April20_Data$Date_Time <- as.POSIXlt.character(April20_Data$Date_Time,tz="GMT",format="%Y-%m-%d %H:%M",optional=FALSE)
str(April20_Data)
April20_Data$Temperature<-as.numeric(April20_Data$Temperature)
class(April20_Data$Temperature)

anyNA(April20_Data)
anyNA(April20_Data$Temperature)
class(April_Data$Temperature)
April_Data$Temperature<-as.character(April_Data$Temperature)
str(April_Data)

list_clnms = c(colnames(April_Data))
for (i in 1:length(list_clnms)){
  April_Data[, list_clnms[i]] <- ifelse(April_Data[, list_clnms[i]]>=0, April_Data[, list_clnms[i]], NA)}
str(April_Data)

as.POSIXlt.character(April_Data$Date_Time,tz="GMT",format="%Y-%m-%d %H:%M",optional=FALSE)
April_Data$Date_Time <- as.POSIXlt.character(April_Data$Date_Time,tz="GMT",format="%Y-%m-%d %H:%M",optional=FALSE)
str(April_Data)
April_Data$Temperature<-as.numeric(April_Data$Temperature)
class(April_Data$Temperature)

anyNA(April_Data)
anyNA(April_Data$Temperature)
class(January_Data$Temperature)
January_Data$Temperature<-as.character(January_Data$Temperature)
str(January_Data)

list_clnms = c(colnames(January_Data))
for (i in 1:length(list_clnms)){
  January_Data[, list_clnms[i]] <- ifelse(January_Data[, list_clnms[i]]>=0, January_Data[, list_clnms[i]], NA)}
str(January_Data)

as.POSIXlt.character(January_Data$Date_Time,tz="GMT",format="%Y-%m-%d %H:%M",optional=FALSE)
January_Data$Date_Time <- as.POSIXlt.character(January_Data$Date_Time,tz="GMT",format="%Y-%m-%d %H:%M",optional=FALSE)
str(January_Data)
January_Data$Temperature<-as.numeric(January_Data$Temperature)
class(January_Data$Temperature)

anyNA(January_Data)
anyNA(January_Data$Temperature)
class(Decembet_Data$Temperature)
Decembet_Data$Temperature<-as.character(Decembet_Data$Temperature)
str(Decembet_Data)

list_clnms = c(colnames(Decembet_Data))
for (i in 1:length(list_clnms)){
  Decembet_Data[, list_clnms[i]] <- ifelse(Decembet_Data[, list_clnms[i]]>=0, Decembet_Data[, list_clnms[i]], NA)}
str(Decembet_Data)

as.POSIXlt.character(Decembet_Data$Date_Time,tz="GMT",format="%Y-%m-%d %H:%M",optional=FALSE)
Decembet_Data$Date_Time <- as.POSIXlt.character(Decembet_Data$Date_Time,tz="GMT",format="%Y-%m-%d %H:%M",optional=FALSE)
str(Decembet_Data)
Decembet_Data$Temperature<-as.numeric(Decembet_Data$Temperature)
class(Decembet_Data$Temperature)

anyNA(Decembet_Data)
anyNA(Decembet_Data$Temperature)
class(February_Data$Temperature)
February_Data$Temperature<-as.character(February_Data$Temperature)
str(February_Data)

list_clnms = c(colnames(February_Data))
for (i in 1:length(list_clnms)){
  February_Data[, list_clnms[i]] <- ifelse(February_Data[, list_clnms[i]]>=0, February_Data[, list_clnms[i]], NA)}
str(February_Data)

as.POSIXlt.character(February_Data$Date_Time,tz="GMT",format="%Y-%m-%d %H:%M",optional=FALSE)
February_Data$Date_Time <- as.POSIXlt.character(February_Data$Date_Time,tz="GMT",format="%Y-%m-%d %H:%M",optional=FALSE)
str(February_Data)
February_Data$Temperature<-as.numeric(February_Data$Temperature)
class(February_Data$Temperature)

anyNA(February_Data)
anyNA(February_Data$Temperature)

class(February20_Data$Temperature)
February20_Data$Temperature<-as.character(February20_Data$Temperature)
str(February20_Data)

list_clnms = c(colnames(February20_Data))
for (i in 1:length(list_clnms)){
  February20_Data[, list_clnms[i]] <- ifelse(February20_Data[, list_clnms[i]]>=0, February20_Data[, list_clnms[i]], NA)}
str(February20_Data)

as.POSIXlt.character(February20_Data$Date_Time,tz="GMT",format="%Y-%m-%d %H:%M",optional=FALSE)
February20_Data$Date_Time <- as.POSIXlt.character(February20_Data$Date_Time,tz="GMT",format="%Y-%m-%d %H:%M",optional=FALSE)
str(February20_Data)
February20_Data$Temperature<-as.numeric(February20_Data$Temperature)
class(February20_Data$Temperature)

anyNA(February20_Data)
anyNA(February20_Data$Temperature)
class(January20_Data$Temperature)
January20_Data$Temperature<-as.character(January20_Data$Temperature)
str(January20_Data)

list_clnms = c(colnames(January20_Data))
for (i in 1:length(list_clnms)){
  January20_Data[, list_clnms[i]] <- ifelse(January20_Data[, list_clnms[i]]>=0, January20_Data[, list_clnms[i]], NA)}
str(January20_Data)

as.POSIXlt.character(January20_Data$Date_Time,tz="GMT",format="%Y-%m-%d %H:%M",optional=FALSE)
January20_Data$Date_Time <- as.POSIXlt.character(January20_Data$Date_Time,tz="GMT",format="%Y-%m-%d %H:%M",optional=FALSE)
str(January20_Data)
January20_Data$Temperature<-as.numeric(January20_Data$Temperature)
class(January20_Data$Temperature)

anyNA(January20_Data)
anyNA(January20_Data$Temperature)
class(July_Data$Temperature)
July_Data$Temperature<-as.character(July_Data$Temperature)
str(July_Data)

list_clnms = c(colnames(July_Data))
for (i in 1:length(list_clnms)){
  July_Data[, list_clnms[i]] <- ifelse(July_Data[, list_clnms[i]]>=0, July_Data[, list_clnms[i]], NA)}
str(July_Data)

as.POSIXlt.character(July_Data$Date_Time,tz="GMT",format="%Y-%m-%d %H:%M",optional=FALSE)
July_Data$Date_Time <- as.POSIXlt.character(July_Data$Date_Time,tz="GMT",format="%Y-%m-%d %H:%M",optional=FALSE)
str(July_Data)
July_Data$Temperature<-as.numeric(July_Data$Temperature)
class(July_Data$Temperature)

anyNA(July_Data)
anyNA(July_Data$Temperature)
class(June_Data$Temperature)
June_Data$Temperature<-as.character(June_Data$Temperature)
str(June_Data)

list_clnms = c(colnames(June_Data))
for (i in 1:length(list_clnms)){
  June_Data[, list_clnms[i]] <- ifelse(June_Data[, list_clnms[i]]>=0, June_Data[, list_clnms[i]], NA)}
str(June_Data)

as.POSIXlt.character(June_Data$Date_Time,tz="GMT",format="%Y-%m-%d %H:%M",optional=FALSE)
June_Data$Date_Time <- as.POSIXlt.character(June_Data$Date_Time,tz="GMT",format="%Y-%m-%d %H:%M",optional=FALSE)
str(June_Data)
June_Data$Temperature<-as.numeric(June_Data$Temperature)
class(June_Data$Temperature)

anyNA(June_Data)
anyNA(June_Data$Temperature)
class(March_Data$Temperature)
March_Data$Temperature<-as.character(March_Data$Temperature)
str(March_Data)

list_clnms = c(colnames(March_Data))
for (i in 1:length(list_clnms)){
  March_Data[, list_clnms[i]] <- ifelse(March_Data[, list_clnms[i]]>=0, March_Data[, list_clnms[i]], NA)}
str(March_Data)

as.POSIXlt.character(March_Data$Date_Time,tz="GMT",format="%Y-%m-%d %H:%M",optional=FALSE)
March_Data$Date_Time <- as.POSIXlt.character(March_Data$Date_Time,tz="GMT",format="%Y-%m-%d %H:%M",optional=FALSE)
str(February_Data)
March_Data$Temperature<-as.numeric(March_Data$Temperature)
class(March_Data$Temperature)

anyNA(March_Data)
anyNA(March_Data$Temperature)
class(March20_Data$Temperature)
March20_Data$Temperature<-as.character(March20_Data$Temperature)
str(March20_Data)

list_clnms = c(colnames(March20_Data))
for (i in 1:length(list_clnms)){
  March20_Data[, list_clnms[i]] <- ifelse(March20_Data[, list_clnms[i]]>=0, March20_Data[, list_clnms[i]], NA)}
str(March20_Data)

as.POSIXlt.character(March20_Data$Date_Time,tz="GMT",format="%Y-%m-%d %H:%M",optional=FALSE)
March20_Data$Date_Time <- as.POSIXlt.character(March20_Data$Date_Time,tz="GMT",format="%Y-%m-%d %H:%M",optional=FALSE)
str(March20_Data)
March20_Data$Temperature<-as.numeric(March20_Data$Temperature)
class(March20_Data$Temperature)

anyNA(March20_Data)
anyNA(March20_Data$Temperature)
class(May_Data$Temperature)
May_Data$Temperature<-as.character(May_Data$Temperature)
str(May_Data)

list_clnms = c(colnames(May_Data))
for (i in 1:length(list_clnms)){
  May_Data[, list_clnms[i]] <- ifelse(May_Data[, list_clnms[i]]>=0, May_Data[, list_clnms[i]], NA)}
str(May_Data)

as.POSIXlt.character(May_Data$Date_Time,tz="GMT",format="%Y-%m-%d %H:%M",optional=FALSE)
May_Data$Date_Time <- as.POSIXlt.character(May_Data$Date_Time,tz="GMT",format="%Y-%m-%d %H:%M",optional=FALSE)
str(May_Data)
May_Data$Temperature<-as.numeric(May_Data$Temperature)
class(May_Data$Temperature)

anyNA(May_Data)
anyNA(May_Data$Temperature)
class(May20_Data$Temperature)
May_Data$Temperature<-as.character(May20_Data$Temperature)
str(May20_Data)

list_clnms = c(colnames(May20_Data))
for (i in 1:length(list_clnms)){
  May20_Data[, list_clnms[i]] <- ifelse(May20_Data[, list_clnms[i]]>=0, May20_Data[, list_clnms[i]], NA)}
str(May20_Data)

as.POSIXlt.character(May20_Data$Date_Time,tz="GMT",format="%Y-%m-%d %H:%M",optional=FALSE)
May20_Data$Date_Time <- as.POSIXlt.character(May20_Data$Date_Time,tz="GMT",format="%Y-%m-%d %H:%M",optional=FALSE)
str(May20_Data)
May20_Data$Temperature<-as.numeric(May20_Data$Temperature)
class(May20_Data$Temperature)

anyNA(May20_Data)
anyNA(May20_Data$Temperature)
class(November_Data$Temperature)
November_Data$Temperature<-as.character(November_Data$Temperature)
str(November_Data)

list_clnms = c(colnames(November_Data))
for (i in 1:length(list_clnms)){
  November_Data[, list_clnms[i]] <- ifelse(November_Data[, list_clnms[i]]>=0, November_Data[, list_clnms[i]], NA)}
str(November_Data)

as.POSIXlt.character(November_Data$Date_Time,tz="GMT",format="%Y-%m-%d %H:%M",optional=FALSE)
November_Data$Date_Time <- as.POSIXlt.character(November_Data$Date_Time,tz="GMT",format="%Y-%m-%d %H:%M",optional=FALSE)
str(November_Data)
November_Data$Temperature<-as.numeric(November_Data$Temperature)
class(November_Data$Temperature)

anyNA(November_Data)
anyNA(November_Data$Temperature)
class(October_Data$Temperature)
October_Data$Temperature<-as.character(October_Data$Temperature)
str(October_Data)

list_clnms = c(colnames(October_Data))
for (i in 1:length(list_clnms)){
  October_Data[, list_clnms[i]] <- ifelse(October_Data[, list_clnms[i]]>=0, October_Data[, list_clnms[i]], NA)}
str(October_Data)

as.POSIXlt.character(October_Data$Date_Time,tz="GMT",format="%Y-%m-%d %H:%M",optional=FALSE)
October_Data$Date_Time <- as.POSIXlt.character(October_Data$Date_Time,tz="GMT",format="%Y-%m-%d %H:%M",optional=FALSE)
str(October_Data)
October_Data$Temperature<-as.numeric(October_Data$Temperature)
class(October_Data$Temperature)

anyNA(October_Data)
anyNA(October_Data$Temperature)
class(September_Data$Temperature)
September_Data$Temperature<-as.character(September_Data$Temperature)
str(September_Data)

list_clnms = c(colnames(September_Data))
for (i in 1:length(list_clnms)){
  September_Data[, list_clnms[i]] <- ifelse(September_Data[, list_clnms[i]]>=0, September_Data[, list_clnms[i]], NA)}
str(September_Data)

as.POSIXlt.character(September_Data$Date_Time,tz="GMT",format="%Y-%m-%d %H:%M",optional=FALSE)
September_Data$Date_Time <- as.POSIXlt.character(September_Data$Date_Time,tz="GMT",format="%Y-%m-%d %H:%M",optional=FALSE)
str(September_Data)
September_Data$Temperature<-as.numeric(September_Data$Temperature)
class(September_Data$Temperature)

anyNA(September_Data)
anyNA(September_Data$Temperature)

January_Data <- January_Data[-c(5,6)]
July_Data <- July_Data[-c(10,11,13)]
August_Data <- August_Data[-c(15,16)]
April_Data <- April_Data[-c(15,16)]
Decembet_Data<-Decembet_Data[-c(10,15)]
January_Data<-January_Data[-c(15,16)]
January_Data<-January_Data[-c(1523969:2361528),]
February_Data <- February_Data[-c(15,16)]
June_Data <- June_Data[-c(15,16)]
March_Data <- March_Data[-c(15,16)]
May_Data <- May_Data[-c(15,16)]
November_Data <- November_Data[-c(15,16)]
October_Data <- October_Data[-c(15,16)]
September_Data <- September_Data[-c(15,16)]



colnames(January_Data)
names(January_Data)[names(January_Data) == 'Vehicle.Count.y'] <- 'Vehicle.Count'
names(January_Data)[names(January_Data) == 'PM10.y'] <- 'PM10'
names(January_Data)[names(January_Data) == 'PM2.5.y'] <- 'PM2.5'
colnames(January_Data)


colnames(Decembet_Data)
names(Decembet_Data)[names(Decembet_Data) == 'Vehicle.Count.x'] <- 'Vehicle.Count'
colnames(Decembet_Data)

Decembet_Data$Particle.Count <- -1

as.POSIXlt.character(Decembet_Data$Date_Time,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
Decembet_Data$Date_Time <- as.POSIXlt.character(Decembet_Data$Date_Time,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
str(Decembet_Data)

Decembet_Data$Particle.Count<-as.numeric(Decembet_Data$Particle.Count)

typeof(January_Data$Date_Time)
January_Data$Date_Time <- as.character(January_Data$Date_Time,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)

library(mice)
miceMod <- mice(January_Data[, !names(January_Data) %in% "NO"], method="rf")  # perform mice imputation, based on random forests.
miceOutput <- complete(miceMod)  # generate the completed data.
January_Data$Date_Time <- as.POSIXlt.character(January_Data$Date_Time,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)

str(January_Data)

# Impute missing data

imp <- mice(January_Data, m = 1)


# After the missing value imputation, we can simply store our imputed data in a new and fully

#completed data set.

# Store imputed data as new data frame

airquality_imputed <- complete(imp)



January_Data_Hourly <- aggregate(list(CO = January_Data$CO ,
                                      NO = January_Data$NO ,
                                      NO2 = January_Data$NO2 ,
                                      Humidity = January_Data$Humidity ,
                                      Temperature = January_Data$Temperature ,
                                      Pressure = January_Data$Pressure ,
                                      O3 = January_Data$O3 ,
                                      Vehicle_Count = January_Data$Vehicle.Count ,
                                      Particle_Count = January_Data$Particle.Count ,
                                      PM4 = January_Data$PM.4 ,
                                      PM10 = January_Data$PM10 ,
                                      PM1 = January_Data$PM1 ,
                                      PM2.5 = January_Data$PM2.5
                                      ), 
                             list(hourofday = cut(January_Data$Date_Time, "1 hour")), 
                             mean)
February_Data_Hourly <- aggregate(list(CO = February_Data$CO ,
                                      NO = February_Data$NO ,
                                      NO2 = February_Data$NO2 ,
                                      Humidity = February_Data$Humidity ,
                                      Temperature = February_Data$Temperature ,
                                      Pressure = February_Data$Pressure ,
                                      O3 = February_Data$O3 ,
                                      Vehicle_Count = February_Data$Vehicle.Count ,
                                      Particle_Count = February_Data$Particle.Count ,
                                      PM4 = February_Data$PM.4 ,
                                      PM10 = February_Data$PM10 ,
                                      PM1 = February_Data$PM1 ,
                                      PM2.5 = February_Data$PM2.5
), 
list(hourofday = cut(February_Data$Date_Time, "1 hour")), 
mean)

March_Data_Hourly <- aggregate(list(CO = March_Data$CO ,
                                       NO = March_Data$NO ,
                                       NO2 = March_Data$NO2 ,
                                       Humidity = March_Data$Humidity ,
                                       Temperature = March_Data$Temperature ,
                                       Pressure = March_Data$Pressure ,
                                       O3 = March_Data$O3 ,
                                       Vehicle_Count = March_Data$Vehicle.Count ,
                                       Particle_Count = March_Data$Particle.Count ,
                                       PM4 = March_Data$PM.4 ,
                                       PM10 = March_Data$PM10 ,
                                       PM1 = March_Data$PM1 ,
                                       PM2.5 = March_Data$PM2.5
), 
list(hourofday = cut(March_Data$Date_Time, "1 hour")), 
mean)

April_Data_Hourly <- aggregate(list(CO = April_Data$CO ,
                                       NO = April_Data$NO ,
                                       NO2 = April_Data$NO2 ,
                                       Humidity = April_Data$Humidity ,
                                       Temperature = April_Data$Temperature ,
                                       Pressure = April_Data$Pressure ,
                                       O3 = April_Data$O3 ,
                                       Vehicle_Count = April_Data$Vehicle.Count ,
                                       Particle_Count = April_Data$Particle.Count ,
                                       PM4 = April_Data$PM.4 ,
                                       PM10 = April_Data$PM10 ,
                                       PM1 = April_Data$PM1 ,
                                       PM2.5 = April_Data$PM2.5
), 
list(hourofday = cut(April_Data$Date_Time, "1 hour")), 
mean)

May_Data_Hourly <- aggregate(list(CO = May_Data$CO ,
                                       NO = May_Data$NO ,
                                       NO2 = May_Data$NO2 ,
                                       Humidity = May_Data$Humidity ,
                                       Temperature = May_Data$Temperature ,
                                       Pressure = May_Data$Pressure ,
                                       O3 = May_Data$O3 ,
                                       Vehicle_Count = May_Data$Vehicle.Count ,
                                       Particle_Count = May_Data$Particle.Count ,
                                       PM4 = May_Data$PM.4 ,
                                       PM10 = May_Data$PM10 ,
                                       PM1 = May_Data$PM1 ,
                                       PM2.5 = May_Data$PM2.5
), 
list(hourofday = cut(May_Data$Date_Time, "1 hour")), 
mean)

June_Data_Hourly <- aggregate(list(CO = June_Data$CO ,
                                       NO = June_Data$NO ,
                                       NO2 = June_Data$NO2 ,
                                       Humidity = June_Data$Humidity ,
                                       Temperature = June_Data$Temperature ,
                                       Pressure = June_Data$Pressure ,
                                       O3 = June_Data$O3 ,
                                       Vehicle_Count = June_Data$Vehicle.Count ,
                                       Particle_Count = June_Data$Particle.Count ,
                                       PM4 = June_Data$PM.4 ,
                                       PM10 = June_Data$PM10 ,
                                       PM1 = June_Data$PM1 ,
                                       PM2.5 = June_Data$PM2.5
), 
list(hourofday = cut(June_Data$Date_Time, "1 hour")), 
mean)

July_Data_Hourly <- aggregate(list(CO = July_Data$CO ,
                                       NO = July_Data$NO ,
                                       NO2 = July_Data$NO2 ,
                                       Humidity = July_Data$Humidity ,
                                       Temperature = July_Data$Temperature ,
                                       Pressure = July_Data$Pressure ,
                                       O3 = July_Data$O3 ,
                                       Vehicle_Count = July_Data$Vehicle.Count ,
                                       Particle_Count = July_Data$Particle.Count ,
                                       PM4 = July_Data$PM.4 ,
                                       PM10 = July_Data$PM10 ,
                                       PM1 = July_Data$PM1 ,
                                       PM2.5 = July_Data$PM2.5
), 
list(hourofday = cut(July_Data$Date_Time, "1 hour")), 
mean)

August_Data_Hourly <- aggregate(list(CO = August_Data$CO ,
                                       NO = August_Data$NO ,
                                       NO2 = August_Data$NO2 ,
                                       Humidity = August_Data$Humidity ,
                                       Temperature = August_Data$Temperature ,
                                       Pressure = August_Data$Pressure ,
                                       O3 = August_Data$O3 ,
                                       Vehicle_Count = August_Data$Vehicle.Count ,
                                       Particle_Count = August_Data$Particle.Count ,
                                       PM4 = August_Data$PM.4 ,
                                       PM10 = August_Data$PM10 ,
                                       PM1 = August_Data$PM1 ,
                                       PM2.5 = August_Data$PM2.5
), 
list(hourofday = cut(August_Data$Date_Time, "1 hour")), 
mean)

September_Data_Hourly <- aggregate(list(CO = September_Data$CO ,
                                       NO = September_Data$NO ,
                                       NO2 = September_Data$NO2 ,
                                       Humidity = September_Data$Humidity ,
                                       Temperature = September_Data$Temperature ,
                                       Pressure = September_Data$Pressure ,
                                       O3 = September_Data$O3 ,
                                       Vehicle_Count = September_Data$Vehicle.Count ,
                                       Particle_Count = September_Data$Particle.Count ,
                                       PM4 = September_Data$PM.4 ,
                                       PM10 = September_Data$PM10 ,
                                       PM1 = September_Data$PM1 ,
                                       PM2.5 = September_Data$PM2.5
), 
list(hourofday = cut(September_Data$Date_Time, "1 hour")), 
mean)

October_Data_Hourly <- aggregate(list(CO = October_Data$CO ,
                                       NO = October_Data$NO ,
                                       NO2 = October_Data$NO2 ,
                                       Humidity = October_Data$Humidity ,
                                       Temperature = October_Data$Temperature ,
                                       Pressure = October_Data$Pressure ,
                                       O3 = October_Data$O3 ,
                                       Vehicle_Count = October_Data$Vehicle.Count ,
                                       Particle_Count = October_Data$Particle.Count ,
                                       PM4 = October_Data$PM.4 ,
                                       PM10 = October_Data$PM10 ,
                                       PM1 = October_Data$PM1 ,
                                       PM2.5 = October_Data$PM2.5
), 
list(hourofday = cut(October_Data$Date_Time, "1 hour")), 
mean)

November_Data_Hourly <- aggregate(list(CO = November_Data$CO ,
                                       NO = November_Data$NO ,
                                       NO2 = November_Data$NO2 ,
                                       Humidity = November_Data$Humidity ,
                                       Temperature = November_Data$Temperature ,
                                       Pressure = November_Data$Pressure ,
                                       O3 = November_Data$O3 ,
                                       Vehicle_Count = November_Data$Vehicle.Count ,
                                       Particle_Count = November_Data$Particle.Count ,
                                       PM4 = November_Data$PM.4 ,
                                       PM10 = November_Data$PM10 ,
                                       PM1 = November_Data$PM1 ,
                                       PM2.5 = November_Data$PM2.5
), 
list(hourofday = cut(November_Data$Date_Time, "1 hour")), 
mean)

December_Data_Hourly <- aggregate(list(CO = Decembet_Data$CO ,
                                       NO = Decembet_Data$NO ,
                                       NO2 = Decembet_Data$NO2 ,
                                       Humidity = Decembet_Data$Humidity ,
                                       Temperature = Decembet_Data$Temperature ,
                                       Pressure = Decembet_Data$Pressure ,
                                       O3 = Decembet_Data$O3 ,
                                       Vehicle_Count = Decembet_Data$Vehicle.Count ,
                                       Particle_Count = Decembet_Data$Particle.Count ,
                                       PM4 = Decembet_Data$PM.4 ,
                                       PM10 = Decembet_Data$PM10 ,
                                       PM1 = Decembet_Data$PM1 ,
                                       PM2.5 = Decembet_Data$PM2.5
), 
list(
  hourofday = cut(Decembet_Data$Date_Time, "1 hour")), 
mean)



Full_2019_Data <- rbind(January_Data_Hourly, February_Data_Hourly)
Full_2019_Data <- rbind(Full_2019_Data, March_Data_Hourly)
Full_2019_Data <- rbind(Full_2019_Data, April_Data_Hourly)
Full_2019_Data <- rbind(Full_2019_Data, May_Data_Hourly)
Full_2019_Data <- rbind(Full_2019_Data, June_Data_Hourly)
Full_2019_Data <- rbind(Full_2019_Data, July_Data_Hourly)
Full_2019_Data <- rbind(Full_2019_Data, August_Data_Hourly)
Full_2019_Data <- rbind(Full_2019_Data, September_Data_Hourly)
Full_2019_Data <- rbind(Full_2019_Data, October_Data_Hourly)
Full_2019_Data <- rbind(Full_2019_Data, November_Data_Hourly)
Full_2019_Data <- rbind(Full_2019_Data, December_Data_Hourly)












