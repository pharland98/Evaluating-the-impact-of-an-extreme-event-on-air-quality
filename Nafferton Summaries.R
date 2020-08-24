setwd("~/Dissertation/Dissertation Datasets/Nafferton Farm/Nafferton_Dec")###this is the location of your files
Air_Quality <- read.csv("Nafferton_Dec_Clean.csv", header= TRUE)
library(tidyr)

summary(Air_Quality)
Air_Quality$Date_Time <- as.POSIXlt.character(Air_Quality$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
Air_Quality<-Air_Quality[-c(1,2)]

mydf_list <- split( Air_Quality , f = Air_Quality$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

PM1_Dec_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(PM1_Dec_df)

summary(PM1_Dec_df$Value)

boxplot(PM1_Dec_df$Value)
boxplot(PM1_Dec_df$Value,plot=FALSE)$out
outliers <- boxplot(PM1_Dec_df$Value,plot=FALSE)$out
print(outliers)
PM1_Dec_df[which(PM1_Dec_df$Value %in% outliers),]
PM1_Dec_df<-PM1_Dec_df[-which(PM1_Dec_df$Value %in% outliers),]
summary(PM1_Dec_df$Value)

setwd("~/Dissertation/Dissertation Datasets/Plots")
png(file = "PM1 Histogram Nafferton 2019.png")
hist(PM1_Dec_df$Value,main = NULL,xlab = "PM1 Concentration 2019",xlim = c(0,10),col = "lightblue",freq=FALSE)
dev.off() #Saving the file
png(file = "LN PM1 Histogram Nafferton 2019.png")
hist(log(PM1_Dec_df$Value),main = NULL,xlab = "Log PM1 Concentration 2019",xlim = c(-4,4),col = "lightblue",freq=FALSE)
dev.off() #Saving the file


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

PM10_Dec_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(PM10_Dec_df)

summary(PM10_Dec_df$Value)

boxplot(PM10_Dec_df$Value)
boxplot(PM10_Dec_df$Value,plot=FALSE)$out
outliers <- boxplot(PM10_Dec_df$Value,plot=FALSE)$out
print(outliers)
PM10_Dec_df[which(PM10_Dec_df$Value %in% outliers),]
PM10_Dec_df<-PM10_Dec_df[-which(PM10_Dec_df$Value %in% outliers),]
summary(PM10_Dec_df$Value)

setwd("~/Dissertation/Dissertation Datasets/Plots")
png(file = "PM10 Histogram Nafferton 2019.png")
hist(PM10_Dec_df$Value,main = NULL,xlab = "PM10 Concentration 2019",xlim = c(0,20),col = "lightblue",freq=FALSE)
dev.off() #Saving the file
png(file = "LN PM10 Histogram Nafferton 2019.png")
hist(log(PM10_Dec_df$Value),main = NULL,xlab = "Log PM10 Concentration 2019",xlim = c(-4,4),col = "lightblue",freq=FALSE)
dev.off() #Saving the file

PM2.5_Dec_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(PM2.5_Dec_df)

summary(PM2.5_Dec_df$Value)

boxplot(PM2.5_Dec_df$Value)
boxplot(PM2.5_Dec_df$Value,plot=FALSE)$out
outliers <- boxplot(PM2.5_Dec_df$Value,plot=FALSE)$out
print(outliers)
PM2.5_Dec_df[which(PM2.5_Dec_df$Value %in% outliers),]
PM2.5_Dec_df<-PM2.5_Dec_df[-which(PM2.5_Dec_df$Value %in% outliers),]
summary(PM2.5_Dec_df$Value)

setwd("~/Dissertation/Dissertation Datasets/Plots")
png(file = "PM2.5 Histogram Nafferton 2019.png")
hist(PM2.5_Dec_df$Value,main = NULL,xlab = "PM2.5 Concentration 2019",xlim = c(0,15),col = "lightblue",freq=FALSE)
dev.off() #Saving the file
setwd("~/Dissertation/Dissertation Datasets/Plots")
png(file = "LN PM2.5 Histogram Nafferton 2019.png")
hist(log(PM2.5_Dec_df$Value),main = NULL,xlab = "Log PM2.5 Concentration 2019",xlim = c(-4,4),col = "lightblue",freq=FALSE)
dev.off() #Saving the file

##########################################################################################
##########################################################################################

setwd("~/Dissertation/Dissertation Datasets/Nafferton Farm/Nafferton_Mar20")###this is the location of your files
Air_Quality_Mar20 <- read.csv("Nafferton_Mar20_Clean.csv", header= TRUE)
library(tidyr)

summary(Air_Quality_Mar20)
Air_Quality_Mar20$Date_Time <- as.POSIXlt.character(Air_Quality_Mar20$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
Air_Quality_Mar20<-Air_Quality_Mar20[-c(1,2)]

mydf_list <- split( Air_Quality_Mar20 , f = Air_Quality_Mar20$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

PM1_Mar20_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(PM1_Mar20_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Nafferton Farm/Nafferton_Apr20")###this is the location of your files
Air_Quality <- read.csv("Nafferton_Apr20_Clean.csv", header= TRUE)
library(tidyr)

summary(Air_Quality)
Air_Quality$Date_Time <- as.POSIXlt.character(Air_Quality$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
Air_Quality<-Air_Quality[-c(1,2)]

mydf_list <- split( Air_Quality , f = Air_Quality$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

PM1_Apr20_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(PM1_Apr20_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Nafferton Farm/Nafferton_May20")###this is the location of your files
Air_Quality <- read.csv("Nafferton_May20_Clean.csv", header= TRUE)
library(tidyr)

summary(Air_Quality)
Air_Quality$Date_Time <- as.POSIXlt.character(Air_Quality$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
Air_Quality<-Air_Quality[-c(1,2)]

mydf_list <- split( Air_Quality , f = Air_Quality$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

PM1_May20_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(PM1_May20_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Nafferton Farm/Nafferton_Jan20")###this is the location of your files
Air_Quality <- read.csv("Nafferton_Jan20_Clean.csv", header= TRUE)
library(tidyr)

summary(Air_Quality)
Air_Quality$Date_Time <- as.POSIXlt.character(Air_Quality$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
Air_Quality<-Air_Quality[-c(1,2)]

mydf_list <- split( Air_Quality , f = Air_Quality$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

PM1_Jan20_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(PM1_Jan20_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Nafferton Farm/Nafferton_Feb20")###this is the location of your files
Air_Quality <- read.csv("Nafferton_Feb20_Clean.csv", header= TRUE)
library(tidyr)

summary(Air_Quality)
Air_Quality$Date_Time <- as.POSIXlt.character(Air_Quality$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
Air_Quality<-Air_Quality[-c(1,2)]

mydf_list <- split( Air_Quality , f = Air_Quality$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

PM1_Feb20_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(PM1_Feb20_df)


################################################################################################


PM1_2020<-rbind(PM1_Jan20_df,PM1_Feb20_df)
PM1_2020<-rbind(PM1_2020,PM1_Mar20_df)
PM1_2020<-rbind(PM1_2020,PM1_Apr20_df)
PM1_2020<-rbind(PM1_2020,PM1_May20_df)

summary(PM1_2020$Value)

boxplot(PM1_2020$Value)
boxplot(PM1_2020$Value,plot=FALSE)$out
outliers <- boxplot(PM1_2020$Value,plot=FALSE)$out
print(outliers)
PM1_2020[which(PM1_2020$Value %in% outliers),]
PM1_2020<-PM1_2020[-which(PM1_2020$Value %in% outliers),]

summary(PM1_2020$Value)


Haym_NO <- rbind(PM1_Dec_df,PM1_2020)


NOH <- Haym_NO %>%
  group_by(date(Date_Time)) %>%
  summarize(Avg_O3 = mean(Value, na.rm = TRUE))%>%
  mutate(location="Nafferton")


str(NOH)
setwd("~/Dissertation/Dissertation Datasets/Nafferton Farm")
write_csv(NOH,'PM1_Nafferton.csv')
PM1N<-read.csv('PM1_Nafferton.csv')


#############################################################################################
#############################################################################################
##########################################################################################
##########################################################################################

setwd("~/Dissertation/Dissertation Datasets/Nafferton Farm/Nafferton_Mar20")###this is the location of your files
Air_Quality_Mar20 <- read.csv("Nafferton_Mar20_Clean.csv", header= TRUE)
library(tidyr)

summary(Air_Quality_Mar20)
Air_Quality_Mar20$Date_Time <- as.POSIXlt.character(Air_Quality_Mar20$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
Air_Quality_Mar20<-Air_Quality_Mar20[-c(1,2)]

mydf_list <- split( Air_Quality_Mar20 , f = Air_Quality_Mar20$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

PM10_Mar20_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(PM10_Mar20_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Nafferton Farm/Nafferton_Apr20")###this is the location of your files
Air_Quality <- read.csv("Nafferton_Apr20_Clean.csv", header= TRUE)
library(tidyr)

summary(Air_Quality)
Air_Quality$Date_Time <- as.POSIXlt.character(Air_Quality$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
Air_Quality<-Air_Quality[-c(1,2)]

mydf_list <- split( Air_Quality , f = Air_Quality$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

PM10_Apr20_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(PM10_Apr20_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Nafferton Farm/Nafferton_May20")###this is the location of your files
Air_Quality <- read.csv("Nafferton_May20_Clean.csv", header= TRUE)
library(tidyr)

summary(Air_Quality)
Air_Quality$Date_Time <- as.POSIXlt.character(Air_Quality$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
Air_Quality<-Air_Quality[-c(1,2)]

mydf_list <- split( Air_Quality , f = Air_Quality$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

PM10_May20_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(PM10_May20_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Nafferton Farm/Nafferton_Jan20")###this is the location of your files
Air_Quality <- read.csv("Nafferton_Jan20_Clean.csv", header= TRUE)
library(tidyr)

summary(Air_Quality)
Air_Quality$Date_Time <- as.POSIXlt.character(Air_Quality$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
Air_Quality<-Air_Quality[-c(1,2)]

mydf_list <- split( Air_Quality , f = Air_Quality$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

PM10_Jan20_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(PM10_Jan20_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Nafferton Farm/Nafferton_Feb20")###this is the location of your files
Air_Quality <- read.csv("Nafferton_Feb20_Clean.csv", header= TRUE)
library(tidyr)

summary(Air_Quality)
Air_Quality$Date_Time <- as.POSIXlt.character(Air_Quality$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
Air_Quality<-Air_Quality[-c(1,2)]

mydf_list <- split( Air_Quality , f = Air_Quality$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

PM10_Feb20_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(PM10_Feb20_df)


################################################################################################


PM10_2020<-rbind(PM10_Jan20_df,PM10_Feb20_df)
PM10_2020<-rbind(PM10_2020,PM10_Mar20_df)
PM10_2020<-rbind(PM10_2020,PM10_Apr20_df)
PM10_2020<-rbind(PM10_2020,PM10_May20_df)

summary(PM10_2020$Value)

boxplot(PM10_2020$Value)
boxplot(PM10_2020$Value,plot=FALSE)$out
outliers <- boxplot(PM10_2020$Value,plot=FALSE)$out
print(outliers)
PM10_2020[which(PM10_2020$Value %in% outliers),]
PM10_2020<-PM10_2020[-which(PM10_2020$Value %in% outliers),]

summary(PM10_2020$Value)


Haym_NO <- rbind(PM10_Dec_df,PM10_2020)


NOH <- Haym_NO %>%
  group_by(date(Date_Time)) %>%
  summarize(Avg_O3 = mean(Value, na.rm = TRUE))%>%
  mutate(location="Nafferton")


str(NOH)
setwd("~/Dissertation/Dissertation Datasets/Nafferton Farm")
write_csv(NOH,'PM10_Nafferton.csv')
PM10N<-read.csv('PM10_Nafferton.csv')


#############################################################################################
#############################################################################################
##########################################################################################
##########################################################################################

setwd("~/Dissertation/Dissertation Datasets/Nafferton Farm/Nafferton_Mar20")###this is the location of your files
Air_Quality_Mar20 <- read.csv("Nafferton_Mar20_Clean.csv", header= TRUE)
library(tidyr)

summary(Air_Quality_Mar20)
Air_Quality_Mar20$Date_Time <- as.POSIXlt.character(Air_Quality_Mar20$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
Air_Quality_Mar20<-Air_Quality_Mar20[-c(1,2)]

mydf_list <- split( Air_Quality_Mar20 , f = Air_Quality_Mar20$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

PM2.5_Mar20_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(PM2.5_Mar20_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Nafferton Farm/Nafferton_Apr20")###this is the location of your files
Air_Quality <- read.csv("Nafferton_Apr20_Clean.csv", header= TRUE)
library(tidyr)

summary(Air_Quality)
Air_Quality$Date_Time <- as.POSIXlt.character(Air_Quality$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
Air_Quality<-Air_Quality[-c(1,2)]

mydf_list <- split( Air_Quality , f = Air_Quality$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

PM2.5_Apr20_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(PM2.5_Apr20_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Nafferton Farm/Nafferton_May20")###this is the location of your files
Air_Quality <- read.csv("Nafferton_May20_Clean.csv", header= TRUE)
library(tidyr)

summary(Air_Quality)
Air_Quality$Date_Time <- as.POSIXlt.character(Air_Quality$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
Air_Quality<-Air_Quality[-c(1,2)]

mydf_list <- split( Air_Quality , f = Air_Quality$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

PM2.5_May20_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(PM2.5_May20_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Nafferton Farm/Nafferton_Jan20")###this is the location of your files
Air_Quality <- read.csv("Nafferton_Jan20_Clean.csv", header= TRUE)
library(tidyr)

summary(Air_Quality)
Air_Quality$Date_Time <- as.POSIXlt.character(Air_Quality$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
Air_Quality<-Air_Quality[-c(1,2)]

mydf_list <- split( Air_Quality , f = Air_Quality$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

PM2.5_Jan20_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(PM2.5_Jan20_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Nafferton Farm/Nafferton_Feb20")###this is the location of your files
Air_Quality <- read.csv("Nafferton_Feb20_Clean.csv", header= TRUE)
library(tidyr)

summary(Air_Quality)
Air_Quality$Date_Time <- as.POSIXlt.character(Air_Quality$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
Air_Quality<-Air_Quality[-c(1,2)]

mydf_list <- split( Air_Quality , f = Air_Quality$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

PM2.5_Feb20_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(PM2.5_Feb20_df)


################################################################################################


PM2.5_2020<-rbind(PM2.5_Jan20_df,PM2.5_Feb20_df)
PM2.5_2020<-rbind(PM2.5_2020,PM2.5_Mar20_df)
PM2.5_2020<-rbind(PM2.5_2020,PM2.5_Apr20_df)
PM2.5_2020<-rbind(PM2.5_2020,PM2.5_May20_df)

summary(PM2.5_2020$Value)

boxplot(PM2.5_2020$Value)
boxplot(PM2.5_2020$Value,plot=FALSE)$out
outliers <- boxplot(PM2.5_2020$Value,plot=FALSE)$out
print(outliers)
PM2.5_2020[which(PM2.5_2020$Value %in% outliers),]
PM2.5_2020<-PM2.5_2020[-which(PM2.5_2020$Value %in% outliers),]

summary(PM2.5_2020$Value)


Haym_NO <- rbind(PM2.5_Dec_df,PM2.5_2020)


NOH <- Haym_NO %>%
  group_by(date(Date_Time)) %>%
  summarize(Avg_O3 = mean(Value, na.rm = TRUE))%>%
  mutate(location="Nafferton")


str(NOH)
setwd("~/Dissertation/Dissertation Datasets/Nafferton Farm")
write_csv(NOH,'PM2.5_Nafferton.csv')
PM2.5N<-read.csv('PM2.5_Nafferton.csv')

setwd("~/Dissertation/Dissertation Datasets/Plots")
png(file = "PM2.5 Histogram Nafferton 2020.png")
hist(PM2.5_2020$Value,main = NULL,xlab = "PM2.5 Concentration 2020",xlim = c(0,20),col = "lightblue",freq=FALSE)
dev.off() #Saving the file

setwd("~/Dissertation/Dissertation Datasets/Plots")
png(file = "PM1 Histogram Nafferton 2020.png")
hist(PM1_2020$Value,main = NULL,xlab = "PM1 Concentration 2020",xlim = c(0,10),col = "lightblue",freq=FALSE)
dev.off() #Saving the file

setwd("~/Dissertation/Dissertation Datasets/Plots")
png(file = "LN PM10 Histogram Nafferton 2020.png")
hist(log(PM10_2020$Value),main = NULL,xlab = "Log PM10 Concentration 2020",xlim = c(-4,4),col = "lightblue",freq=FALSE)
dev.off() #Saving the file

png(file = "LN PM2.5 Histogram Nafferton 2020.png")
hist(log(PM2.5_2020$Value),main = NULL,xlab = "Log PM2.5 Concentration 2020",xlim = c(-4,4),col = "lightblue",freq=FALSE)
dev.off() #Saving the file

png(file = "LN PM1 Histogram Nafferton 2020.png")
hist(log(PM1_2020$Value),main = NULL,xlab = "Log PM1 Concentration 2020",xlim = c(-4,4),col = "lightblue",freq=FALSE)
dev.off() #Saving the file


png(file = "PM10 Histogram Nafferton 2020.png")
hist(PM10_2020$Value,main = NULL,xlab = "PM10 Concentration 2020",xlim = c(0,25),col = "lightblue",freq=FALSE)
dev.off() #Saving the file

