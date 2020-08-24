setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymaket Jan-Mar19")
January_Data <- read.csv("Jan19_18Feb19Mid_Clean.csv",header = TRUE)

library(tidyr)

summary(January_Data)
str(January_Data)
January_Data$Date_Time <- as.POSIXlt.character(January_Data$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
January_Data<-January_Data[-c(1,2,5)]

mydf_list <- split( January_Data , f = January_Data$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Jan_df <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Jan_df)

###############################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Feb")
February_Data <- read.csv("Haymarket_Feb_Clean.csv",header = TRUE)


library(tidyr)

summary(February_Data)
February_Data$Date_Time <- as.POSIXlt.character(February_Data$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
February_Data<-February_Data[-c(1,2)]

mydf_list <- split( February_Data , f = February_Data$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Feb_df <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Feb_df)

############################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Mar")###this is the location of files
Air_Quality_Mar <- read.csv("Haymarket_Mar_Clean.csv", header= TRUE)
library(tidyr)

summary(Air_Quality_Mar)
Air_Quality_Mar$Date_Time <- as.POSIXlt.character(Air_Quality_Mar$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
Air_Quality_Mar<-Air_Quality_Mar[-c(1,2)]

mydf_list <- split( Air_Quality_Mar , f = Air_Quality_Mar$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Mar_df <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Mar_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Apr")###this is the location of files
Air_Quality <- read.csv("Haymarket_Apr_Clean.csv", header= TRUE)
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

CO_Apr_df <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Apr_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_May")###this is the location of files
Air_Quality <- read.csv("Haymarket_May_Clean.csv", header= TRUE)
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

CO_May_df <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_May_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Jun")###this is the location of files
Air_Quality <- read.csv("Haymarket_Jun_Clean.csv", header= TRUE)
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

CO_Jun_df <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Jun_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Jul")###this is the location of files
Air_Quality <- read.csv("Haymarket_Jul_Clean.csv", header= TRUE)
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

CO_Jul_df <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Jul_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Aug")###this is the location of files
Air_Quality <- read.csv("Haymarket_Aug_Clean.csv", header= TRUE)
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

CO_Aug_df <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Aug_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Sep")###this is the location of files
Air_Quality <- read.csv("Haymarket_Sep_Clean.csv", header= TRUE)
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

CO_Sep_df <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Sep_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Oct")###this is the location of files
Air_Quality <- read.csv("Haymarket_Oct_Clean.csv", header= TRUE)
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

CO_Oct_df <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Oct_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Nov")###this is the location of files
Air_Quality <- read.csv("Haymarket_Nov_Clean.csv", header= TRUE)
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

CO_Nov_df <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Nov_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Dec")###this is the location of files
Air_Quality <- read.csv("Haymarket_Dec_Clean.csv", header= TRUE)
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

CO_Dec_df <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Dec_df)



CO_2019<-rbind(CO_Mar_df,CO_Apr_df)
CO_2019<-rbind(CO_2019,CO_May_df)
CO_2019<-rbind(CO_2019,CO_Feb_df)
CO_2019<-rbind(CO_2019,CO_Jan_df)
CO_2019<-rbind(CO_2019,CO_Jun_df)
CO_2019<-rbind(CO_2019,CO_Jul_df)
CO_2019<-rbind(CO_2019,CO_Aug_df)
CO_2019<-rbind(CO_2019,CO_Sep_df)
CO_2019<-rbind(CO_2019,CO_Oct_df)
CO_2019<-rbind(CO_2019,CO_Nov_df)
CO_2019<-rbind(CO_2019,CO_Dec_df)

summary(CO_2019$Value)

boxplot(CO_2019$Value)
boxplot(CO_2019$Value,plot=FALSE)$out
outliers <- boxplot(CO_2019$Value,plot=FALSE)$out
print(outliers)
CO_2019[which(CO_2019$Value %in% outliers),]
CO_2019<-CO_2019[-which(CO_2019$Value %in% outliers),]
summary(CO_2019$Value)


#############################################################################################
#############################################################################################
#############################################################################################
setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Jan20")
January_Data <- read.csv("Haymarket_Jan20_Clean.csv",header = TRUE)

library(tidyr)

summary(January_Data)
str(January_Data)
January_Data$Date_Time <- as.POSIXlt.character(January_Data$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
January_Data<-January_Data[-c(1,2)]

mydf_list <- split( January_Data , f = January_Data$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Jan_df <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Jan_df)

###############################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Feb20")
February_Data <- read.csv("Haymarket_Feb20_Clean.csv",header = TRUE)


library(tidyr)

summary(February_Data)
February_Data$Date_Time <- as.POSIXlt.character(February_Data$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
February_Data<-February_Data[-c(1,2)]

mydf_list <- split( February_Data , f = February_Data$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Feb_df <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Feb_df)

############################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Mar20")###this is the location of files
Air_Quality_Mar <- read.csv("Haymarket_Mar20_Clean.csv", header= TRUE)
library(tidyr)

summary(Air_Quality_Mar)
Air_Quality_Mar$Date_Time <- as.POSIXlt.character(Air_Quality_Mar$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
Air_Quality_Mar<-Air_Quality_Mar[-c(1,2)]

mydf_list <- split( Air_Quality_Mar , f = Air_Quality_Mar$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Mar_df <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Mar_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Apr20")###this is the location of files
Air_Quality <- read.csv("Haymarket_Apr20_Clean.csv", header= TRUE)
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

CO_Apr_df <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Apr_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_May20")###this is the location of files
Air_Quality <- read.csv("Haymarket_May20_Clean.csv", header= TRUE)
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
library(lubridate)
CO_May_df <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_May_df)




CO_2020<-rbind(CO_Mar_df,CO_Apr_df)
CO_2020<-rbind(CO_2020,CO_May_df)
CO_2020<-rbind(CO_2020,CO_Feb_df)
CO_2020<-rbind(CO_2020,CO_Jan_df)


summary(CO_2020$Value)

boxplot(CO_2020$Value)
boxplot(CO_2020$Value,plot=FALSE)$out
outliers <- boxplot(CO_2020$Value,plot=FALSE)$out
print(outliers)
CO_2020[which(CO_2020$Value %in% outliers),]
CO_2020<-CO_2020[-which(CO_2020$Value %in% outliers),]
summary(CO_2020$Value)



setwd("~/Dissertation/Dissertation Datasets/Plots")
png(file = "CO Histogram Haymarket 2019.png")
hist(CO_2019$Value,main = NULL,xlab = "CO Concentration 2019",xlim = c(0,500),col = "lightblue",freq=FALSE)
dev.off() #Saving the file
png(file = "CO Histogram Haymarket 2020.png")
hist(CO_2020$Value,main = NULL,xlab = "CO Concentration 2020",xlim = c(0,500),col = "lightblue",freq=FALSE)
dev.off() #Saving the file

Haym_CO <- rbind(CO_2019,CO_2020)

plot(Haym_CO$Date_Time,Haym_CO$Value)

COH <- Haym_CO %>%
  group_by(date(Date_Time)) %>%
  summarize(Avg_CO = mean(Value, na.rm = TRUE))%>%
  mutate(location="Haymarket")

str(COH)
setwd("~/Dissertation/Dissertation Datasets/Haymarket")
write_csv(COH,'CO_Haymarket.csv')
COH<-read.csv('CO_Haymarket.csv')

setwd("~/Dissertation/Dissertation Datasets/Haymarket")
COH<-read.csv('CO_Haymarket.csv')
setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station")
COG<-read.csv('CO_Gateshead.csv')
setwd("~/Dissertation/Dissertation Datasets/RVI")
COR<-read.csv('CO_RVI.csv')

CO<-rbind(COG,COH)
CO<-rbind(CO,COR)
str(CO)
CO$Date<-as.Date(CO$date.Date_Time.)

setwd("~/Dissertation/Dissertation Datasets/Plots")
p <- ggplot(CO,aes(Date,Avg_CO,)) + geom_path(aes(colour=location)) + scale_x_date() + facet_wrap(vars(location)) + labs(y= (expression(paste(
  "CO Concentration (",
  µ, g, "/", m^3,
  ")", sep=""))), x = "Date") + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location') 
p
ggsave("CO Path Facet.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(CO,aes(Date,Avg_CO)) + geom_path(aes(colour=location)) + scale_x_date() + labs(y=  (expression(paste(
  "CO Concentration (",
  µ, g, "/", m^3,
  ")", sep=""))), x = "Date") + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location')
p
ggsave("CO Path.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(CO,aes(Date,Avg_CO)) + geom_smooth(aes(colour=location),span=0.4) + scale_x_date() + facet_wrap(vars(location)) + labs(y=  (expression(paste(
  "CO Concentration (",
  µ, g, "/", m^3,
  ")", sep=""))), x = "Date")  + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location')
p
ggsave("CO Smooth Facet.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(CO,aes(Date,Avg_CO)) + geom_smooth(aes(colour=location),span=0.4) + scale_x_date(breaks = pretty_breaks(17)) + labs(y=  (expression(paste(
  "CO Concentration (",
  µ, g, "/", m^3,
  ")", sep=""))), x = "Date")  + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location')+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=15)) + theme(axis.text.y = element_text(size=15))

p
ggsave("CO Smooth.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(CO,aes(Date,Avg_CO)) + geom_point(aes(colour=location)) + scale_x_date() + labs(y=  (expression(paste(
  "CO Concentration (",
  µ, g, "/", m^3,
  ")", sep=""))), x = "Date") + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location')
p
ggsave("CO Point.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(CO,aes(Date,Avg_CO)) + geom_point(aes(colour=location)) + scale_x_date() + facet_wrap(vars(location)) + labs(y=  (expression(paste(
  "CO Concentration (",
  µ, g, "/", m^3,
  ")", sep=""))), x = "Date") + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location') 
p
ggsave("CO Point Facet.png", p, width = 24, height = 14,dpi = 300 )#save to file





aes(colour=location)

str(Haym_CO$Date_Time)
Haym_CO$Date_Time <- as.POSIXct(Haym_CO$Date_Time)

##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymaket Jan-Mar19")
January_Data <- read.csv("Jan19_18Feb19Mid_Clean.csv",header = TRUE)

library(tidyr)

summary(January_Data)
str(January_Data)
January_Data$Date_Time <- as.POSIXlt.character(January_Data$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
January_Data<-January_Data[-c(1,2,5)]

mydf_list <- split( January_Data , f = January_Data$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Jan_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Jan_df)

###############################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Feb")
February_Data <- read.csv("Haymarket_Feb_Clean.csv",header = TRUE)


library(tidyr)

summary(February_Data)
February_Data$Date_Time <- as.POSIXlt.character(February_Data$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
February_Data<-February_Data[-c(1,2)]

mydf_list <- split( February_Data , f = February_Data$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Feb_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Feb_df)

############################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Mar")###this is the location of files
Air_Quality_Mar <- read.csv("Haymarket_Mar_Clean.csv", header= TRUE)
library(tidyr)

summary(Air_Quality_Mar)
Air_Quality_Mar$Date_Time <- as.POSIXlt.character(Air_Quality_Mar$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
Air_Quality_Mar<-Air_Quality_Mar[-c(1,2)]

mydf_list <- split( Air_Quality_Mar , f = Air_Quality_Mar$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Mar_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Mar_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Apr")###this is the location of files
Air_Quality <- read.csv("Haymarket_Apr_Clean.csv", header= TRUE)
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

CO_Apr_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Apr_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_May")###this is the location of files
Air_Quality <- read.csv("Haymarket_May_Clean.csv", header= TRUE)
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

CO_May_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_May_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Jun")###this is the location of files
Air_Quality <- read.csv("Haymarket_Jun_Clean.csv", header= TRUE)
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

CO_Jun_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Jun_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Jul")###this is the location of files
Air_Quality <- read.csv("Haymarket_Jul_Clean.csv", header= TRUE)
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

CO_Jul_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Jul_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Aug")###this is the location of files
Air_Quality <- read.csv("Haymarket_Aug_Clean.csv", header= TRUE)
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

CO_Aug_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Aug_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Sep")###this is the location of files
Air_Quality <- read.csv("Haymarket_Sep_Clean.csv", header= TRUE)
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

CO_Sep_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Sep_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Oct")###this is the location of files
Air_Quality <- read.csv("Haymarket_Oct_Clean.csv", header= TRUE)
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

CO_Oct_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Oct_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Nov")###this is the location of files
Air_Quality <- read.csv("Haymarket_Nov_Clean.csv", header= TRUE)
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

CO_Nov_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Nov_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Dec")###this is the location of files
Air_Quality <- read.csv("Haymarket_Dec_Clean.csv", header= TRUE)
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

CO_Dec_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Dec_df)



CO_2019<-rbind(CO_Mar_df,CO_Apr_df)
CO_2019<-rbind(CO_2019,CO_May_df)
CO_2019<-rbind(CO_2019,CO_Feb_df)
CO_2019<-rbind(CO_2019,CO_Jan_df)
CO_2019<-rbind(CO_2019,CO_Jun_df)
CO_2019<-rbind(CO_2019,CO_Jul_df)
CO_2019<-rbind(CO_2019,CO_Aug_df)
CO_2019<-rbind(CO_2019,CO_Sep_df)
CO_2019<-rbind(CO_2019,CO_Oct_df)
CO_2019<-rbind(CO_2019,CO_Nov_df)
CO_2019<-rbind(CO_2019,CO_Dec_df)

anyNA(CO_2019)
CO_2019[CO_2019 < 0] <- NA
CO_2019<-na.omit(CO_2019)

summary(CO_2019$Value)

boxplot(CO_2019$Value)
boxplot(CO_2019$Value,plot=FALSE)$out
outliers <- boxplot(CO_2019$Value,plot=FALSE)$out
print(outliers)
CO_2019[which(CO_2019$Value %in% outliers),]
CO_2019<-CO_2019[-which(CO_2019$Value %in% outliers),]
summary(CO_2019$Value)


#############################################################################################
#############################################################################################
#############################################################################################
setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Jan20")
January_Data <- read.csv("Haymarket_Jan20_Clean.csv",header = TRUE)

library(tidyr)

summary(January_Data)
str(January_Data)
January_Data$Date_Time <- as.POSIXlt.character(January_Data$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
January_Data<-January_Data[-c(1,2)]

mydf_list <- split( January_Data , f = January_Data$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Jan_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Jan_df)

###############################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Feb20")
February_Data <- read.csv("Haymarket_Feb20_Clean.csv",header = TRUE)


library(tidyr)

summary(February_Data)
February_Data$Date_Time <- as.POSIXlt.character(February_Data$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
February_Data<-February_Data[-c(1,2)]

mydf_list <- split( February_Data , f = February_Data$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Feb_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Feb_df)

############################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Mar20")###this is the location of files
Air_Quality_Mar <- read.csv("Haymarket_Mar20_Clean.csv", header= TRUE)
library(tidyr)

summary(Air_Quality_Mar)
Air_Quality_Mar$Date_Time <- as.POSIXlt.character(Air_Quality_Mar$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
Air_Quality_Mar<-Air_Quality_Mar[-c(1,2)]

mydf_list <- split( Air_Quality_Mar , f = Air_Quality_Mar$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Mar_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Mar_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Apr20")###this is the location of files
Air_Quality <- read.csv("Haymarket_Apr20_Clean.csv", header= TRUE)
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

CO_Apr_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Apr_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_May20")###this is the location of files
Air_Quality <- read.csv("Haymarket_May20_Clean.csv", header= TRUE)
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

CO_May_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_May_df)




CO_2020<-rbind(CO_Mar_df,CO_Apr_df)
CO_2020<-rbind(CO_2020,CO_May_df)
CO_2020<-rbind(CO_2020,CO_Feb_df)
CO_2020<-rbind(CO_2020,CO_Jan_df)

anyNA(CO_2020)
CO_2020[CO_2020 < 0] <- NA
CO_2020<-na.omit(CO_2020)

summary(CO_2020$Value)

boxplot(CO_2020$Value)
boxplot(CO_2020$Value,plot=FALSE)$out
outliers <- boxplot(CO_2020$Value,plot=FALSE)$out
print(outliers)
CO_2020[which(CO_2020$Value %in% outliers),]
CO_2020<-CO_2020[-which(CO_2020$Value %in% outliers),]
summary(CO_2020$Value)








summary(CO_2019$Value)
summary(CO_2020$Value)



Haym_NO <- rbind(CO_2019,CO_2020)


NOH <- Haym_NO %>%
  group_by(date(Date_Time)) %>%
  summarize(Avg_NO = mean(Value, na.rm = TRUE))%>%
  mutate(location="Haymarket")


str(NOH)
setwd("~/Dissertation/Dissertation Datasets/Haymarket")
write_csv(NOH,'NO_Haymarket.csv')
NOH<-read.csv('NO_Haymarket.csv')

setwd("~/Dissertation/Dissertation Datasets/Haymarket")
NOH<-read.csv('NO_Haymarket.csv')
setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station")
NOG<-read.csv('NO_Gateshead.csv')
setwd("~/Dissertation/Dissertation Datasets/RVI")
NOR<-read.csv('NO_RVI.csv')
setwd("~/Dissertation/Dissertation Datasets/Dinnington near Airport")
NOD<-read.csv('NO_Dinnington.csv')

NO<-rbind(NOG,NOH)
NO<-rbind(NO,NOR)
NO<-rbind(NO,NOD)

str(NO)
NO$Date<-as.Date(NO$date.Date_Time.)

setwd("~/Dissertation/Dissertation Datasets/Plots")
p <- ggplot(NO,aes(Date,Avg_NO,)) + geom_path(aes(colour=location)) + scale_x_date(breaks = pretty_breaks(10)) + facet_wrap(vars(location)) + labs(y= (expression(paste(
  "NO Concentration (",
  ??, g, "/", m^3,
  ")", sep=""))), x = "Date") + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location')
p
ggsave("NO Path Facet.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(NO,aes(Date,Avg_NO)) + geom_path(aes(colour=location)) + scale_x_date()  + labs(y= (expression(paste(
  "NO Concentration (",
  ??, g, "/", m^3,
  ")", sep=""))), x = "Date") + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location')
p
ggsave("NO Path.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(NO,aes(Date,Avg_NO)) + geom_smooth(aes(colour=location),span=0.4) + scale_x_date() + facet_wrap(vars(location))  + labs(y= (expression(paste(
  "NO Concentration (",
  ??, g, "/", m^3,
  ")", sep=""))), x = "Date") + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location')
p
ggsave("NO Smooth Facet.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(NO,aes(Date,Avg_NO)) + geom_smooth(aes(colour=location),span=0.4) + scale_x_date(breaks = pretty_breaks(17))  + labs(y= (expression(paste(
  "NO Concentration (",
  µ, g, "/", m^3,
  ")", sep=""))), x = "Date") + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location') + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=15)) + theme(axis.text.y = element_text(size=15))
p
ggsave("NO Smooth.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(NO,aes(Date,Avg_NO)) + geom_point(aes(colour=location)) + scale_x_date()  + labs(y= (expression(paste(
  "NO Concentration (",
  ??, g, "/", m^3,
  ")", sep=""))), x = "Date") + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location')
p
ggsave("NO Point.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(NO,aes(Date,Avg_NO)) + geom_point(aes(colour=location)) + scale_x_date() + facet_wrap(vars(location))  + labs(y= (expression(paste(
  "NO Concentration (",
  ??, g, "/", m^3,
  ")", sep=""))), x = "Date") + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location')
p
ggsave("NO Point Facet.png", p, width = 24, height = 14,dpi = 300 )#save to file




setwd("~/Dissertation/Dissertation Datasets/Plots")
png(file = "NO Histogram Haymarket 2019.png")
hist(CO_2019$Value,main = NULL,xlab = "NO Concentration 2019",xlim = c(0,100),col = "lightblue",freq=FALSE)
dev.off() #Saving the file
png(file = "NO Histogram Haymarket 2020.png")
hist(CO_2020$Value,main = NULL,xlab = "NO Concentration 2020",xlim = c(0,100),col = "lightblue",freq=FALSE)
dev.off() #Saving the file
png(file = "LN NO Histogram Haymarket 2019.png")
hist(log(CO_2019$Value),main = NULL,xlab = "Log NO Concentration 2019",xlim = c(-6,6),col = "lightblue",freq=FALSE)
dev.off() #Saving the file
png(file = "LN NO Histogram Haymarket 2020.png")
hist(log(CO_2020$Value),main = NULL,xlab = "Log NO Concentration 2020",xlim = c(-6,6),col = "lightblue",freq=FALSE)
dev.off() #Saving the file

##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymaket Jan-Mar19")
January_Data <- read.csv("Jan19_18Feb19Mid_Clean.csv",header = TRUE)

library(tidyr)

summary(January_Data)
str(January_Data)
January_Data$Date_Time <- as.POSIXlt.character(January_Data$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
January_Data<-January_Data[-c(1,2,5)]

mydf_list <- split( January_Data , f = January_Data$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Jan_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Jan_df)

###############################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Feb")
February_Data <- read.csv("Haymarket_Feb_Clean.csv",header = TRUE)


library(tidyr)

summary(February_Data)
February_Data$Date_Time <- as.POSIXlt.character(February_Data$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
February_Data<-February_Data[-c(1,2)]

mydf_list <- split( February_Data , f = February_Data$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Feb_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Feb_df)

############################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Mar")###this is the location of files
Air_Quality_Mar <- read.csv("Haymarket_Mar_Clean.csv", header= TRUE)
library(tidyr)

summary(Air_Quality_Mar)
Air_Quality_Mar$Date_Time <- as.POSIXlt.character(Air_Quality_Mar$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
Air_Quality_Mar<-Air_Quality_Mar[-c(1,2)]

mydf_list <- split( Air_Quality_Mar , f = Air_Quality_Mar$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Mar_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Mar_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Apr")###this is the location of files
Air_Quality <- read.csv("Haymarket_Apr_Clean.csv", header= TRUE)
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

CO_Apr_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Apr_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_May")###this is the location of files
Air_Quality <- read.csv("Haymarket_May_Clean.csv", header= TRUE)
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

CO_May_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_May_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Jun")###this is the location of files
Air_Quality <- read.csv("Haymarket_Jun_Clean.csv", header= TRUE)
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

CO_Jun_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Jun_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Jul")###this is the location of files
Air_Quality <- read.csv("Haymarket_Jul_Clean.csv", header= TRUE)
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

CO_Jul_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Jul_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Aug")###this is the location of files
Air_Quality <- read.csv("Haymarket_Aug_Clean.csv", header= TRUE)
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

CO_Aug_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Aug_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Sep")###this is the location of files
Air_Quality <- read.csv("Haymarket_Sep_Clean.csv", header= TRUE)
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

CO_Sep_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Sep_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Oct")###this is the location of files
Air_Quality <- read.csv("Haymarket_Oct_Clean.csv", header= TRUE)
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

CO_Oct_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Oct_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Nov")###this is the location of files
Air_Quality <- read.csv("Haymarket_Nov_Clean.csv", header= TRUE)
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

CO_Nov_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Nov_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Dec")###this is the location of files
Air_Quality <- read.csv("Haymarket_Dec_Clean.csv", header= TRUE)
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

CO_Dec_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Dec_df)



CO_2019<-rbind(CO_Mar_df,CO_Apr_df)
CO_2019<-rbind(CO_2019,CO_May_df)
CO_2019<-rbind(CO_2019,CO_Feb_df)
CO_2019<-rbind(CO_2019,CO_Jan_df)
CO_2019<-rbind(CO_2019,CO_Jun_df)
CO_2019<-rbind(CO_2019,CO_Jul_df)
CO_2019<-rbind(CO_2019,CO_Aug_df)
CO_2019<-rbind(CO_2019,CO_Sep_df)
CO_2019<-rbind(CO_2019,CO_Oct_df)
CO_2019<-rbind(CO_2019,CO_Nov_df)
CO_2019<-rbind(CO_2019,CO_Dec_df)

anyNA(CO_2019)
CO_2019[CO_2019 < 0] <- NA
CO_2019<-na.omit(CO_2019)

summary(CO_2019$Value)

boxplot(CO_2019$Value)
boxplot(CO_2019$Value,plot=FALSE)$out
outliers <- boxplot(CO_2019$Value,plot=FALSE)$out
print(outliers)
CO_2019[which(CO_2019$Value %in% outliers),]
CO_2019<-CO_2019[-which(CO_2019$Value %in% outliers),]
summary(CO_2019$Value)


#############################################################################################
#############################################################################################
#############################################################################################
setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Jan20")
January_Data <- read.csv("Haymarket_Jan20_Clean.csv",header = TRUE)

library(tidyr)

summary(January_Data)
str(January_Data)
January_Data$Date_Time <- as.POSIXlt.character(January_Data$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
January_Data<-January_Data[-c(1,2)]

mydf_list <- split( January_Data , f = January_Data$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Jan_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Jan_df)

###############################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Feb20")
February_Data <- read.csv("Haymarket_Feb20_Clean.csv",header = TRUE)


library(tidyr)

summary(February_Data)
February_Data$Date_Time <- as.POSIXlt.character(February_Data$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
February_Data<-February_Data[-c(1,2)]

mydf_list <- split( February_Data , f = February_Data$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Feb_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Feb_df)

############################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Mar20")###this is the location of files
Air_Quality_Mar <- read.csv("Haymarket_Mar20_Clean.csv", header= TRUE)
library(tidyr)

summary(Air_Quality_Mar)
Air_Quality_Mar$Date_Time <- as.POSIXlt.character(Air_Quality_Mar$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
Air_Quality_Mar<-Air_Quality_Mar[-c(1,2)]

mydf_list <- split( Air_Quality_Mar , f = Air_Quality_Mar$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Mar_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Mar_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Apr20")###this is the location of files
Air_Quality <- read.csv("Haymarket_Apr20_Clean.csv", header= TRUE)
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

CO_Apr_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Apr_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_May20")###this is the location of files
Air_Quality <- read.csv("Haymarket_May20_Clean.csv", header= TRUE)
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

CO_May_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_May_df)




CO_2020<-rbind(CO_Mar_df,CO_Apr_df)
CO_2020<-rbind(CO_2020,CO_May_df)
CO_2020<-rbind(CO_2020,CO_Feb_df)
CO_2020<-rbind(CO_2020,CO_Jan_df)

anyNA(CO_2020)
CO_2020[CO_2020 < 0] <- NA
CO_2020<-na.omit(CO_2020)

summary(CO_2020$Value)

boxplot(CO_2020$Value)
boxplot(CO_2020$Value,plot=FALSE)$out
outliers <- boxplot(CO_2020$Value,plot=FALSE)$out
print(outliers)
CO_2020[which(CO_2020$Value %in% outliers),]
CO_2020<-CO_2020[-which(CO_2020$Value %in% outliers),]
summary(CO_2020$Value)

Haym_NO <- rbind(CO_2019,CO_2020)


NOH <- Haym_NO %>%
  group_by(date(Date_Time)) %>%
  summarize(Avg_NO2 = mean(Value, na.rm = TRUE))%>%
  mutate(location="Haymarket")


str(NOH)
setwd("~/Dissertation/Dissertation Datasets/Haymarket")
write_csv(NOH,'NO2_Haymarket.csv')
NOH<-read.csv('NO2_Haymarket.csv')

setwd("~/Dissertation/Dissertation Datasets/Haymarket")
NOH<-read.csv('NO2_Haymarket.csv')
setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station")
NOG<-read.csv('NO2_Gateshead.csv')
setwd("~/Dissertation/Dissertation Datasets/RVI")
NOR<-read.csv('NO2_RVI.csv')
setwd("~/Dissertation/Dissertation Datasets/Dinnington near Airport")
NOD<-read.csv('NO2_Dinnington.csv')

NO<-rbind(NOG,NOH)
NO<-rbind(NO,NOR)
NO<-rbind(NO,NOD)

str(NO)
NO$Date<-as.Date(NO$date.Date_Time.)

setwd("~/Dissertation/Dissertation Datasets/Plots")
p <- ggplot(NO,aes(Date,Avg_NO2,)) + geom_path(aes(colour=location)) + scale_x_date() + facet_wrap(vars(location)) + labs(y= (expression(paste(
  "NO2 Concentration (",
  ??, g, "/", m^3,
  ")", sep=""))), x = "Date") + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location')
p
ggsave("NO2 Path Facet.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(NO,aes(Date,Avg_NO2)) + geom_path(aes(colour=location)) + scale_x_date()  + labs(y= (expression(paste(
  "NO2 Concentration (",
  ??, g, "/", m^3,
  ")", sep=""))), x = "Date") + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location')
p
ggsave("NO2 Path.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(NO,aes(Date,Avg_NO2)) + geom_smooth(aes(colour=location),span=0.4) + scale_x_date() + facet_wrap(vars(location))  + labs(y= (expression(paste(
  "NO2 Concentration (",
  ??, g, "/", m^3,
  ")", sep=""))), x = "Date") + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location')
p
ggsave("NO2 Smooth Facet.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(NO,aes(Date,Avg_NO2)) + geom_smooth(aes(colour=location),span=0.4) + scale_x_date(breaks = pretty_breaks(17))  + labs(y= (expression(paste(
  "NO2 Concentration (",
  µ, g, "/", m^3,
  ")", sep=""))), x = "Date") + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location')+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=15)) + theme(axis.text.y = element_text(size=15))
p
ggsave("NO2 Smooth.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(NO,aes(Date,Avg_NO2)) + geom_point(aes(colour=location)) + scale_x_date()  + labs(y= (expression(paste(
  "NO2 Concentration (",
  ??, g, "/", m^3,
  ")", sep=""))), x = "Date") + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location')
p
ggsave("NO2 Point.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(NO,aes(Date,Avg_NO2)) + geom_point(aes(colour=location)) + scale_x_date() + facet_wrap(vars(location))  + labs(y= (expression(paste(
  "NO2 Concentration (",
  ??, g, "/", m^3,
  ")", sep=""))), x = "Date") + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location')
p
ggsave("NO2 Point Facet.png", p, width = 24, height = 14,dpi = 300 )#save to file



setwd("~/Dissertation/Dissertation Datasets/Plots")
png(file = "NO2 Histogram Haymarket 2019.png")
hist(CO_2019$Value,main = NULL,xlab = "NO2 Concentration 2019",xlim = c(0,100),col = "lightblue",freq=FALSE)
dev.off() #Saving the file
png(file = "NO2 Histogram Haymarket 2020.png")
hist(CO_2020$Value,main = NULL,xlab = "NO2 Concentration 2020",xlim = c(0,100),col = "lightblue",freq=FALSE)
dev.off() #Saving the file
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymaket Jan-Mar19")
January_Data <- read.csv("Jan19_18Feb19Mid_Clean.csv",header = TRUE)

library(tidyr)

summary(January_Data)
str(January_Data)
January_Data$Date_Time <- as.POSIXlt.character(January_Data$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
January_Data<-January_Data[-c(1,2,5)]

mydf_list <- split( January_Data , f = January_Data$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Jan_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Jan_df)

###############################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Feb")
February_Data <- read.csv("Haymarket_Feb_Clean.csv",header = TRUE)


library(tidyr)

summary(February_Data)
February_Data$Date_Time <- as.POSIXlt.character(February_Data$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
February_Data<-February_Data[-c(1,2)]

mydf_list <- split( February_Data , f = February_Data$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Feb_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Feb_df)

############################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Mar")###this is the location of files
Air_Quality_Mar <- read.csv("Haymarket_Mar_Clean.csv", header= TRUE)
library(tidyr)

summary(Air_Quality_Mar)
Air_Quality_Mar$Date_Time <- as.POSIXlt.character(Air_Quality_Mar$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
Air_Quality_Mar<-Air_Quality_Mar[-c(1,2)]

mydf_list <- split( Air_Quality_Mar , f = Air_Quality_Mar$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Mar_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Mar_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Apr")###this is the location of files
Air_Quality <- read.csv("Haymarket_Apr_Clean.csv", header= TRUE)
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

CO_Apr_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Apr_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_May")###this is the location of files
Air_Quality <- read.csv("Haymarket_May_Clean.csv", header= TRUE)
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

CO_May_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_May_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Jun")###this is the location of files
Air_Quality <- read.csv("Haymarket_Jun_Clean.csv", header= TRUE)
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

CO_Jun_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Jun_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Jul")###this is the location of files
Air_Quality <- read.csv("Haymarket_Jul_Clean.csv", header= TRUE)
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

CO_Jul_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Jul_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Aug")###this is the location of files
Air_Quality <- read.csv("Haymarket_Aug_Clean.csv", header= TRUE)
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

CO_Aug_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Aug_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Sep")###this is the location of files
Air_Quality <- read.csv("Haymarket_Sep_Clean.csv", header= TRUE)
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

CO_Sep_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Sep_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Oct")###this is the location of files
Air_Quality <- read.csv("Haymarket_Oct_Clean.csv", header= TRUE)
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

CO_Oct_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Oct_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Nov")###this is the location of files
Air_Quality <- read.csv("Haymarket_Nov_Clean.csv", header= TRUE)
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

CO_Nov_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Nov_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Dec")###this is the location of files
Air_Quality <- read.csv("Haymarket_Dec_Clean.csv", header= TRUE)
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

CO_Dec_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Dec_df)



CO_2019<-rbind(CO_Mar_df,CO_Apr_df)
CO_2019<-rbind(CO_2019,CO_May_df)
CO_2019<-rbind(CO_2019,CO_Feb_df)
CO_2019<-rbind(CO_2019,CO_Jan_df)
CO_2019<-rbind(CO_2019,CO_Jun_df)
CO_2019<-rbind(CO_2019,CO_Jul_df)
CO_2019<-rbind(CO_2019,CO_Aug_df)
CO_2019<-rbind(CO_2019,CO_Sep_df)
CO_2019<-rbind(CO_2019,CO_Oct_df)
CO_2019<-rbind(CO_2019,CO_Nov_df)
CO_2019<-rbind(CO_2019,CO_Dec_df)

summary(CO_2019$Value)

anyNA(CO_2019)
CO_2019[CO_2019 < 0] <- NA
CO_2019<-na.omit(CO_2019)

boxplot(CO_2019$Value)
boxplot(CO_2019$Value,plot=FALSE)$out
outliers <- boxplot(CO_2019$Value,plot=FALSE)$out
print(outliers)
CO_2019[which(CO_2019$Value %in% outliers),]
CO_2019<-CO_2019[-which(CO_2019$Value %in% outliers),]
summary(CO_2019$Value)


#############################################################################################
#############################################################################################
#############################################################################################
setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Jan20")
January_Data <- read.csv("Haymarket_Jan20_Clean.csv",header = TRUE)

library(tidyr)

summary(January_Data)
str(January_Data)
January_Data$Date_Time <- as.POSIXlt.character(January_Data$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
January_Data<-January_Data[-c(1,2)]

mydf_list <- split( January_Data , f = January_Data$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Jan_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Jan_df)

###############################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Feb20")
February_Data <- read.csv("Haymarket_Feb20_Clean.csv",header = TRUE)


library(tidyr)

summary(February_Data)
February_Data$Date_Time <- as.POSIXlt.character(February_Data$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
February_Data<-February_Data[-c(1,2)]

mydf_list <- split( February_Data , f = February_Data$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Feb_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Feb_df)

############################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Mar20")###this is the location of files
Air_Quality_Mar <- read.csv("Haymarket_Mar20_Clean.csv", header= TRUE)
library(tidyr)

summary(Air_Quality_Mar)
Air_Quality_Mar$Date_Time <- as.POSIXlt.character(Air_Quality_Mar$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
Air_Quality_Mar<-Air_Quality_Mar[-c(1,2)]

mydf_list <- split( Air_Quality_Mar , f = Air_Quality_Mar$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Mar_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Mar_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Apr20")###this is the location of files
Air_Quality <- read.csv("Haymarket_Apr20_Clean.csv", header= TRUE)
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

CO_Apr_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Apr_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_May20")###this is the location of files
Air_Quality <- read.csv("Haymarket_May20_Clean.csv", header= TRUE)
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

CO_May_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_May_df)




CO_2020<-rbind(CO_Mar_df,CO_Apr_df)
CO_2020<-rbind(CO_2020,CO_May_df)
CO_2020<-rbind(CO_2020,CO_Feb_df)
CO_2020<-rbind(CO_2020,CO_Jan_df)

anyNA(CO_2020)
CO_2020[CO_2020 < 0] <- NA
CO_2020<-na.omit(CO_2020)

summary(CO_2020$Value)

boxplot(CO_2020$Value)
boxplot(CO_2020$Value,plot=FALSE)$out
outliers <- boxplot(CO_2020$Value,plot=FALSE)$out
print(outliers)
CO_2020[which(CO_2020$Value %in% outliers),]
CO_2020<-CO_2020[-which(CO_2020$Value %in% outliers),]
summary(CO_2020$Value)

Haym_NO <- rbind(CO_2019,CO_2020)


NOH <- Haym_NO %>%
  group_by(date(Date_Time)) %>%
  summarize(Avg_O3 = mean(Value, na.rm = TRUE))%>%
  mutate(location="Haymarket")




str(NOH)
setwd("~/Dissertation/Dissertation Datasets/Haymarket")
write_csv(NOH,'O3_Haymarket.csv')
O3H<-read.csv('O3_Haymarket.csv')

setwd("~/Dissertation/Dissertation Datasets/Haymarket")
O3H<-read.csv('O3_Haymarket.csv')
setwd("~/Dissertation/Dissertation Datasets/Dinnington near Airport")
O3D<-read.csv('O3_Dinnington.csv')

NO<-rbind(O3D,O3H)

str(NO)
NO$Date<-as.Date(NO$date.Date_Time.)

setwd("~/Dissertation/Dissertation Datasets/Plots")
p <- ggplot(NO,aes(Date,Avg_O3,)) + geom_path(aes(colour=location)) + scale_x_date() + facet_wrap(vars(location)) + labs(y= "O3 Concentration (ppb)", x = "Date") + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location')
p
ggsave("O3 Path Facet.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(NO,aes(Date,Avg_O3)) + geom_path(aes(colour=location)) + scale_x_date()  + labs(y= "O3 Concentration (ppb)", x = "Date")+ theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location')
p
ggsave("O3 Path.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(NO,aes(Date,Avg_O3)) + geom_smooth(aes(colour=location),span=0.4) + scale_x_date() + facet_wrap(vars(location))  + labs(y= "O3 Concentration (ppb)", x = "Date")+ theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location')
p
ggsave("O3 Smooth Facet.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(NO,aes(Date,Avg_O3)) + geom_smooth(aes(colour=location),span=0.4) + scale_x_date(breaks = pretty_breaks(17))  + labs(y= "O3 Concentration (ppb)", x = "Date")+ theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location')+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=15)) + theme(axis.text.y = element_text(size=15))
p
ggsave("O3 Smooth.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(NO,aes(Date,Avg_O3)) + geom_point(aes(colour=location)) + scale_x_date()  + labs(y= "O3 Concentration (ppb)", x = "Date")+ theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location')
p
ggsave("O3 Point.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(NO,aes(Date,Avg_O3)) + geom_point(aes(colour=location)) + scale_x_date() + facet_wrap(vars(location))  + labs(y= "O3 Concentration (ppb)", x = "Date")+ theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location')
p
ggsave("O3 Point Facet.png", p, width = 24, height = 14,dpi = 300 )#save to file

setwd("~/Dissertation/Dissertation Datasets/Plots")
png(file = "O3 Histogram Haymarket 2019.png")
hist(CO_2019$Value,main = NULL,xlab = "O3 Concentration 2019",xlim = c(0,40),col = "lightblue",freq=FALSE)
dev.off() #Saving the file
png(file = "O3 Histogram Haymarket 2020.png")
hist(CO_2020$Value,main = NULL,xlab = "O3 Concentration 2020",xlim = c(0,40),col = "lightblue",freq=FALSE)
dev.off() #Saving the file

##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymaket Jan-Mar19")
January_Data <- read.csv("Jan19_18Feb19Mid_Clean.csv",header = TRUE)

library(tidyr)

summary(January_Data)
str(January_Data)
January_Data$Date_Time <- as.POSIXlt.character(January_Data$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
January_Data<-January_Data[-c(1,2,5)]

mydf_list <- split( January_Data , f = January_Data$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Jan_df <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Jan_df)

###############################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Feb")
February_Data <- read.csv("Haymarket_Feb_Clean.csv",header = TRUE)


library(tidyr)

summary(February_Data)
February_Data$Date_Time <- as.POSIXlt.character(February_Data$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
February_Data<-February_Data[-c(1,2)]

mydf_list <- split( February_Data , f = February_Data$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Feb_df <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Feb_df)

############################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Mar")###this is the location of files
Air_Quality_Mar <- read.csv("Haymarket_Mar_Clean.csv", header= TRUE)
library(tidyr)

summary(Air_Quality_Mar)
Air_Quality_Mar$Date_Time <- as.POSIXlt.character(Air_Quality_Mar$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
Air_Quality_Mar<-Air_Quality_Mar[-c(1,2)]

mydf_list <- split( Air_Quality_Mar , f = Air_Quality_Mar$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Mar_df <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Mar_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Apr")###this is the location of files
Air_Quality <- read.csv("Haymarket_Apr_Clean.csv", header= TRUE)
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

CO_Apr_df <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Apr_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_May")###this is the location of files
Air_Quality <- read.csv("Haymarket_May_Clean.csv", header= TRUE)
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

CO_May_df <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_May_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Jun")###this is the location of files
Air_Quality <- read.csv("Haymarket_Jun_Clean.csv", header= TRUE)
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

CO_Jun_df <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Jun_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Jul")###this is the location of files
Air_Quality <- read.csv("Haymarket_Jul_Clean.csv", header= TRUE)
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

CO_Jul_df <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Jul_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Aug")###this is the location of files
Air_Quality <- read.csv("Haymarket_Aug_Clean.csv", header= TRUE)
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

CO_Aug_df <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Aug_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Sep")###this is the location of files
Air_Quality <- read.csv("Haymarket_Sep_Clean.csv", header= TRUE)
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

CO_Sep_df <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Sep_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Oct")###this is the location of files
Air_Quality <- read.csv("Haymarket_Oct_Clean.csv", header= TRUE)
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

CO_Oct_df <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Oct_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Nov")###this is the location of files
Air_Quality <- read.csv("Haymarket_Nov_Clean.csv", header= TRUE)
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

CO_Nov_df <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Nov_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Dec")###this is the location of files
Air_Quality <- read.csv("Haymarket_Dec_Clean.csv", header= TRUE)
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

CO_Dec_df <- mydf_list[6]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Dec_df)



CO_2019<-rbind(CO_Mar_df,CO_Apr_df)
CO_2019<-rbind(CO_2019,CO_May_df)
CO_2019<-rbind(CO_2019,CO_Feb_df)
CO_2019<-rbind(CO_2019,CO_Jan_df)
CO_2019<-rbind(CO_2019,CO_Jun_df)
CO_2019<-rbind(CO_2019,CO_Jul_df)
CO_2019<-rbind(CO_2019,CO_Aug_df)
CO_2019<-rbind(CO_2019,CO_Sep_df)
CO_2019<-rbind(CO_2019,CO_Oct_df)
CO_2019<-rbind(CO_2019,CO_Nov_df)
CO_2019<-rbind(CO_2019,CO_Dec_df)

anyNA(CO_2019)
CO_2019[CO_2019 < 0] <- NA
CO_2019<-na.omit(CO_2019)

summary(CO_2019$Value)

boxplot(CO_2019$Value)
boxplot(CO_2019$Value,plot=FALSE)$out
outliers <- boxplot(CO_2019$Value,plot=FALSE)$out
print(outliers)
CO_2019[which(CO_2019$Value %in% outliers),]
CO_2019<-CO_2019[-which(CO_2019$Value %in% outliers),]
summary(CO_2019$Value)


#############################################################################################
#############################################################################################
#############################################################################################
setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Jan20")
January_Data <- read.csv("Haymarket_Jan20_Clean.csv",header = TRUE)

library(tidyr)

summary(January_Data)
str(January_Data)
January_Data$Date_Time <- as.POSIXlt.character(January_Data$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
January_Data<-January_Data[-c(1,2)]

mydf_list <- split( January_Data , f = January_Data$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Jan_df <- mydf_list[6]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Jan_df)

###############################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Feb20")
February_Data <- read.csv("Haymarket_Feb20_Clean.csv",header = TRUE)


library(tidyr)

summary(February_Data)
February_Data$Date_Time <- as.POSIXlt.character(February_Data$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
February_Data<-February_Data[-c(1,2)]

mydf_list <- split( February_Data , f = February_Data$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Feb_df <- mydf_list[6]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Feb_df)

############################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Mar20")###this is the location of files
Air_Quality_Mar <- read.csv("Haymarket_Mar20_Clean.csv", header= TRUE)
library(tidyr)

summary(Air_Quality_Mar)
Air_Quality_Mar$Date_Time <- as.POSIXlt.character(Air_Quality_Mar$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
Air_Quality_Mar<-Air_Quality_Mar[-c(1,2)]

mydf_list <- split( Air_Quality_Mar , f = Air_Quality_Mar$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Mar_df <- mydf_list[6]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Mar_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Apr20")###this is the location of files
Air_Quality <- read.csv("Haymarket_Apr20_Clean.csv", header= TRUE)
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

CO_Apr_df <- mydf_list[6]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Apr_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_May20")###this is the location of files
Air_Quality <- read.csv("Haymarket_May20_Clean.csv", header= TRUE)
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

CO_May_df <- mydf_list[6]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_May_df)




CO_2020<-rbind(CO_Mar_df,CO_Apr_df)
CO_2020<-rbind(CO_2020,CO_May_df)
CO_2020<-rbind(CO_2020,CO_Feb_df)
CO_2020<-rbind(CO_2020,CO_Jan_df)

anyNA(CO_2020)
CO_2020[CO_2020 < 0] <- NA
CO_2020<-na.omit(CO_2020)

summary(CO_2020$Value)

boxplot(CO_2020$Value)
boxplot(CO_2020$Value,plot=FALSE)$out
outliers <- boxplot(CO_2020$Value,plot=FALSE)$out
print(outliers)
CO_2020[which(CO_2020$Value %in% outliers),]
CO_2020<-CO_2020[-which(CO_2020$Value %in% outliers),]
summary(CO_2020$Value)

Haym_NO <- rbind(CO_2019,CO_2020)


NOH <- Haym_NO %>%
  group_by(date(Date_Time)) %>%
  summarize(Avg_O3 = mean(Value, na.rm = TRUE))%>%
  mutate(location="Haymarket")


str(NOH)
setwd("~/Dissertation/Dissertation Datasets/Haymarket")
write_csv(NOH,'PM4_Haymarket.csv')
PM4H<-read.csv('PM4_Haymarket.csv')

setwd("~/Dissertation/Dissertation Datasets/Haymarket")
PM4H<-read.csv('PM4_Haymarket.csv')
setwd("~/Dissertation/Dissertation Datasets/Dinnington near Airport")
PM4D<-read.csv('PM4_Dinnington.csv')

NO<-rbind(PM4D,PM4H)

str(NO)
NO$Date<-as.Date(NO$date.Date_Time.)

setwd("~/Dissertation/Dissertation Datasets/Plots")
p <- ggplot(NO,aes(Date,Avg_O3,)) + geom_path(aes(colour=location)) + scale_x_date() + facet_wrap(vars(location)) + labs(y= (expression(paste(
  "PM4 Concentration (",
  ??, g, "/", m^3,
  ")", sep=""))), x = "Date") + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location') 
p
ggsave("PM4 Path Facet.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(NO,aes(Date,Avg_O3)) + geom_path(aes(colour=location)) + scale_x_date()  + labs(y= (expression(paste(
  "PM4 Concentration (",
  ??, g, "/", m^3,
  ")", sep=""))), x = "Date") + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location') 
p
ggsave("PM4 Path.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(NO,aes(Date,Avg_O3)) + geom_smooth(aes(colour=location),span=0.4) + scale_x_date() + facet_wrap(vars(location))  + labs(y= (expression(paste(
  "PM4 Concentration (",
  ??, g, "/", m^3,
  ")", sep=""))), x = "Date") + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location') 
p
ggsave("PM4 Smooth Facet.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(NO,aes(Date,Avg_O3)) + geom_smooth(aes(colour=location),span=0.4) + scale_x_date(breaks = pretty_breaks(17))  + labs(y= (expression(paste(
  "PM4 Concentration (",
  µ, g, "/", m^3,
  ")", sep=""))), x = "Date") + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location') + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=15)) + theme(axis.text.y = element_text(size=15))
p
ggsave("PM4 Smooth.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(NO,aes(Date,Avg_O3)) + geom_point(aes(colour=location)) + scale_x_date()  + labs(y=(expression(paste(
  "PM4 Concentration (",
  ??, g, "/", m^3,
  ")", sep=""))), x = "Date") + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location') 
p
ggsave("PM4 Point.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(NO,aes(Date,Avg_O3)) + geom_point(aes(colour=location)) + scale_x_date() + facet_wrap(vars(location))  + labs(y= (expression(paste(
  "PM4 Concentration (",
  ??, g, "/", m^3,
  ")", sep=""))), x = "Date") + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location') 
p
ggsave("PM4 Point Facet.png", p, width = 24, height = 14,dpi = 300 )#save to file

setwd("~/Dissertation/Dissertation Datasets/Plots")
png(file = "PM4 Histogram Haymarket 2019.png")
hist(CO_2019$Value,main = NULL,xlab = "PM4 Concentration 2019",xlim = c(0,20),col = "lightblue",freq=FALSE)
dev.off() #Saving the file
png(file = "PM4 Histogram Haymarket 2020.png")
hist(CO_2020$Value,main = NULL,xlab = "PM4 Concentration 2020",xlim = c(0,20),col = "lightblue",freq=FALSE)
dev.off() #Saving the file
png(file = "LN PM4 Histogram Haymarket 2019.png")
hist(log(CO_2019$Value),main = NULL,xlab = "Log PM4 Concentration 2019",xlim = c(-6,6),col = "lightblue",freq=FALSE)
dev.off() #Saving the file
png(file = "LN PM4 Histogram Haymarket 2020.png")
hist(log(CO_2020$Value),main = NULL,xlab = "Log PM4 Concentration 2020",xlim = c(-6,6),col = "lightblue",freq=FALSE)
dev.off() #Saving the file


##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymaket Jan-Mar19")
January_Data <- read.csv("Jan19_18Feb19Mid_Clean.csv",header = TRUE)

library(tidyr)

summary(January_Data)
str(January_Data)
January_Data$Date_Time <- as.POSIXlt.character(January_Data$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
January_Data<-January_Data[-c(1,2,5)]

mydf_list <- split( January_Data , f = January_Data$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Jan_df <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Jan_df)

###############################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Feb")
February_Data <- read.csv("Haymarket_Feb_Clean.csv",header = TRUE)


library(tidyr)

summary(February_Data)
February_Data$Date_Time <- as.POSIXlt.character(February_Data$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
February_Data<-February_Data[-c(1,2)]

mydf_list <- split( February_Data , f = February_Data$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Feb_df <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Feb_df)

############################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Mar")###this is the location of files
Air_Quality_Mar <- read.csv("Haymarket_Mar_Clean.csv", header= TRUE)
library(tidyr)

summary(Air_Quality_Mar)
Air_Quality_Mar$Date_Time <- as.POSIXlt.character(Air_Quality_Mar$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
Air_Quality_Mar<-Air_Quality_Mar[-c(1,2)]

mydf_list <- split( Air_Quality_Mar , f = Air_Quality_Mar$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Mar_df <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Mar_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Apr")###this is the location of files
Air_Quality <- read.csv("Haymarket_Apr_Clean.csv", header= TRUE)
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

CO_Apr_df <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Apr_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_May")###this is the location of files
Air_Quality <- read.csv("Haymarket_May_Clean.csv", header= TRUE)
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

CO_May_df <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_May_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Jun")###this is the location of files
Air_Quality <- read.csv("Haymarket_Jun_Clean.csv", header= TRUE)
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

CO_Jun_df <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Jun_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Jul")###this is the location of files
Air_Quality <- read.csv("Haymarket_Jul_Clean.csv", header= TRUE)
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

CO_Jul_df <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Jul_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Aug")###this is the location of files
Air_Quality <- read.csv("Haymarket_Aug_Clean.csv", header= TRUE)
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

CO_Aug_df <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Aug_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Sep")###this is the location of files
Air_Quality <- read.csv("Haymarket_Sep_Clean.csv", header= TRUE)
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

CO_Sep_df <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Sep_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Oct")###this is the location of files
Air_Quality <- read.csv("Haymarket_Oct_Clean.csv", header= TRUE)
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

CO_Oct_df <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Oct_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Nov")###this is the location of files
Air_Quality <- read.csv("Haymarket_Nov_Clean.csv", header= TRUE)
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

CO_Nov_df <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Nov_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Dec")###this is the location of files
Air_Quality <- read.csv("Haymarket_Dec_Clean.csv", header= TRUE)
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

CO_Dec_df <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Dec_df)



CO_2019<-rbind(CO_Mar_df,CO_Apr_df)
CO_2019<-rbind(CO_2019,CO_May_df)
CO_2019<-rbind(CO_2019,CO_Feb_df)
CO_2019<-rbind(CO_2019,CO_Jan_df)
CO_2019<-rbind(CO_2019,CO_Jun_df)
CO_2019<-rbind(CO_2019,CO_Jul_df)
CO_2019<-rbind(CO_2019,CO_Aug_df)
CO_2019<-rbind(CO_2019,CO_Sep_df)
CO_2019<-rbind(CO_2019,CO_Oct_df)
CO_2019<-rbind(CO_2019,CO_Nov_df)
CO_2019<-rbind(CO_2019,CO_Dec_df)

anyNA(CO_2019)
CO_2019[CO_2019 < 0] <- NA
CO_2019<-na.omit(CO_2019)

summary(CO_2019$Value)

boxplot(CO_2019$Value)
boxplot(CO_2019$Value,plot=FALSE)$out
outliers <- boxplot(CO_2019$Value,plot=FALSE)$out
print(outliers)
CO_2019[which(CO_2019$Value %in% outliers),]
CO_2019<-CO_2019[-which(CO_2019$Value %in% outliers),]
summary(CO_2019$Value)


#############################################################################################
#############################################################################################
#############################################################################################
setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Jan20")
January_Data <- read.csv("Haymarket_Jan20_Clean.csv",header = TRUE)

library(tidyr)

summary(January_Data)
str(January_Data)
January_Data$Date_Time <- as.POSIXlt.character(January_Data$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
January_Data<-January_Data[-c(1,2)]

mydf_list <- split( January_Data , f = January_Data$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Jan_df <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Jan_df)

###############################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Feb20")
February_Data <- read.csv("Haymarket_Feb20_Clean.csv",header = TRUE)


library(tidyr)

summary(February_Data)
February_Data$Date_Time <- as.POSIXlt.character(February_Data$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
February_Data<-February_Data[-c(1,2)]

mydf_list <- split( February_Data , f = February_Data$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Feb_df <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Feb_df)

############################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Mar20")###this is the location of files
Air_Quality_Mar <- read.csv("Haymarket_Mar20_Clean.csv", header= TRUE)
library(tidyr)

summary(Air_Quality_Mar)
Air_Quality_Mar$Date_Time <- as.POSIXlt.character(Air_Quality_Mar$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
Air_Quality_Mar<-Air_Quality_Mar[-c(1,2)]

mydf_list <- split( Air_Quality_Mar , f = Air_Quality_Mar$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Mar_df <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Mar_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Apr20")###this is the location of files
Air_Quality <- read.csv("Haymarket_Apr20_Clean.csv", header= TRUE)
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

CO_Apr_df <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Apr_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_May20")###this is the location of files
Air_Quality <- read.csv("Haymarket_May20_Clean.csv", header= TRUE)
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

CO_May_df <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_May_df)




CO_2020<-rbind(CO_Mar_df,CO_Apr_df)
CO_2020<-rbind(CO_2020,CO_May_df)
CO_2020<-rbind(CO_2020,CO_Feb_df)
CO_2020<-rbind(CO_2020,CO_Jan_df)

anyNA(CO_2020)
CO_2020[CO_2020 < 0] <- NA
CO_2020<-na.omit(CO_2020)

summary(CO_2020$Value)

boxplot(CO_2020$Value)
boxplot(CO_2020$Value,plot=FALSE)$out
outliers <- boxplot(CO_2020$Value,plot=FALSE)$out
print(outliers)
CO_2020[which(CO_2020$Value %in% outliers),]
CO_2020<-CO_2020[-which(CO_2020$Value %in% outliers),]
summary(CO_2020$Value)


Haym_NO <- rbind(CO_2019,CO_2020)


NOH <- Haym_NO %>%
  group_by(date(Date_Time)) %>%
  summarize(Avg_O3 = mean(Value, na.rm = TRUE))%>%
  mutate(location="Haymarket")


str(NOH)
setwd("~/Dissertation/Dissertation Datasets/Haymarket")
write_csv(NOH,'PM1_Haymarket.csv')
PM1H<-read.csv('PM1_Haymarket.csv')

setwd("~/Dissertation/Dissertation Datasets/Haymarket")
PM1H<-read.csv('PM1_Haymarket.csv')
setwd("~/Dissertation/Dissertation Datasets/Dinnington near Airport")
PM1D<-read.csv('PM1_Dinnington.csv')
setwd("~/Dissertation/Dissertation Datasets/Nafferton Farm")
PM1N<-read.csv('PM1_Nafferton.csv')

NO<-rbind(PM1D,PM1H)
NO<-rbind(NO,PM1N)

str(NO)
NO$Date<-as.Date(NO$date.Date_Time.)

setwd("~/Dissertation/Dissertation Datasets/Plots")
p <- ggplot(NO,aes(Date,Avg_O3,)) + geom_path(aes(colour=location)) + scale_x_date() + facet_wrap(vars(location)) + labs(y= (expression(paste(
  "PM1 Concentration (",
  ??, g, "/", m^3,
  ")", sep=""))), x = "Date") + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location') 
p
ggsave("PM1 Path Facet.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(NO,aes(Date,Avg_O3)) + geom_path(aes(colour=location)) + scale_x_date()  + labs(y= (expression(paste(
  "PM1 Concentration (",
  ??, g, "/", m^3,
  ")", sep=""))), x = "Date") + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location') 
p
ggsave("PM1 Path.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(NO,aes(Date,Avg_O3)) + geom_smooth(aes(colour=location),span=0.4) + scale_x_date() + facet_wrap(vars(location))  + labs(y= (expression(paste(
  "PM1 Concentration (",
  ??, g, "/", m^3,
  ")", sep=""))), x = "Date") + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location') 
p
ggsave("PM1 Smooth Facet.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(NO,aes(Date,Avg_O3)) + geom_smooth(aes(colour=location),span=0.4) + scale_x_date(breaks = pretty_breaks(17))  + labs(y= (expression(paste(
  "PM1 Concentration (",
  µ, g, "/", m^3,
  ")", sep=""))), x = "Date") + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location')+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=15)) + theme(axis.text.y = element_text(size=15)) 
p
ggsave("PM1 Smooth.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(NO,aes(Date,Avg_O3)) + geom_point(aes(colour=location)) + scale_x_date()  + labs(y= (expression(paste(
  "PM1 Concentration (",
  ??, g, "/", m^3,
  ")", sep=""))), x = "Date") + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location') 
p
ggsave("PM1 Point.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(NO,aes(Date,Avg_O3)) + geom_point(aes(colour=location)) + scale_x_date() + facet_wrap(vars(location))  + labs(y= (expression(paste(
  "PM1 Concentration (",
  ??, g, "/", m^3,
  ")", sep=""))), x = "Date") + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location') 
p
ggsave("PM1 Point Facet.png", p, width = 24, height = 14,dpi = 300 )#save to file


setwd("~/Dissertation/Dissertation Datasets/Plots")
png(file = "PM1 Histogram Haymarket 2019.png")
hist(CO_2019$Value,main = NULL,xlab = "PM1 Concentration 2019",xlim = c(0,6),col = "lightblue",freq=FALSE)
dev.off() #Saving the file
png(file = "PM1 Histogram Haymarket 2020.png")
hist(CO_2020$Value,main = NULL,xlab = "PM1 Concentration 2020",xlim = c(0,6),col = "lightblue",freq=FALSE)
dev.off() #Saving the file
png(file = "PM1 LN Histogram Haymarket 2019.png")
hist(log(CO_2019$Value),main = NULL,xlab = "Log PM1 Concentration 2019",xlim = c(-4,4),col = "lightblue",freq=FALSE)
dev.off() #Saving the file
png(file = "PM1 LN Histogram Haymarket 2020.png")
hist(log(CO_2020$Value),main = NULL,xlab = "Log PM1 Concentration 2020",xlim = c(-6,4),col = "lightblue",freq=FALSE)
dev.off() #Saving the file

##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymaket Jan-Mar19")
January_Data <- read.csv("Jan19_18Feb19Mid_Clean.csv",header = TRUE)

library(tidyr)

summary(January_Data)
str(January_Data)
January_Data$Date_Time <- as.POSIXlt.character(January_Data$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
January_Data<-January_Data[-c(1,2,5)]

mydf_list <- split( January_Data , f = January_Data$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Jan_df <- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Jan_df)

###############################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Feb")
February_Data <- read.csv("Haymarket_Feb_Clean.csv",header = TRUE)


library(tidyr)

summary(February_Data)
February_Data$Date_Time <- as.POSIXlt.character(February_Data$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
February_Data<-February_Data[-c(1,2)]

mydf_list <- split( February_Data , f = February_Data$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Feb_df <- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Feb_df)

############################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Mar")###this is the location of files
Air_Quality_Mar <- read.csv("Haymarket_Mar_Clean.csv", header= TRUE)
library(tidyr)

summary(Air_Quality_Mar)
Air_Quality_Mar$Date_Time <- as.POSIXlt.character(Air_Quality_Mar$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
Air_Quality_Mar<-Air_Quality_Mar[-c(1,2)]

mydf_list <- split( Air_Quality_Mar , f = Air_Quality_Mar$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Mar_df <- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Mar_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Apr")###this is the location of files
Air_Quality <- read.csv("Haymarket_Apr_Clean.csv", header= TRUE)
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

CO_Apr_df <- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Apr_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_May")###this is the location of files
Air_Quality <- read.csv("Haymarket_May_Clean.csv", header= TRUE)
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

CO_May_df <- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_May_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Jun")###this is the location of files
Air_Quality <- read.csv("Haymarket_Jun_Clean.csv", header= TRUE)
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

CO_Jun_df <- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Jun_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Jul")###this is the location of files
Air_Quality <- read.csv("Haymarket_Jul_Clean.csv", header= TRUE)
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

CO_Jul_df <- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Jul_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Aug")###this is the location of files
Air_Quality <- read.csv("Haymarket_Aug_Clean.csv", header= TRUE)
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

CO_Aug_df <- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Aug_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Sep")###this is the location of files
Air_Quality <- read.csv("Haymarket_Sep_Clean.csv", header= TRUE)
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

CO_Sep_df <- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Sep_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Oct")###this is the location of  files
Air_Quality <- read.csv("Haymarket_Oct_Clean.csv", header= TRUE)
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

CO_Oct_df <- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Oct_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Nov")###this is the location of  files
Air_Quality <- read.csv("Haymarket_Nov_Clean.csv", header= TRUE)
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

CO_Nov_df <- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Nov_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Dec")###this is the location of  files
Air_Quality <- read.csv("Haymarket_Dec_Clean.csv", header= TRUE)
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

CO_Dec_df <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Dec_df)



CO_2019<-rbind(CO_Mar_df,CO_Apr_df)
CO_2019<-rbind(CO_2019,CO_May_df)
CO_2019<-rbind(CO_2019,CO_Feb_df)
CO_2019<-rbind(CO_2019,CO_Jan_df)
CO_2019<-rbind(CO_2019,CO_Jun_df)
CO_2019<-rbind(CO_2019,CO_Jul_df)
CO_2019<-rbind(CO_2019,CO_Aug_df)
CO_2019<-rbind(CO_2019,CO_Sep_df)
CO_2019<-rbind(CO_2019,CO_Oct_df)
CO_2019<-rbind(CO_2019,CO_Nov_df)
CO_2019<-rbind(CO_2019,CO_Dec_df)

anyNA(CO_2019)
CO_2019[CO_2019 < 0] <- NA
CO_2019<-na.omit(CO_2019)

summary(CO_2019$Value)

boxplot(CO_2019$Value)
boxplot(CO_2019$Value,plot=FALSE)$out
outliers <- boxplot(CO_2019$Value,plot=FALSE)$out
print(outliers)
CO_2019[which(CO_2019$Value %in% outliers),]
CO_2019<-CO_2019[-which(CO_2019$Value %in% outliers),]
summary(CO_2019$Value)


#############################################################################################
#############################################################################################
#############################################################################################
setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Jan20")
January_Data <- read.csv("Haymarket_Jan20_Clean.csv",header = TRUE)

library(tidyr)

summary(January_Data)
str(January_Data)
January_Data$Date_Time <- as.POSIXlt.character(January_Data$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
January_Data<-January_Data[-c(1,2)]

mydf_list <- split( January_Data , f = January_Data$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Jan_df <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Jan_df)

###############################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Feb20")
February_Data <- read.csv("Haymarket_Feb20_Clean.csv",header = TRUE)


library(tidyr)

summary(February_Data)
February_Data$Date_Time <- as.POSIXlt.character(February_Data$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
February_Data<-February_Data[-c(1,2)]

mydf_list <- split( February_Data , f = February_Data$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Feb_df <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Feb_df)

############################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Mar20")###this is the location of files
Air_Quality_Mar <- read.csv("Haymarket_Mar20_Clean.csv", header= TRUE)
library(tidyr)

summary(Air_Quality_Mar)
Air_Quality_Mar$Date_Time <- as.POSIXlt.character(Air_Quality_Mar$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
Air_Quality_Mar<-Air_Quality_Mar[-c(1,2)]

mydf_list <- split( Air_Quality_Mar , f = Air_Quality_Mar$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Mar_df <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Mar_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Apr20")
Air_Quality <- read.csv("Haymarket_Apr20_Clean.csv", header= TRUE)
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

CO_Apr_df <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Apr_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_May20")
Air_Quality <- read.csv("Haymarket_May20_Clean.csv", header= TRUE)
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

CO_May_df <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_May_df)




CO_2020<-rbind(CO_Mar_df,CO_Apr_df)
CO_2020<-rbind(CO_2020,CO_May_df)
CO_2020<-rbind(CO_2020,CO_Feb_df)
CO_2020<-rbind(CO_2020,CO_Jan_df)

anyNA(CO_2020)
CO_2020[CO_2020 < 0] <- NA
CO_2020<-na.omit(CO_2020)

summary(CO_2020$Value)

boxplot(CO_2020$Value)
boxplot(CO_2020$Value,plot=FALSE)$out
outliers <- boxplot(CO_2020$Value,plot=FALSE)$out
print(outliers)
CO_2020[which(CO_2020$Value %in% outliers),]
CO_2020<-CO_2020[-which(CO_2020$Value %in% outliers),]
summary(CO_2020$Value)


Haym_NO <- rbind(CO_2019,CO_2020)


NOH <- Haym_NO %>%
  group_by(date(Date_Time)) %>%
  summarize(Avg_O3 = mean(Value, na.rm = TRUE))%>%
  mutate(location="Haymarket")


str(NOH)
setwd("~/Dissertation/Dissertation Datasets/Haymarket")
write_csv(NOH,'PM10_Haymarket.csv')
PM10H<-read.csv('PM10_Haymarket.csv')

setwd("~/Dissertation/Dissertation Datasets/Haymarket")
PM10H<-read.csv('PM10_Haymarket.csv')
setwd("~/Dissertation/Dissertation Datasets/Dinnington near Airport")
PM10D<-read.csv('PM10_Dinnington.csv')
setwd("~/Dissertation/Dissertation Datasets/Nafferton Farm")
PM10N<-read.csv('PM10_Nafferton.csv')

NO<-rbind(PM10D,PM10H)
NO<-rbind(NO,PM10N)

str(NO)
NO$Date<-as.Date(NO$date.Date_Time.)

setwd("~/Dissertation/Dissertation Datasets/Plots")
p <- ggplot(NO,aes(Date,Avg_O3,)) + geom_path(aes(colour=location)) + scale_x_date() + facet_wrap(vars(location)) + labs(y= (expression(paste(
  "PM10 Concentration (",
  ??, g, "/", m^3,
  ")", sep=""))), x = "Date") + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location') 
p
ggsave("PM10 Path Facet.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(NO,aes(Date,Avg_O3)) + geom_path(aes(colour=location)) + scale_x_date()  + labs(y= (expression(paste(
  "PM10 Concentration (",
  ??, g, "/", m^3,
  ")", sep=""))), x = "Date") + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location') 
p
ggsave("PM10 Path.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(NO,aes(Date,Avg_O3)) + geom_smooth(aes(colour=location),span=0.4) + scale_x_date() + facet_wrap(vars(location))  + labs(y= (expression(paste(
  "PM10 Concentration (",
  ??, g, "/", m^3,
  ")", sep=""))), x = "Date") + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location') 
p
ggsave("PM10 Smooth Facet.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(NO,aes(Date,Avg_O3)) + geom_smooth(aes(colour=location),span=0.4) + scale_x_date(breaks = pretty_breaks(17))  + labs(y= (expression(paste(
  "PM10 Concentration (",
  µ, g, "/", m^3,
  ")", sep=""))), x = "Date") + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location')+  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=15)) + theme(axis.text.y = element_text(size=15)) 
p
ggsave("PM10 Smooth.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(NO,aes(Date,Avg_O3)) + geom_point(aes(colour=location)) + scale_x_date()  + labs(y= (expression(paste(
  "PM10 Concentration (",
  ??, g, "/", m^3,
  ")", sep=""))), x = "Date") + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location') 
p
ggsave("PM10 Point.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(NO,aes(Date,Avg_O3)) + geom_point(aes(colour=location)) + scale_x_date() + facet_wrap(vars(location))  + labs(y= (expression(paste(
  "PM10 Concentration (",
  ??, g, "/", m^3,
  ")", sep=""))), x = "Date") + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location') 
p
ggsave("PM10 Point Facet.png", p, width = 24, height = 14,dpi = 300 )#save to file


setwd("~/Dissertation/Dissertation Datasets/Plots")
png(file = "PM10 Histogram Haymarket 2019.png")
hist(CO_2019$Value,main = NULL,xlab = "PM10 Concentration 2019",xlim = c(0,30),col = "lightblue",freq=FALSE)
dev.off() #Saving the file
png(file = "PM10 Histogram Haymarket 2020.png")
hist(CO_2020$Value,main = NULL,xlab = "PM10 Concentration 2020",xlim = c(0,30),col = "lightblue",freq=FALSE)
dev.off() #Saving the file
png(file = "LN PM10 Histogram Haymarket 2019.png")
hist(log(CO_2019$Value),main = NULL,xlab = "Log PM10 Concentration 2019",xlim = c(-4,4),col = "lightblue",freq=FALSE)
dev.off() #Saving the file
png(file = "LN PM10 Histogram Haymarket 2020.png")
hist(log(CO_2020$Value),main = NULL,xlab = "Log PM10 Concentration 2020",xlim = c(-6,4),col = "lightblue",freq=FALSE)
dev.off() #Saving the file

##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymaket Jan-Mar19")
January_Data <- read.csv("Jan19_18Feb19Mid_Clean.csv",header = TRUE)

library(tidyr)

summary(January_Data)
str(January_Data)
January_Data$Date_Time <- as.POSIXlt.character(January_Data$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
January_Data<-January_Data[-c(1,2,5)]

mydf_list <- split( January_Data , f = January_Data$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Jan_df <- mydf_list[10]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Jan_df)

###############################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Feb")
February_Data <- read.csv("Haymarket_Feb_Clean.csv",header = TRUE)


library(tidyr)

summary(February_Data)
February_Data$Date_Time <- as.POSIXlt.character(February_Data$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
February_Data<-February_Data[-c(1,2)]

mydf_list <- split( February_Data , f = February_Data$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Feb_df <- mydf_list[10]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Feb_df)

############################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Mar")
Air_Quality_Mar <- read.csv("Haymarket_Mar_Clean.csv", header= TRUE)
library(tidyr)

summary(Air_Quality_Mar)
Air_Quality_Mar$Date_Time <- as.POSIXlt.character(Air_Quality_Mar$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
Air_Quality_Mar<-Air_Quality_Mar[-c(1,2)]

mydf_list <- split( Air_Quality_Mar , f = Air_Quality_Mar$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Mar_df <- mydf_list[10]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Mar_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Apr")
Air_Quality <- read.csv("Haymarket_Apr_Clean.csv", header= TRUE)
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

CO_Apr_df <- mydf_list[10]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Apr_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_May")
Air_Quality <- read.csv("Haymarket_May_Clean.csv", header= TRUE)
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

CO_May_df <- mydf_list[10]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_May_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Jun")
Air_Quality <- read.csv("Haymarket_Jun_Clean.csv", header= TRUE)
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

CO_Jun_df <- mydf_list[10]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Jun_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Jul")
Air_Quality <- read.csv("Haymarket_Jul_Clean.csv", header= TRUE)
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

CO_Jul_df <- mydf_list[10]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Jul_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Aug")
Air_Quality <- read.csv("Haymarket_Aug_Clean.csv", header= TRUE)
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

CO_Aug_df <- mydf_list[10]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Aug_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Sep")
Air_Quality <- read.csv("Haymarket_Sep_Clean.csv", header= TRUE)
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

CO_Sep_df <- mydf_list[10]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Sep_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Oct")
Air_Quality <- read.csv("Haymarket_Oct_Clean.csv", header= TRUE)
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

CO_Oct_df <- mydf_list[10]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Oct_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Nov")
Air_Quality <- read.csv("Haymarket_Nov_Clean.csv", header= TRUE)
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

CO_Nov_df <- mydf_list[10]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Nov_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Dec")
Air_Quality <- read.csv("Haymarket_Dec_Clean.csv", header= TRUE)
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

CO_Dec_df <- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Dec_df)



CO_2019<-rbind(CO_Mar_df,CO_Apr_df)
CO_2019<-rbind(CO_2019,CO_May_df)
CO_2019<-rbind(CO_2019,CO_Feb_df)
CO_2019<-rbind(CO_2019,CO_Jan_df)
CO_2019<-rbind(CO_2019,CO_Jun_df)
CO_2019<-rbind(CO_2019,CO_Jul_df)
CO_2019<-rbind(CO_2019,CO_Aug_df)
CO_2019<-rbind(CO_2019,CO_Sep_df)
CO_2019<-rbind(CO_2019,CO_Oct_df)
CO_2019<-rbind(CO_2019,CO_Nov_df)
CO_2019<-rbind(CO_2019,CO_Dec_df)

anyNA(CO_2019)
CO_2019[CO_2019 < 0] <- NA
CO_2019<-na.omit(CO_2019)

summary(CO_2019$Value)

boxplot(CO_2019$Value)
boxplot(CO_2019$Value,plot=FALSE)$out
outliers <- boxplot(CO_2019$Value,plot=FALSE)$out
print(outliers)
CO_2019[which(CO_2019$Value %in% outliers),]
CO_2019<-CO_2019[-which(CO_2019$Value %in% outliers),]
summary(CO_2019$Value)


#############################################################################################
#############################################################################################
#############################################################################################
setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Jan20")
January_Data <- read.csv("Haymarket_Jan20_Clean.csv",header = TRUE)

library(tidyr)

summary(January_Data)
str(January_Data)
January_Data$Date_Time <- as.POSIXlt.character(January_Data$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
January_Data<-January_Data[-c(1,2)]

mydf_list <- split( January_Data , f = January_Data$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Jan_df <- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Jan_df)

###############################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Feb20")
February_Data <- read.csv("Haymarket_Feb20_Clean.csv",header = TRUE)


library(tidyr)

summary(February_Data)
February_Data$Date_Time <- as.POSIXlt.character(February_Data$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
February_Data<-February_Data[-c(1,2)]

mydf_list <- split( February_Data , f = February_Data$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Feb_df <- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Feb_df)

############################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Mar20")
Air_Quality_Mar <- read.csv("Haymarket_Mar20_Clean.csv", header= TRUE)
library(tidyr)

summary(Air_Quality_Mar)
Air_Quality_Mar$Date_Time <- as.POSIXlt.character(Air_Quality_Mar$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
Air_Quality_Mar<-Air_Quality_Mar[-c(1,2)]

mydf_list <- split( Air_Quality_Mar , f = Air_Quality_Mar$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

CO_Mar_df <- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Mar_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Apr20")
Air_Quality <- read.csv("Haymarket_Apr20_Clean.csv", header= TRUE)
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

CO_Apr_df <- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Apr_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_May20")
Air_Quality <- read.csv("Haymarket_May20_Clean.csv", header= TRUE)
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

CO_May_df <- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_May_df)




CO_2020<-rbind(CO_Mar_df,CO_Apr_df)
CO_2020<-rbind(CO_2020,CO_May_df)
CO_2020<-rbind(CO_2020,CO_Feb_df)
CO_2020<-rbind(CO_2020,CO_Jan_df)

anyNA(CO_2020)
CO_2020[CO_2020 < 0] <- NA
CO_2020<-na.omit(CO_2020)

summary(CO_2020$Value)

boxplot(CO_2020$Value)
boxplot(CO_2020$Value,plot=FALSE)$out
outliers <- boxplot(CO_2020$Value,plot=FALSE)$out
print(outliers)
CO_2020[which(CO_2020$Value %in% outliers),]
CO_2020<-CO_2020[-which(CO_2020$Value %in% outliers),]
summary(CO_2020$Value)

Haym_NO <- rbind(CO_2019,CO_2020)


NOH <- Haym_NO %>%
  group_by(date(Date_Time)) %>%
  summarize(Avg_O3 = mean(Value, na.rm = TRUE))%>%
  mutate(location="Haymarket")


str(NOH)
setwd("~/Dissertation/Dissertation Datasets/Haymarket")
write_csv(NOH,'PM2.5_Haymarket.csv')
PM2.5H<-read.csv('PM2.5_Haymarket.csv')

setwd("~/Dissertation/Dissertation Datasets/Haymarket")
PM2.5H<-read.csv('PM2.5_Haymarket.csv')
setwd("~/Dissertation/Dissertation Datasets/Dinnington near Airport")
PM2.5D<-read.csv('PM2.5_Dinnington.csv')
setwd("~/Dissertation/Dissertation Datasets/Nafferton Farm")
PM2.5N<-read.csv('PM2.5_Nafferton.csv')

NO<-rbind(PM2.5D,PM2.5H)
NO<-rbind(NO,PM2.5N)

str(NO)
NO$Date<-as.Date(NO$date.Date_Time.)

setwd("~/Dissertation/Dissertation Datasets/Plots")
p <- ggplot(NO,aes(Date,Avg_O3,)) + geom_path(aes(colour=location)) + scale_x_date() + facet_wrap(vars(location)) + labs(y= (expression(paste(
  "PM2.5 Concentration (",
  ??, g, "/", m^3,
  ")", sep=""))), x = "Date") + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location') 
p
ggsave("PM2.5 Path Facet.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(NO,aes(Date,Avg_O3)) + geom_path(aes(colour=location)) + scale_x_date()  + labs(y= (expression(paste(
  "PM2.5 Concentration (",
  ??, g, "/", m^3,
  ")", sep=""))), x = "Date") + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location') 
p
p
ggsave("PM2.5 Path.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(NO,aes(Date,Avg_O3)) + geom_smooth(aes(colour=location),span=0.4) + scale_x_date() + facet_wrap(vars(location))  + labs(y=(expression(paste(
  "PM2.5 Concentration (",
  ??, g, "/", m^3,
  ")", sep=""))), x = "Date") + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location') 
p
p
ggsave("PM2.5 Smooth Facet.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(NO,aes(Date,Avg_O3)) + geom_smooth(aes(colour=location),span=0.4) + scale_x_date(breaks = pretty_breaks(17))  + labs(y=(expression(paste(
  "PM2.5 Concentration (",
  µ, g, "/", m^3,
  ")", sep=""))), x = "Date") + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location') + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=15)) + theme(axis.text.y = element_text(size=15))
p
p
ggsave("PM2.5 Smooth.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(NO,aes(Date,Avg_O3)) + geom_point(aes(colour=location)) + scale_x_date()  + labs(y= (expression(paste(
  "PM2.5 Concentration (",
  ??, g, "/", m^3,
  ")", sep=""))), x = "Date") + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location') 
p
p
ggsave("PM2.5 Point.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(NO,aes(Date,Avg_O3)) + geom_point(aes(colour=location)) + scale_x_date() + facet_wrap(vars(location))  + labs(y= (expression(paste(
  "PM2.5 Concentration (",
  ??, g, "/", m^3,
  ")", sep=""))), x = "Date") + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location') 
p
p
ggsave("PM2.5 Point Facet.png", p, width = 24, height = 14,dpi = 300 )#save to file


setwd("~/Dissertation/Dissertation Datasets/Plots")
png(file = "PM2.5 LN Histogram Haymarket 2019.png")
hist((CO_2019$Value),main = NULL,xlab = "PM2.5 Concentration 2019",xlim = c(0,12),col = "lightblue",freq=FALSE)
dev.off() #Saving the file

png(file = "PM2.5 Histogram Haymarket 2020.png")
hist(CO_2020$Value,main = NULL,xlab = "PM2.5 Concentration 2020",xlim = c(0,12),col = "lightblue",freq=FALSE)
dev.off() #Saving the file

png(file = "PM2.5 LN Histogram Haymarket 2019.png")
hist(log(CO_2019$Value),main = NULL,xlab = " Log PM2.5 Concentration 2019",xlim = c(-4,4),col = "lightblue",freq=FALSE)
dev.off() #Saving the file

png(file = "PM2.5 LN Histogram Haymarket 2020.png")
hist(log(CO_2020$Value),main = NULL,xlab = "Log PM2.5 Concentration 2020",xlim = c(-4,4),col = "lightblue",freq=FALSE)
dev.off() #Saving the file
### ??=mu where seen