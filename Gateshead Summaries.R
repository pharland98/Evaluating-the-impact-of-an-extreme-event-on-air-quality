setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station/GatesheadBS_Aug")###this is the location of your files
Air_Quality <- read.csv("Gateshead_Aug_Clean.csv", header= TRUE)
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

setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station/GatesheadBS_Sep")###this is the location of your files
Air_Quality <- read.csv("Gateshead_Sep_Clean.csv", header= TRUE)
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

setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station/GatesheadBS_Oct")###this is the location of your files
Air_Quality <- read.csv("Gateshead_Oct_Clean.csv", header= TRUE)
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

setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station/GatesheadBS_Nov")###this is the location of your files
Air_Quality <- read.csv("Gateshead_Nov_Clean.csv", header= TRUE)
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

setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station/GatesheadBS_Dec")###this is the location of your files
Air_Quality <- read.csv("Gateshead_Dec_Clean.csv", header= TRUE)
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


################################################################################################


CO_2019<-rbind(CO_Aug_df,CO_Sep_df)
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

##########################################################################################
##########################################################################################

setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station/GatesheadBS_Mar20")###this is the location of your files
Air_Quality_Mar20 <- read.csv("Gateshead_Mar20_Clean.csv", header= TRUE)
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

CO_Mar20_df <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Mar20_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station/GatesheadBS_Apr20")###this is the location of your files
Air_Quality <- read.csv("Gateshead_Apr20_Clean.csv", header= TRUE)
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

CO_Apr20_df <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Apr20_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station/GatesheadBS_May20")###this is the location of your files
Air_Quality <- read.csv("Gateshead_May20_Clean.csv", header= TRUE)
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

CO_May20_df <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_May20_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station/GatesheadBS_Jan20")###this is the location of your files
Air_Quality <- read.csv("Gateshead_Jan20_Clean.csv", header= TRUE)
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

CO_Jan20_df <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Jan20_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station/GatesheadBS_Feb20")###this is the location of your files
Air_Quality <- read.csv("Gateshead_Feb20_Clean.csv", header= TRUE)
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

CO_Feb20_df <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(CO_Feb20_df)


################################################################################################


CO_2020<-rbind(CO_Jan20_df,CO_Feb20_df)
CO_2020<-rbind(CO_2020,CO_Mar20_df)
CO_2020<-rbind(CO_2020,CO_Apr20_df)
CO_2020<-rbind(CO_2020,CO_May20_df)

summary(CO_2020$Value)

boxplot(CO_2020$Value)
boxplot(CO_2020$Value,plot=FALSE)$out
outliers <- boxplot(CO_2020$Value,plot=FALSE)$out
print(outliers)
CO_2020[which(CO_2020$Value %in% outliers),]
CO_2020<-CO_2020[-which(CO_2020$Value %in% outliers),]

summary(CO_2020$Value)

Gateshead_CO <- rbind(CO_2019,CO_2020)



COG <- Gateshead_CO %>%
  group_by(date(Date_Time)) %>%
  summarize(Avg_CO = mean(Value, na.rm = TRUE))%>%
  mutate(location="Gateshead")

str(COG)
setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station")
write_csv(COG,'CO_Gateshead.csv')
COG<-read.csv('CO_Gateshead.csv')


setwd("~/Dissertation/Dissertation Datasets/Plots")
png(file = "CO Histogram Gateshead 2019.png")
hist(CO_2019$Value,main = NULL,xlab = "CO Concentration 2019",xlim = c(0,800),col = "lightblue",freq=FALSE)
dev.off() #Saving the file
png(file = "CO Histogram Gateshead 2020.png")
hist(CO_2020$Value,main = NULL,xlab = "CO Concentration 2020",xlim = c(0,600),col = "lightblue",freq=FALSE)
dev.off() #Saving the file

plot()
ggplot(CO_2019,aes('Date_Time','Value')) + 
  geom_line(aes(color='red'))



#########################################################################################
#########################################################################################
setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station/GatesheadBS_Aug")###this is the location of your files
Air_Quality <- read.csv("Gateshead_Aug_Clean.csv", header= TRUE)
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

NO_Aug_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(NO_Aug_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station/GatesheadBS_Sep")###this is the location of your files
Air_Quality <- read.csv("Gateshead_Sep_Clean.csv", header= TRUE)
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

NO_Sep_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(NO_Sep_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station/GatesheadBS_Oct")###this is the location of your files
Air_Quality <- read.csv("Gateshead_Oct_Clean.csv", header= TRUE)
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

NO_Oct_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(NO_Oct_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station/GatesheadBS_Nov")###this is the location of your files
Air_Quality <- read.csv("Gateshead_Nov_Clean.csv", header= TRUE)
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

NO_Nov_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(NO_Nov_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station/GatesheadBS_Dec")###this is the location of your files
Air_Quality <- read.csv("Gateshead_Dec_Clean.csv", header= TRUE)
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

NO_Dec_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(NO_Dec_df)


################################################################################################


NO_2019<-rbind(NO_Aug_df,NO_Sep_df)
NO_2019<-rbind(NO_2019,NO_Oct_df)
NO_2019<-rbind(NO_2019,NO_Nov_df)
NO_2019<-rbind(NO_2019,NO_Dec_df)

summary(NO_2019$Value)

boxplot(NO_2019$Value)
boxplot(NO_2019$Value,plot=FALSE)$out
outliers <- boxplot(NO_2019$Value,plot=FALSE)$out
print(outliers)
NO_2019[which(NO_2019$Value %in% outliers),]
NO_2019<-NO_2019[-which(NO_2019$Value %in% outliers),]
summary(NO_2019$Value)

##########################################################################################
##########################################################################################

setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station/GatesheadBS_Mar20")###this is the location of your files
Air_Quality_Mar20 <- read.csv("Gateshead_Mar20_Clean.csv", header= TRUE)
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

NO_Mar20_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(NO_Mar20_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station/GatesheadBS_Apr20")###this is the location of your files
Air_Quality <- read.csv("Gateshead_Apr20_Clean.csv", header= TRUE)
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

NO_Apr20_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(NO_Apr20_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station/GatesheadBS_May20")###this is the location of your files
Air_Quality <- read.csv("Gateshead_May20_Clean.csv", header= TRUE)
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

NO_May20_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(NO_May20_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station/GatesheadBS_Jan20")###this is the location of your files
Air_Quality <- read.csv("Gateshead_Jan20_Clean.csv", header= TRUE)
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

NO_Jan20_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(NO_Jan20_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station/GatesheadBS_Feb20")###this is the location of your files
Air_Quality <- read.csv("Gateshead_Feb20_Clean.csv", header= TRUE)
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

NO_Feb20_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(NO_Feb20_df)


################################################################################################


NO_2020<-rbind(NO_Jan20_df,NO_Feb20_df)
NO_2020<-rbind(NO_2020,NO_Mar20_df)
NO_2020<-rbind(NO_2020,NO_Apr20_df)
NO_2020<-rbind(NO_2020,NO_May20_df)

summary(NO_2020$Value)

boxplot(NO_2020$Value)
boxplot(NO_2020$Value,plot=FALSE)$out
outliers <- boxplot(NO_2020$Value,plot=FALSE)$out
print(outliers)
NO_2020[which(NO_2020$Value %in% outliers),]
NO_2020<-NO_2020[-which(NO_2020$Value %in% outliers),]

summary(NO_2020$Value)

Gateshead_NO <- rbind(NO_2019,NO_2020)


NOG <- Gateshead_NO %>%
  group_by(date(Date_Time)) %>%
  summarize(Avg_NO = mean(Value, na.rm = TRUE))%>%
  mutate(location="Gateshead")


str(NOG)
setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station")
write_csv(NOG,'NO_Gateshead.csv')
NOG<-read.csv('NO_Gateshead.csv')

setwd("~/Dissertation/Dissertation Datasets/Plots")
png(file = "NO Histogram Gateshead 2019.png")
hist(NO_2019$Value,main = NULL,xlab = "NO Concentration 2019",xlim = c(0,250),col = "lightblue",freq=FALSE)
dev.off() #Saving the file
png(file = "NO Histogram Gateshead 2020.png")
hist(NO_2020$Value,main = NULL,xlab = "NO Concentration 2020",xlim = c(0,150),col = "lightblue",freq=FALSE)
dev.off() #Saving the file

#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station/GatesheadBS_Aug")###this is the location of your files
Air_Quality <- read.csv("Gateshead_Aug_Clean.csv", header= TRUE)
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

NO2_Aug_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(NO2_Aug_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station/GatesheadBS_Sep")###this is the location of your files
Air_Quality <- read.csv("Gateshead_Sep_Clean.csv", header= TRUE)
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

NO2_Sep_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(NO2_Sep_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station/GatesheadBS_Oct")###this is the location of your files
Air_Quality <- read.csv("Gateshead_Oct_Clean.csv", header= TRUE)
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

NO2_Oct_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(NO2_Oct_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station/GatesheadBS_Nov")###this is the location of your files
Air_Quality <- read.csv("Gateshead_Nov_Clean.csv", header= TRUE)
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

NO2_Nov_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(NO2_Nov_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station/GatesheadBS_Dec")###this is the location of your files
Air_Quality <- read.csv("Gateshead_Dec_Clean.csv", header= TRUE)
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

NO2_Dec_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(NO2_Dec_df)


################################################################################################


NO2_2019<-rbind(NO2_Aug_df,NO2_Sep_df)
NO2_2019<-rbind(NO2_2019,NO2_Oct_df)
NO2_2019<-rbind(NO2_2019,NO2_Nov_df)
NO2_2019<-rbind(NO2_2019,NO2_Dec_df)

summary(NO2_2019$Value)

boxplot(NO2_2019$Value)
boxplot(NO2_2019$Value,plot=FALSE)$out
outliers <- boxplot(NO2_2019$Value,plot=FALSE)$out
print(outliers)
NO2_2019[which(NO2_2019$Value %in% outliers),]
NO2_2019<-NO2_2019[-which(NO2_2019$Value %in% outliers),]
summary(NO2_2019$Value)

##########################################################################################
##########################################################################################

setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station/GatesheadBS_Mar20")###this is the location of your files
Air_Quality_Mar20 <- read.csv("Gateshead_Mar20_Clean.csv", header= TRUE)
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

NO2_Mar20_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(NO2_Mar20_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station/GatesheadBS_Apr20")###this is the location of your files
Air_Quality <- read.csv("Gateshead_Apr20_Clean.csv", header= TRUE)
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

NO2_Apr20_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(NO2_Apr20_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station/GatesheadBS_May20")###this is the location of your files
Air_Quality <- read.csv("Gateshead_May20_Clean.csv", header= TRUE)
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

NO2_May20_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(NO2_May20_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station/GatesheadBS_Jan20")###this is the location of your files
Air_Quality <- read.csv("Gateshead_Jan20_Clean.csv", header= TRUE)
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

NO2_Jan20_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(NO2_Jan20_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station/GatesheadBS_Feb20")###this is the location of your files
Air_Quality <- read.csv("Gateshead_Feb20_Clean.csv", header= TRUE)
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

NO2_Feb20_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(NO2_Feb20_df)


################################################################################################


NO2_2020<-rbind(NO2_Jan20_df,NO2_Feb20_df)
NO2_2020<-rbind(NO2_2020,NO2_Mar20_df)
NO2_2020<-rbind(NO2_2020,NO2_Apr20_df)
NO2_2020<-rbind(NO2_2020,NO2_May20_df)

summary(NO2_2020$Value)

boxplot(NO2_2020$Value)
boxplot(NO2_2020$Value,plot=FALSE)$out
outliers <- boxplot(NO2_2020$Value,plot=FALSE)$out
print(outliers)
NO2_2020[which(NO2_2020$Value %in% outliers),]
NO2_2020<-NO2_2020[-which(NO2_2020$Value %in% outliers),]

summary(NO2_2020$Value)

Gateshead_NO <- rbind(NO2_2019,NO2_2020)


NOG <- Gateshead_NO %>%
  group_by(date(Date_Time)) %>%
  summarize(Avg_NO2 = mean(Value, na.rm = TRUE))%>%
  mutate(location="Gateshead")


str(NOG)
setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station")
write_csv(NOG,'NO2_Gateshead.csv')
NOG<-read.csv('NO2_Gateshead.csv')

setwd("~/Dissertation/Dissertation Datasets/Plots")
png(file = "NO2 Histogram Gateshead 2019.png")
hist(NO2_2019$Value,main = NULL,xlab = "NO2 Concentration 2019",xlim = c(0,200),col = "lightblue",freq=FALSE)
dev.off() #Saving the file
png(file = "NO2 Histogram Gateshead 2020.png")
hist(NO2_2020$Value,main = NULL,xlab = "NO2 Concentration 2020",xlim = c(0,200),col = "lightblue",freq=FALSE)
dev.off() #Saving the file
png(file = "LN NO2 Histogram Gateshead 2019.png")
hist(log(NO2_2019$Value),main = NULL,xlab = "Log NO2 Concentration 2019",xlim = c(-80,20),col = "lightblue",freq=FALSE)
dev.off() #Saving the file
png(file = "LN NO2 Histogram Gateshead 2020.png")
hist(log(NO2_2020$Value),main = NULL,xlab = "Log NO2 Concentration 2020",xlim = c(-40,10),col = "lightblue",freq=FALSE)
dev.off() #Saving the file

#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station/GatesheadBS_Aug")###this is the location of your files
Air_Quality <- read.csv("Gateshead_Aug_Clean.csv", header= TRUE)
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

sound_Aug_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(sound_Aug_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station/GatesheadBS_Sep")###this is the location of your files
Air_Quality <- read.csv("Gateshead_Sep_Clean.csv", header= TRUE)
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

sound_Sep_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(sound_Sep_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station/GatesheadBS_Oct")###this is the location of your files
Air_Quality <- read.csv("Gateshead_Oct_Clean.csv", header= TRUE)
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

sound_Oct_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(sound_Oct_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station/GatesheadBS_Nov")###this is the location of your files
Air_Quality <- read.csv("Gateshead_Nov_Clean.csv", header= TRUE)
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

sound_Nov_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(sound_Nov_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station/GatesheadBS_Dec")###this is the location of your files
Air_Quality <- read.csv("Gateshead_Dec_Clean.csv", header= TRUE)
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

sound_Dec_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(sound_Dec_df)


################################################################################################


sound_2019<-rbind(sound_Aug_df,sound_Sep_df)
sound_2019<-rbind(sound_2019,sound_Oct_df)
sound_2019<-rbind(sound_2019,sound_Nov_df)
sound_2019<-rbind(sound_2019,sound_Dec_df)

summary(sound_2019$Value)

boxplot(sound_2019$Value)
boxplot(sound_2019$Value,plot=FALSE)$out
outliers <- boxplot(sound_2019$Value,plot=FALSE)$out
print(outliers)
sound_2019[which(sound_2019$Value %in% outliers),]
sound_2019<-sound_2019[-which(sound_2019$Value %in% outliers),]
summary(sound_2019$Value)

##########################################################################################
##########################################################################################

setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station/GatesheadBS_Mar20")###this is the location of your files
Air_Quality_Mar20 <- read.csv("Gateshead_Mar20_Clean.csv", header= TRUE)
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

sound_Mar20_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(sound_Mar20_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station/GatesheadBS_Apr20")###this is the location of your files
Air_Quality <- read.csv("Gateshead_Apr20_Clean.csv", header= TRUE)
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

sound_Apr20_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(sound_Apr20_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station/GatesheadBS_May20")###this is the location of your files
Air_Quality <- read.csv("Gateshead_May20_Clean.csv", header= TRUE)
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

sound_May20_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(sound_May20_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station/GatesheadBS_Jan20")###this is the location of your files
Air_Quality <- read.csv("Gateshead_Jan20_Clean.csv", header= TRUE)
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

sound_Jan20_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(sound_Jan20_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station/GatesheadBS_Feb20")###this is the location of your files
Air_Quality <- read.csv("Gateshead_Feb20_Clean.csv", header= TRUE)
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

sound_Feb20_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(sound_Feb20_df)


################################################################################################


sound_2020<-rbind(sound_Jan20_df,sound_Feb20_df)
sound_2020<-rbind(sound_2020,sound_Mar20_df)
sound_2020<-rbind(sound_2020,sound_Apr20_df)
sound_2020<-rbind(sound_2020,sound_May20_df)

summary(sound_2020$Value)

boxplot(sound_2020$Value)
boxplot(sound_2020$Value,plot=FALSE)$out
outliers <- boxplot(sound_2020$Value,plot=FALSE)$out
print(outliers)
sound_2020[which(sound_2020$Value %in% outliers),]
sound_2020<-sound_2020[-which(sound_2020$Value %in% outliers),]

summary(sound_2020$Value)


Haym_NO <- rbind(sound_2019,sound_2020)


NOH <- Haym_NO %>%
  group_by(date(Date_Time)) %>%
  summarize(Avg_O3 = mean(Value, na.rm = TRUE))%>%
  mutate(location="Gateshead")


str(NOH)
setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station")
write_csv(NOH,'Sound_Gateshead.csv')
SG<-read.csv('Sound_Gateshead.csv')

setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station")
SG<-read.csv('Sound_Gateshead.csv')
setwd("~/Dissertation/Dissertation Datasets/RVI")
SR<-read.csv('Sound_RVI.csv')


NO<-rbind(SG,SR)

str(NO)
NO$Date<-as.Date(NO$date.Date_Time.)

setwd("~/Dissertation/Dissertation Datasets/Plots")
p <- ggplot(NO,aes(Date,Avg_O3,)) + geom_path(aes(colour=location)) + scale_x_date() + facet_wrap(vars(location)) + labs(y= "Sound Level (Db)", x = "Date")  + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location')  
p
ggsave("Sound Path Facet.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(NO,aes(Date,Avg_O3)) + geom_path(aes(colour=location)) + scale_x_date()  + labs(y= "Sound Level (Db)", x = "Date")  + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location')
p
ggsave("Sound Path.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(NO,aes(Date,Avg_O3)) + geom_smooth(aes(colour=location),span=0.4) + scale_x_date() + facet_wrap(vars(location))  + labs(y= "Sound Level (Db)", x = "Date")  + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location')
p
ggsave("Sound Smooth Facet.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(NO,aes(Date,Avg_O3)) + geom_smooth(aes(colour=location),span=0.4) + scale_x_date(breaks = pretty_breaks(17))  + labs(y= "Sound Level (Db)", x = "Date")  + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location')+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=15)) + theme(axis.text.y = element_text(size=15))
p
ggsave("Sound Smooth.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(NO,aes(Date,Avg_O3)) + geom_point(aes(colour=location)) + scale_x_date()  +labs(y= "Sound Level (Db)", x = "Date")  + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location')
p
ggsave("Sound Point.png", p, width = 24, height = 14,dpi = 300 )#save to file

p <- ggplot(NO,aes(Date,Avg_O3)) + geom_point(aes(colour=location)) + scale_x_date() + facet_wrap(vars(location))  + labs(y= "Sound Level (Db)", x = "Date")  + theme(legend.text=element_text(size=15),legend.title=element_text(size=20))+ labs(color='Location')
p
ggsave("Sound Point Facet.png", p, width = 24, height = 14,dpi = 300 )#save to file

setwd("~/Dissertation/Dissertation Datasets/Plots")
png(file = "Sound Histogram Gateshead 2019.png")
hist(sound_2019$Value,main = NULL,xlab = "Sound Level 2019",xlim = c(50,100),col = "lightblue",freq=FALSE)
dev.off() #Saving the file
png(file = "Sound Histogram Gateshead 2020.png")
hist(sound_2020$Value,main = NULL,xlab = "Sound Level 2020",xlim = c(50,100),col = "lightblue",freq=FALSE)
dev.off() #Saving the file
png(file = "LN Sound Histogram Gateshead 2019.png")
hist(log(sound_2019$Value),main = NULL,xlab = "Log Sound Level 2019",xlim = c(3.4,4.6),col = "lightblue",freq=FALSE)
dev.off() #Saving the file
png(file = "LN Sound Histogram Gateshead 2020.png")
hist(log(sound_2020$Value),main = NULL,xlab = "Log Sound Level 2020",xlim = c(3.4,4.6),col = "lightblue",freq=FALSE)
dev.off() #Saving the file

#########################################################################################
#########################################################################################