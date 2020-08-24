setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Mar")###this is the location of your files
Air_Quality_Mar <- read.csv("RVI_Mar_Clean.csv", header= TRUE)
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

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Apr")###this is the location of your files
Air_Quality <- read.csv("RVI_Apr_Clean.csv", header= TRUE)
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

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_May")###this is the location of your files
Air_Quality <- read.csv("RVI_May_Clean.csv", header= TRUE)
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

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Jun")###this is the location of your files
Air_Quality <- read.csv("RVI_Jun_Clean.csv", header= TRUE)
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

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Jul")###this is the location of your files
Air_Quality <- read.csv("RVI_Jul_Clean.csv", header= TRUE)
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

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Aug")###this is the location of your files
Air_Quality <- read.csv("RVI_Aug_Clean.csv", header= TRUE)
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

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Sep")###this is the location of your files
Air_Quality <- read.csv("RVI_Sep_Clean.csv", header= TRUE)
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

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Oct")###this is the location of your files
Air_Quality <- read.csv("RVI_Oct_Clean.csv", header= TRUE)
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

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Nov")###this is the location of your files
Air_Quality <- read.csv("RVI_Nov_Clean.csv", header= TRUE)
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

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Dec")###this is the location of your files
Air_Quality <- read.csv("RVI_Dec_Clean.csv", header= TRUE)
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

##########################################################################################
##########################################################################################

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Mar20")###this is the location of your files
Air_Quality_Mar20 <- read.csv("RVI_Mar20_Clean.csv", header= TRUE)
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

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Apr20")###this is the location of your files
Air_Quality <- read.csv("RVI_Apr20_Clean.csv", header= TRUE)
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

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_May20")###this is the location of your files
Air_Quality <- read.csv("RVI_May20_Clean.csv", header= TRUE)
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

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Jan20")###this is the location of your files
Air_Quality <- read.csv("RVI_Jan20_Clean.csv", header= TRUE)
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

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Feb20")###this is the location of your files
Air_Quality <- read.csv("RVI_Feb20_Clean.csv", header= TRUE)
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
CO_2020<-rbind(CO_2020,CO_Apr_df)
CO_2020<-rbind(CO_2020,CO_May_df)

summary(CO_2020$Value)

boxplot(CO_2020$Value)
boxplot(CO_2020$Value,plot=FALSE)$out
outliers <- boxplot(CO_2020$Value,plot=FALSE)$out
print(outliers)
CO_2020[which(CO_2020$Value %in% outliers),]
CO_2020<-CO_2020[-which(CO_2020$Value %in% outliers),]

summary(CO_2020$Value)

RVI_CO <- rbind(CO_2019,CO_2020)



COR <- RVI_CO %>%
  group_by(date(Date_Time)) %>%
  summarize(Avg_CO = mean(Value, na.rm = TRUE))%>%
  mutate(location="RVI")

str(COR)
setwd("~/Dissertation/Dissertation Datasets/RVI")
write_csv(COR,'CO_RVI.csv')
COR<-read.csv('CO_RVI.csv')

setwd("~/Dissertation/Dissertation Datasets/Plots")
png(file = "CO Histogram RVI 2019.png")
hist(CO_2019$Value,main = NULL,xlab = "CO Level 2019",xlim = c(0,700),col = "lightblue",freq=FALSE)
dev.off() #Saving the file
png(file = "CO Histogram RVI 2020.png")
hist(CO_2020$Value,main = NULL,xlab = "CO Level 2020",xlim = c(0,700),col = "lightblue",freq=FALSE)
dev.off() #Saving the file

#########################################################################################
#########################################################################################
setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Mar")###this is the location of your files
Air_Quality_Mar <- read.csv("RVI_Mar_Clean.csv", header= TRUE)
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

NO_Mar_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(NO_Mar_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Apr")###this is the location of your files
Air_Quality <- read.csv("RVI_Apr_Clean.csv", header= TRUE)
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

NO_Apr_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(NO_Apr_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_May")###this is the location of your files
Air_Quality <- read.csv("RVI_May_Clean.csv", header= TRUE)
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

NO_May_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(NO_May_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Jun")###this is the location of your files
Air_Quality <- read.csv("RVI_Jun_Clean.csv", header= TRUE)
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

NO_Jun_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(NO_Jun_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Jul")###this is the location of your files
Air_Quality <- read.csv("RVI_Jul_Clean.csv", header= TRUE)
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

NO_Jul_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(NO_Jul_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Aug")###this is the location of your files
Air_Quality <- read.csv("RVI_Aug_Clean.csv", header= TRUE)
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

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Sep")###this is the location of your files
Air_Quality <- read.csv("RVI_Sep_Clean.csv", header= TRUE)
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

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Oct")###this is the location of your files
Air_Quality <- read.csv("RVI_Oct_Clean.csv", header= TRUE)
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

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Nov")###this is the location of your files
Air_Quality <- read.csv("RVI_Nov_Clean.csv", header= TRUE)
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

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Dec")###this is the location of your files
Air_Quality <- read.csv("RVI_Dec_Clean.csv", header= TRUE)
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



NO_2019<-rbind(NO_Mar_df,NO_Apr_df)
NO_2019<-rbind(NO_2019,NO_May_df)
NO_2019<-rbind(NO_2019,NO_Jun_df)
NO_2019<-rbind(NO_2019,NO_Jul_df)
NO_2019<-rbind(NO_2019,NO_Aug_df)
NO_2019<-rbind(NO_2019,NO_Sep_df)
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

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Mar20")###this is the location of your files
Air_Quality_Mar <- read.csv("RVI_Mar20_Clean.csv", header= TRUE)
library(tidyr)

summary(Air_Quality_Mar20)
Air_Quality_Mar$Date_Time <- as.POSIXlt.character(Air_Quality_Mar$Timestamp,tz="GMT",format="%d/%m/%Y %H:%M",optional=FALSE)
Air_Quality_Mar<-Air_Quality_Mar[-c(1,2)]

mydf_list <- split( Air_Quality_Mar , f = Air_Quality_Mar$Variable )##save each parameter as a separate df with its name in mydf_list


library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

NO_Mar20_df <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(NO_Mar20_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Apr20")###this is the location of your files
Air_Quality <- read.csv("RVI_Apr20_Clean.csv", header= TRUE)
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

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_May20")###this is the location of your files
Air_Quality <- read.csv("RVI_May20_Clean.csv", header= TRUE)
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

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Jan20")###this is the location of your files
Air_Quality <- read.csv("RVI_Jan20_Clean.csv", header= TRUE)
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

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Feb20")###this is the location of your files
Air_Quality <- read.csv("RVI_Feb20_Clean.csv", header= TRUE)
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

RVI_NO <- rbind(NO_2019,NO_2020)


NOR <- RVI_NO %>%
  group_by(date(Date_Time)) %>%
  summarize(Avg_NO = mean(Value, na.rm = TRUE))%>%
  mutate(location="RVI")


str(NOR)
setwd("~/Dissertation/Dissertation Datasets/RVI")
write_csv(NOR,'NO_RVI.csv')
NOR<-read.csv('NO_RVI.csv')


setwd("~/Dissertation/Dissertation Datasets/Plots")
png(file = "NO Histogram RVI 2019.png")
hist(NO_2019$Value,main = NULL,xlab = "NO Concentration 2019",xlim = c(00,50),col = "lightblue",freq=FALSE)
dev.off() #Saving the file
png(file = "NO Histogram RVI 2020.png")
hist(NO_2020$Value,main = NULL,xlab = "NO Concentration 2020",xlim = c(0,100),col = "lightblue",freq=FALSE)
dev.off() #Saving the file



#########################################################################################
#########################################################################################

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Mar")###this is the location of your files
Air_Quality_Mar <- read.csv("RVI_Mar_Clean.csv", header= TRUE)
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

NO2_Mar_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(NO2_Mar_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Apr")###this is the location of your files
Air_Quality <- read.csv("RVI_Apr_Clean.csv", header= TRUE)
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

NO2_Apr_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(NO2_Apr_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_May")###this is the location of your files
Air_Quality <- read.csv("RVI_May_Clean.csv", header= TRUE)
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

NO2_May_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(NO2_May_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Jun")###this is the location of your files
Air_Quality <- read.csv("RVI_Jun_Clean.csv", header= TRUE)
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

NO2_Jun_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(NO2_Jun_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Jul")###this is the location of your files
Air_Quality <- read.csv("RVI_Jul_Clean.csv", header= TRUE)
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

NO2_Jul_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(NO2_Jul_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Aug")###this is the location of your files
Air_Quality <- read.csv("RVI_Aug_Clean.csv", header= TRUE)
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

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Sep")###this is the location of your files
Air_Quality <- read.csv("RVI_Sep_Clean.csv", header= TRUE)
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

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Oct")###this is the location of your files
Air_Quality <- read.csv("RVI_Oct_Clean.csv", header= TRUE)
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

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Nov")###this is the location of your files
Air_Quality <- read.csv("RVI_Nov_Clean.csv", header= TRUE)
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

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Dec")###this is the location of your files
Air_Quality <- read.csv("RVI_Dec_Clean.csv", header= TRUE)
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



NO2_2019<-rbind(NO2_Mar_df,NO2_Apr_df)
NO2_2019<-rbind(NO2_2019,NO2_May_df)
NO2_2019<-rbind(NO2_2019,NO2_Jun_df)
NO2_2019<-rbind(NO2_2019,NO2_Jul_df)
NO2_2019<-rbind(NO2_2019,NO2_Aug_df)
NO2_2019<-rbind(NO2_2019,NO2_Sep_df)
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

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Mar20")###this is the location of your files
Air_Quality_Mar <- read.csv("RVI_Mar20_Clean.csv", header= TRUE)
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

NO2_Mar20_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(NO2_Mar20_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Apr20")###this is the location of your files
Air_Quality <- read.csv("RVI_Apr20_Clean.csv", header= TRUE)
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

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_May20")###this is the location of your files
Air_Quality <- read.csv("RVI_May20_Clean.csv", header= TRUE)
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

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Jan20")###this is the location of your files
Air_Quality <- read.csv("RVI_Jan20_Clean.csv", header= TRUE)
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

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Feb20")###this is the location of your files
Air_Quality <- read.csv("RVI_Feb20_Clean.csv", header= TRUE)
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
NO2_2020<-rbind(NO2_2020,NO2_Apr_df)
NO2_2020<-rbind(NO2_2020,NO2_May_df)

summary(NO2_2020$Value)

boxplot(NO2_2020$Value)
boxplot(NO2_2020$Value,plot=FALSE)$out
outliers <- boxplot(NO2_2020$Value,plot=FALSE)$out
print(outliers)
NO2_2020[which(NO2_2020$Value %in% outliers),]
NO2_2020<-NO2_2020[-which(NO2_2020$Value %in% outliers),]

summary(NO2_2020$Value)

RVI_NO <- rbind(NO2_2019,NO2_2020)


NOR <- RVI_NO %>%
  group_by(date(Date_Time)) %>%
  summarize(Avg_NO2 = mean(Value, na.rm = TRUE))%>%
  mutate(location="RVI")


str(NOR)
setwd("~/Dissertation/Dissertation Datasets/RVI")
write_csv(NOR,'NO2_RVI.csv')
NOR<-read.csv('NO2_RVI.csv')


setwd("~/Dissertation/Dissertation Datasets/Plots")
png(file = "NO2 Histogram RVI 2019.png")
hist(NO2_2019$Value,main = NULL,xlab = "NO2 Concentration 2019",xlim = c(0,100),col = "lightblue",freq=FALSE)
dev.off() #Saving the file
png(file = "NO2 Histogram RVI 2020.png")
hist(NO2_2020$Value,main = NULL,xlab = "NO2 Concentration 2020",xlim = c(0,100),col = "lightblue",freq=FALSE)
dev.off() #Saving the file






#########################################################################################
#########################################################################################

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Mar")###this is the location of your files
Air_Quality_Mar <- read.csv("RVI_Mar_Clean.csv", header= TRUE)
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

Sound_Mar_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(Sound_Mar_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Apr")###this is the location of your files
Air_Quality <- read.csv("RVI_Apr_Clean.csv", header= TRUE)
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

Sound_Apr_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(Sound_Apr_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_May")###this is the location of your files
Air_Quality <- read.csv("RVI_May_Clean.csv", header= TRUE)
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

Sound_May_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(Sound_May_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Jun")###this is the location of your files
Air_Quality <- read.csv("RVI_Jun_Clean.csv", header= TRUE)
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

Sound_Jun_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(Sound_Jun_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Jul")###this is the location of your files
Air_Quality <- read.csv("RVI_Jul_Clean.csv", header= TRUE)
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

Sound_Jul_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(Sound_Jul_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Aug")###this is the location of your files
Air_Quality <- read.csv("RVI_Aug_Clean.csv", header= TRUE)
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

Sound_Aug_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(Sound_Aug_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Sep")###this is the location of your files
Air_Quality <- read.csv("RVI_Sep_Clean.csv", header= TRUE)
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

Sound_Sep_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(Sound_Sep_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Oct")###this is the location of your files
Air_Quality <- read.csv("RVI_Oct_Clean.csv", header= TRUE)
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

Sound_Oct_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(Sound_Oct_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Nov")###this is the location of your files
Air_Quality <- read.csv("RVI_Nov_Clean.csv", header= TRUE)
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

Sound_Nov_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(Sound_Nov_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Dec")###this is the location of your files
Air_Quality <- read.csv("RVI_Dec_Clean.csv", header= TRUE)
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

Sound_Dec_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(Sound_Dec_df)



Sound_2019<-rbind(Sound_Mar_df,Sound_Apr_df)
Sound_2019<-rbind(Sound_2019,Sound_May_df)
Sound_2019<-rbind(Sound_2019,Sound_Jun_df)
Sound_2019<-rbind(Sound_2019,Sound_Jul_df)
Sound_2019<-rbind(Sound_2019,Sound_Aug_df)
Sound_2019<-rbind(Sound_2019,Sound_Sep_df)
Sound_2019<-rbind(Sound_2019,Sound_Oct_df)
Sound_2019<-rbind(Sound_2019,Sound_Nov_df)
Sound_2019<-rbind(Sound_2019,Sound_Dec_df)

summary(Sound_2019$Value)

boxplot(Sound_2019$Value)
boxplot(Sound_2019$Value,plot=FALSE)$out
outliers <- boxplot(Sound_2019$Value,plot=FALSE)$out
print(outliers)
Sound_2019[which(Sound_2019$Value %in% outliers),]
Sound_2019<-Sound_2019[-which(Sound_2019$Value %in% outliers),]
summary(Sound_2019$Value)

##########################################################################################
##########################################################################################

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Mar20")###this is the location of your files
Air_Quality_Mar <- read.csv("RVI_Mar20_Clean.csv", header= TRUE)
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

Sound_Mar20_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(Sound_Mar20_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Apr20")###this is the location of your files
Air_Quality <- read.csv("RVI_Apr20_Clean.csv", header= TRUE)
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

Sound_Apr20_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(Sound_Apr20_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_May20")###this is the location of your files
Air_Quality <- read.csv("RVI_May20_Clean.csv", header= TRUE)
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

Sound_May20_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(Sound_May20_df)


################################################################################################

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Jan20")###this is the location of your files
Air_Quality <- read.csv("RVI_Jan20_Clean.csv", header= TRUE)
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

Sound_Jan20_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(Sound_Jan20_df)

################################################################################################

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Feb20")###this is the location of your files
Air_Quality <- read.csv("RVI_Feb20_Clean.csv", header= TRUE)
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

Sound_Feb20_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


str(Sound_Feb20_df)


################################################################################################


Sound_2020<-rbind(Sound_Jan20_df,Sound_Feb20_df)
Sound_2020<-rbind(Sound_2020,Sound_Mar20_df)
Sound_2020<-rbind(Sound_2020,Sound_Apr20_df)
Sound_2020<-rbind(Sound_2020,Sound_May20_df)

summary(Sound_2020$Value)

boxplot(Sound_2020$Value)
boxplot(Sound_2020$Value,plot=FALSE)$out
outliers <- boxplot(Sound_2020$Value,plot=FALSE)$out
print(outliers)
Sound_2020[which(Sound_2020$Value %in% outliers),]
Sound_2020<-Sound_2020[-which(Sound_2020$Value %in% outliers),]

summary(Sound_2020$Value)




Haym_NO <- rbind(Sound_2019,Sound_2020)


NOH <- Haym_NO %>%
  group_by(date(Date_Time)) %>%
  summarize(Avg_O3 = mean(Value, na.rm = TRUE))%>%
  mutate(location="RVI")


str(NOH)
setwd("~/Dissertation/Dissertation Datasets/RVI")
write_csv(NOH,'Sound_RVI.csv')
SR<-read.csv('Sound_RVI.csv')


setwd("~/Dissertation/Dissertation Datasets/Plots")
png(file = "Sound Histogram RVI 2019.png")
hist(Sound_2019$Value,main = NULL,xlab = "Sound Concentration 2019",xlim = c(50,100),col = "lightblue",freq=FALSE)
dev.off() #Saving the file
png(file = "Sound Histogram RVI 2020.png")
hist(Sound_2020$Value,main = NULL,xlab = "Sound Concentration 2020",xlim = c(50,100),col = "lightblue",freq=FALSE)
dev.off() #Saving the file
png(file = "LN Sound Histogram RVI 2019.png")
hist(log(Sound_2019$Value),main = NULL,xlab = "Log Sound Level 2019",xlim = c(3.8,4.6),col = "lightblue",freq=FALSE)
dev.off() #Saving the file
png(file = "LN Sound Histogram RVI 2020.png")
hist(log(Sound_2020$Value),main = NULL,xlab = "Log Sound Level 2020",xlim = c(3.8,4.6),col = "lightblue",freq=FALSE)
dev.off() #Saving the file
