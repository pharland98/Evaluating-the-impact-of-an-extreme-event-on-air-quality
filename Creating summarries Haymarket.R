setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymaket Jan-Mar19")###this is the location of files
df <- read.csv("Haymarket Jan Clean.csv", header= TRUE)
str(df)
df<-df[-c(1523969:2361528),]

df2 <- df[, -c(17, 18)]
str(df2)

library(dplyr)
library(purrr)
library(ggplot2)

list_clnms = c(colnames(df2))

for (i in 1:length(list_clnms)){
  df2[, list_clnms[i]] <- ifelse(df2[, list_clnms[i]]>=0, df2[, list_clnms[i]], NA)
  
}

str(df2)

library(plyr)

numcolwise(sum)(df2, na.rm=TRUE)###sum of numeric columns in df2


#####reshape excel file
library(reshape2)
melted <- melt(df2, id.vars = c("Date_Time")  )#####melt excel file to a new df

library(tidyr)
melted <- melted %>% drop_na()#remove rows with NA in melted df
#write.csv(melted, "melted.csv")

library(lubridate)
melted$Date_Time <-ymd_hms(melted$Date_Time)
str(melted)


mydf <- melted
str(mydf)
mydf$Date_Time <-as_datetime(mydf$Date_Time)
str(mydf)



mydf_list <- split( mydf , f = mydf$variable )##save each parameter as a separate df with its name in mydf_list

length(mydf_list)###check of number of parameters as df in the list 

Co_df_Jan <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble
NO_df_Jan <- mydf_list[2]%>%
  map_df(as_tibble)##to convert any list to data tibble
NO2_df_Jan <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble
Humidity_df_Jan <- mydf_list[6]%>%
  map_df(as_tibble)##to convert any list to data tibble
O3_df_Jan <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble  
Pressure_df_Jan <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble
Temperature_df_Jan <- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble
Veh_df_Jan <- mydf_list[10]%>%
  map_df(as_tibble)##to convert any list to data tibble
Part_df_Jan <- mydf_list[11]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM4_df_Jan <- mydf_list[12]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM1_df_Jan <- mydf_list[13]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM10_df_Jan <- mydf_list[14]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM2.5_df_Jan <- mydf_list[15]%>%
  map_df(as_tibble)##to convert any list to data tibble



#######################################################################################
setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Feb")###this is the location of files
df <- read.csv("Haymarket Feb Clean.csv", header= TRUE)
str(df)

df2 <- df[, -c(16, 15)]
str(df2)



list_clnms = c(colnames(df2))

for (i in 1:length(list_clnms)){
  df2[, list_clnms[i]] <- ifelse(df2[, list_clnms[i]]>=0, df2[, list_clnms[i]], NA)
  
}

str(df2)

library(plyr)

numcolwise(sum)(df2, na.rm=TRUE)###sum of numeric columns in df2



#####reshape excel file
library(reshape2)
melted <- melt(df2, id.vars = c("Date_Time")  )#####melt excel file to a new df

library(tidyr)
melted <- melted %>% drop_na()#remove rows with NA in melted df
#write.csv(melted, "melted.csv")

library(lubridate)
melted$Date_Time <-ymd_hms(melted$Date_Time)
str(melted)


mydf <- melted
str(mydf)
mydf$Date_Time <-as_datetime(mydf$Date_Time)
str(mydf)



mydf_list <- split( mydf , f = mydf$variable )##save each parameter as a separate df with its name in mydf_list

length(mydf_list)###check of number of parameters as df in the list 
Co_df_Feb <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble
NO_df_Feb <- mydf_list[2]%>%
  map_df(as_tibble)##to convert any list to data tibble
NO2_df_Feb <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble
Humidity_df_Feb <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble
O3_df_Feb <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble  
Pressure_df_Feb <- mydf_list[6]%>%
  map_df(as_tibble)##to convert any list to data tibble
Temperature_df_Feb <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble
Veh_df_Feb <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble
Part_df_Feb <- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM4_df_Feb <- mydf_list[10]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM1_df_Feb <- mydf_list[11]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM10_df_Feb <- mydf_list[12]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM2.5_df_Feb <- mydf_list[13]%>%
  map_df(as_tibble)##to convert any list to data tibble


#######################################################################################
setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Mar")###this is the location of files
df <- read.csv("Haymarket Mar Clean.csv", header= TRUE)
str(df)

df2 <- df[, -c(16, 15)]
str(df2)



list_clnms = c(colnames(df2))

for (i in 1:length(list_clnms)){
  df2[, list_clnms[i]] <- ifelse(df2[, list_clnms[i]]>=0, df2[, list_clnms[i]], NA)
  
}

str(df2)

library(plyr)

numcolwise(sum)(df2, na.rm=TRUE)###sum of numeric columns in df2



#####reshape excel file
library(reshape2)
melted <- melt(df2, id.vars = c("Date_Time")  )#####melt xcel file to a new df

library(tidyr)
melted <- melted %>% drop_na()#remove rows with NA in melted df
#write.csv(melted, "melted.csv")

library(lubridate)
melted$Date_Time <-ymd_hms(melted$Date_Time)
str(melted)


mydf <- melted
str(mydf)
mydf$Date_Time <-as_datetime(mydf$Date_Time)
str(mydf)



mydf_list <- split( mydf , f = mydf$variable )##save each parameter as a separate df with its name in mydf_list

length(mydf_list)###check of number of parameters as df in the list 
Co_df_Mar <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble
NO_df_Mar <- mydf_list[2]%>%
  map_df(as_tibble)##to convert any list to data tibble
NO2_df_Mar <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble
Humidity_df_Mar <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble
O3_df_Mar <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble  
Pressure_df_Mar <- mydf_list[6]%>%
  map_df(as_tibble)##to convert any list to data tibble
Temperature_df_Mar <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble
Veh_df_Mar <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble
Part_df_Mar <- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM4_df_Mar <- mydf_list[10]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM1_df_Mar <- mydf_list[11]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM10_df_Mar <- mydf_list[12]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM2.5_df_Mar <- mydf_list[13]%>%
  map_df(as_tibble)##to convert any list to data tibble


#######################################################################################
setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Apr")###this is the location of files
df <- read.csv("Haymarket Apr Clean.csv", header= TRUE)
str(df)

df2 <- df[, -c(16, 15)]
str(df2)



### if we want replace bad values (here minus values) with NA (or any value)
list_clnms = c(colnames(df2))

for (i in 1:length(list_clnms)){
  df2[, list_clnms[i]] <- ifelse(df2[, list_clnms[i]]>=0, df2[, list_clnms[i]], NA)
  
}

str(df2)

library(plyr)

numcolwise(sum)(df2, na.rm=TRUE)###sum of numeric columns in df2



#####reshape excel file
library(reshape2)
melted <- melt(df2, id.vars = c("Date_Time")  )

library(tidyr)
melted <- melted %>% drop_na()
#write.csv(melted, "melted.csv")

library(lubridate)
melted$Date_Time <-ymd_hms(melted$Date_Time)
str(melted)


mydf <- melted
str(mydf)
mydf$Date_Time <-as_datetime(mydf$Date_Time)
str(mydf)



mydf_list <- split( mydf , f = mydf$variable )##save each parameter as a separate df with its name in mydf_list

length(mydf_list)###check of number of parameters as df in the list 
Co_df_Apr <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble
NO_df_Apr <- mydf_list[2]%>%
  map_df(as_tibble)##to convert any list to data tibble
NO2_df_Apr <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble
Humidity_df_Apr <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble
O3_df_Apr <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble  
Pressure_df_Apr <- mydf_list[6]%>%
  map_df(as_tibble)##to convert any list to data tibble
Temperature_df_Apr <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble
Veh_df_Apr <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble
Part_df_Apr <- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM4_df_Apr <- mydf_list[10]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM1_df_Apr <- mydf_list[11]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM10_df_Apr <- mydf_list[12]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM2.5_df_Apr <- mydf_list[13]%>%
  map_df(as_tibble)##to convert any list to data tibble


#######################################################################################
setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_May")###this is the location of files
df <- read.csv("Haymarket May Clean.csv", header= TRUE)
str(df)

df2 <- df[, -c(16, 15)]
str(df2)



list_clnms = c(colnames(df2))

for (i in 1:length(list_clnms)){
  df2[, list_clnms[i]] <- ifelse(df2[, list_clnms[i]]>=0, df2[, list_clnms[i]], NA)
  
}

str(df2)

library(plyr)

numcolwise(sum)(df2, na.rm=TRUE)###sum of numeric columns in df2



#####reshape excel file
library(reshape2)
melted <- melt(df2, id.vars = c("Date_Time")  )#####melt excel file to a new df

library(tidyr)
melted <- melted %>% drop_na()#remove rows with NA in melted df
#write.csv(melted, "melted.csv")

library(lubridate)
melted$Date_Time <-ymd_hms(melted$Date_Time)
str(melted)


mydf <- melted
str(mydf)
mydf$Date_Time <-as_datetime(mydf$Date_Time)
str(mydf)



mydf_list <- split( mydf , f = mydf$variable )##save each parameter as a separate df with its name in mydf_list

length(mydf_list)###check of number of parameters as df in the list 
Co_df_May <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble
NO_df_May <- mydf_list[2]%>%
  map_df(as_tibble)##to convert any list to data tibble
NO2_df_May <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble
Humidity_df_May <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble
O3_df_May <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble  
Pressure_df_May <- mydf_list[6]%>%
  map_df(as_tibble)##to convert any list to data tibble
Temperature_df_May <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble
Veh_df_May <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble
Part_df_May <- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM4_df_May <- mydf_list[10]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM1_df_May <- mydf_list[11]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM10_df_May <- mydf_list[12]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM2.5_df_May <- mydf_list[13]%>%
  map_df(as_tibble)##to convert any list to data tibble


#######################################################################################
setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Jun")
df <- read.csv("Haymarket Jun Clean.csv", header= TRUE)
str(df)

#create new df which is df2 without columns 17 1nd 18
df2 <- df[, -c(16, 15)]
str(df2)



list_clnms = c(colnames(df2))

for (i in 1:length(list_clnms)){
  df2[, list_clnms[i]] <- ifelse(df2[, list_clnms[i]]>=0, df2[, list_clnms[i]], NA)
  
}

str(df2)

library(plyr)

numcolwise(sum)(df2, na.rm=TRUE)###sum of numeric columns in df2



#####reshape excel file
library(reshape2)
melted <- melt(df2, id.vars = c("Date_Time")  )#####melt excel file to a new df

library(tidyr)
melted <- melted %>% drop_na()#remove rows with NA in melted df
#write.csv(melted, "melted.csv")

library(lubridate)
melted$Date_Time <-ymd_hms(melted$Date_Time)
str(melted)


mydf <- melted
str(mydf)
mydf$Date_Time <-as_datetime(mydf$Date_Time)
str(mydf)



mydf_list <- split( mydf , f = mydf$variable )##save each parameter as a separate df with its name in mydf_list

length(mydf_list)###check of number of parameters as df in the list 
Co_df_Jun <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble
NO_df_Jun <- mydf_list[2]%>%
  map_df(as_tibble)##to convert any list to data tibble
NO2_df_Jun <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble
Humidity_df_Jun <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble
O3_df_Jun <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble  
Pressure_df_Jun <- mydf_list[6]%>%
  map_df(as_tibble)##to convert any list to data tibble
Temperature_df_Jun <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble
Veh_df_Jun <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble
Part_df_Jun <- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM4_df_Jun <- mydf_list[10]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM1_df_Jun <- mydf_list[11]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM10_df_Jun <- mydf_list[12]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM2.5_df_Jun <- mydf_list[13]%>%
  map_df(as_tibble)##to convert any list to data tibble


#######################################################################################
setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Jul")###this is the location of  files
df <- read.csv("Haymarket Jul Clean.csv", header= TRUE)
str(df)

#create new df which is df2 without columns 17 1nd 18
df2 <- df[, -c(16, 15)]
str(df2)



### replace bad values (here minus values) with NA (or any value)
list_clnms = c(colnames(df2))

for (i in 1:length(list_clnms)){
  df2[, list_clnms[i]] <- ifelse(df2[, list_clnms[i]]>=0, df2[, list_clnms[i]], NA)
  
}

str(df2)

library(plyr)

numcolwise(sum)(df2, na.rm=TRUE)###sum of numeric columns in df2



#####reshape excel file
library(reshape2)
melted <- melt(df2, id.vars = c("Date_Time")  )#####melt excel file to a new df

library(tidyr)
melted <- melted %>% drop_na()#remove rows with NA in melted df
#write.csv(melted, "melted.csv")

library(lubridate)
melted$Date_Time <-ymd_hms(melted$Date_Time)
str(melted)


mydf <- melted
str(mydf)
mydf$Date_Time <-as_datetime(mydf$Date_Time)
str(mydf)



mydf_list <- split( mydf , f = mydf$variable )##save each parameter as a separate df with its name in mydf_list

length(mydf_list)###check of number of parameters as df in the list 
Co_df_Jul <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble
NO_df_Jul <- mydf_list[2]%>%
  map_df(as_tibble)##to convert any list to data tibble
NO2_df_Jul <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble
Humidity_df_Jul <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble
O3_df_Jul <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble  
Pressure_df_Jul <- mydf_list[6]%>%
  map_df(as_tibble)##to convert any list to data tibble
Temperature_df_Jul <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble
Veh_df_Jul <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble
Part_df_Jul <- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM4_df_Jul <- mydf_list[10]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM1_df_Jul <- mydf_list[11]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM10_df_Jul <- mydf_list[12]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM2.5_df_Jul <- mydf_list[13]%>%
  map_df(as_tibble)##to convert any list to data tibble



#######################################################################################
setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Aug")###this is the location of files
df <- read.csv("Haymarket Aug Clean.csv", header= TRUE)
str(df)

df2 <- df[, -c(16, 15)]###lets get rid of these columns for now
str(df2)



list_clnms = c(colnames(df2))

for (i in 1:length(list_clnms)){
  df2[, list_clnms[i]] <- ifelse(df2[, list_clnms[i]]>=0, df2[, list_clnms[i]], NA)
  
}

str(df2)

library(plyr)

numcolwise(sum)(df2, na.rm=TRUE)###sum of numeric columns in df2



#####reshape excel file
library(reshape2)
melted <- melt(df2, id.vars = c("Date_Time")  )#####melt excel file to a new df

library(tidyr)
melted <- melted %>% drop_na()#remove rows with NA in melted df
#write.csv(melted, "melted.csv")

library(lubridate)
melted$Date_Time <-ymd_hms(melted$Date_Time)
str(melted)


mydf <- melted
str(mydf)
mydf$Date_Time <-as_datetime(mydf$Date_Time)
str(mydf)



mydf_list <- split( mydf , f = mydf$variable )##save each parameter as a separate df with its name in mydf_list

length(mydf_list)###check of number of parameters as df in the list 
Co_df_Aug <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble
NO_df_Aug <- mydf_list[2]%>%
  map_df(as_tibble)##to convert any list to data tibble
NO2_df_Aug <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble
Humidity_df_Aug <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble
O3_df_Aug <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble  
Pressure_df_Aug <- mydf_list[6]%>%
  map_df(as_tibble)##to convert any list to data tibble
Temperature_df_Aug <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble
Veh_df_Aug <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble
Part_df_Aug <- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM4_df_Aug <- mydf_list[10]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM1_df_Aug <- mydf_list[11]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM10_df_Aug <- mydf_list[12]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM2.5_df_Aug <- mydf_list[13]%>%
  map_df(as_tibble)##to convert any list to data tibble



#######################################################################################
setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Sep")###this is the location of files
df <- read.csv("Haymarket Sep Clean.csv", header= TRUE)
str(df)

#create new df which is df2 without columns 17 1nd 18
df2 <- df[, -c(16, 15)]
str(df2)



list_clnms = c(colnames(df2))

for (i in 1:length(list_clnms)){
  df2[, list_clnms[i]] <- ifelse(df2[, list_clnms[i]]>=0, df2[, list_clnms[i]], NA)
  
}

str(df2)

library(plyr)

numcolwise(sum)(df2, na.rm=TRUE)###sum of numeric columns in df2



#####reshape ur excel file
library(reshape2)
melted <- melt(df2, id.vars = c("Date_Time")  )#####melt excel file to a new df

library(tidyr)
melted <- melted %>% drop_na()#remove rows with NA in melted df
#write.csv(melted, "melted.csv")

library(lubridate)
melted$Date_Time <-ymd_hms(melted$Date_Time)
str(melted)


mydf <- melted
str(mydf)
mydf$Date_Time <-as_datetime(mydf$Date_Time)
str(mydf)



mydf_list <- split( mydf , f = mydf$variable )##save each parameter as a separate df with its name in mydf_list

length(mydf_list)###check of number of parameters as df in the list 
Co_df_Sep <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble
NO_df_Sep <- mydf_list[2]%>%
  map_df(as_tibble)##to convert any list to data tibble
NO2_df_Sep <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble
Humidity_df_Sep <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble
O3_df_Sep <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble  
Pressure_df_Sep <- mydf_list[6]%>%
  map_df(as_tibble)##to convert any list to data tibble
Temperature_df_Sep <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble
Veh_df_Sep <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble
Part_df_Sep <- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM4_df_Sep <- mydf_list[10]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM1_df_Sep <- mydf_list[11]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM10_df_Sep <- mydf_list[12]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM2.5_df_Sep <- mydf_list[13]%>%
  map_df(as_tibble)##to convert any list to data tibble



#######################################################################################
setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Oct")###this is the location of files
df <- read.csv("Haymarket Oct Clean.csv", header= TRUE)
str(df)

#create new df which is df2 without columns 17 1nd 18
df2 <- df[, -c(16, 15)]
str(df2)



list_clnms = c(colnames(df2))

for (i in 1:length(list_clnms)){
  df2[, list_clnms[i]] <- ifelse(df2[, list_clnms[i]]>=0, df2[, list_clnms[i]], NA)
  
}

str(df2)

library(plyr)

numcolwise(sum)(df2, na.rm=TRUE)###sum of numeric columns in df2



#####reshape ur excel file
library(reshape2)
melted <- melt(df2, id.vars = c("Date_Time")  )

library(tidyr)
melted <- melted %>% drop_na()
#write.csv(melted, "melted.csv")

library(lubridate)
melted$Date_Time <-ymd_hms(melted$Date_Time)
str(melted)


mydf <- melted
str(mydf)
mydf$Date_Time <-as_datetime(mydf$Date_Time)
str(mydf)



mydf_list <- split( mydf , f = mydf$variable )##save each parameter as a separate df with its name in mydf_list

length(mydf_list)###check of number of parameters as df in the list 
Co_df_Oct <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble
NO_df_Oct <- mydf_list[2]%>%
  map_df(as_tibble)##to convert any list to data tibble
NO2_df_Oct <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble
Humidity_df_Oct <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble
O3_df_Oct <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble  
Pressure_df_Oct <- mydf_list[6]%>%
  map_df(as_tibble)##to convert any list to data tibble
Temperature_df_Oct <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble
Veh_df_Oct <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble
Part_df_Oct <- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM4_df_Oct <- mydf_list[10]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM1_df_Oct <- mydf_list[11]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM10_df_Oct <- mydf_list[12]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM2.5_df_Oct <- mydf_list[13]%>%
  map_df(as_tibble)##to convert any list to data tibble



#######################################################################################
setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Nov")###this is the location of files
df <- read.csv("Haymarket Nov Clean.csv", header= TRUE)
str(df)

#create new df which is df2 without columns 17 1nd 18
df2 <- df[, -c(16, 15)]
str(df2)



list_clnms = c(colnames(df2))

for (i in 1:length(list_clnms)){
  df2[, list_clnms[i]] <- ifelse(df2[, list_clnms[i]]>=0, df2[, list_clnms[i]], NA)
  
}

str(df2)

library(plyr)

numcolwise(sum)(df2, na.rm=TRUE)###sum of numeric columns in df2



#####reshape excel file
library(reshape2)
melted <- melt(df2, id.vars = c("Date_Time")  )#####melt excel file to a new df

library(tidyr)
melted <- melted %>% drop_na()#remove rows with NA in melted df
#write.csv(melted, "melted.csv")

library(lubridate)
melted$Date_Time <-ymd_hms(melted$Date_Time)
str(melted)


mydf <- melted
str(mydf)
mydf$Date_Time <-as_datetime(mydf$Date_Time)
str(mydf)



mydf_list <- split( mydf , f = mydf$variable )##save each parameter as a separate df with its name in mydf_list

length(mydf_list)###check of number of parameters as df in the list 
Co_df_Nov <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble
NO_df_Nov <- mydf_list[2]%>%
  map_df(as_tibble)##to convert any list to data tibble
NO2_df_Nov <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble
Humidity_df_Nov <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble
O3_df_Nov <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble  
Pressure_df_Nov <- mydf_list[6]%>%
  map_df(as_tibble)##to convert any list to data tibble
Temperature_df_Nov <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble
Veh_df_Nov <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble
Part_df_Nov <- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM4_df_Nov <- mydf_list[10]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM1_df_Nov <- mydf_list[11]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM10_df_Nov <- mydf_list[12]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM2.5_df_Nov <- mydf_list[13]%>%
  map_df(as_tibble)##to convert any list to data tibble



#######################################################################################
setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Dec")###this is the location of your files
df <- read.csv("Haymarket Dec Clean.csv", header= TRUE)
str(df)

#create new df which is df2 without columns 17 1nd 18
df2 <- df[, -c(10, 15)]
str(df2)



### if we want replace bad values (here minus values) with NA (or any value)
list_clnms = c(colnames(df2))

for (i in 1:length(list_clnms)){
  df2[, list_clnms[i]] <- ifelse(df2[, list_clnms[i]]>=0, df2[, list_clnms[i]], NA)
  
}

str(df2)

library(plyr)

numcolwise(sum)(df2, na.rm=TRUE)###sum of numeric columns in df2



#####reshape excel file
library(reshape2)
melted <- melt(df2, id.vars = c("Date_Time")  )#####melt excel file to a new df

library(tidyr)
melted <- melted %>% drop_na()#remove rows with NA in melted df
#write.csv(melted, "melted.csv")

library(lubridate)
melted$Date_Time <-ymd_hms(melted$Date_Time)
str(melted)


mydf <- melted
str(mydf)
mydf$Date_Time <-as_datetime(mydf$Date_Time)
str(mydf)



mydf_list <- split( mydf , f = mydf$variable )##save each parameter as a separate df with its name in mydf_list

length(mydf_list)###check of number of parameters as df in the list 
Co_df_Dec <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble
NO_df_Dec <- mydf_list[2]%>%
  map_df(as_tibble)##to convert any list to data tibble
NO2_df_Dec<- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble
Humidity_df_Dec <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble
O3_df_Dec <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble  
Pressure_df_Dec <- mydf_list[6]%>%
  map_df(as_tibble)##to convert any list to data tibble
Temperature_df_Dec <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble
Veh_df_Dec <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM4_df_Dec<- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM1_df_Dec <- mydf_list[10]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM10_df_Dec <- mydf_list[11]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM2.5_df_Dec <- mydf_list[12]%>%
  map_df(as_tibble)##to convert any list to data tibble

###########################################################################################
setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Jan20")###this is the location of your files
df <- read.csv("Haymarket Jan20 Clean.csv", header= TRUE)
str(df)

#create new df which is df2 without columns 17 1nd 18
df2 <- df[, -c(14, 15)]
str(df2)



###replace bad values (here minus values) with NA (or any value)
list_clnms = c(colnames(df2))

for (i in 1:length(list_clnms)){
  df2[, list_clnms[i]] <- ifelse(df2[, list_clnms[i]]>=0, df2[, list_clnms[i]], NA)
  
}

str(df2)

library(plyr)

numcolwise(sum)(df2, na.rm=TRUE)###sum of numeric columns in df2



#####reshape excel file
library(reshape2)
melted <- melt(df2, id.vars = c("Date_Time")  )#####melt excel file to a new df

library(tidyr)
melted <- melted %>% drop_na()#remove rows with NA in melted df
#write.csv(melted, "melted.csv")

library(lubridate)
melted$Date_Time <-ymd_hms(melted$Date_Time)
str(melted)


mydf <- melted
str(mydf)
mydf$Date_Time <-as_datetime(mydf$Date_Time)
str(mydf)



mydf_list <- split( mydf , f = mydf$variable )##save each parameter as a separate df with its name in mydf_list

length(mydf_list)###check of number of parameters as df in the list 

Co_df_Jan20 <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble
NO_df_Jan20 <- mydf_list[2]%>%
  map_df(as_tibble)##to convert any list to data tibble
NO2_df_Jan20 <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble
Humidity_df_Jan20 <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble
O3_df_Jan20 <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble  
Pressure_df_Jan20 <- mydf_list[6]%>%
  map_df(as_tibble)##to convert any list to data tibble
Temperature_df_Jan20 <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble
Veh_df_Jan20 <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM4_df_Jan20 <- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM1_df_Jan20 <- mydf_list[10]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM10_df_Jan20 <- mydf_list[11]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM2.5_df_Jan20 <- mydf_list[12]%>%
  map_df(as_tibble)##to convert any list to data tibble



#######################################################################################
setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Feb20")###this is the location of your files
df <- read.csv("Haymarket Feb20 Clean.csv", header= TRUE)
str(df)

#create new df which is df2 without columns 17 1nd 18
df2 <- df[, -c(14, 15)]
str(df2)



### replace bad values (here minus values) with NA (or any value)
list_clnms = c(colnames(df2))

for (i in 1:length(list_clnms)){
  df2[, list_clnms[i]] <- ifelse(df2[, list_clnms[i]]>=0, df2[, list_clnms[i]], NA)
  
}

str(df2)

library(plyr)

numcolwise(sum)(df2, na.rm=TRUE)###sum of numeric columns in df2



#####reshape excel file
library(reshape2)
melted <- melt(df2, id.vars = c("Date_Time")  )#####melt excel file to a new df

library(tidyr)
melted <- melted %>% drop_na()#remove rows with NA in melted df
#write.csv(melted, "melted.csv")

library(lubridate)
melted$Date_Time <-ymd_hms(melted$Date_Time)
str(melted)


mydf <- melted
str(mydf)
mydf$Date_Time <-as_datetime(mydf$Date_Time)
str(mydf)



mydf_list <- split( mydf , f = mydf$variable )##save each parameter as a separate df with its name in mydf_list

length(mydf_list)###check of number of parameters as df in the list 
Co_df_Feb20 <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble
NO_df_Feb20 <- mydf_list[2]%>%
  map_df(as_tibble)##to convert any list to data tibble
NO2_df_Feb20 <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble
Humidity_df_Feb20 <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble
O3_df_Feb20 <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble  
Pressure_df_Feb20 <- mydf_list[6]%>%
  map_df(as_tibble)##to convert any list to data tibble
Temperature_df_Feb20 <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble
Veh_df_Feb20 <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM4_df_Feb20 <- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM1_df_Feb20 <- mydf_list[10]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM10_df_Feb20 <- mydf_list[11]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM2.5_df_Feb20 <- mydf_list[12]%>%
  map_df(as_tibble)##to convert any list to data tibble


#######################################################################################
setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Mar20")###this is the location of your files
df <- read.csv("Haymarket Mar20 Clean.csv", header= TRUE)
str(df)

#create new df which is df2 without columns 17 1nd 18
df2 <- df[, -c(14, 15)]
str(df2)



### replace bad values (here minus values) with NA (or any value)
list_clnms = c(colnames(df2))

for (i in 1:length(list_clnms)){
  df2[, list_clnms[i]] <- ifelse(df2[, list_clnms[i]]>=0, df2[, list_clnms[i]], NA)
  
}

str(df2)

library(plyr)

numcolwise(sum)(df2, na.rm=TRUE)###sum of numeric columns in df2



#####reshape excel file
library(reshape2)
melted <- melt(df2, id.vars = c("Date_Time")  )#####melt excel file to a new df

library(tidyr)
melted <- melted %>% drop_na()#remove rows with NA in melted df
#write.csv(melted, "melted.csv")

library(lubridate)
melted$Date_Time <-ymd_hms(melted$Date_Time)
str(melted)


mydf <- melted
str(mydf)
mydf$Date_Time <-as_datetime(mydf$Date_Time)
str(mydf)



mydf_list <- split( mydf , f = mydf$variable )##save each parameter as a separate df with its name in mydf_list

length(mydf_list)###check of number of parameters as df in the list 
Co_df_Mar20 <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble
NO_df_Mar20 <- mydf_list[2]%>%
  map_df(as_tibble)##to convert any list to data tibble
NO2_df_Mar20 <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble
Humidity_df_Mar20 <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble
O3_df_Mar20 <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble  
Pressure_df_Mar20 <- mydf_list[6]%>%
  map_df(as_tibble)##to convert any list to data tibble
Temperature_df_Mar20 <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble
Veh_df_Mar20 <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM4_df_Mar20 <- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM1_df_Mar20 <- mydf_list[10]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM10_df_Mar20 <- mydf_list[11]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM2.5_df_Mar20 <- mydf_list[12]%>%
  map_df(as_tibble)##to convert any list to data tibble


#######################################################################################
setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Apr20")###this is the location of your files
df <- read.csv("Haymarket Apr20 Clean.csv", header= TRUE)
str(df)

#create new df which is df2 without columns 17 1nd 18
df2 <- df[, -c(14, 15)]
str(df2)



### replace bad values (here minus values) with NA (or any value)
list_clnms = c(colnames(df2))

for (i in 1:length(list_clnms)){
  df2[, list_clnms[i]] <- ifelse(df2[, list_clnms[i]]>=0, df2[, list_clnms[i]], NA)
  
}

str(df2)

library(plyr)

numcolwise(sum)(df2, na.rm=TRUE)###sum of numeric columns in df2



#####reshape excel file
library(reshape2)
melted <- melt(df2, id.vars = c("Date_Time")  )#####melt excel file to a new df

library(tidyr)
melted <- melted %>% drop_na()#remove rows with NA in melted df
#write.csv(melted, "melted.csv")

library(lubridate)
melted$Date_Time <-ymd_hms(melted$Date_Time)
str(melted)


mydf <- melted
str(mydf)
mydf$Date_Time <-as_datetime(mydf$Date_Time)
str(mydf)



mydf_list <- split( mydf , f = mydf$variable )##save each parameter as a separate df with its name in mydf_list

length(mydf_list)###check of number of parameters as df in the list 
Co_df_Apr20 <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble
NO_df_Apr20 <- mydf_list[2]%>%
  map_df(as_tibble)##to convert any list to data tibble
NO2_df_Apr20 <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble
Humidity_df_Apr20 <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble
O3_df_Apr20 <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble  
Pressure_df_Apr20 <- mydf_list[6]%>%
  map_df(as_tibble)##to convert any list to data tibble
Temperature_df_Apr20 <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble
Veh_df_Apr20 <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM4_df_Apr20 <- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM1_df_Apr20 <- mydf_list[10]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM10_df_Apr20 <- mydf_list[11]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM2.5_df_Apr20 <- mydf_list[12]%>%
  map_df(as_tibble)##to convert any list to data tibble


#######################################################################################
setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_May20")###this is the location of your files
df <- read.csv("Haymarket May20 Clean.csv", header= TRUE)
str(df)

#create new df which is df2 without columns 17 1nd 18
df2 <- df[, -c(14, 15)]
str(df2)



### replace bad values (here minus values) with NA (or any value)
list_clnms = c(colnames(df2))

for (i in 1:length(list_clnms)){
  df2[, list_clnms[i]] <- ifelse(df2[, list_clnms[i]]>=0, df2[, list_clnms[i]], NA)
  
}

str(df2)

library(plyr)

numcolwise(sum)(df2, na.rm=TRUE)###sum of numeric columns in df2



#####reshape ur excel file
library(reshape2)
melted <- melt(df2, id.vars = c("Date_Time")  )#####melt excel file to a new df

library(tidyr)
melted <- melted %>% drop_na()#remove rows with NA in melted df
#write.csv(melted, "melted.csv")

library(lubridate)
melted$Date_Time <-ymd_hms(melted$Date_Time)
str(melted)


mydf <- melted
str(mydf)
mydf$Date_Time <-as_datetime(mydf$Date_Time)
str(mydf)



mydf_list <- split( mydf , f = mydf$variable )##save each parameter as a separate df with its name in mydf_list

length(mydf_list)###check of number of parameters as df in the list 
Co_df_May20 <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble
NO_df_May20 <- mydf_list[2]%>%
  map_df(as_tibble)##to convert any list to data tibble
NO2_df_May20 <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble
Humidity_df_May20 <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble
O3_df_May20 <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble  
Pressure_df_May20 <- mydf_list[6]%>%
  map_df(as_tibble)##to convert any list to data tibble
Temperature_df_May20 <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble
Veh_df_May20 <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM4_df_May20 <- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM1_df_May20 <- mydf_list[10]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM10_df_May20 <- mydf_list[11]%>%
  map_df(as_tibble)##to convert any list to data tibble
PM2.5_df_May20 <- mydf_list[12]%>%
  map_df(as_tibble)##to convert any list to data tibble


###############################################################################################

CO_2019<- rbind(Co_df_Jan,Co_df_Feb)
CO_2019<- rbind(CO_2019,Co_df_Mar)
CO_2019<- rbind(CO_2019,Co_df_Apr)
CO_2019<- rbind(CO_2019,Co_df_May)
CO_2019<- rbind(CO_2019,Co_df_Jun)
CO_2019<- rbind(CO_2019,Co_df_Jul)
CO_2019<- rbind(CO_2019,Co_df_Aug)
CO_2019<- rbind(CO_2019,Co_df_Sep)
CO_2019<- rbind(CO_2019,Co_df_Oct)
CO_2019<- rbind(CO_2019,Co_df_Nov)
CO_2019<- rbind(CO_2019,Co_df_Dec)

CO_2020<- rbind(Co_df_Jan20,Co_df_Feb20)
CO_2020<- rbind(CO_2020,Co_df_Mar20)
CO_2020<- rbind(CO_2020,Co_df_Apr20)
CO_2020<- rbind(CO_2020,Co_df_May20)

NO_2019<- rbind(NO_df_Jan,NO_df_Feb)
NO_2019<- rbind(NO_2019,NO_df_Mar)
NO_2019<- rbind(NO_2019,NO_df_Apr)
NO_2019<- rbind(NO_2019,NO_df_May)
NO_2019<- rbind(NO_2019,NO_df_Jun)
NO_2019<- rbind(NO_2019,NO_df_Jul)
NO_2019<- rbind(NO_2019,NO_df_Aug)
NO_2019<- rbind(NO_2019,NO_df_Sep)
NO_2019<- rbind(NO_2019,NO_df_Oct)
NO_2019<- rbind(NO_2019,NO_df_Nov)
NO_2019<- rbind(NO_2019,NO_df_Dec)

NO_2020<- rbind(NO_df_Jan20,NO_df_Feb20)
NO_2020<- rbind(NO_2020,NO_df_Mar20)
NO_2020<- rbind(NO_2020,NO_df_Apr20)
NO_2020<- rbind(NO_2020,NO_df_May20)

NO2_2019<- rbind(NO2_df_Jan,NO2_df_Feb)
NO2_2019<- rbind(NO2_2019,NO2_df_Mar)
NO2_2019<- rbind(NO2_2019,NO2_df_Apr)
NO2_2019<- rbind(NO2_2019,NO2_df_May)
NO2_2019<- rbind(NO2_2019,NO2_df_Jun)
NO2_2019<- rbind(NO2_2019,NO2_df_Jul)
NO2_2019<- rbind(NO2_2019,NO2_df_Aug)
NO2_2019<- rbind(NO2_2019,NO2_df_Sep)
NO2_2019<- rbind(NO2_2019,NO2_df_Oct)
NO2_2019<- rbind(NO2_2019,NO2_df_Nov)
NO2_2019<- rbind(NO2_2019,NO2_df_Dec)

NO2_2020<- rbind(NO2_df_Jan20,NO2_df_Feb20)
NO2_2020<- rbind(NO2_2020,NO2_df_Mar20)
NO2_2020<- rbind(NO2_2020,NO2_df_Apr20)
NO2_2020<- rbind(NO2_2020,NO2_df_May20)

O3_2019<- rbind(O3_df_Jan,O3_df_Feb)
O3_2019<- rbind(O3_2019,O3_df_Mar)
O3_2019<- rbind(O3_2019,O3_df_Apr)
O3_2019<- rbind(O3_2019,O3_df_May)
O3_2019<- rbind(O3_2019,O3_df_Jun)
O3_2019<- rbind(O3_2019,O3_df_Jul)
O3_2019<- rbind(O3_2019,O3_df_Aug)
O3_2019<- rbind(O3_2019,O3_df_Sep)
O3_2019<- rbind(O3_2019,O3_df_Oct)
O3_2019<- rbind(O3_2019,O3_df_Nov)
O3_2019<- rbind(O3_2019,O3_df_Dec)

O3_2020<- rbind(O3_df_Jan20,O3_df_Feb20)
O3_2020<- rbind(O3_2020,O3_df_Mar20)
O3_2020<- rbind(O3_2020,O3_df_Apr20)
O3_2020<- rbind(O3_2020,O3_df_May20)

Veh_2019<- rbind(Veh_df_Jan,Veh_df_Feb)
Veh_2019<- rbind(Veh_2019,Veh_df_Mar)
Veh_2019<- rbind(Veh_2019,Veh_df_Apr)
Veh_2019<- rbind(Veh_2019,Veh_df_May)
Veh_2019<- rbind(Veh_2019,Veh_df_Jun)
Veh_2019<- rbind(Veh_2019,Veh_df_Jul)
Veh_2019<- rbind(Veh_2019,Veh_df_Aug)
Veh_2019<- rbind(Veh_2019,Veh_df_Sep)
Veh_2019<- rbind(Veh_2019,Veh_df_Oct)
Veh_2019<- rbind(Veh_2019,Veh_df_Nov)
Veh_2019<- rbind(Veh_2019,Veh_df_Dec)

Veh_2020<- rbind(Veh_df_Jan20,Veh_df_Feb20)
Veh_2020<- rbind(Veh_2020,Veh_df_Mar20)
Veh_2020<- rbind(Veh_2020,Veh_df_Apr20)
Veh_2020<- rbind(Veh_2020,Veh_df_May20)

Part_2019<- rbind(Part_df_Jan,Part_df_Feb)
Part_2019<- rbind(Part_2019,Part_df_Mar)
Part_2019<- rbind(Part_2019,Part_df_Apr)
Part_2019<- rbind(Part_2019,Part_df_May)
Part_2019<- rbind(Part_2019,Part_df_Jun)
Part_2019<- rbind(Part_2019,Part_df_Jul)
Part_2019<- rbind(Part_2019,Part_df_Aug)
Part_2019<- rbind(Part_2019,Part_df_Sep)
Part_2019<- rbind(Part_2019,Part_df_Oct)
Part_2019<- rbind(Part_2019,Part_df_Nov)

PM4_2019<- rbind(PM4_df_Jan,PM4_df_Feb)
PM4_2019<- rbind(PM4_2019,PM4_df_Mar)
PM4_2019<- rbind(PM4_2019,PM4_df_Apr)
PM4_2019<- rbind(PM4_2019,PM4_df_May)
PM4_2019<- rbind(PM4_2019,PM4_df_Jun)
PM4_2019<- rbind(PM4_2019,PM4_df_Jul)
PM4_2019<- rbind(PM4_2019,PM4_df_Aug)
PM4_2019<- rbind(PM4_2019,PM4_df_Sep)
PM4_2019<- rbind(PM4_2019,PM4_df_Oct)
PM4_2019<- rbind(PM4_2019,PM4_df_Nov)
PM4_2019<- rbind(PM4_2019,PM4_df_Dec)

PM4_2020<- rbind(PM4_df_Jan20,PM4_df_Feb20)
PM4_2020<- rbind(PM4_2020,PM4_df_Mar20)
PM4_2020<- rbind(PM4_2020,PM4_df_Apr20)
PM4_2020<- rbind(PM4_2020,PM4_df_May20)

PM1_2019<- rbind(PM1_df_Jan,PM1_df_Feb)
PM1_2019<- rbind(PM1_2019,PM1_df_Mar)
PM1_2019<- rbind(PM1_2019,PM1_df_Apr)
PM1_2019<- rbind(PM1_2019,PM1_df_May)
PM1_2019<- rbind(PM1_2019,PM1_df_Jun)
PM1_2019<- rbind(PM1_2019,PM1_df_Jul)
PM1_2019<- rbind(PM1_2019,PM1_df_Aug)
PM1_2019<- rbind(PM1_2019,PM1_df_Sep)
PM1_2019<- rbind(PM1_2019,PM1_df_Oct)
PM1_2019<- rbind(PM1_2019,PM1_df_Nov)
PM1_2019<- rbind(PM1_2019,PM1_df_Dec)

PM1_2020<- rbind(PM1_df_Jan20,PM1_df_Feb20)
PM1_2020<- rbind(PM1_2020,PM1_df_Mar20)
PM1_2020<- rbind(PM1_2020,PM1_df_Apr20)
PM1_2020<- rbind(PM1_2020,PM1_df_May20)

PM10_2019<- rbind(PM10_df_Jan,PM10_df_Feb)
PM10_2019<- rbind(PM10_2019,PM10_df_Mar)
PM10_2019<- rbind(PM10_2019,PM10_df_Apr)
PM10_2019<- rbind(PM10_2019,PM10_df_May)
PM10_2019<- rbind(PM10_2019,PM10_df_Jun)
PM10_2019<- rbind(PM10_2019,PM10_df_Jul)
PM10_2019<- rbind(PM10_2019,PM10_df_Aug)
PM10_2019<- rbind(PM10_2019,PM10_df_Sep)
PM10_2019<- rbind(PM10_2019,PM10_df_Oct)
PM10_2019<- rbind(PM10_2019,PM10_df_Nov)
PM10_2019<- rbind(PM10_2019,PM10_df_Dec)

PM10_2020<- rbind(PM10_df_Jan20,PM10_df_Feb20)
PM10_2020<- rbind(PM10_2020,PM10_df_Mar20)
PM10_2020<- rbind(PM10_2020,PM10_df_Apr20)
PM10_2020<- rbind(PM10_2020,PM10_df_May20)

PM2.5_2019<- rbind(PM2.5_df_Jan,PM2.5_df_Feb)
PM2.5_2019<- rbind(PM2.5_2019,PM2.5_df_Mar)
PM2.5_2019<- rbind(PM2.5_2019,PM2.5_df_Apr)
PM2.5_2019<- rbind(PM2.5_2019,PM2.5_df_May)
PM2.5_2019<- rbind(PM2.5_2019,PM2.5_df_Jun)
PM2.5_2019<- rbind(PM2.5_2019,PM2.5_df_Jul)
PM2.5_2019<- rbind(PM2.5_2019,PM2.5_df_Aug)
PM2.5_2019<- rbind(PM2.5_2019,PM2.5_df_Sep)
PM2.5_2019<- rbind(PM2.5_2019,PM2.5_df_Oct)
PM2.5_2019<- rbind(PM2.5_2019,PM2.5_df_Nov)
PM2.5_2019<- rbind(PM2.5_2019,PM2.5_df_Dec)

PM2.5_2020<- rbind(PM2.5_df_Jan20,PM2.5_df_Feb20)
PM2.5_2020<- rbind(PM2.5_2020,PM2.5_df_Mar20)
PM2.5_2020<- rbind(PM2.5_2020,PM2.5_df_Apr20)
PM2.5_2020<- rbind(PM2.5_2020,PM2.5_df_May20)



summary(CO_2019$variable)
CO_2019$value<- as.numeric(CO_2019$value)
newdf <- filter(CO_2019,CO_2019$variable == "CO")
str(CO_2019)
summary(newdf$value)
summary(CO_2019$variable)

boxplot(newdf$value)
boxplot(newdf$value,plot=FALSE)$out
outliers <- boxplot(newdf$value,plot=FALSE)$out
print(outliers)
newdf[which(newdf$value %in% outliers),]
Mydf<-newdf[-which(newdf$value %in% outliers),]
boxplot(Mydf$value)

summary(Mydf$value)
Mydf$value<- as.numeric(Mydf$value)
newdf <- filter(Mydf,Mydf$variable == "CO")
str(Mydf)
summary(newdf$value)




summary(CO_2020$variable)
CO_2020$value<- as.numeric(CO_2020$value)
newdf <- filter(CO_2020,CO_2020$variable == "CO")
str(CO_2020)
summary(newdf$value)

boxplot(newdf$value)
boxplot(newdf$value,plot=FALSE)$out
outliers <- boxplot(newdf$value,plot=FALSE)$out
print(outliers)
newdf[which(newdf$value %in% outliers),]
Mydf<-newdf[-which(newdf$value %in% outliers),]
boxplot(Mydf$value)

summary(Mydf$value)
Mydf$value<- as.numeric(Mydf$value)
newdf <- filter(Mydf,Mydf$variable == "CO")
str(Mydf)
summary(newdf$value)


summary(NO_2019$variable)
NO_2019$value<- as.numeric(NO_2019$value)
newdf <- filter(NO_2019,NO_2019$variable == "NO")
str(NO_2019)
summary(newdf$value)

boxplot(newdf$value)
boxplot(newdf$value,plot=FALSE)$out
outliers <- boxplot(newdf$value,plot=FALSE)$out
print(outliers)
newdf[which(newdf$value %in% outliers),]
Mydf<-newdf[-which(newdf$value %in% outliers),]
boxplot(Mydf$value)

summary(Mydf$value)
Mydf$value<- as.numeric(Mydf$value)
newdf <- filter(Mydf,Mydf$variable == "NO")
str(Mydf)
summary(newdf$value)




summary(NO_2020$variable)
NO_2020$value<- as.numeric(NO_2020$value)
newdf <- filter(NO_2020,NO_2020$variable == "NO")
str(NO_2020)
summary(newdf$value)

boxplot(newdf$value)
boxplot(newdf$value,plot=FALSE)$out
outliers <- boxplot(newdf$value,plot=FALSE)$out
print(outliers)
newdf[which(newdf$value %in% outliers),]
Mydf<-newdf[-which(newdf$value %in% outliers),]
boxplot(Mydf$value)

summary(Mydf$value)
Mydf$value<- as.numeric(Mydf$value)
newdf <- filter(Mydf,Mydf$variable == "NO")
str(Mydf)
summary(newdf$value)




summary(NO2_2019$variable)
NO2_2019$value<- as.numeric(NO2_2019$value)
newdf <- filter(NO2_2019,NO2_2019$variable == "NO2")
str(NO2_2019)
summary(newdf$value)

boxplot(newdf$value)
boxplot(newdf$value,plot=FALSE)$out
outliers <- boxplot(newdf$value,plot=FALSE)$out
print(outliers)
newdf[which(newdf$value %in% outliers),]
Mydf<-newdf[-which(newdf$value %in% outliers),]
boxplot(Mydf$value)

summary(Mydf$value)
Mydf$value<- as.numeric(Mydf$value)
newdf <- filter(Mydf,Mydf$variable == "NO2")
str(Mydf)
summary(newdf$value)




summary(NO2_2020$variable)
summary(NO2_2020$value)
NO2_2020$value<- as.numeric(NO2_2020$value)
newdf <- filter(NO2_2020,NO2_2020$variable == "NO2")
str(NO2_2020)
summary(newdf$value)

boxplot(newdf$value)
boxplot(newdf$value,plot=FALSE)$out
outliers <- boxplot(newdf$value,plot=FALSE)$out
print(outliers)
newdf[which(newdf$value %in% outliers),]
newdf[-which(newdf$value>=72.816),]
newdf <- newdf[-which(newdf$value>=72.816),]
Mydf<-newdf[-which(newdf$value %in% outliers),]
boxplot(Mydf$value)

summary(Mydf$value)
Mydf$value<- as.numeric(Mydf$value)
newdf <- filter(Mydf,Mydf$variable == "NO2")
str(Mydf)
summary(newdf$value)


summary(O3_2019$variable)
O3_2019$value<- as.numeric(O3_2019$value)
newdf <- filter(O3_2019,O3_2019$variable == "O3")
str(O3_2019)
summary(newdf$value)

boxplot(newdf$value)
boxplot(newdf$value,plot=FALSE)$out
outliers <- boxplot(newdf$value,plot=FALSE)$out
print(outliers)
newdf[which(newdf$value %in% outliers),]
Mydf<-newdf[-which(newdf$value %in% outliers),]
boxplot(Mydf$value)

summary(Mydf$value)
Mydf$value<- as.numeric(Mydf$value)
newdf <- filter(Mydf,Mydf$variable == "O3")
str(Mydf)
summary(newdf$value)




summary(O3_2020$variable)
summary(O3_2020$value)
O3_2020$value<- as.numeric(O3_2020$value)
newdf <- filter(O3_2020,O3_2020$variable == "O3")
str(O3_2020)
summary(newdf$value)

boxplot(newdf$value)
boxplot(newdf$value,plot=FALSE)$out
outliers <- boxplot(newdf$value,plot=FALSE)$out
print(outliers)
newdf[which(newdf$value %in% outliers),]
Mydf<-newdf[-which(newdf$value %in% outliers),]
boxplot(Mydf$value)

summary(Mydf$value)
Mydf$value<- as.numeric(Mydf$value)
newdf <- filter(Mydf,Mydf$variable == "O3")
str(Mydf)
summary(newdf$value)







summary(Veh_2019$variable)
Veh_2019$value<- as.numeric(Veh_2019$value)
str(Veh_2019)
summary(newdf$value)

boxplot(newdf$value)
boxplot(newdf$value,plot=FALSE)$out
outliers <- boxplot(newdf$value,plot=FALSE)$out
print(outliers)
newdf[which(newdf$value %in% outliers),]
Mydf<-newdf[-which(newdf$value %in% outliers),]
boxplot(Mydf$value)

summary(Mydf$value)
Mydf$value<- as.numeric(Mydf$value)
str(Mydf)
summary(newdf$value)




summary(Veh_2020$variable)
summary(Veh_2020$value)
Veh_2020$value<- as.numeric(Veh_2020$value)
str(Veh_2020)
summary(newdf$value)

boxplot(newdf$value)
boxplot(newdf$value,plot=FALSE)$out
outliers <- boxplot(newdf$value,plot=FALSE)$out
print(outliers)
newdf[which(newdf$value %in% outliers),]
Mydf<-newdf[-which(newdf$value %in% outliers),]
boxplot(Mydf$value)

summary(Mydf$value)
Mydf$value<- as.numeric(Mydf$value)
str(Mydf)
summary(newdf$value)




summary(Part_2019$variable)
Part_2019$value<- as.numeric(Part_2019$value)
newdf <- filter(Part_2019,Part_2019$variable == "Particle.Count")
str(Part_2019)
summary(newdf$value)

boxplot(newdf$value)
boxplot(newdf$value,plot=FALSE)$out
outliers <- boxplot(newdf$value,plot=FALSE)$out
print(outliers)
newdf[which(newdf$value %in% outliers),]
Mydf<-newdf[-which(newdf$value %in% outliers),]
boxplot(Mydf$value)

summary(Mydf$value)
Mydf$value<- as.numeric(Mydf$value)
newdf <- filter(Mydf,Mydf$variable == "Particle.Count")
str(Mydf)
summary(newdf$value)



summary(PM4_2019$variable)
PM4_2019$value<- as.numeric(PM4_2019$value)
newdf <- filter(PM4_2019,PM4_2019$variable == "PM.4")
str(PM4_2019)
summary(newdf$value)

boxplot(newdf$value)
boxplot(newdf$value,plot=FALSE)$out
outliers <- boxplot(newdf$value,plot=FALSE)$out
print(outliers)
newdf[which(newdf$value %in% outliers),]
Mydf<-newdf[-which(newdf$value %in% outliers),]
boxplot(Mydf$value)

summary(Mydf$value)
Mydf$value<- as.numeric(Mydf$value)
newdf <- filter(Mydf,Mydf$variable == "PM.4")
str(Mydf)
summary(newdf$value)




summary(PM4_2020$variable)
summary(PM4_2020$value)
PM4_2020$value<- as.numeric(PM4_2020$value)
newdf <- filter(PM4_2020,PM4_2020$variable == "PM.4")
str(PM4_2020)
summary(newdf$value)

boxplot(newdf$value)
boxplot(newdf$value,plot=FALSE)$out
outliers <- boxplot(newdf$value,plot=FALSE)$out
print(outliers)
newdf[which(newdf$value %in% outliers),]
Mydf<-newdf[-which(newdf$value %in% outliers),]
boxplot(Mydf$value)

summary(Mydf$value)
Mydf$value<- as.numeric(Mydf$value)
newdf <- filter(Mydf,Mydf$variable == "PM.4")
str(Mydf)
summary(newdf$value)


summary(PM1_2019$variable)
PM1_2019$value<- as.numeric(PM1_2019$value)
newdf <- filter(PM1_2019,PM1_2019$variable == "PM1")
str(PM1_2019)
summary(newdf$value)

boxplot(newdf$value)
boxplot(newdf$value,plot=FALSE)$out
outliers <- boxplot(newdf$value,plot=FALSE)$out
print(outliers)
newdf[which(newdf$value %in% outliers),]
Mydf<-newdf[-which(newdf$value %in% outliers),]
boxplot(Mydf$value)

summary(Mydf$value)
Mydf$value<- as.numeric(Mydf$value)
newdf <- filter(Mydf,Mydf$variable == "PM1")
str(Mydf)
summary(newdf$value)




summary(PM1_2020$variable)
summary(PM1_2020$value)
PM1_2020$value<- as.numeric(PM1_2020$value)
newdf <- filter(PM1_2020,PM1_2020$variable == "PM1")
str(PM1_2020)
summary(newdf$value)

boxplot(newdf$value)
boxplot(newdf$value,plot=FALSE)$out
outliers <- boxplot(newdf$value,plot=FALSE)$out
print(outliers)
newdf[which(newdf$value %in% outliers),]
Mydf<-newdf[-which(newdf$value %in% outliers),]
boxplot(Mydf$value)

summary(Mydf$value)
Mydf$value<- as.numeric(Mydf$value)
newdf <- filter(Mydf,Mydf$variable == "PM1")
str(Mydf)
summary(newdf$value)



summary(PM10_2019$variable)
PM10_2019$value<- as.numeric(PM10_2019$value)
newdf <- filter(PM10_2019,PM10_2019$variable == "PM10")
str(PM10_2019)
summary(newdf$value)

boxplot(newdf$value)
boxplot(newdf$value,plot=FALSE)$out
outliers <- boxplot(newdf$value,plot=FALSE)$out
print(outliers)
newdf[which(newdf$value %in% outliers),]
Mydf<-newdf[-which(newdf$value %in% outliers),]
boxplot(Mydf$value)

summary(Mydf$value)
Mydf$value<- as.numeric(Mydf$value)
newdf <- filter(Mydf,Mydf$variable == "PM10")
str(Mydf)
summary(newdf$value)




summary(PM10_2020$variable)
summary(PM10_2020$value)
PM10_2020$value<- as.numeric(PM10_2020$value)
newdf <- filter(PM10_2020,PM10_2020$variable == "PM10")
str(PM10_2020)
summary(newdf$value)

boxplot(newdf$value)
boxplot(newdf$value,plot=FALSE)$out
outliers <- boxplot(newdf$value,plot=FALSE)$out
print(outliers)
newdf[which(newdf$value %in% outliers),]
Mydf<-newdf[-which(newdf$value %in% outliers),]
boxplot(Mydf$value)

summary(Mydf$value)
Mydf$value<- as.numeric(Mydf$value)
newdf <- filter(Mydf,Mydf$variable == "PM10")
str(Mydf)
summary(newdf$value)



summary(PM2.5_2019$variable)
PM2.5_2019$value<- as.numeric(PM2.5_2019$value)
newdf <- filter(PM2.5_2019,PM2.5_2019$variable == "PM2.5")
str(PM2.5_2019)
summary(newdf$value)

boxplot(newdf$value)
boxplot(newdf$value,plot=FALSE)$out
outliers <- boxplot(newdf$value,plot=FALSE)$out
print(outliers)
newdf[which(newdf$value %in% outliers),]
Mydf<-newdf[-which(newdf$value %in% outliers),]
boxplot(Mydf$value)

summary(Mydf$value)
Mydf$value<- as.numeric(Mydf$value)
newdf <- filter(Mydf,Mydf$variable == "PM2.5")
str(Mydf)
summary(newdf$value)




summary(PM2.5_2020$variable)
summary(PM2.5_2020$value)
PM2.5_2020$value<- as.numeric(PM2.5_2020$value)
newdf <- filter(PM2.5_2020,PM2.5_2020$variable == "PM2.5")
str(PM2.5_2020)
summary(newdf$value)

boxplot(newdf$value)
boxplot(newdf$value,plot=FALSE)$out
outliers <- boxplot(newdf$value,plot=FALSE)$out
print(outliers)
newdf[which(newdf$value %in% outliers),]
Mydf<-newdf[-which(newdf$value %in% outliers),]
boxplot(Mydf$value)

summary(Mydf$value)
Mydf$value<- as.numeric(Mydf$value)
newdf <- filter(Mydf,Mydf$variable == "PM2.5")
str(Mydf)
summary(newdf$value)
