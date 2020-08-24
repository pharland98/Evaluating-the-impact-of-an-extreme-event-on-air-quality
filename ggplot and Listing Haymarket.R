setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymaket Jan-Mar19")###this is the location of  files
df <- read.csv("Haymarket Jan Clean.csv", header= TRUE)
str(df)
df<-df[-c(1523969:2361528),]

df2 <- df[, -c(17, 18)]
str(df2)



list_clnms = c(colnames(df2))

for (i in 1:length(list_clnms)){
  df2[, list_clnms[i]] <- ifelse(df2[, list_clnms[i]]>=0, df2[, list_clnms[i]], NA)
  
}

str(df2)

library(plyr)

numcolwise(sum)(df2, na.rm=TRUE)###sum of numeric columns in df2


boxplot(df2$CO)
boxplot(df2$CO,plot=FALSE)$out
outliers <- boxplot(df2$CO,plot=FALSE)$out
print(outliers)
df2[which(df2$CO %in% outliers),]
df2<-df2[-which(df2$CO %in% outliers),]
boxplot(df2$NO)
boxplot(df2$NO,plot=FALSE)$out
outliers <- boxplot(df2$NO,plot=FALSE)$out
print(outliers)
df2[which(df2$NO %in% outliers),]
df2<-df2[-which(df2$NO %in% outliers),]
boxplot(df2$NO2)
boxplot(df2$NO2,plot=FALSE)$out
outliers <- boxplot(df2$NO2,plot=FALSE)$out
print(outliers)
df2[which(df2$NO2 %in% outliers),]
df2<-df2[-which(df2$NO2 %in% outliers),]
boxplot(df2$Humidity)
boxplot(df2$Humidity,plot=FALSE)$out
outliers <- boxplot(df2$Humidity,plot=FALSE)$out
print(outliers)
df2[which(df2$Humidity %in% outliers),]
df2<-df2[-which(df2$Humidity %in% outliers),]
boxplot(df2$O3)
boxplot(df2$O3,plot=FALSE)$out
outliers <- boxplot(df2$O3,plot=FALSE)$out
print(outliers)
df2[which(df2$O3 %in% outliers),]
df2<-df2[-which(df2$O3 %in% outliers),]
boxplot(df2$PM.4)
boxplot(df2$PM.4,plot=FALSE)$out
outliers <- boxplot(df2$PM.4,plot=FALSE)$out
print(outliers)
df2[which(df2$PM.4 %in% outliers),]
df2<-df2[-which(df2$PM.4 %in% outliers),]
boxplot(df2$PM1)
boxplot(df2$PM1,plot=FALSE)$out
outliers <- boxplot(df2$PM1,plot=FALSE)$out
print(outliers)
df2[which(df2$PM1 %in% outliers),]
df2<-df2[-which(df2$PM1 %in% outliers),]
boxplot(df2$PM10)
boxplot(df2$PM10,plot=FALSE)$out
outliers <- boxplot(df2$PM10,plot=FALSE)$out
print(outliers)
df2[which(df2$PM10 %in% outliers),]
df2<-df2[-which(df2$PM10 %in% outliers),]
boxplot(df2$PM2.5)
boxplot(df2$PM2.5,plot=FALSE)$out
outliers <- boxplot(df2$PM2.5,plot=FALSE)$out
print(outliers)
df2[which(df2$PM2.5 %in% outliers),]
df2<-df2[-which(df2$PM2.5 %in% outliers),]

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

##
library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

####Co as 
#####lets extract the data for Co from our list object mydf_list, we  name the df as  Co_df , then
#we can use this new df for the following analysis:

Co_df <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble

#mean by day using qplot
Co_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarize(margin = mean(value)) %>%
  qplot(day, margin, data = .)+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("10 days"))+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+labs(y="Co")+
  theme_bw()+geom_point(size = 7, color="red")

##or ggplot mean value by  day
Co_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarize(margin = mean(value)) %>%
  ggplot(aes(x= day, y=margin))+geom_point(size = 5, color = "blue")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("10 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Co")+
  theme_bw()


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

Co_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO (ug/m^3)", x="Day")+
  theme_bw()


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

Co_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Co")+
  theme_bw()


##orggplot  box plot by week note: for boxplot x-axis need to be factor

Co_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Co")+
  theme_bw()###needs some time

####and so on similar for other parameters, similar analysis and other graph types

#####if you want to save  mean, sd, se, ci for all your parameters in alist
##for example by week
Mean_lists_week = list()
for (i in 1:length(mydf_list)) {
  
  m <-mydf_list[[i]]%>%
    mutate(week = round_date(Date_Time, "week")) %>%
    group_by(week) %>%
    summarySE(measurevar="value", groupvars=c("week"))
  
  listname <- paste("Mean_by_week_", names(mydf_list[i]))
  
  Mean_lists_week[[listname]] <-m
  
}
############
##or by day

Mean_lists_day = list()
for (i in 1:length(mydf_list)) {
  
  m <-mydf_list[[i]]%>%
    mutate(day = round_date(Date_Time, "day")) %>%
    group_by(day) %>%
    summarySE(measurevar="value", groupvars=c("day"))
  
  listname <- paste("Mean_by_day_", names(mydf_list[i]))
  
  Mean_lists_day[[listname]] <-m
  
}
  
PM2.5_df <- mydf_list[15]%>%
  map_df(as_tibble)##to convert any list to data tibble

#mean by day using qplot
PM2.5_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarize(margin = mean(value)) %>%
  qplot(day, margin, data = .)+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("10 days"))+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+labs(y="PM2.5")+
  theme_bw()+geom_point(size = 7, color="red")

##or ggplot mean value by  day
PM2.5_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarize(margin = mean(value)) %>%
  ggplot(aes(x= day, y=margin))+geom_point(size = 5, color = "blue")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("10 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM2.5")+
  theme_bw()


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

PM2.5_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM2.5", x="Day")+
  theme_bw()


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

PM2.5_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM2.5",x="Week")+
  theme_bw()


##orggplot  box plot by week note: for boxplot x-axis need to be factor

PM2.5_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM2.5",x="Week")+
  theme_bw()###needs some time

#############################################################################################
#######################################################################################################
#######################################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Feb")###this is the location of  files
df_Feb <- read.csv("Haymarket Feb Clean.csv", header= TRUE)
str(df_Feb)

df2_Feb <- df_Feb[, -c(15, 16)]
str(df2_Feb)



list_clnms = c(colnames(df2_Feb))

for (i in 1:length(list_clnms_Feb)){
  df2_Feb[, list_clnms_Feb[i]] <- ifelse(df2_Feb[, list_clnms_Feb[i]]>=0, df2_Feb[, list_clnms_Feb[i]], NA)
  
}

str(df2_Feb)

library(plyr)

numcolwise(sum)(df2_Feb, na.rm=TRUE)###sum of numeric columns in df2


boxplot(df2_Feb$CO)
boxplot(df2_Feb$CO,plot=FALSE)$out
outliers <- boxplot(df2_Feb$CO,plot=FALSE)$out
print(outliers)
df2_Feb[which(df2_Feb$CO %in% outliers),]
df2_Feb<-df2_Feb[-which(df2_Feb$CO %in% outliers),]
boxplot(df2_Feb$NO)
boxplot(df2_Feb$NO,plot=FALSE)$out
outliers <- boxplot(df2_Feb$NO,plot=FALSE)$out
print(outliers)
df2_Feb[which(df2_Feb$NO %in% outliers),]
df2_Feb<-df2_Feb[-which(df2_Feb$NO %in% outliers),]
boxplot(df2_Feb$NO2)
boxplot(df2_Feb$NO2,plot=FALSE)$out
outliers <- boxplot(df2_Feb$NO2,plot=FALSE)$out
print(outliers)
df2_Feb[which(df2_Feb$NO2 %in% outliers),]
df2_Feb<-df2_Feb[-which(df2_Feb$NO2 %in% outliers),]
boxplot(df2_Feb$Humidity)
boxplot(df2_Feb$Humidity,plot=FALSE)$out
outliers <- boxplot(df2_Feb$Humidity,plot=FALSE)$out
print(outliers)
df2_Feb[which(df2_Feb$Humidity %in% outliers),]
df2_Feb<-df2_Feb[-which(df2_Feb$Humidity %in% outliers),]
boxplot(df2_Feb$O3)
boxplot(df2_Feb$O3,plot=FALSE)$out
outliers <- boxplot(df2_Feb$O3,plot=FALSE)$out
print(outliers)
df2_Feb[which(df2_Feb$O3 %in% outliers),]
df2_Feb<-df2_Feb[-which(df2_Feb$O3 %in% outliers),]
boxplot(df2_Feb$PM.4)
boxplot(df2_Feb$PM.4,plot=FALSE)$out
outliers <- boxplot(df2_Feb$PM.4,plot=FALSE)$out
print(outliers)
df2_Feb[which(df2_Feb$PM.4 %in% outliers),]
df2_Feb<-df2_Feb[-which(df2_Feb$PM.4 %in% outliers),]
boxplot(df2_Feb$PM1)
boxplot(df2_Feb$PM1,plot=FALSE)$out
outliers <- boxplot(df2_Feb$PM1,plot=FALSE)$out
print(outliers)
df2_Feb[which(df2_Feb$PM1 %in% outliers),]
df2_Feb<-df2_Feb[-which(df2_Feb$PM1 %in% outliers),]
boxplot(df2_Feb$PM10)
boxplot(df2_Feb$PM10,plot=FALSE)$out
outliers <- boxplot(df2_Feb$PM10,plot=FALSE)$out
print(outliers)
df2_Feb[which(df2_Feb$PM10 %in% outliers),]
df2_Feb<-df2_Feb[-which(df2_Feb$PM10 %in% outliers),]
boxplot(df2_Feb$PM2.5)
boxplot(df2_Feb$PM2.5,plot=FALSE)$out
outliers <- boxplot(df2_Feb$PM2.5,plot=FALSE)$out
print(outliers)
df2_Feb[which(df2_Feb$PM2.5 %in% outliers),]
df2_Feb<-df2_Feb[-which(df2_Feb$PM2.5 %in% outliers),]


#####reshape excel file
library(reshape2)
melted_Feb <- melt(df2_Feb, id.vars = c("Date_Time")  )#####melt excel file to a new df

library(tidyr)
melted_Feb <- melted_Feb %>% drop_na()#remove rows with NA in melted df
#write.csv(melted, "melted.csv")

library(lubridate)
melted_Feb$Date_Time <-ymd_hms(melted_Feb$Date_Time)
str(melted_Feb)


mydf_Feb <- melted_Feb
str(mydf_Feb)
mydf_Feb$Date_Time <-as_datetime(mydf_Feb$Date_Time)
str(mydf_Feb)



mydf_Feb_list <- split( mydf_Feb , f = mydf_Feb$variable )##save each parameter as a separate df with its name in mydf_list

length(mydf_Feb_list)###check of number of parameters as df in the list 

##
library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

Mean_lists_week = list()
for (i in 1:length(mydf_Feb_list)) {
  
  m <-mydf_list[[i]]%>%
    mutate(week = round_date(Date_Time, "week")) %>%
    group_by(week) %>%
    summarySE(measurevar="value", groupvars=c("week"))
  
  listname <- paste("Mean_by_week_", names(mydf_Feb_list[i]))
  
  Mean_lists_week[[listname]] <-m
  
}
############
##or by day

Mean_lists_day = list()
for (i in 1:length(mydf_Feb_list)) {
  
  m <-mydf_list[[i]]%>%
    mutate(day = round_date(Date_Time, "day")) %>%
    group_by(day) %>%
    summarySE(measurevar="value", groupvars=c("day"))
  
  listname <- paste("Mean_by_day_", names(mydf_Feb_list[i]))
  
  Mean_lists_day[[listname]] <-m
  
}

CO_df <- mydf_Feb_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-CO_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO", x="Day")+
  theme_bw()
ggsave("CO Daily Mean Feb .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-CO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()
ggsave("CO Weekly Mean Feb .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-CO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()###needs some time

ggsave("CO Weekly Boxplot Feb .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################

p<-NO_df <- mydf_Feb_list[2]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO", x="Day")+
  theme_bw()
ggsave("NO Daily Mean Feb .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()
ggsave("NO Weekly Mean Feb .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-NO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()###needs some time

ggsave("NO Weekly Boxplot Feb .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###############################################################################################

NO2_df <- mydf_Feb_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO2_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2", x="Day")+
  theme_bw()
ggsave("NO2 Daily Mean Feb .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO2_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()
ggsave("NO2 Weekly Mean Feb .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-NO2_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()###needs some time

ggsave("NO2 Weekly Boxplot Feb .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

########################################################################################

Hum_df <- mydf_Feb_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Hum_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity", x="Day")+
  theme_bw()
ggsave("Hum Daily Mean Feb .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Hum_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()
ggsave("Hum Weekly Mean Feb .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Hum_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()###needs some time

ggsave("Hum Weekly Boxplot Feb .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
O3_df <- mydf_Feb_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-O3_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="O3", x="Day")+
  theme_bw()
ggsave("O3 Daily Mean Feb .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-O3_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="O3",x="Week")+
  theme_bw()
ggsave("O3 Weekly Mean Feb .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-O3_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="O3",x="Week")+
  theme_bw()###needs some time

ggsave("O3 Weekly Boxplot Feb .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Pressure_df <- mydf_Feb_list[6]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Pressure_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Pressure", x="Day")+
  theme_bw()
ggsave("Pressure Daily Mean Feb .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Pressure_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Pressure",x="Week")+
  theme_bw()
ggsave("Pressure Weekly Mean Feb .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Pressure_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Pressure",x="Week")+
  theme_bw()###needs some time

ggsave("Pressure Weekly Boxplot Feb .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Temp_df <- mydf_Feb_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Temp_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature", x="Day")+
  theme_bw()
ggsave("Temperature Daily Mean Feb .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Temp_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()
ggsave("Temperature Weekly Mean Feb .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Temp_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()###needs some time

ggsave("Temperature Weekly Boxplot Feb .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Veh_df <- mydf_Feb_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Veh_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Vehicles", x="Day")+
  theme_bw()
ggsave("Vehicles Daily Mean Feb .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Veh_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Vehicles",x="Week")+
  theme_bw()
ggsave("Vehicles Weekly Mean Feb .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Veh_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Vehicles",x="Week")+
  theme_bw()###needs some time

ggsave("Vehicles Weekly Boxplot Feb .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Part_df <- mydf_Feb_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Part_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Particle Count", x="Day")+
  theme_bw()
ggsave("Particle Count Daily Mean Feb .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Part_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Particle Count",x="Week")+
  theme_bw()
ggsave("Particle Count Weekly Mean Feb .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Part_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Particle Count",x="Week")+
  theme_bw()###needs some time

ggsave("Particle Count Weekly Boxplot Feb .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM4_df <- mydf_Feb_list[10]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM4_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM4", x="Day")+
  theme_bw()
ggsave("PM4 Daily Mean Feb .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM4_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM4",x="Week")+
  theme_bw()
ggsave("PM4 Weekly Mean Feb .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM4_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM4",x="Week")+
  theme_bw()###needs some time

ggsave("PM4 Weekly Boxplot Feb .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM1_df <- mydf_Feb_list[11]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM1_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM1", x="Day")+
  theme_bw()
ggsave("PM1 Daily Mean Feb .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM1_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM1",x="Week")+
  theme_bw()
ggsave("PM1 Weekly Mean Feb .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM1_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM1",x="Week")+
  theme_bw()###needs some time

ggsave("PM1 Weekly Boxplot Feb .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM10_df <- mydf_Feb_list[12]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM10_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM10", x="Day")+
  theme_bw()
ggsave("PM10 Daily Mean Feb .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM10_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM10",x="Week")+
  theme_bw()
ggsave("PM10 Weekly Mean Feb .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM10_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM10",x="Week")+
  theme_bw()###needs some time

ggsave("PM10 Weekly Boxplot Feb .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM2.5_df <- mydf_Feb_list[13]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM2.5_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM2.5", x="Day")+
  theme_bw()
ggsave("PM2.5 Daily Mean Feb .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM2.5_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM2.5",x="Week")+
  theme_bw()
ggsave("PM2.5 Weekly Mean Feb .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM2.5_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM2.5",x="Week")+
  theme_bw()###needs some time

ggsave("PM2.5 Weekly Boxplot Feb .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#############################################################################################
#######################################################################################################
#######################################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Mar")###this is the location of files
df_Mar <- read.csv("Haymarket Mar Clean.csv", header= TRUE)
str(df_Mar)

df2_Mar <- df_Mar[, -c(15, 16)]
str(df2_Mar)



list_clnms = c(colnames(df2_Mar))

for (i in 1:length(list_clnms_Mar)){
  df2_Mar[, list_clnms_Mar[i]] <- ifelse(df2_Mar[, list_clnms_Mar[i]]>=0, df2_Mar[, list_clnms_Mar[i]], NA)
  
}

str(df2_Mar)

library(plyr)

numcolwise(sum)(df2_Mar, na.rm=TRUE)###sum of numeric columns in df2

boxplot(df2_Mar$CO)
boxplot(df2_Mar$CO,plot=FALSE)$out
outliers <- boxplot(df2_Mar$CO,plot=FALSE)$out
print(outliers)
df2_Mar[which(df2_Mar$CO %in% outliers),]
df2_Mar<-df2_Mar[-which(df2_Mar$CO %in% outliers),]
boxplot(df2_Mar$NO)
boxplot(df2_Mar$NO,plot=FALSE)$out
outliers <- boxplot(df2_Mar$NO,plot=FALSE)$out
print(outliers)
df2_Mar[which(df2_Mar$NO %in% outliers),]
df2_Mar<-df2_Mar[-which(df2_Mar$NO %in% outliers),]
boxplot(df2_Mar$NO2)
boxplot(df2_Mar$NO2,plot=FALSE)$out
outliers <- boxplot(df2_Mar$NO2,plot=FALSE)$out
print(outliers)
df2_Mar[which(df2_Mar$NO2 %in% outliers),]
df2_Mar<-df2_Mar[-which(df2_Mar$NO2 %in% outliers),]
boxplot(df2_Mar$Humidity)
boxplot(df2_Mar$Humidity,plot=FALSE)$out
outliers <- boxplot(df2_Mar$Humidity,plot=FALSE)$out
print(outliers)
df2_Mar[which(df2_Mar$Humidity %in% outliers),]
df2_Mar<-df2_Mar[-which(df2_Mar$Humidity %in% outliers),]
boxplot(df2_Mar$O3)
boxplot(df2_Mar$O3,plot=FALSE)$out
outliers <- boxplot(df2_Mar$O3,plot=FALSE)$out
print(outliers)
df2_Mar[which(df2_Mar$O3 %in% outliers),]
df2_Mar<-df2_Mar[-which(df2_Mar$O3 %in% outliers),]
boxplot(df2_Mar$PM.4)
boxplot(df2_Mar$PM.4,plot=FALSE)$out
outliers <- boxplot(df2_Mar$PM.4,plot=FALSE)$out
print(outliers)
df2_Mar[which(df2_Mar$PM.4 %in% outliers),]
df2_Mar<-df2_Mar[-which(df2_Mar$PM.4 %in% outliers),]
boxplot(df2_Mar$PM1)
boxplot(df2_Mar$PM1,plot=FALSE)$out
outliers <- boxplot(df2_Mar$PM1,plot=FALSE)$out
print(outliers)
df2_Mar[which(df2_Mar$PM1 %in% outliers),]
df2_Mar<-df2_Mar[-which(df2_Mar$PM1 %in% outliers),]
boxplot(df2_Mar$PM10)
boxplot(df2_Mar$PM10,plot=FALSE)$out
outliers <- boxplot(df2_Mar$PM10,plot=FALSE)$out
print(outliers)
df2_Mar[which(df2_Mar$PM10 %in% outliers),]
df2_Mar<-df2_Mar[-which(df2_Mar$PM10 %in% outliers),]
boxplot(df2_Mar$PM2.5)
boxplot(df2_Mar$PM2.5,plot=FALSE)$out
outliers <- boxplot(df2_Mar$PM2.5,plot=FALSE)$out
print(outliers)
df2_Mar[which(df2_Mar$PM2.5 %in% outliers),]
df2_Mar<-df2_Mar[-which(df2_Mar$PM2.5 %in% outliers),]

#####reshape excel file
library(reshape2)
melted_Mar <- melt(df2_Mar, id.vars = c("Date_Time")  )#####melt excel file to a new df

library(tidyr)
melted_Mar <- melted_Mar%>% drop_na()#remove rows with NA in melted df
#write.csv(melted, "melted.csv")

library(lubridate)
melted_Mar$Date_Time <-ymd_hms(melted_Mar$Date_Time)
str(melted_Feb)


mydf_Mar <- melted_Mar
str(mydf_Mar)
mydf_Mar$Date_Time <-as_datetime(mydf_Mar$Date_Time)
str(mydf_Mar)



mydf_Mar_list <- split( mydf_Mar , f = mydf_Mar$variable )##save each parameter as a separate df with its name in mydf_list

length(mydf_Mar_list)###check of number of parameters as df in the list 

##
library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

Mean_lists_week = list()
for (i in 1:length(mydf_Mar_list)) {
  
  m <-mydf_list[[i]]%>%
    mutate(week = round_date(Date_Time, "week")) %>%
    group_by(week) %>%
    summarySE(measurevar="value", groupvars=c("week"))
  
  listname <- paste("Mean_by_week_", names(mydf_Mar_list[i]))
  
  Mean_lists_week[[listname]] <-m
  
}
############
##or by day

Mean_lists_day = list()
for (i in 1:length(mydf_Mar_list)) {
  
  m <-mydf_list[[i]]%>%
    mutate(day = round_date(Date_Time, "day")) %>%
    group_by(day) %>%
    summarySE(measurevar="value", groupvars=c("day"))
  
  listname <- paste("Mean_by_day_", names(mydf_Mar_list[i]))
  
  Mean_lists_day[[listname]] <-m
  
}

CO_df <- mydf_Mar_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-CO_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO", x="Day")+
  theme_bw()
ggsave("CO Daily Mean Mar .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-CO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()
ggsave("CO Weekly Mean Mar .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-CO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()###needs some time

ggsave("CO Weekly Boxplot Mar .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################

p<-NO_df <- mydf_Mar_list[2]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO", x="Day")+
  theme_bw()
ggsave("NO Daily Mean Mar .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()
ggsave("NO Weekly Mean Mar .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-NO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()###needs some time

ggsave("NO Weekly Boxplot Mar .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###############################################################################################

NO2_df <- mydf_Mar_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO2_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2", x="Day")+
  theme_bw()
ggsave("NO2 Daily Mean Mar .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO2_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()
ggsave("NO2 Weekly Mean Mar .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-NO2_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()###needs some time

ggsave("NO2 Weekly Boxplot Mar .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

########################################################################################

Hum_df <- mydf_Mar_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Hum_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity", x="Day")+
  theme_bw()
ggsave("Hum Daily Mean Mar .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Hum_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()
ggsave("Hum Weekly Mean Mar .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Hum_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()###needs some time

ggsave("Hum Weekly Boxplot Mar .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
O3_df <- mydf_Mar_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-O3_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="O3", x="Day")+
  theme_bw()
ggsave("O3 Daily Mean Mar .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-O3_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="O3",x="Week")+
  theme_bw()
ggsave("O3 Weekly Mean Mar .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-O3_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="O3",x="Week")+
  theme_bw()###needs some time

ggsave("O3 Weekly Boxplot Mar .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Pressure_df <- mydf_Mar_list[6]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Pressure_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Pressure", x="Day")+
  theme_bw()
ggsave("Pressure Daily Mean Mar .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Pressure_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Pressure",x="Week")+
  theme_bw()
ggsave("Pressure Weekly Mean Mar .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Pressure_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Pressure",x="Week")+
  theme_bw()###needs some time

ggsave("Pressure Weekly Boxplot Mar .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Temp_df <- mydf_Mar_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Temp_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature", x="Day")+
  theme_bw()
ggsave("Temperature Daily Mean Mar .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Temp_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()
ggsave("Temperature Weekly Mean Mar .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Temp_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()###needs some time

ggsave("Temperature Weekly Boxplot Mar .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Veh_df <- mydf_Mar_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Veh_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Vehicles", x="Day")+
  theme_bw()
ggsave("Vehicles Daily Mean Mar .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Veh_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Vehicles",x="Week")+
  theme_bw()
ggsave("Vehicles Weekly Mean Mar .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Veh_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Vehicles",x="Week")+
  theme_bw()###needs some time

ggsave("Vehicles Weekly Boxplot Mar .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Part_df <- mydf_Mar_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Part_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Particle Count", x="Day")+
  theme_bw()
ggsave("Particle Count Daily Mean Mar .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Part_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Particle Count",x="Week")+
  theme_bw()
ggsave("Particle Count Weekly Mean Mar .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Part_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Particle Count",x="Week")+
  theme_bw()###needs some time

ggsave("Particle Count Weekly Boxplot Mar .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM4_df <- mydf_Mar_list[10]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM4_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM4", x="Day")+
  theme_bw()
ggsave("PM4 Daily Mean Mar .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM4_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM4",x="Week")+
  theme_bw()
ggsave("PM4 Weekly Mean Mar .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM4_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM4",x="Week")+
  theme_bw()###needs some time

ggsave("PM4 Weekly Boxplot Mar .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM1_df <- mydf_Mar_list[11]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM1_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM1", x="Day")+
  theme_bw()
ggsave("PM1 Daily Mean Mar .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM1_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM1",x="Week")+
  theme_bw()
ggsave("PM1 Weekly Mean Mar .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM1_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM1",x="Week")+
  theme_bw()###needs some time

ggsave("PM1 Weekly Boxplot Mar .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM10_df <- mydf_Mar_list[12]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM10_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM10", x="Day")+
  theme_bw()
ggsave("PM10 Daily Mean Mar .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM10_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM10",x="Week")+
  theme_bw()
ggsave("PM10 Weekly Mean Mar .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM10_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM10",x="Week")+
  theme_bw()###needs some time

ggsave("PM10 Weekly Boxplot Mar .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM2.5_df <- mydf_Mar_list[13]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM2.5_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM2.5", x="Day")+
  theme_bw()
ggsave("PM2.5 Daily Mean Mar .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM2.5_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM2.5",x="Week")+
  theme_bw()
ggsave("PM2.5 Weekly Mean Mar .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM2.5_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM2.5",x="Week")+
  theme_bw()###needs some time

ggsave("PM2.5 Weekly Boxplot Mar .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#############################################################################################
#######################################################################################################
#######################################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Apr")###this is the location of files
df <- read.csv("Haymarket Apr Clean.csv", header= TRUE)
str(df)

df2 <- df[, -c(15, 16)]



list_clnms = c(colnames(df2))

for (i in 1:length(list_clnms)){
  df2[, list_clnms[i]] <- ifelse(df2[, list_clnms[i]]>=0, df2[, list_clnms[i]], NA)
  
}

str(df2)

library(plyr)

numcolwise(sum)(df2, na.rm=TRUE)###sum of numeric columns in df2

###it appears one of columns is empty? so we will have 15 variables not 16

boxplot(df2$CO)
boxplot(df2$CO,plot=FALSE)$out
outliers <- boxplot(df2$CO,plot=FALSE)$out
print(outliers)
df2[which(df2$CO %in% outliers),]
df2<-df2[-which(df2$CO %in% outliers),]
boxplot(df2$NO)
boxplot(df2$NO,plot=FALSE)$out
outliers <- boxplot(df2$NO,plot=FALSE)$out
print(outliers)
df2[which(df2$NO %in% outliers),]
df2<-df2[-which(df2$NO %in% outliers),]
boxplot(df2$NO2)
boxplot(df2$NO2,plot=FALSE)$out
outliers <- boxplot(df2$NO2,plot=FALSE)$out
print(outliers)
df2[which(df2$NO2 %in% outliers),]
df2<-df2[-which(df2$NO2 %in% outliers),]
boxplot(df2$Humidity)
boxplot(df2$Humidity,plot=FALSE)$out
outliers <- boxplot(df2$Humidity,plot=FALSE)$out
print(outliers)
df2[which(df2$Humidity %in% outliers),]
df2<-df2[-which(df2$Humidity %in% outliers),]
boxplot(df2$O3)
boxplot(df2$O3,plot=FALSE)$out
outliers <- boxplot(df2$O3,plot=FALSE)$out
print(outliers)
df2[which(df2$O3 %in% outliers),]
df2<-df2[-which(df2$O3 %in% outliers),]
boxplot(df2$PM.4)
boxplot(df2$PM.4,plot=FALSE)$out
outliers <- boxplot(df2$PM.4,plot=FALSE)$out
print(outliers)
df2[which(df2$PM.4 %in% outliers),]
df2<-df2[-which(df2$PM.4 %in% outliers),]
boxplot(df2$PM1)
boxplot(df2$PM1,plot=FALSE)$out
outliers <- boxplot(df2$PM1,plot=FALSE)$out
print(outliers)
df2[which(df2$PM1 %in% outliers),]
df2<-df2[-which(df2$PM1 %in% outliers),]
boxplot(df2$PM10)
boxplot(df2$PM10,plot=FALSE)$out
outliers <- boxplot(df2$PM10,plot=FALSE)$out
print(outliers)
df2[which(df2$PM10 %in% outliers),]
df2<-df2[-which(df2$PM10 %in% outliers),]
boxplot(df2$PM2.5)
boxplot(df2$PM2.5,plot=FALSE)$out
outliers <- boxplot(df2$PM2.5,plot=FALSE)$out
print(outliers)
df2[which(df2$PM2.5 %in% outliers),]
df2<-df2[-which(df2$PM2.5 %in% outliers),]

#####reshape excel file
library(reshape2)
melted <- melt(df2, id.vars = c("Date_Time")  )#####melt excel file to a new df

library(tidyr)
melted <- melted%>% drop_na()#remove rows with NA in melted df
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

##
library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

Mean_lists_week = list()
for (i in 1:length(mydf_list)) {
  
  m <-mydf_list[[i]]%>%
    mutate(week = round_date(Date_Time, "week")) %>%
    group_by(week) %>%
    summarySE(measurevar="value", groupvars=c("week"))
  
  listname <- paste("Mean_by_week_", names(mydf_list[i]))
  
  Mean_lists_week[[listname]] <-m
  
}
############
##or by day

Mean_lists_day = list()
for (i in 1:length(mydf_Mar_list)) {
  
  m <-mydf_list[[i]]%>%
    mutate(day = round_date(Date_Time, "day")) %>%
    group_by(day) %>%
    summarySE(measurevar="value", groupvars=c("day"))
  
  listname <- paste("Mean_by_day_", names(mydf_list[i]))
  
  Mean_lists_day[[listname]] <-m
  
}

CO_df <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-CO_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO", x="Day")+
  theme_bw()
ggsave("CO Daily Mean Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-CO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()
ggsave("CO Weekly Mean Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-CO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()###needs some time

ggsave("CO Weekly Boxplot Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################

p<-NO_df <- mydf_list[2]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO", x="Day")+
  theme_bw()
ggsave("NO Daily Mean Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()
ggsave("NO Weekly Mean Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-NO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()###needs some time

ggsave("NO Weekly Boxplot Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###############################################################################################

NO2_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO2_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2", x="Day")+
  theme_bw()
ggsave("NO2 Daily Mean Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO2_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()
ggsave("NO2 Weekly Mean Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-NO2_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()###needs some time

ggsave("NO2 Weekly Boxplot Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

########################################################################################

Hum_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Hum_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity", x="Day")+
  theme_bw()
ggsave("Hum Daily Mean Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Hum_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()
ggsave("Hum Weekly Mean Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Hum_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()###needs some time

ggsave("Hum Weekly Boxplot Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
O3_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-O3_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="O3", x="Day")+
  theme_bw()
ggsave("O3 Daily Mean Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-O3_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="O3",x="Week")+
  theme_bw()
ggsave("O3 Weekly Mean Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-O3_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="O3",x="Week")+
  theme_bw()###needs some time

ggsave("O3 Weekly Boxplot Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Pressure_df <- mydf_list[6]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Pressure_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Pressure", x="Day")+
  theme_bw()
ggsave("Pressure Daily Mean Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Pressure_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Pressure",x="Week")+
  theme_bw()
ggsave("Pressure Weekly Mean Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Pressure_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Pressure",x="Week")+
  theme_bw()###needs some time

ggsave("Pressure Weekly Boxplot Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Temp_df <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Temp_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature", x="Day")+
  theme_bw()
ggsave("Temperature Daily Mean Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Temp_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()
ggsave("Temperature Weekly Mean Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Temp_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()###needs some time

ggsave("Temperature Weekly Boxplot Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Veh_df <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Veh_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Vehicles", x="Day")+
  theme_bw()
ggsave("Vehicles Daily Mean Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Veh_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Vehicles",x="Week")+
  theme_bw()
ggsave("Vehicles Weekly Mean Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Veh_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Vehicles",x="Week")+
  theme_bw()###needs some time

ggsave("Vehicles Weekly Boxplot Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Part_df <- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Part_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Particle Count", x="Day")+
  theme_bw()
ggsave("Particle Count Daily Mean Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Part_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Particle Count",x="Week")+
  theme_bw()
ggsave("Particle Count Weekly Mean Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Part_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Particle Count",x="Week")+
  theme_bw()###needs some time

ggsave("Particle Count Weekly Boxplot Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM4_df <- mydf_list[10]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM4_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM4", x="Day")+
  theme_bw()
ggsave("PM4 Daily Mean Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM4_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM4",x="Week")+
  theme_bw()
ggsave("PM4 Weekly Mean Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM4_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM4",x="Week")+
  theme_bw()###needs some time

ggsave("PM4 Weekly Boxplot Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM1_df <- mydf_list[11]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM1_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM1", x="Day")+
  theme_bw()
ggsave("PM1 Daily Mean Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM1_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM1",x="Week")+
  theme_bw()
ggsave("PM1 Weekly Mean Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM1_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM1",x="Week")+
  theme_bw()###needs some time

ggsave("PM1 Weekly Boxplot Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM10_df <- mydf_list[12]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM10_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM10", x="Day")+
  theme_bw()
ggsave("PM10 Daily Mean Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM10_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM10",x="Week")+
  theme_bw()
ggsave("PM10 Weekly Mean Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM10_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM10",x="Week")+
  theme_bw()###needs some time

ggsave("PM10 Weekly Boxplot Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM2.5_df <- mydf_list[13]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM2.5_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM2.5", x="Day")+
  theme_bw()
ggsave("PM2.5 Daily Mean Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM2.5_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM2.5",x="Week")+
  theme_bw()
ggsave("PM2.5 Weekly Mean Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM2.5_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM2.5",x="Week")+
  theme_bw()###needs some time

ggsave("PM2.5 Weekly Boxplot Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#############################################################################################
#######################################################################################################
#######################################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_May")###this is the location of  files
df <- read.csv("Haymarket May Clean.csv", header= TRUE)
str(df)

#create new df which is df2 without columns 17 1nd 18
df2 <- df[, -c(15, 16)]###lets get rid of these columns for now
str(df2)



### if we want replace bad values (here minus values) with NA (or any value)
list_clnms = c(colnames(df2))

for (i in 1:length(list_clnms)){
  df2[, list_clnms[i]] <- ifelse(df2[, list_clnms[i]]>=0, df2[, list_clnms[i]], NA)
  
}

str(df2)

library(plyr)

numcolwise(sum)(df2, na.rm=TRUE)###sum of numeric columns in df2

###it appears one of columns is empty? so we will have 15 variables not 16

boxplot(df2$CO)
boxplot(df2$CO,plot=FALSE)$out
outliers <- boxplot(df2$CO,plot=FALSE)$out
print(outliers)
df2[which(df2$CO %in% outliers),]
df2<-df2[-which(df2$CO %in% outliers),]
boxplot(df2$NO)
boxplot(df2$NO,plot=FALSE)$out
outliers <- boxplot(df2$NO,plot=FALSE)$out
print(outliers)
df2[which(df2$NO %in% outliers),]
df2<-df2[-which(df2$NO %in% outliers),]
boxplot(df2$NO2)
boxplot(df2$NO2,plot=FALSE)$out
outliers <- boxplot(df2$NO2,plot=FALSE)$out
print(outliers)
df2[which(df2$NO2 %in% outliers),]
df2<-df2[-which(df2$NO2 %in% outliers),]
boxplot(df2$Humidity)
boxplot(df2$Humidity,plot=FALSE)$out
outliers <- boxplot(df2$Humidity,plot=FALSE)$out
print(outliers)
df2[which(df2$Humidity %in% outliers),]
df2<-df2[-which(df2$Humidity %in% outliers),]
boxplot(df2$O3)
boxplot(df2$O3,plot=FALSE)$out
outliers <- boxplot(df2$O3,plot=FALSE)$out
print(outliers)
df2[which(df2$O3 %in% outliers),]
df2<-df2[-which(df2$O3 %in% outliers),]
boxplot(df2$PM.4)
boxplot(df2$PM.4,plot=FALSE)$out
outliers <- boxplot(df2$PM.4,plot=FALSE)$out
print(outliers)
df2[which(df2$PM.4 %in% outliers),]
df2<-df2[-which(df2$PM.4 %in% outliers),]
boxplot(df2$PM1)
boxplot(df2$PM1,plot=FALSE)$out
outliers <- boxplot(df2$PM1,plot=FALSE)$out
print(outliers)
df2[which(df2$PM1 %in% outliers),]
df2<-df2[-which(df2$PM1 %in% outliers),]
boxplot(df2$PM10)
boxplot(df2$PM10,plot=FALSE)$out
outliers <- boxplot(df2$PM10,plot=FALSE)$out
print(outliers)
df2[which(df2$PM10 %in% outliers),]
df2<-df2[-which(df2$PM10 %in% outliers),]
boxplot(df2$PM2.5)
boxplot(df2$PM2.5,plot=FALSE)$out
outliers <- boxplot(df2$PM2.5,plot=FALSE)$out
print(outliers)
df2[which(df2$PM2.5 %in% outliers),]
df2<-df2[-which(df2$PM2.5 %in% outliers),]


#####reshape excel file
library(reshape2)
melted <- melt(df2, id.vars = c("Date_Time")  )#####melt excel file to a new df

library(tidyr)
melted <- melted%>% drop_na()#remove rows with NA in melted df
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

##
library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

Mean_lists_week = list()
for (i in 1:length(mydf_list)) {
  
  m <-mydf_list[[i]]%>%
    mutate(week = round_date(Date_Time, "week")) %>%
    group_by(week) %>%
    summarySE(measurevar="value", groupvars=c("week"))
  
  listname <- paste("Mean_by_week_", names(mydf_list[i]))
  
  Mean_lists_week[[listname]] <-m
  
}
############
##or by day

Mean_lists_day = list()
for (i in 1:length(mydf_Mar_list)) {
  
  m <-mydf_list[[i]]%>%
    mutate(day = round_date(Date_Time, "day")) %>%
    group_by(day) %>%
    summarySE(measurevar="value", groupvars=c("day"))
  
  listname <- paste("Mean_by_day_", names(mydf_list[i]))
  
  Mean_lists_day[[listname]] <-m
  
}

CO_df <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-CO_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO", x="Day")+
  theme_bw()
ggsave("CO Daily Mean May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-CO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()
ggsave("CO Weekly Mean May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-CO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()###needs some time

ggsave("CO Weekly Boxplot May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################

p<-NO_df <- mydf_list[2]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO", x="Day")+
  theme_bw()
ggsave("NO Daily Mean May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()
ggsave("NO Weekly Mean May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-NO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()###needs some time

ggsave("NO Weekly Boxplot May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###############################################################################################

NO2_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO2_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2", x="Day")+
  theme_bw()
ggsave("NO2 Daily Mean May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO2_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()
ggsave("NO2 Weekly Mean May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-NO2_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()###needs some time

ggsave("NO2 Weekly Boxplot May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

########################################################################################

Hum_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Hum_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity", x="Day")+
  theme_bw()
ggsave("Hum Daily Mean May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Hum_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()
ggsave("Hum Weekly Mean May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Hum_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()###needs some time

ggsave("Hum Weekly Boxplot May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
O3_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-O3_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="O3", x="Day")+
  theme_bw()
ggsave("O3 Daily Mean May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-O3_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="O3",x="Week")+
  theme_bw()
ggsave("O3 Weekly Mean May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-O3_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="O3",x="Week")+
  theme_bw()###needs some time

ggsave("O3 Weekly Boxplot May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Pressure_df <- mydf_list[6]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Pressure_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Pressure", x="Day")+
  theme_bw()
ggsave("Pressure Daily Mean May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Pressure_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Pressure",x="Week")+
  theme_bw()
ggsave("Pressure Weekly Mean May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Pressure_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Pressure",x="Week")+
  theme_bw()###needs some time

ggsave("Pressure Weekly Boxplot May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Temp_df <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Temp_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature", x="Day")+
  theme_bw()
ggsave("Temperature Daily Mean May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Temp_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()
ggsave("Temperature Weekly Mean May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Temp_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()###needs some time

ggsave("Temperature Weekly Boxplot May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Veh_df <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Veh_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Vehicles", x="Day")+
  theme_bw()
ggsave("Vehicles Daily Mean May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Veh_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Vehicles",x="Week")+
  theme_bw()
ggsave("Vehicles Weekly Mean May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Veh_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Vehicles",x="Week")+
  theme_bw()###needs some time

ggsave("Vehicles Weekly Boxplot May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Part_df <- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Part_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Particle Count", x="Day")+
  theme_bw()
ggsave("Particle Count Daily Mean May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Part_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Particle Count",x="Week")+
  theme_bw()
ggsave("Particle Count Weekly Mean May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Part_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Particle Count",x="Week")+
  theme_bw()###needs some time

ggsave("Particle Count Weekly Boxplot May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM4_df <- mydf_list[10]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM4_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM4", x="Day")+
  theme_bw()
ggsave("PM4 Daily Mean May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM4_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM4",x="Week")+
  theme_bw()
ggsave("PM4 Weekly Mean May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM4_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM4",x="Week")+
  theme_bw()###needs some time

ggsave("PM4 Weekly Boxplot May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM1_df <- mydf_list[11]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM1_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM1", x="Day")+
  theme_bw()
ggsave("PM1 Daily Mean May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM1_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM1",x="Week")+
  theme_bw()
ggsave("PM1 Weekly Mean May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM1_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM1",x="Week")+
  theme_bw()###needs some time

ggsave("PM1 Weekly Boxplot May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM10_df <- mydf_list[12]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM10_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM10", x="Day")+
  theme_bw()
ggsave("PM10 Daily Mean May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM10_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM10",x="Week")+
  theme_bw()
ggsave("PM10 Weekly Mean May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM10_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM10",x="Week")+
  theme_bw()###needs some time

ggsave("PM10 Weekly Boxplot May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM2.5_df <- mydf_list[13]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM2.5_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM2.5", x="Day")+
  theme_bw()
ggsave("PM2.5 Daily Mean May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM2.5_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM2.5",x="Week")+
  theme_bw()
ggsave("PM2.5 Weekly Mean May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM2.5_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM2.5",x="Week")+
  theme_bw()###needs some time

ggsave("PM2.5 Weekly Boxplot May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#############################################################################################
#######################################################################################################
#######################################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Jun")###this is the location of files
df <- read.csv("Haymarket Jun Clean.csv", header= TRUE)
str(df)

df2 <- df[, -c(15, 16)]
str(df2)



list_clnms = c(colnames(df2))

for (i in 1:length(list_clnms)){
  df2[, list_clnms[i]] <- ifelse(df2[, list_clnms[i]]>=0, df2[, list_clnms[i]], NA)
  
}

str(df2)

library(plyr)

numcolwise(sum)(df2, na.rm=TRUE)###sum of numeric columns in df2


boxplot(df2$CO)
boxplot(df2$CO,plot=FALSE)$out
outliers <- boxplot(df2$CO,plot=FALSE)$out
print(outliers)
df2[which(df2$CO %in% outliers),]
df2<-df2[-which(df2$CO %in% outliers),]
boxplot(df2$NO)
boxplot(df2$NO,plot=FALSE)$out
outliers <- boxplot(df2$NO,plot=FALSE)$out
print(outliers)
df2[which(df2$NO %in% outliers),]
df2<-df2[-which(df2$NO %in% outliers),]
boxplot(df2$NO2)
boxplot(df2$NO2,plot=FALSE)$out
outliers <- boxplot(df2$NO2,plot=FALSE)$out
print(outliers)
df2[which(df2$NO2 %in% outliers),]
df2<-df2[-which(df2$NO2 %in% outliers),]
boxplot(df2$Humidity)
boxplot(df2$Humidity,plot=FALSE)$out
outliers <- boxplot(df2$Humidity,plot=FALSE)$out
print(outliers)
df2[which(df2$Humidity %in% outliers),]
df2<-df2[-which(df2$Humidity %in% outliers),]
boxplot(df2$O3)
boxplot(df2$O3,plot=FALSE)$out
outliers <- boxplot(df2$O3,plot=FALSE)$out
print(outliers)
df2[which(df2$O3 %in% outliers),]
df2<-df2[-which(df2$O3 %in% outliers),]
boxplot(df2$PM.4)
boxplot(df2$PM.4,plot=FALSE)$out
outliers <- boxplot(df2$PM.4,plot=FALSE)$out
print(outliers)
df2[which(df2$PM.4 %in% outliers),]
df2<-df2[-which(df2$PM.4 %in% outliers),]
boxplot(df2$PM1)
boxplot(df2$PM1,plot=FALSE)$out
outliers <- boxplot(df2$PM1,plot=FALSE)$out
print(outliers)
df2[which(df2$PM1 %in% outliers),]
df2<-df2[-which(df2$PM1 %in% outliers),]
boxplot(df2$PM10)
boxplot(df2$PM10,plot=FALSE)$out
outliers <- boxplot(df2$PM10,plot=FALSE)$out
print(outliers)
df2[which(df2$PM10 %in% outliers),]
df2<-df2[-which(df2$PM10 %in% outliers),]
boxplot(df2$PM2.5)
boxplot(df2$PM2.5,plot=FALSE)$out
outliers <- boxplot(df2$PM2.5,plot=FALSE)$out
print(outliers)
df2[which(df2$PM2.5 %in% outliers),]
df2<-df2[-which(df2$PM2.5 %in% outliers),]


#####reshape excel file
library(reshape2)
melted <- melt(df2, id.vars = c("Date_Time")  )#####melt excel file to a new df

library(tidyr)
melted <- melted%>% drop_na()#remove rows with NA in melted df
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

##
library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

Mean_lists_week = list()
for (i in 1:length(mydf_list)) {
  
  m <-mydf_list[[i]]%>%
    mutate(week = round_date(Date_Time, "week")) %>%
    group_by(week) %>%
    summarySE(measurevar="value", groupvars=c("week"))
  
  listname <- paste("Mean_by_week_", names(mydf_list[i]))
  
  Mean_lists_week[[listname]] <-m
  
}
############
##or by day

Mean_lists_day = list()
for (i in 1:length(mydf_Mar_list)) {
  
  m <-mydf_list[[i]]%>%
    mutate(day = round_date(Date_Time, "day")) %>%
    group_by(day) %>%
    summarySE(measurevar="value", groupvars=c("day"))
  
  listname <- paste("Mean_by_day_", names(mydf_list[i]))
  
  Mean_lists_day[[listname]] <-m
  
}

CO_df <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-CO_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO", x="Day")+
  theme_bw()
ggsave("CO Daily Mean Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-CO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()
ggsave("CO Weekly Mean Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-CO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()###needs some time

ggsave("CO Weekly Boxplot Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################

p<-NO_df <- mydf_list[2]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO", x="Day")+
  theme_bw()
ggsave("NO Daily Mean Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()
ggsave("NO Weekly Mean Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-NO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()###needs some time

ggsave("NO Weekly Boxplot Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###############################################################################################

NO2_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO2_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2", x="Day")+
  theme_bw()
ggsave("NO2 Daily Mean Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO2_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()
ggsave("NO2 Weekly Mean Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-NO2_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()###needs some time

ggsave("NO2 Weekly Boxplot Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

########################################################################################

Hum_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Hum_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity", x="Day")+
  theme_bw()
ggsave("Hum Daily Mean Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Hum_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()
ggsave("Hum Weekly Mean Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Hum_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()###needs some time

ggsave("Hum Weekly Boxplot Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
O3_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-O3_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="O3", x="Day")+
  theme_bw()
ggsave("O3 Daily Mean Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-O3_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="O3",x="Week")+
  theme_bw()
ggsave("O3 Weekly Mean Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-O3_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="O3",x="Week")+
  theme_bw()###needs some time

ggsave("O3 Weekly Boxplot Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Pressure_df <- mydf_list[6]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Pressure_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Pressure", x="Day")+
  theme_bw()
ggsave("Pressure Daily Mean Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Pressure_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Pressure",x="Week")+
  theme_bw()
ggsave("Pressure Weekly Mean Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Pressure_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Pressure",x="Week")+
  theme_bw()###needs some time

ggsave("Pressure Weekly Boxplot Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Temp_df <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Temp_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature", x="Day")+
  theme_bw()
ggsave("Temperature Daily Mean Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Temp_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()
ggsave("Temperature Weekly Mean Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Temp_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()###needs some time

ggsave("Temperature Weekly Boxplot Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Veh_df <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Veh_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Vehicles", x="Day")+
  theme_bw()
ggsave("Vehicles Daily Mean Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Veh_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Vehicles",x="Week")+
  theme_bw()
ggsave("Vehicles Weekly Mean Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Veh_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Vehicles",x="Week")+
  theme_bw()###needs some time

ggsave("Vehicles Weekly Boxplot Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Part_df <- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Part_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Particle Count", x="Day")+
  theme_bw()
ggsave("Particle Count Daily Mean Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Part_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Particle Count",x="Week")+
  theme_bw()
ggsave("Particle Count Weekly Mean Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Part_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Particle Count",x="Week")+
  theme_bw()###needs some time

ggsave("Particle Count Weekly Boxplot Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM4_df <- mydf_list[10]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM4_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM4", x="Day")+
  theme_bw()
ggsave("PM4 Daily Mean Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM4_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM4",x="Week")+
  theme_bw()
ggsave("PM4 Weekly Mean Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM4_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM4",x="Week")+
  theme_bw()###needs some time

ggsave("PM4 Weekly Boxplot Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM1_df <- mydf_list[11]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM1_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM1", x="Day")+
  theme_bw()
ggsave("PM1 Daily Mean Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM1_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM1",x="Week")+
  theme_bw()
ggsave("PM1 Weekly Mean Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM1_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM1",x="Week")+
  theme_bw()###needs some time

ggsave("PM1 Weekly Boxplot Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM10_df <- mydf_list[12]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM10_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM10", x="Day")+
  theme_bw()
ggsave("PM10 Daily Mean Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM10_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM10",x="Week")+
  theme_bw()
ggsave("PM10 Weekly Mean Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM10_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM10",x="Week")+
  theme_bw()###needs some time

ggsave("PM10 Weekly Boxplot Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM2.5_df <- mydf_list[13]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM2.5_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM2.5", x="Day")+
  theme_bw()
ggsave("PM2.5 Daily Mean Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM2.5_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM2.5",x="Week")+
  theme_bw()
ggsave("PM2.5 Weekly Mean Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM2.5_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM2.5",x="Week")+
  theme_bw()###needs some time

ggsave("PM2.5 Weekly Boxplot Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file
#############################################################################################
#######################################################################################################
#######################################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Jul")###this is the location of  files
df <- read.csv("Haymarket Jul Clean.csv", header= TRUE)
str(df)

df2 <- df[, -c(15, 16)]
str(df2)



list_clnms = c(colnames(df2))

for (i in 1:length(list_clnms)){
  df2[, list_clnms[i]] <- ifelse(df2[, list_clnms[i]]>=0, df2[, list_clnms[i]], NA)
  
}

str(df2)

library(plyr)

numcolwise(sum)(df2, na.rm=TRUE)###sum of numeric columns in df2


boxplot(df2$CO)
boxplot(df2$CO,plot=FALSE)$out
outliers <- boxplot(df2$CO,plot=FALSE)$out
print(outliers)
df2[which(df2$CO %in% outliers),]
df2<-df2[-which(df2$CO %in% outliers),]
boxplot(df2$NO)
boxplot(df2$NO,plot=FALSE)$out
outliers <- boxplot(df2$NO,plot=FALSE)$out
print(outliers)
df2[which(df2$NO %in% outliers),]
df2<-df2[-which(df2$NO %in% outliers),]
boxplot(df2$NO2)
boxplot(df2$NO2,plot=FALSE)$out
outliers <- boxplot(df2$NO2,plot=FALSE)$out
print(outliers)
df2[which(df2$NO2 %in% outliers),]
df2<-df2[-which(df2$NO2 %in% outliers),]
boxplot(df2$Humidity)
boxplot(df2$Humidity,plot=FALSE)$out
outliers <- boxplot(df2$Humidity,plot=FALSE)$out
print(outliers)
df2[which(df2$Humidity %in% outliers),]
df2<-df2[-which(df2$Humidity %in% outliers),]
boxplot(df2$O3)
boxplot(df2$O3,plot=FALSE)$out
outliers <- boxplot(df2$O3,plot=FALSE)$out
print(outliers)
df2[which(df2$O3 %in% outliers),]
df2<-df2[-which(df2$O3 %in% outliers),]
boxplot(df2$PM.4)
boxplot(df2$PM.4,plot=FALSE)$out
outliers <- boxplot(df2$PM.4,plot=FALSE)$out
print(outliers)
df2[which(df2$PM.4 %in% outliers),]
df2<-df2[-which(df2$PM.4 %in% outliers),]
boxplot(df2$PM1)
boxplot(df2$PM1,plot=FALSE)$out
outliers <- boxplot(df2$PM1,plot=FALSE)$out
print(outliers)
df2[which(df2$PM1 %in% outliers),]
df2<-df2[-which(df2$PM1 %in% outliers),]
boxplot(df2$PM10)
boxplot(df2$PM10,plot=FALSE)$out
outliers <- boxplot(df2$PM10,plot=FALSE)$out
print(outliers)
df2[which(df2$PM10 %in% outliers),]
df2<-df2[-which(df2$PM10 %in% outliers),]
boxplot(df2$PM2.5)
boxplot(df2$PM2.5,plot=FALSE)$out
outliers <- boxplot(df2$PM2.5,plot=FALSE)$out
print(outliers)
df2[which(df2$PM2.5 %in% outliers),]
df2<-df2[-which(df2$PM2.5 %in% outliers),]


#####reshape excel file
library(reshape2)
melted <- melt(df2, id.vars = c("Date_Time")  )#####melt excel file to a new df

library(tidyr)
melted <- melted%>% drop_na()#remove rows with NA in melted df
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

##
library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

Mean_lists_week = list()
for (i in 1:length(mydf_list)) {
  
  m <-mydf_list[[i]]%>%
    mutate(week = round_date(Date_Time, "week")) %>%
    group_by(week) %>%
    summarySE(measurevar="value", groupvars=c("week"))
  
  listname <- paste("Mean_by_week_", names(mydf_list[i]))
  
  Mean_lists_week[[listname]] <-m
  
}
############
##or by day

Mean_lists_day = list()
for (i in 1:length(mydf_Mar_list)) {
  
  m <-mydf_list[[i]]%>%
    mutate(day = round_date(Date_Time, "day")) %>%
    group_by(day) %>%
    summarySE(measurevar="value", groupvars=c("day"))
  
  listname <- paste("Mean_by_day_", names(mydf_list[i]))
  
  Mean_lists_day[[listname]] <-m
  
}

CO_df <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-CO_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO", x="Day")+
  theme_bw()
ggsave("CO Daily Mean Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-CO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()
ggsave("CO Weekly Mean Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-CO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()###needs some time

ggsave("CO Weekly Boxplot Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################

p<-NO_df <- mydf_list[2]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO", x="Day")+
  theme_bw()
ggsave("NO Daily Mean Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()
ggsave("NO Weekly Mean Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-NO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()###needs some time

ggsave("NO Weekly Boxplot Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###############################################################################################

NO2_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO2_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2", x="Day")+
  theme_bw()
ggsave("NO2 Daily Mean Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO2_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()
ggsave("NO2 Weekly Mean Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-NO2_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()###needs some time

ggsave("NO2 Weekly Boxplot Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

########################################################################################

Hum_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Hum_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity", x="Day")+
  theme_bw()
ggsave("Hum Daily Mean Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Hum_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()
ggsave("Hum Weekly Mean Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Hum_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()###needs some time

ggsave("Hum Weekly Boxplot Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
O3_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-O3_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="O3", x="Day")+
  theme_bw()
ggsave("O3 Daily Mean Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-O3_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="O3",x="Week")+
  theme_bw()
ggsave("O3 Weekly Mean Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-O3_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="O3",x="Week")+
  theme_bw()###needs some time

ggsave("O3 Weekly Boxplot Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Pressure_df <- mydf_list[6]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Pressure_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Pressure", x="Day")+
  theme_bw()
ggsave("Pressure Daily Mean Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Pressure_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Pressure",x="Week")+
  theme_bw()
ggsave("Pressure Weekly Mean Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Pressure_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Pressure",x="Week")+
  theme_bw()###needs some time

ggsave("Pressure Weekly Boxplot Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Temp_df <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Temp_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature", x="Day")+
  theme_bw()
ggsave("Temperature Daily Mean Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Temp_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()
ggsave("Temperature Weekly Mean Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Temp_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()###needs some time

ggsave("Temperature Weekly Boxplot Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Veh_df <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Veh_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Vehicles", x="Day")+
  theme_bw()
ggsave("Vehicles Daily Mean Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Veh_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Vehicles",x="Week")+
  theme_bw()
ggsave("Vehicles Weekly Mean Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Veh_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Vehicles",x="Week")+
  theme_bw()###needs some time

ggsave("Vehicles Weekly Boxplot Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Part_df <- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Part_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Particle Count", x="Day")+
  theme_bw()
ggsave("Particle Count Daily Mean Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Part_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Particle Count",x="Week")+
  theme_bw()
ggsave("Particle Count Weekly Mean Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Part_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Particle Count",x="Week")+
  theme_bw()###needs some time

ggsave("Particle Count Weekly Boxplot Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM4_df <- mydf_list[10]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM4_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM4", x="Day")+
  theme_bw()
ggsave("PM4 Daily Mean Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM4_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM4",x="Week")+
  theme_bw()
ggsave("PM4 Weekly Mean Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM4_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM4",x="Week")+
  theme_bw()###needs some time

ggsave("PM4 Weekly Boxplot Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM1_df <- mydf_list[11]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM1_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM1", x="Day")+
  theme_bw()
ggsave("PM1 Daily Mean Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM1_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM1",x="Week")+
  theme_bw()
ggsave("PM1 Weekly Mean Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM1_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM1",x="Week")+
  theme_bw()###needs some time

ggsave("PM1 Weekly Boxplot Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM10_df <- mydf_list[12]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM10_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM10", x="Day")+
  theme_bw()
ggsave("PM10 Daily Mean Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM10_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM10",x="Week")+
  theme_bw()
ggsave("PM10 Weekly Mean Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM10_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM10",x="Week")+
  theme_bw()###needs some time

ggsave("PM10 Weekly Boxplot Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM2.5_df <- mydf_list[13]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM2.5_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM2.5", x="Day")+
  theme_bw()
ggsave("PM2.5 Daily Mean Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM2.5_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM2.5",x="Week")+
  theme_bw()
ggsave("PM2.5 Weekly Mean Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM2.5_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM2.5",x="Week")+
  theme_bw()###needs some time

ggsave("PM2.5 Weekly Boxplot Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#############################################################################################
#######################################################################################################
#######################################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Aug")###this is the location of files
df <- read.csv("Haymarket Aug Clean.csv", header= TRUE)
str(df)

df2 <- df[, -c(15, 16)]
str(df2)



list_clnms = c(colnames(df2))

for (i in 1:length(list_clnms)){
  df2[, list_clnms[i]] <- ifelse(df2[, list_clnms[i]]>=0, df2[, list_clnms[i]], NA)
  
}

str(df2)

library(plyr)

numcolwise(sum)(df2, na.rm=TRUE)###sum of numeric columns in df2


boxplot(df2$CO)
boxplot(df2$CO,plot=FALSE)$out
outliers <- boxplot(df2$CO,plot=FALSE)$out
print(outliers)
df2[which(df2$CO %in% outliers),]
df2<-df2[-which(df2$CO %in% outliers),]
boxplot(df2$NO)
boxplot(df2$NO,plot=FALSE)$out
outliers <- boxplot(df2$NO,plot=FALSE)$out
print(outliers)
df2[which(df2$NO %in% outliers),]
df2<-df2[-which(df2$NO %in% outliers),]
boxplot(df2$NO2)
boxplot(df2$NO2,plot=FALSE)$out
outliers <- boxplot(df2$NO2,plot=FALSE)$out
print(outliers)
df2[which(df2$NO2 %in% outliers),]
df2<-df2[-which(df2$NO2 %in% outliers),]
boxplot(df2$Humidity)
boxplot(df2$Humidity,plot=FALSE)$out
outliers <- boxplot(df2$Humidity,plot=FALSE)$out
print(outliers)
df2[which(df2$Humidity %in% outliers),]
df2<-df2[-which(df2$Humidity %in% outliers),]
boxplot(df2$O3)
boxplot(df2$O3,plot=FALSE)$out
outliers <- boxplot(df2$O3,plot=FALSE)$out
print(outliers)
df2[which(df2$O3 %in% outliers),]
df2<-df2[-which(df2$O3 %in% outliers),]
boxplot(df2$PM.4)
boxplot(df2$PM.4,plot=FALSE)$out
outliers <- boxplot(df2$PM.4,plot=FALSE)$out
print(outliers)
df2[which(df2$PM.4 %in% outliers),]
df2<-df2[-which(df2$PM.4 %in% outliers),]
boxplot(df2$PM1)
boxplot(df2$PM1,plot=FALSE)$out
outliers <- boxplot(df2$PM1,plot=FALSE)$out
print(outliers)
df2[which(df2$PM1 %in% outliers),]
df2<-df2[-which(df2$PM1 %in% outliers),]
boxplot(df2$PM10)
boxplot(df2$PM10,plot=FALSE)$out
outliers <- boxplot(df2$PM10,plot=FALSE)$out
print(outliers)
df2[which(df2$PM10 %in% outliers),]
df2<-df2[-which(df2$PM10 %in% outliers),]
boxplot(df2$PM2.5)
boxplot(df2$PM2.5,plot=FALSE)$out
outliers <- boxplot(df2$PM2.5,plot=FALSE)$out
print(outliers)
df2[which(df2$PM2.5 %in% outliers),]
df2<-df2[-which(df2$PM2.5 %in% outliers),]


#####reshape excel file
library(reshape2)
melted <- melt(df2, id.vars = c("Date_Time")  )#####melt excel file to a new df

library(tidyr)
melted <- melted%>% drop_na()#remove rows with NA in melted df
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

##
library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

Mean_lists_week = list()
for (i in 1:length(mydf_list)) {
  
  m <-mydf_list[[i]]%>%
    mutate(week = round_date(Date_Time, "week")) %>%
    group_by(week) %>%
    summarySE(measurevar="value", groupvars=c("week"))
  
  listname <- paste("Mean_by_week_", names(mydf_list[i]))
  
  Mean_lists_week[[listname]] <-m
  
}
############
##or by day

Mean_lists_day = list()
for (i in 1:length(mydf_Mar_list)) {
  
  m <-mydf_list[[i]]%>%
    mutate(day = round_date(Date_Time, "day")) %>%
    group_by(day) %>%
    summarySE(measurevar="value", groupvars=c("day"))
  
  listname <- paste("Mean_by_day_", names(mydf_list[i]))
  
  Mean_lists_day[[listname]] <-m
  
}

CO_df <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-CO_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO", x="Day")+
  theme_bw()
ggsave("CO Daily Mean Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-CO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()
ggsave("CO Weekly Mean Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-CO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()###needs some time

ggsave("CO Weekly Boxplot Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################

p<-NO_df <- mydf_list[2]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO", x="Day")+
  theme_bw()
ggsave("NO Daily Mean Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()
ggsave("NO Weekly Mean Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-NO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()###needs some time

ggsave("NO Weekly Boxplot Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###############################################################################################

NO2_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO2_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2", x="Day")+
  theme_bw()
ggsave("NO2 Daily Mean Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO2_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()
ggsave("NO2 Weekly Mean Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-NO2_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()###needs some time

ggsave("NO2 Weekly Boxplot Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

########################################################################################

Hum_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Hum_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity", x="Day")+
  theme_bw()
ggsave("Hum Daily Mean Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Hum_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()
ggsave("Hum Weekly Mean Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Hum_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()###needs some time

ggsave("Hum Weekly Boxplot Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
O3_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-O3_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="O3", x="Day")+
  theme_bw()
ggsave("O3 Daily Mean Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-O3_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="O3",x="Week")+
  theme_bw()
ggsave("O3 Weekly Mean Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-O3_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="O3",x="Week")+
  theme_bw()###needs some time

ggsave("O3 Weekly Boxplot Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Pressure_df <- mydf_list[6]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Pressure_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Pressure", x="Day")+
  theme_bw()
ggsave("Pressure Daily Mean Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Pressure_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Pressure",x="Week")+
  theme_bw()
ggsave("Pressure Weekly Mean Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Pressure_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Pressure",x="Week")+
  theme_bw()###needs some time

ggsave("Pressure Weekly Boxplot Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Temp_df <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Temp_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature", x="Day")+
  theme_bw()
ggsave("Temperature Daily Mean Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Temp_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()
ggsave("Temperature Weekly Mean Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Temp_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()###needs some time

ggsave("Temperature Weekly Boxplot Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Veh_df <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Veh_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Vehicles", x="Day")+
  theme_bw()
ggsave("Vehicles Daily Mean Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Veh_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Vehicles",x="Week")+
  theme_bw()
ggsave("Vehicles Weekly Mean Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Veh_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Vehicles",x="Week")+
  theme_bw()###needs some time

ggsave("Vehicles Weekly Boxplot Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Part_df <- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Part_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Particle Count", x="Day")+
  theme_bw()
ggsave("Particle Count Daily Mean Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Part_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Particle Count",x="Week")+
  theme_bw()
ggsave("Particle Count Weekly Mean Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Part_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Particle Count",x="Week")+
  theme_bw()###needs some time

ggsave("Particle Count Weekly Boxplot Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM4_df <- mydf_list[10]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM4_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM4", x="Day")+
  theme_bw()
ggsave("PM4 Daily Mean Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM4_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM4",x="Week")+
  theme_bw()
ggsave("PM4 Weekly Mean Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM4_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM4",x="Week")+
  theme_bw()###needs some time

ggsave("PM4 Weekly Boxplot Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM1_df <- mydf_list[11]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM1_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM1", x="Day")+
  theme_bw()
ggsave("PM1 Daily Mean Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM1_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM1",x="Week")+
  theme_bw()
ggsave("PM1 Weekly Mean Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM1_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM1",x="Week")+
  theme_bw()###needs some time

ggsave("PM1 Weekly Boxplot Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM10_df <- mydf_list[12]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM10_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM10", x="Day")+
  theme_bw()
ggsave("PM10 Daily Mean Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM10_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM10",x="Week")+
  theme_bw()
ggsave("PM10 Weekly Mean Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM10_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM10",x="Week")+
  theme_bw()###needs some time

ggsave("PM10 Weekly Boxplot Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM2.5_df <- mydf_list[13]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM2.5_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM2.5", x="Day")+
  theme_bw()
ggsave("PM2.5 Daily Mean Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM2.5_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM2.5",x="Week")+
  theme_bw()
ggsave("PM2.5 Weekly Mean Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM2.5_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM2.5",x="Week")+
  theme_bw()###needs some time

ggsave("PM2.5 Weekly Boxplot Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#############################################################################################
#######################################################################################################
#######################################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Sep")###this is the location of  files
df <- read.csv("Haymarket Sep Clean.csv", header= TRUE)
str(df)

df2 <- df[, -c(15, 16)]
str(df2)



list_clnms = c(colnames(df2))

for (i in 1:length(list_clnms)){
  df2[, list_clnms[i]] <- ifelse(df2[, list_clnms[i]]>=0, df2[, list_clnms[i]], NA)
  
}

str(df2)

library(plyr)

numcolwise(sum)(df2, na.rm=TRUE)###sum of numeric columns in df2


boxplot(df2$CO)
boxplot(df2$CO,plot=FALSE)$out
outliers <- boxplot(df2$CO,plot=FALSE)$out
print(outliers)
df2[which(df2$CO %in% outliers),]
df2<-df2[-which(df2$CO %in% outliers),]
boxplot(df2$NO)
boxplot(df2$NO,plot=FALSE)$out
outliers <- boxplot(df2$NO,plot=FALSE)$out
print(outliers)
df2[which(df2$NO %in% outliers),]
df2<-df2[-which(df2$NO %in% outliers),]
boxplot(df2$NO2)
boxplot(df2$NO2,plot=FALSE)$out
outliers <- boxplot(df2$NO2,plot=FALSE)$out
print(outliers)
df2[which(df2$NO2 %in% outliers),]
df2<-df2[-which(df2$NO2 %in% outliers),]
boxplot(df2$Humidity)
boxplot(df2$Humidity,plot=FALSE)$out
outliers <- boxplot(df2$Humidity,plot=FALSE)$out
print(outliers)
df2[which(df2$Humidity %in% outliers),]
df2<-df2[-which(df2$Humidity %in% outliers),]
boxplot(df2$O3)
boxplot(df2$O3,plot=FALSE)$out
outliers <- boxplot(df2$O3,plot=FALSE)$out
print(outliers)
df2[which(df2$O3 %in% outliers),]
df2<-df2[-which(df2$O3 %in% outliers),]
boxplot(df2$PM.4)
boxplot(df2$PM.4,plot=FALSE)$out
outliers <- boxplot(df2$PM.4,plot=FALSE)$out
print(outliers)
df2[which(df2$PM.4 %in% outliers),]
df2<-df2[-which(df2$PM.4 %in% outliers),]
boxplot(df2$PM1)
boxplot(df2$PM1,plot=FALSE)$out
outliers <- boxplot(df2$PM1,plot=FALSE)$out
print(outliers)
df2[which(df2$PM1 %in% outliers),]
df2<-df2[-which(df2$PM1 %in% outliers),]
boxplot(df2$PM10)
boxplot(df2$PM10,plot=FALSE)$out
outliers <- boxplot(df2$PM10,plot=FALSE)$out
print(outliers)
df2[which(df2$PM10 %in% outliers),]
df2<-df2[-which(df2$PM10 %in% outliers),]
boxplot(df2$PM2.5)
boxplot(df2$PM2.5,plot=FALSE)$out
outliers <- boxplot(df2$PM2.5,plot=FALSE)$out
print(outliers)
df2[which(df2$PM2.5 %in% outliers),]
df2<-df2[-which(df2$PM2.5 %in% outliers),]


#####reshape excel file
library(reshape2)
melted <- melt(df2, id.vars = c("Date_Time")  )#####melt  excel file to a new df

library(tidyr)
melted <- melted%>% drop_na()#remove rows with NA in melted df
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

##
library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

Mean_lists_week = list()
for (i in 1:length(mydf_list)) {
  
  m <-mydf_list[[i]]%>%
    mutate(week = round_date(Date_Time, "week")) %>%
    group_by(week) %>%
    summarySE(measurevar="value", groupvars=c("week"))
  
  listname <- paste("Mean_by_week_", names(mydf_list[i]))
  
  Mean_lists_week[[listname]] <-m
  
}
############
##or by day

Mean_lists_day = list()
for (i in 1:length(mydf_Mar_list)) {
  
  m <-mydf_list[[i]]%>%
    mutate(day = round_date(Date_Time, "day")) %>%
    group_by(day) %>%
    summarySE(measurevar="value", groupvars=c("day"))
  
  listname <- paste("Mean_by_day_", names(mydf_list[i]))
  
  Mean_lists_day[[listname]] <-m
  
}

CO_df <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-CO_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO", x="Day")+
  theme_bw()
ggsave("CO Daily Mean Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-CO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()
ggsave("CO Weekly Mean Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-CO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()###needs some time

ggsave("CO Weekly Boxplot Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################

p<-NO_df <- mydf_list[2]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO", x="Day")+
  theme_bw()
ggsave("NO Daily Mean Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()
ggsave("NO Weekly Mean Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-NO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()###needs some time

ggsave("NO Weekly Boxplot Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###############################################################################################

NO2_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO2_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2", x="Day")+
  theme_bw()
ggsave("NO2 Daily Mean Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO2_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()
ggsave("NO2 Weekly Mean Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-NO2_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()###needs some time

ggsave("NO2 Weekly Boxplot Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

########################################################################################

Hum_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Hum_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity", x="Day")+
  theme_bw()
ggsave("Hum Daily Mean Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Hum_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()
ggsave("Hum Weekly Mean Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Hum_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()###needs some time

ggsave("Hum Weekly Boxplot Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
O3_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-O3_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="O3", x="Day")+
  theme_bw()
ggsave("O3 Daily Mean Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-O3_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="O3",x="Week")+
  theme_bw()
ggsave("O3 Weekly Mean Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-O3_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="O3",x="Week")+
  theme_bw()###needs some time

ggsave("O3 Weekly Boxplot Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Pressure_df <- mydf_list[6]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Pressure_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Pressure", x="Day")+
  theme_bw()
ggsave("Pressure Daily Mean Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Pressure_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Pressure",x="Week")+
  theme_bw()
ggsave("Pressure Weekly Mean Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Pressure_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Pressure",x="Week")+
  theme_bw()###needs some time

ggsave("Pressure Weekly Boxplot Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Temp_df <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Temp_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature", x="Day")+
  theme_bw()
ggsave("Temperature Daily Mean Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Temp_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()
ggsave("Temperature Weekly Mean Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Temp_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()###needs some time

ggsave("Temperature Weekly Boxplot Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Veh_df <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Veh_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Vehicles", x="Day")+
  theme_bw()
ggsave("Vehicles Daily Mean Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Veh_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Vehicles",x="Week")+
  theme_bw()
ggsave("Vehicles Weekly Mean Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Veh_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Vehicles",x="Week")+
  theme_bw()###needs some time

ggsave("Vehicles Weekly Boxplot Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Part_df <- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Part_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Particle Count", x="Day")+
  theme_bw()
ggsave("Particle Count Daily Mean Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Part_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Particle Count",x="Week")+
  theme_bw()
ggsave("Particle Count Weekly Mean Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Part_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Particle Count",x="Week")+
  theme_bw()###needs some time

ggsave("Particle Count Weekly Boxplot Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM4_df <- mydf_list[10]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM4_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM4", x="Day")+
  theme_bw()
ggsave("PM4 Daily Mean Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM4_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM4",x="Week")+
  theme_bw()
ggsave("PM4 Weekly Mean Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM4_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM4",x="Week")+
  theme_bw()###needs some time

ggsave("PM4 Weekly Boxplot Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM1_df <- mydf_list[11]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM1_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM1", x="Day")+
  theme_bw()
ggsave("PM1 Daily Mean Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM1_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM1",x="Week")+
  theme_bw()
ggsave("PM1 Weekly Mean Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM1_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM1",x="Week")+
  theme_bw()###needs some time

ggsave("PM1 Weekly Boxplot Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM10_df <- mydf_list[12]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM10_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM10", x="Day")+
  theme_bw()
ggsave("PM10 Daily Mean Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM10_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM10",x="Week")+
  theme_bw()
ggsave("PM10 Weekly Mean Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM10_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM10",x="Week")+
  theme_bw()###needs some time

ggsave("PM10 Weekly Boxplot Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM2.5_df <- mydf_list[13]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM2.5_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM2.5", x="Day")+
  theme_bw()
ggsave("PM2.5 Daily Mean Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM2.5_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM2.5",x="Week")+
  theme_bw()
ggsave("PM2.5 Weekly Mean Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM2.5_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM2.5",x="Week")+
  theme_bw()###needs some time

ggsave("PM2.5 Weekly Boxplot Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#############################################################################################
#######################################################################################################
#######################################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Oct")###this is the location of files
df <- read.csv("Haymarket Oct Clean.csv", header= TRUE)
str(df)

df2 <- df[, -c(15, 16)]
str(df2)



list_clnms = c(colnames(df2))

for (i in 1:length(list_clnms)){
  df2[, list_clnms[i]] <- ifelse(df2[, list_clnms[i]]>=0, df2[, list_clnms[i]], NA)
  
}

str(df2)

library(plyr)

numcolwise(sum)(df2, na.rm=TRUE)###sum of numeric columns in df2


boxplot(df2$CO)
boxplot(df2$CO,plot=FALSE)$out
outliers <- boxplot(df2$CO,plot=FALSE)$out
print(outliers)
df2[which(df2$CO %in% outliers),]
df2<-df2[-which(df2$CO %in% outliers),]
boxplot(df2$NO)
boxplot(df2$NO,plot=FALSE)$out
outliers <- boxplot(df2$NO,plot=FALSE)$out
print(outliers)
df2[which(df2$NO %in% outliers),]
df2<-df2[-which(df2$NO %in% outliers),]
boxplot(df2$NO2)
boxplot(df2$NO2,plot=FALSE)$out
outliers <- boxplot(df2$NO2,plot=FALSE)$out
print(outliers)
df2[which(df2$NO2 %in% outliers),]
df2<-df2[-which(df2$NO2 %in% outliers),]
boxplot(df2$Humidity)
boxplot(df2$Humidity,plot=FALSE)$out
outliers <- boxplot(df2$Humidity,plot=FALSE)$out
print(outliers)
df2[which(df2$Humidity %in% outliers),]
df2<-df2[-which(df2$Humidity %in% outliers),]
boxplot(df2$O3)
boxplot(df2$O3,plot=FALSE)$out
outliers <- boxplot(df2$O3,plot=FALSE)$out
print(outliers)
df2[which(df2$O3 %in% outliers),]
df2<-df2[-which(df2$O3 %in% outliers),]
boxplot(df2$PM.4)
boxplot(df2$PM.4,plot=FALSE)$out
outliers <- boxplot(df2$PM.4,plot=FALSE)$out
print(outliers)
df2[which(df2$PM.4 %in% outliers),]
df2<-df2[-which(df2$PM.4 %in% outliers),]
boxplot(df2$PM1)
boxplot(df2$PM1,plot=FALSE)$out
outliers <- boxplot(df2$PM1,plot=FALSE)$out
print(outliers)
df2[which(df2$PM1 %in% outliers),]
df2<-df2[-which(df2$PM1 %in% outliers),]
boxplot(df2$PM10)
boxplot(df2$PM10,plot=FALSE)$out
outliers <- boxplot(df2$PM10,plot=FALSE)$out
print(outliers)
df2[which(df2$PM10 %in% outliers),]
df2<-df2[-which(df2$PM10 %in% outliers),]
boxplot(df2$PM2.5)
boxplot(df2$PM2.5,plot=FALSE)$out
outliers <- boxplot(df2$PM2.5,plot=FALSE)$out
print(outliers)
df2[which(df2$PM2.5 %in% outliers),]
df2<-df2[-which(df2$PM2.5 %in% outliers),]


#####reshape excel file
library(reshape2)
melted <- melt(df2, id.vars = c("Date_Time")  )#####melt excel file to a new df

library(tidyr)
melted <- melted%>% drop_na()#remove rows with NA in melted df
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

##
library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

Mean_lists_week = list()
for (i in 1:length(mydf_list)) {
  
  m <-mydf_list[[i]]%>%
    mutate(week = round_date(Date_Time, "week")) %>%
    group_by(week) %>%
    summarySE(measurevar="value", groupvars=c("week"))
  
  listname <- paste("Mean_by_week_", names(mydf_list[i]))
  
  Mean_lists_week[[listname]] <-m
  
}
############
##or by day

Mean_lists_day = list()
for (i in 1:length(mydf_Mar_list)) {
  
  m <-mydf_list[[i]]%>%
    mutate(day = round_date(Date_Time, "day")) %>%
    group_by(day) %>%
    summarySE(measurevar="value", groupvars=c("day"))
  
  listname <- paste("Mean_by_day_", names(mydf_list[i]))
  
  Mean_lists_day[[listname]] <-m
  
}

CO_df <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-CO_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO", x="Day")+
  theme_bw()
ggsave("CO Daily Mean Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-CO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()
ggsave("CO Weekly Mean Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-CO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()###needs some time

ggsave("CO Weekly Boxplot Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################

p<-NO_df <- mydf_list[2]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO", x="Day")+
  theme_bw()
ggsave("NO Daily Mean Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()
ggsave("NO Weekly Mean Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-NO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()###needs some time

ggsave("NO Weekly Boxplot Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###############################################################################################

NO2_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO2_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2", x="Day")+
  theme_bw()
ggsave("NO2 Daily Mean Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO2_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()
ggsave("NO2 Weekly Mean Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-NO2_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()###needs some time

ggsave("NO2 Weekly Boxplot Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

########################################################################################

Hum_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Hum_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity", x="Day")+
  theme_bw()
ggsave("Hum Daily Mean Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Hum_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()
ggsave("Hum Weekly Mean Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Hum_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()###needs some time

ggsave("Hum Weekly Boxplot Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
O3_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-O3_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="O3", x="Day")+
  theme_bw()
ggsave("O3 Daily Mean Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-O3_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="O3",x="Week")+
  theme_bw()
ggsave("O3 Weekly Mean Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-O3_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="O3",x="Week")+
  theme_bw()###needs some time

ggsave("O3 Weekly Boxplot Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Pressure_df <- mydf_list[6]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Pressure_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Pressure", x="Day")+
  theme_bw()
ggsave("Pressure Daily Mean Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Pressure_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Pressure",x="Week")+
  theme_bw()
ggsave("Pressure Weekly Mean Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Pressure_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Pressure",x="Week")+
  theme_bw()###needs some time

ggsave("Pressure Weekly Boxplot Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Temp_df <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Temp_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature", x="Day")+
  theme_bw()
ggsave("Temperature Daily Mean Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Temp_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()
ggsave("Temperature Weekly Mean Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Temp_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()###needs some time

ggsave("Temperature Weekly Boxplot Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Veh_df <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Veh_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Vehicles", x="Day")+
  theme_bw()
ggsave("Vehicles Daily Mean Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Veh_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Vehicles",x="Week")+
  theme_bw()
ggsave("Vehicles Weekly Mean Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Veh_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Vehicles",x="Week")+
  theme_bw()###needs some time

ggsave("Vehicles Weekly Boxplot Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Part_df <- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Part_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Particle Count", x="Day")+
  theme_bw()
ggsave("Particle Count Daily Mean Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Part_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Particle Count",x="Week")+
  theme_bw()
ggsave("Particle Count Weekly Mean Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Part_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Particle Count",x="Week")+
  theme_bw()###needs some time

ggsave("Particle Count Weekly Boxplot Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM4_df <- mydf_list[10]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM4_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM4", x="Day")+
  theme_bw()
ggsave("PM4 Daily Mean Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM4_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM4",x="Week")+
  theme_bw()
ggsave("PM4 Weekly Mean Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM4_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM4",x="Week")+
  theme_bw()###needs some time

ggsave("PM4 Weekly Boxplot Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM1_df <- mydf_list[11]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM1_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM1", x="Day")+
  theme_bw()
ggsave("PM1 Daily Mean Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM1_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM1",x="Week")+
  theme_bw()
ggsave("PM1 Weekly Mean Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM1_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM1",x="Week")+
  theme_bw()###needs some time

ggsave("PM1 Weekly Boxplot Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM10_df <- mydf_list[12]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM10_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM10", x="Day")+
  theme_bw()
ggsave("PM10 Daily Mean Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM10_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM10",x="Week")+
  theme_bw()
ggsave("PM10 Weekly Mean Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM10_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM10",x="Week")+
  theme_bw()###needs some time

ggsave("PM10 Weekly Boxplot Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM2.5_df <- mydf_list[13]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM2.5_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM2.5", x="Day")+
  theme_bw()
ggsave("PM2.5 Daily Mean Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM2.5_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM2.5",x="Week")+
  theme_bw()
ggsave("PM2.5 Weekly Mean Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM2.5_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM2.5",x="Week")+
  theme_bw()###needs some time

ggsave("PM2.5 Weekly Boxplot Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#############################################################################################
#######################################################################################################
#######################################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Nov")###this is the location of files
df <- read.csv("Haymarket Nov Clean.csv", header= TRUE)
str(df)

df2 <- df[, -c(15, 16)]
str(df2)



list_clnms = c(colnames(df2))

for (i in 1:length(list_clnms)){
  df2[, list_clnms[i]] <- ifelse(df2[, list_clnms[i]]>=0, df2[, list_clnms[i]], NA)
  
}

str(df2)

library(plyr)

numcolwise(sum)(df2, na.rm=TRUE)###sum of numeric columns in df2


boxplot(df2$CO)
boxplot(df2$CO,plot=FALSE)$out
outliers <- boxplot(df2$CO,plot=FALSE)$out
print(outliers)
df2[which(df2$CO %in% outliers),]
df2<-df2[-which(df2$CO %in% outliers),]
boxplot(df2$NO)
boxplot(df2$NO,plot=FALSE)$out
outliers <- boxplot(df2$NO,plot=FALSE)$out
print(outliers)
df2[which(df2$NO %in% outliers),]
df2<-df2[-which(df2$NO %in% outliers),]
boxplot(df2$NO2)
boxplot(df2$NO2,plot=FALSE)$out
outliers <- boxplot(df2$NO2,plot=FALSE)$out
print(outliers)
df2[which(df2$NO2 %in% outliers),]
df2<-df2[-which(df2$NO2 %in% outliers),]
boxplot(df2$Humidity)
boxplot(df2$Humidity,plot=FALSE)$out
outliers <- boxplot(df2$Humidity,plot=FALSE)$out
print(outliers)
df2[which(df2$Humidity %in% outliers),]
df2<-df2[-which(df2$Humidity %in% outliers),]
boxplot(df2$O3)
boxplot(df2$O3,plot=FALSE)$out
outliers <- boxplot(df2$O3,plot=FALSE)$out
print(outliers)
df2[which(df2$O3 %in% outliers),]
df2<-df2[-which(df2$O3 %in% outliers),]
boxplot(df2$PM.4)
boxplot(df2$PM.4,plot=FALSE)$out
outliers <- boxplot(df2$PM.4,plot=FALSE)$out
print(outliers)
df2[which(df2$PM.4 %in% outliers),]
df2<-df2[-which(df2$PM.4 %in% outliers),]
boxplot(df2$PM1)
boxplot(df2$PM1,plot=FALSE)$out
outliers <- boxplot(df2$PM1,plot=FALSE)$out
print(outliers)
df2[which(df2$PM1 %in% outliers),]
df2<-df2[-which(df2$PM1 %in% outliers),]
boxplot(df2$PM10)
boxplot(df2$PM10,plot=FALSE)$out
outliers <- boxplot(df2$PM10,plot=FALSE)$out
print(outliers)
df2[which(df2$PM10 %in% outliers),]
df2<-df2[-which(df2$PM10 %in% outliers),]
boxplot(df2$PM2.5)
boxplot(df2$PM2.5,plot=FALSE)$out
outliers <- boxplot(df2$PM2.5,plot=FALSE)$out
print(outliers)
df2[which(df2$PM2.5 %in% outliers),]
df2<-df2[-which(df2$PM2.5 %in% outliers),]


#####reshape excel file
library(reshape2)
melted <- melt(df2, id.vars = c("Date_Time")  )#####melt excel file to a new df

library(tidyr)
melted <- melted%>% drop_na()#remove rows with NA in melted df
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

##
library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

Mean_lists_week = list()
for (i in 1:length(mydf_list)) {
  
  m <-mydf_list[[i]]%>%
    mutate(week = round_date(Date_Time, "week")) %>%
    group_by(week) %>%
    summarySE(measurevar="value", groupvars=c("week"))
  
  listname <- paste("Mean_by_week_", names(mydf_list[i]))
  
  Mean_lists_week[[listname]] <-m
  
}
############
##or by day

Mean_lists_day = list()
for (i in 1:length(mydf_Mar_list)) {
  
  m <-mydf_list[[i]]%>%
    mutate(day = round_date(Date_Time, "day")) %>%
    group_by(day) %>%
    summarySE(measurevar="value", groupvars=c("day"))
  
  listname <- paste("Mean_by_day_", names(mydf_list[i]))
  
  Mean_lists_day[[listname]] <-m
  
}

CO_df <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-CO_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO", x="Day")+
  theme_bw()
ggsave("CO Daily Mean Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-CO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()
ggsave("CO Weekly Mean Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-CO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()###needs some time

ggsave("CO Weekly Boxplot Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################

p<-NO_df <- mydf_list[2]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO", x="Day")+
  theme_bw()
ggsave("NO Daily Mean Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()
ggsave("NO Weekly Mean Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-NO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()###needs some time

ggsave("NO Weekly Boxplot Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###############################################################################################

NO2_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO2_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2", x="Day")+
  theme_bw()
ggsave("NO2 Daily Mean Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO2_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()
ggsave("NO2 Weekly Mean Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-NO2_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()###needs some time

ggsave("NO2 Weekly Boxplot Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

########################################################################################

Hum_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Hum_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity", x="Day")+
  theme_bw()
ggsave("Hum Daily Mean Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Hum_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()
ggsave("Hum Weekly Mean Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Hum_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()###needs some time

ggsave("Hum Weekly Boxplot Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
O3_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-O3_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="O3", x="Day")+
  theme_bw()
ggsave("O3 Daily Mean Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-O3_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="O3",x="Week")+
  theme_bw()
ggsave("O3 Weekly Mean Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-O3_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="O3",x="Week")+
  theme_bw()###needs some time

ggsave("O3 Weekly Boxplot Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Pressure_df <- mydf_list[6]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Pressure_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Pressure", x="Day")+
  theme_bw()
ggsave("Pressure Daily Mean Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Pressure_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Pressure",x="Week")+
  theme_bw()
ggsave("Pressure Weekly Mean Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Pressure_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Pressure",x="Week")+
  theme_bw()###needs some time

ggsave("Pressure Weekly Boxplot Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Temp_df <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Temp_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature", x="Day")+
  theme_bw()
ggsave("Temperature Daily Mean Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Temp_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()
ggsave("Temperature Weekly Mean Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Temp_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()###needs some time

ggsave("Temperature Weekly Boxplot Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Veh_df <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Veh_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Vehicles", x="Day")+
  theme_bw()
ggsave("Vehicles Daily Mean Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Veh_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Vehicles",x="Week")+
  theme_bw()
ggsave("Vehicles Weekly Mean Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Veh_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Vehicles",x="Week")+
  theme_bw()###needs some time

ggsave("Vehicles Weekly Boxplot Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Part_df <- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Part_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Particle Count", x="Day")+
  theme_bw()
ggsave("Particle Count Daily Mean Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Part_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Particle Count",x="Week")+
  theme_bw()
ggsave("Particle Count Weekly Mean Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Part_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Particle Count",x="Week")+
  theme_bw()###needs some time

ggsave("Particle Count Weekly Boxplot Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM4_df <- mydf_list[10]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM4_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM4", x="Day")+
  theme_bw()
ggsave("PM4 Daily Mean Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM4_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM4",x="Week")+
  theme_bw()
ggsave("PM4 Weekly Mean Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM4_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM4",x="Week")+
  theme_bw()###needs some time

ggsave("PM4 Weekly Boxplot Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM1_df <- mydf_list[11]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM1_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM1", x="Day")+
  theme_bw()
ggsave("PM1 Daily Mean Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM1_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM1",x="Week")+
  theme_bw()
ggsave("PM1 Weekly Mean Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM1_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM1",x="Week")+
  theme_bw()###needs some time

ggsave("PM1 Weekly Boxplot Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM10_df <- mydf_list[12]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM10_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM10", x="Day")+
  theme_bw()
ggsave("PM10 Daily Mean Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM10_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM10",x="Week")+
  theme_bw()
ggsave("PM10 Weekly Mean Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM10_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM10",x="Week")+
  theme_bw()###needs some time

ggsave("PM10 Weekly Boxplot Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM2.5_df <- mydf_list[13]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM2.5_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM2.5", x="Day")+
  theme_bw()
ggsave("PM2.5 Daily Mean Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM2.5_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM2.5",x="Week")+
  theme_bw()
ggsave("PM2.5 Weekly Mean Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM2.5_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM2.5",x="Week")+
  theme_bw()###needs some time

ggsave("PM2.5 Weekly Boxplot Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#############################################################################################
#######################################################################################################
#######################################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Dec")###this is the location of files
df <- read.csv("Haymarket Dec Clean.csv", header= TRUE)
str(df)

df2 <- df[, -c(15, 10)]
str(df2)



list_clnms = c(colnames(df2))

for (i in 1:length(list_clnms)){
  df2[, list_clnms[i]] <- ifelse(df2[, list_clnms[i]]>=0, df2[, list_clnms[i]], NA)
  
}

str(df2)

library(plyr)

numcolwise(sum)(df2, na.rm=TRUE)###sum of numeric columns in df2


boxplot(df2$CO)
boxplot(df2$CO,plot=FALSE)$out
outliers <- boxplot(df2$CO,plot=FALSE)$out
print(outliers)
df2[which(df2$CO %in% outliers),]
df2<-df2[-which(df2$CO %in% outliers),]
boxplot(df2$NO)
boxplot(df2$NO,plot=FALSE)$out
outliers <- boxplot(df2$NO,plot=FALSE)$out
print(outliers)
df2[which(df2$NO %in% outliers),]
df2<-df2[-which(df2$NO %in% outliers),]
boxplot(df2$NO2)
boxplot(df2$NO2,plot=FALSE)$out
outliers <- boxplot(df2$NO2,plot=FALSE)$out
print(outliers)
df2[which(df2$NO2 %in% outliers),]
df2<-df2[-which(df2$NO2 %in% outliers),]
boxplot(df2$Humidity)
boxplot(df2$Humidity,plot=FALSE)$out
outliers <- boxplot(df2$Humidity,plot=FALSE)$out
print(outliers)
df2[which(df2$Humidity %in% outliers),]
df2<-df2[-which(df2$Humidity %in% outliers),]
boxplot(df2$O3)
boxplot(df2$O3,plot=FALSE)$out
outliers <- boxplot(df2$O3,plot=FALSE)$out
print(outliers)
df2[which(df2$O3 %in% outliers),]
df2<-df2[-which(df2$O3 %in% outliers),]
boxplot(df2$PM.4)
boxplot(df2$PM.4,plot=FALSE)$out
outliers <- boxplot(df2$PM.4,plot=FALSE)$out
print(outliers)
df2[which(df2$PM.4 %in% outliers),]
df2<-df2[-which(df2$PM.4 %in% outliers),]
boxplot(df2$PM1)
boxplot(df2$PM1,plot=FALSE)$out
outliers <- boxplot(df2$PM1,plot=FALSE)$out
print(outliers)
df2[which(df2$PM1 %in% outliers),]
df2<-df2[-which(df2$PM1 %in% outliers),]
boxplot(df2$PM10)
boxplot(df2$PM10,plot=FALSE)$out
outliers <- boxplot(df2$PM10,plot=FALSE)$out
print(outliers)
df2[which(df2$PM10 %in% outliers),]
df2<-df2[-which(df2$PM10 %in% outliers),]
boxplot(df2$PM2.5)
boxplot(df2$PM2.5,plot=FALSE)$out
outliers <- boxplot(df2$PM2.5,plot=FALSE)$out
print(outliers)
df2[which(df2$PM2.5 %in% outliers),]
df2<-df2[-which(df2$PM2.5 %in% outliers),]


#####reshape excel file
library(reshape2)
melted <- melt(df2, id.vars = c("Date_Time")  )#####melt excel file to a new df

library(tidyr)
melted <- melted%>% drop_na()#remove rows with NA in melted df
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

##
library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)

Mean_lists_week = list()
for (i in 1:length(mydf_list)) {
  
  m <-mydf_list[[i]]%>%
    mutate(week = round_date(Date_Time, "week")) %>%
    group_by(week) %>%
    summarySE(measurevar="value", groupvars=c("week"))
  
  listname <- paste("Mean_by_week_", names(mydf_list[i]))
  
  Mean_lists_week[[listname]] <-m
  
}
############
##or by day

Mean_lists_day = list()
for (i in 1:length(mydf_Mar_list)) {
  
  m <-mydf_list[[i]]%>%
    mutate(day = round_date(Date_Time, "day")) %>%
    group_by(day) %>%
    summarySE(measurevar="value", groupvars=c("day"))
  
  listname <- paste("Mean_by_day_", names(mydf_list[i]))
  
  Mean_lists_day[[listname]] <-m
  
}

CO_df <- mydf_list[1]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-CO_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO", x="Day")+
  theme_bw()
ggsave("CO Daily Mean Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-CO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()
ggsave("CO Weekly Mean Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-CO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()###needs some time

ggsave("CO Weekly Boxplot Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################

p<-NO_df <- mydf_list[2]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO", x="Day")+
  theme_bw()
ggsave("NO Daily Mean Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()
ggsave("NO Weekly Mean Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-NO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()###needs some time

ggsave("NO Weekly Boxplot Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###############################################################################################

NO2_df <- mydf_list[3]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO2_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2", x="Day")+
  theme_bw()
ggsave("NO2 Daily Mean Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-NO2_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()
ggsave("NO2 Weekly Mean Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-NO2_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()###needs some time

ggsave("NO2 Weekly Boxplot Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

########################################################################################

Hum_df <- mydf_list[4]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Hum_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity", x="Day")+
  theme_bw()
ggsave("Hum Daily Mean Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Hum_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()
ggsave("Hum Weekly Mean Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Hum_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()###needs some time

ggsave("Hum Weekly Boxplot Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
O3_df <- mydf_list[5]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-O3_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="O3", x="Day")+
  theme_bw()
ggsave("O3 Daily Mean Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-O3_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="O3",x="Week")+
  theme_bw()
ggsave("O3 Weekly Mean Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-O3_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="O3",x="Week")+
  theme_bw()###needs some time

ggsave("O3 Weekly Boxplot Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Pressure_df <- mydf_list[6]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Pressure_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Pressure", x="Day")+
  theme_bw()
ggsave("Pressure Daily Mean Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Pressure_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Pressure",x="Week")+
  theme_bw()
ggsave("Pressure Weekly Mean Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Pressure_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Pressure",x="Week")+
  theme_bw()###needs some time

ggsave("Pressure Weekly Boxplot Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Temp_df <- mydf_list[7]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Temp_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature", x="Day")+
  theme_bw()
ggsave("Temperature Daily Mean Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Temp_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()
ggsave("Temperature Weekly Mean Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Temp_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()###needs some time

ggsave("Temperature Weekly Boxplot Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
Veh_df <- mydf_list[8]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Veh_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Vehicles", x="Day")+
  theme_bw()
ggsave("Vehicles Daily Mean Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-Veh_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Vehicles",x="Week")+
  theme_bw()
ggsave("Vehicles Weekly Mean Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Veh_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Vehicles",x="Week")+
  theme_bw()###needs some time

ggsave("Vehicles Weekly Boxplot Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


###########################################################################################
PM4_df <- mydf_list[9]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM4_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM4", x="Day")+
  theme_bw()
ggsave("PM4 Daily Mean Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM4_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM4",x="Week")+
  theme_bw()
ggsave("PM4 Weekly Mean Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM4_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM4",x="Week")+
  theme_bw()###needs some time

ggsave("PM4 Weekly Boxplot Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM1_df <- mydf_list[10]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM1_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM1", x="Day")+
  theme_bw()
ggsave("PM1 Daily Mean Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM1_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM1",x="Week")+
  theme_bw()
ggsave("PM1 Weekly Mean Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM1_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM1",x="Week")+
  theme_bw()###needs some time

ggsave("PM1 Weekly Boxplot Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM10_df <- mydf_list[11]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM10_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM10", x="Day")+
  theme_bw()
ggsave("PM10 Daily Mean Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM10_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM10",x="Week")+
  theme_bw()
ggsave("PM10 Weekly Mean Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM10_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM10",x="Week")+
  theme_bw()###needs some time

ggsave("PM10 Weekly Boxplot Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM2.5_df <- mydf_list[12]%>%
  map_df(as_tibble)##to convert any list to data tibble


##or ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM2.5_df %>%
  mutate(day = round_date(Date_Time, "day")) %>%
  group_by(day) %>%
  summarySE(measurevar="value", groupvars=c("day"))%>%
  ggplot(aes(x= day, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM2.5", x="Day")+
  theme_bw()
ggsave("PM2.5 Daily Mean Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot mean+sd,se,ci by week
pd <- position_dodge(0.4) # move them .05 to the left and right

p<-PM2.5_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  summarySE(measurevar="value", groupvars=c("week"))%>%
  ggplot(aes(x= week, y=value,colour=variable ))+
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM2.5",x="Week")+
  theme_bw()
ggsave("PM2.5 Weekly Mean Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM2.5_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM2.5",x="Week")+
  theme_bw()###needs some time

ggsave("PM2.5 Weekly Boxplot Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#############################################################################################
#######################################################################################################
#######################################################################################################################

