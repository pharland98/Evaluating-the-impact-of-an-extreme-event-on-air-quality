#############################################################################################
#######################################################################################################
#######################################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Jan20")
df <- read.csv("Haymarket Jan20 Clean.csv", header= TRUE)
str(df)

#create new df which is df2 without columns 17 1nd 18
df2 <- df[, -c(14, 15)]###lets get rid of these columns for now
str(df2)



### replace bad values (here minus values) with NA (or any value)
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
melted <- melt(df2, id.vars = c("Date_Time")  )#####melt you excel file to a new df

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
for (i in 1:length(mydf_list)) {
  
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
ggsave("CO Daily Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("CO Weekly Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-CO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()###needs some time

ggsave("CO Weekly Boxplot Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("NO Daily Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("NO Weekly Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-NO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()###needs some time

ggsave("NO Weekly Boxplot Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("NO2 Daily Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("NO2 Weekly Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-NO2_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()###needs some time

ggsave("NO2 Weekly Boxplot Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("Hum Daily Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("Hum Weekly Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Hum_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()###needs some time

ggsave("Hum Weekly Boxplot Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("O3 Daily Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("O3 Weekly Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-O3_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="O3",x="Week")+
  theme_bw()###needs some time

ggsave("O3 Weekly Boxplot Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("Pressure Daily Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("Pressure Weekly Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Pressure_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Pressure",x="Week")+
  theme_bw()###needs some time

ggsave("Pressure Weekly Boxplot Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("Temperature Daily Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("Temperature Weekly Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Temp_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()###needs some time

ggsave("Temperature Weekly Boxplot Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("Vehicles Daily Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("Vehicles Weekly Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Veh_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Vehicles",x="Week")+
  theme_bw()###needs some time

ggsave("Vehicles Weekly Boxplot Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file



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
ggsave("PM4 Daily Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("PM4 Weekly Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM4_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM4",x="Week")+
  theme_bw()###needs some time

ggsave("PM4 Weekly Boxplot Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("PM1 Daily Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("PM1 Weekly Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM1_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM1",x="Week")+
  theme_bw()###needs some time

ggsave("PM1 Weekly Boxplot Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("PM10 Daily Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("PM10 Weekly Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM10_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM10",x="Week")+
  theme_bw()###needs some time

ggsave("PM10 Weekly Boxplot Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("PM2.5 Daily Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("PM2.5 Weekly Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM2.5_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM2.5",x="Week")+
  theme_bw()###needs some time

ggsave("PM2.5 Weekly Boxplot Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#############################################################################################
#######################################################################################################
#######################################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Feb20")###this is the location of files
df_Feb <- read.csv("Haymarket Feb20 Clean.csv", header= TRUE)
str(df_Feb)

#create new df which is df2 without columns 17 1nd 18
df2_Feb <- df_Feb[, -c(15, 14)]
str(df2_Feb)



### replace bad values (here minus values) with NA (or any value)
list_clnms = c(colnames(df2_Feb))

for (i in 1:length(list_clnms_Feb)){
  df2_Feb[, list_clnms_Feb[i]] <- ifelse(df2_Feb[, list_clnms_Feb[i]]>=0, df2_Feb[, list_clnms_Feb[i]], NA)
  
}

str(df2_Feb)

library(plyr)

numcolwise(sum)(df2_Feb, na.rm=TRUE)###sum of numeric columns in df2


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

#####reshape ur excel file
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
ggsave("CO Daily Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("CO Weekly Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-CO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()###needs some time

ggsave("CO Weekly Boxplot Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("NO Daily Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("NO Weekly Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-NO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()###needs some time

ggsave("NO Weekly Boxplot Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("NO2 Daily Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("NO2 Weekly Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-NO2_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()###needs some time

ggsave("NO2 Weekly Boxplot Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("Hum Daily Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("Hum Weekly Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Hum_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()###needs some time

ggsave("Hum Weekly Boxplot Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("O3 Daily Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("O3 Weekly Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-O3_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="O3",x="Week")+
  theme_bw()###needs some time

ggsave("O3 Weekly Boxplot Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("Pressure Daily Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("Pressure Weekly Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Pressure_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Pressure",x="Week")+
  theme_bw()###needs some time

ggsave("Pressure Weekly Boxplot Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("Temperature Daily Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("Temperature Weekly Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Temp_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()###needs some time

ggsave("Temperature Weekly Boxplot Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("Vehicles Daily Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("Vehicles Weekly Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Veh_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Vehicles",x="Week")+
  theme_bw()###needs some time

ggsave("Vehicles Weekly Boxplot Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


###########################################################################################
PM4_df <- mydf_Feb_list[9]%>%
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
ggsave("PM4 Daily Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("PM4 Weekly Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM4_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM4",x="Week")+
  theme_bw()###needs some time

ggsave("PM4 Weekly Boxplot Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM1_df <- mydf_Feb_list[10]%>%
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
ggsave("PM1 Daily Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("PM1 Weekly Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM1_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM1",x="Week")+
  theme_bw()###needs some time

ggsave("PM1 Weekly Boxplot Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM10_df <- mydf_Feb_list[11]%>%
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
ggsave("PM10 Daily Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("PM10 Weekly Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM10_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM10",x="Week")+
  theme_bw()###needs some time

ggsave("PM10 Weekly Boxplot Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM2.5_df <- mydf_Feb_list[12]%>%
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
ggsave("PM2.5 Daily Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("PM2.5 Weekly Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM2.5_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM2.5",x="Week")+
  theme_bw()###needs some time

ggsave("PM2.5 Weekly Boxplot Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#############################################################################################
#######################################################################################################
#######################################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Mar20")###this is the location of files
df_Mar <- read.csv("Haymarket Mar20 Clean.csv", header= TRUE)
str(df_Mar)

#create new df which is df2 without columns 17 1nd 18
df2_Mar <- df_Mar[, -c(15, 14)]
str(df2_Mar)



###replace bad values (here minus values) with NA (or any value)
list_clnms = c(colnames(df2_Mar))

for (i in 1:length(list_clnms_Mar)){
  df2_Mar[, list_clnms_Mar[i]] <- ifelse(df2_Mar[, list_clnms_Mar[i]]>=0, df2_Mar[, list_clnms_Mar[i]], NA)
  
}

str(df2_Mar)

library(plyr)

numcolwise(sum)(df2_Mar, na.rm=TRUE)###sum of numeric columns in df2


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
melted_Mar <- melt(df2_Mar, id.vars = c("Date_Time")  )#####melt excel file to a new df

library(tidyr)
melted_Mar <- melted_Mar%>% drop_na()#remove rows with NA in melted df
#write.csv(melted, "melted.csv")

library(lubridate)
melted_Mar$Date_Time <-ymd_hms(melted_Mar$Date_Time)
str(melted_Mar)


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
ggsave("CO Daily Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("CO Weekly Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-CO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()###needs some time

ggsave("CO Weekly Boxplot Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("NO Daily Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("NO Weekly Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-NO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()###needs some time

ggsave("NO Weekly Boxplot Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("NO2 Daily Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("NO2 Weekly Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-NO2_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()###needs some time

ggsave("NO2 Weekly Boxplot Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("Hum Daily Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("Hum Weekly Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Hum_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()###needs some time

ggsave("Hum Weekly Boxplot Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("O3 Daily Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("O3 Weekly Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-O3_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="O3",x="Week")+
  theme_bw()###needs some time

ggsave("O3 Weekly Boxplot Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("Pressure Daily Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("Pressure Weekly Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Pressure_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Pressure",x="Week")+
  theme_bw()###needs some time

ggsave("Pressure Weekly Boxplot Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("Temperature Daily Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("Temperature Weekly Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Temp_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()###needs some time

ggsave("Temperature Weekly Boxplot Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("Vehicles Daily Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("Vehicles Weekly Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Veh_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Vehicles",x="Week")+
  theme_bw()###needs some time

ggsave("Vehicles Weekly Boxplot Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file



###########################################################################################
PM4_df <- mydf_Mar_list[9]%>%
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
ggsave("PM4 Daily Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("PM4 Weekly Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM4_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM4",x="Week")+
  theme_bw()###needs some time

ggsave("PM4 Weekly Boxplot Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM1_df <- mydf_Mar_list[10]%>%
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
ggsave("PM1 Daily Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("PM1 Weekly Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM1_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM1",x="Week")+
  theme_bw()###needs some time

ggsave("PM1 Weekly Boxplot Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM10_df <- mydf_Mar_list[11]%>%
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
ggsave("PM10 Daily Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("PM10 Weekly Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM10_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM10",x="Week")+
  theme_bw()###needs some time

ggsave("PM10 Weekly Boxplot Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

###########################################################################################
PM2.5_df <- mydf_Mar_list[12]%>%
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
ggsave("PM2.5 Daily Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("PM2.5 Weekly Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM2.5_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM2.5",x="Week")+
  theme_bw()###needs some time

ggsave("PM2.5 Weekly Boxplot Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#############################################################################################
#######################################################################################################
#######################################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_Apr20")###this is the location of files
df <- read.csv("Haymarket Apr20 Clean.csv", header= TRUE)
str(df)

#create new df which is df2 without columns 17 1nd 18
df2 <- df[, -c(15, 16)]
str(df2)



### replace bad values (here minus values) with NA (or any value)
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

#####reshape ur excel file
library(reshape2)
melted <- melt(df2, id.vars = c("Date_Time")  )#####melt  excel file to a new df

library(tidyr)
melted <- melted%>% drop_na()#remove rows with NA in melted df
#write.csv(melted, "melted.csv")

library(lubridate)
melted$Date_Time <-ymd_hms(melted$Date_Time)
str(melted)


mydf <- melted_Mar
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
for (i in 1:length(mydf_list)) {
  
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
ggsave("CO Daily Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("CO Weekly Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-CO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()###needs some time

ggsave("CO Weekly Boxplot Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("NO Daily Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("NO Weekly Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-NO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()###needs some time

ggsave("NO Weekly Boxplot Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("NO2 Daily Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("NO2 Weekly Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-NO2_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()###needs some time

ggsave("NO2 Weekly Boxplot Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("Hum Daily Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("Hum Weekly Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Hum_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()###needs some time

ggsave("Hum Weekly Boxplot Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("O3 Daily Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("O3 Weekly Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-O3_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="O3",x="Week")+
  theme_bw()###needs some time

ggsave("O3 Weekly Boxplot Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("Pressure Daily Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("Pressure Weekly Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Pressure_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Pressure",x="Week")+
  theme_bw()###needs some time

ggsave("Pressure Weekly Boxplot Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("Temperature Weekly Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Temp_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()###needs some time

ggsave("Temperature Weekly Boxplot Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("Vehicles Daily Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("Vehicles Weekly Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Veh_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Vehicles",x="Week")+
  theme_bw()###needs some time

ggsave("Vehicles Weekly Boxplot Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file



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
ggsave("PM4 Daily Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("PM4 Weekly Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM4_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM4",x="Week")+
  theme_bw()###needs some time

ggsave("PM4 Weekly Boxplot Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("PM1 Daily Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("PM1 Weekly Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM1_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM1",x="Week")+
  theme_bw()###needs some time

ggsave("PM1 Weekly Boxplot Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("PM10 Daily Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("PM10 Weekly Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM10_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM10",x="Week")+
  theme_bw()###needs some time

ggsave("PM10 Weekly Boxplot Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("PM2.5 Daily Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("PM2.5 Weekly Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM2.5_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM2.5",x="Week")+
  theme_bw()###needs some time

ggsave("PM2.5 Weekly Boxplot Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#############################################################################################
#######################################################################################################
#######################################################################################################################

setwd("~/Dissertation/Dissertation Datasets/Haymarket/Haymarket_May20")###this is the location of your files
df <- read.csv("Haymarket May20 Clean.csv", header= TRUE)
str(df)

#create new df which is df2 without columns 17 1nd 18
df2 <- df[, -c(15, 16)]
str(df2)



###replace bad values (here minus values) with NA (or any value)
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

#####reshape ur excel file
library(reshape2)
melted <- melt(df2, id.vars = c("Date_Time")  )#####melt excel file to a new df

library(tidyr)
melted <- melted%>% drop_na()#remove rows with NA in melted df
#write.csv(melted, "melted.csv")

library(lubridate)
melted$Date_Time <-ymd_hms(melted$Date_Time)
str(melted)


mydf <- melted_Mar
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
for (i in 1:length(mydf_list)) {
  
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
ggsave("CO Daily Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("CO Weekly Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-CO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()###needs some time

ggsave("CO Weekly Boxplot May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("NO Daily Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("NO Weekly Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-NO_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()###needs some time

ggsave("NO Weekly Boxplot May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("NO2 Daily Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("NO2 Weekly Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-NO2_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()###needs some time

ggsave("NO2 Weekly Boxplot May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("Hum Daily Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("Hum Weekly Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Hum_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()###needs some time

ggsave("Hum Weekly Boxplot May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("O3 Daily Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("O3 Weekly Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-O3_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="O3",x="Week")+
  theme_bw()###needs some time

ggsave("O3 Weekly Boxplot May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("Pressure Daily Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("Pressure Weekly Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Pressure_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Pressure",x="Week")+
  theme_bw()###needs some time

ggsave("Pressure Weekly Boxplot May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("Temperature Daily Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("Temperature Weekly Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Temp_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()###needs some time

ggsave("Temperature Weekly Boxplot May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("Vehicles Daily Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("Vehicles Weekly Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-Veh_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Vehicles",x="Week")+
  theme_bw()###needs some time

ggsave("Vehicles Weekly Boxplot May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("PM4 Daily Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("PM4 Weekly Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM4_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM4",x="Week")+
  theme_bw()###needs some time

ggsave("PM4 Weekly Boxplot May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("PM1 Daily Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("PM1 Weekly Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM1_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM1",x="Week")+
  theme_bw()###needs some time

ggsave("PM1 Weekly Boxplot May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("PM10 Daily Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("PM10 Weekly Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM10_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM10",x="Week")+
  theme_bw()###needs some time

ggsave("PM10 Weekly Boxplot May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

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
ggsave("PM2.5 Daily Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("PM2.5 Weekly Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##orggplot  box plot by week note: for boxplot x-axis need to be factor

p<-PM2.5_df %>%
  mutate(week = round_date(Date_Time, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="PM2.5",x="Week")+
  theme_bw()###needs some time

ggsave("PM2.5 Weekly Boxplot May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

