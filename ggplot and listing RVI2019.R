setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Apr")###this is the location of  files
Air_Quality <- read.csv("RVI_Apr_Clean.csv", header= TRUE)
library(tidyr)
#########
############
summary(Air_Quality)

str(Air_Quality)

###we have Timestamp not Date_Time
##make new column Date_Time
Air_Quality$Date_Time <-dmy_hm(Air_Quality$Timestamp)
str(Air_Quality) ##ok now

##remove unwanted column
Air_Quality<-Air_Quality[-c(1,2)]

str(Air_Quality)
mydf_list <- split( Air_Quality , f = Air_Quality$Variable )##save each parameter as a separate df with its name in mydf_list

library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)
library(rio)
library(lubridate)
str(Air_Quality)
str(mydf_list)

#######calculating mean of values by day and putting in Mean_lists_day list
Mean_lists_day = list()
for (i in 1:length(mydf_list)) {
  
  m <-mydf_list[[i]]%>%
    mutate(day = round_date(Date_Time, "day")) %>%
    group_by(day) %>%
    summarySE(measurevar="Value", groupvars=c("day"))
  
  listname <- paste("Mean_by_day_", names(mydf_list[i]))
  
  Mean_lists_day[[listname]] <-m
  
}


CO_df <- Mean_lists_day[1]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(CO_df$Value)
boxplot(CO_df$Value,plot=FALSE)$out
outliers <- boxplot(CO_df$Value,plot=FALSE)$out
print(outliers)

CO_df<-CO_df[-which(CO_df$Value %in% outliers),]
boxplot(CO_df$Value)
str(CO_df)

anyNA(CO_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right



########################day################################################
str(CO_df)
p <- CO_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO", x="Day")+
  theme_bw()
p
ggsave("CO Daily Mean Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- CO_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="day")+
  theme_bw()###needs some time
p
ggsave("CO 3 day Boxplot Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(CO_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- CO_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()
p
ggsave("CO Weekly Mean Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- CO_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()###needs some time
p
ggsave("CO Weekly Boxplot Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- CO_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= month, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("month"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="month")+
  theme_bw()
p
ggsave("CO month Mean Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- CO_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="month")+
  theme_bw()###needs some time
p
ggsave("CO month Boxplot Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file



######END of my correction .....you can repeat as above for other parameters

###you can repeat line 56-168 for each parameter
####################################



######i removed other parameters..  please correct as above for each parameter...line 56-168.






#############################################################################################
#############################################################################################
################################corrected down .....we can means od days for day, week and month plots

Hum_df <- Mean_lists_day[2]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO


anyNA(Hum_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(Hum_df)
p <- Hum_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity", x="Day")+
  theme_bw()
p
ggsave("Hum Daily Mean Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- Hum_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="day")+
  theme_bw()###needs some time
p
ggsave("Hum 3 day Boxplot Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(Hum_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- Hum_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()
p
ggsave("Humidity Weekly Mean Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- Hum_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()###needs some time
p
ggsave("Humidity Weekly Boxplot Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#######################################################################################


NO_df <- Mean_lists_day[3]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(NO_df$Value)
boxplot(NO_df$Value,plot=FALSE)$out
outliers <- boxplot(NO_df$Value,plot=FALSE)$out
print(outliers)

NO_df<-NO_df[-which(NO_df$Value %in% outliers),]
boxplot(NO_df$Value)
str(NO_df)

anyNA(NO_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(NO_df)
p <- NO_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO", x="Day")+
  theme_bw()
p
ggsave("NO Daily Mean Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- NO_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="day")+
  theme_bw()###needs some time
p
ggsave("NO 3 day Boxplot Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(NO_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- NO_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()
p
ggsave("NO Weekly Mean Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- NO_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()###needs some time
p
ggsave("NO Weekly Boxplot Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#######################################################################################

NO2_df <- Mean_lists_day[4]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(NO2_df$Value)
boxplot(NO2_df$Value,plot=FALSE)$out
outliers <- boxplot(NO2_df$Value,plot=FALSE)$out
print(outliers)

NO2_df<-NO2_df[-which(NO2_df$Value %in% outliers),]
boxplot(NO2_df$Value)
str(NO2_df)

anyNA(NO2_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(NO2_df)
p <- NO2_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2", x="Day")+
  theme_bw()
p
ggsave("NO2 Daily Mean Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- NO2_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="day")+
  theme_bw()###needs some time
p
ggsave("NO2 3 day Boxplot Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(NO2_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- NO2_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()
p
ggsave("NO2 Weekly Mean Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- NO2_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()###needs some time
p
ggsave("NO2 Weekly Boxplot Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#######################################################################################
#######################################################################################

Sound_df <- Mean_lists_day[5]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
#

anyNA(Sound_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(Sound_df)
p <- Sound_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound", x="Day")+
  theme_bw()
p
ggsave("Sound Daily Mean Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- Sound_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="day")+
  theme_bw()###needs some time
p
ggsave("Sound 3 day Boxplot Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(Sound_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- Sound_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="Week")+
  theme_bw()
p
ggsave("Sound Weekly Mean Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- Sound_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="Week")+
  theme_bw()###needs some time
p
ggsave("Sound Weekly Boxplot Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#######################################################################################
#######################################################################################

Temp_df <- Mean_lists_day[6]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO

anyNA(Temp_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(Temp_df)
p <- Temp_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature", x="Day")+
  theme_bw()
p
ggsave("Temp Daily Mean Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- Temp_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="day")+
  theme_bw()###needs some time
p
ggsave("Temp 3 day Boxplot Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(Temp_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- Temp_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()
p
ggsave("Temp Weekly Mean Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- Temp_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()###needs some time
p
ggsave("Temp Weekly Boxplot Apr .jpg", p, width = 24, height = 14,dpi = 300 )#save to file
#
#
#
#
#
#
#
#
#
#
#
#######################################################################################

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_May")###this is the location of your files
Air_Quality <- read.csv("RVI_May_Clean.csv", header= TRUE)
library(tidyr)
#########
############
summary(Air_Quality)

str(Air_Quality)

###we have Timestamp not Date_Time
##make new column Date_Time
Air_Quality$Date_Time <-dmy_hm(Air_Quality$Timestamp)
str(Air_Quality) ##ok now

##remove unwanted column
Air_Quality<-Air_Quality[-c(1,2)]

str(Air_Quality)
mydf_list <- split( Air_Quality , f = Air_Quality$Variable )##save each parameter as a separate df with its name in mydf_list

library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)
library(rio)
library(lubridate)
str(Air_Quality)
str(mydf_list)

#######calculating mean of values by day and putting in Mean_lists_day list
Mean_lists_day = list()
for (i in 1:length(mydf_list)) {
  
  m <-mydf_list[[i]]%>%
    mutate(day = round_date(Date_Time, "day")) %>%
    group_by(day) %>%
    summarySE(measurevar="Value", groupvars=c("day"))
  
  listname <- paste("Mean_by_day_", names(mydf_list[i]))
  
  Mean_lists_day[[listname]] <-m
  
}


################################corrected down .....we can means od days for day, week and month plots
CO_df <- Mean_lists_day[1]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(CO_df$Value)
boxplot(CO_df$Value,plot=FALSE)$out
outliers <- boxplot(CO_df$Value,plot=FALSE)$out
print(outliers)

CO_df<-CO_df[-which(CO_df$Value %in% outliers),]
boxplot(CO_df$Value)
str(CO_df)

anyNA(CO_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(CO_df)
p <- CO_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO", x="Day")+
  theme_bw()
p
ggsave("CO Daily Mean May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- CO_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="day")+
  theme_bw()###needs some time
p
ggsave("CO 3 day Boxplot May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(CO_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- CO_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()
p
ggsave("CO Weekly Mean May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- CO_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()###needs some time
p
ggsave("CO Weekly Boxplot May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- CO_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= month, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("month"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="month")+
  theme_bw()
p
ggsave("CO month Mean May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- CO_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="month")+
  theme_bw()###needs some time
p
ggsave("CO month Boxplot May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file



######END of my correction .....you can repeat as above for other parameters

###you can repeat line 56-168 for each parameter
####################################



######i removed other parameters..  please correct as above for each parameter...line 56-168.






#############################################################################################
#############################################################################################
################################corrected down .....we can means od days for day, week and month plots

Hum_df <- Mean_lists_day[2]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO


anyNA(Hum_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(Hum_df)
p <- Hum_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity", x="Day")+
  theme_bw()
p
ggsave("Hum Daily Mean May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- Hum_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="day")+
  theme_bw()###needs some time
p
ggsave("Hum 3 day Boxplot May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(Hum_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- Hum_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()
p
ggsave("Humidity Weekly Mean May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- Hum_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()###needs some time
p
ggsave("Humidity Weekly Boxplot May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#######################################################################################


NO_df <- Mean_lists_day[3]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(NO_df$Value)
boxplot(NO_df$Value,plot=FALSE)$out
outliers <- boxplot(NO_df$Value,plot=FALSE)$out
print(outliers)

NO_df<-NO_df[-which(NO_df$Value %in% outliers),]
boxplot(NO_df$Value)
str(NO_df)

anyNA(NO_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(NO_df)
p <- NO_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO", x="Day")+
  theme_bw()
p
ggsave("NO Daily Mean May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- NO_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="day")+
  theme_bw()###needs some time
p
ggsave("NO 3 day Boxplot May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(NO_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- NO_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()
p
ggsave("NO Weekly Mean May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- NO_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()###needs some time
p
ggsave("NO Weekly Boxplot May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#######################################################################################

NO2_df <- Mean_lists_day[4]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(NO2_df$Value)
boxplot(NO2_df$Value,plot=FALSE)$out
outliers <- boxplot(NO2_df$Value,plot=FALSE)$out
print(outliers)

NO2_df<-NO2_df[-which(NO2_df$Value %in% outliers),]
boxplot(NO2_df$Value)
str(NO2_df)

anyNA(NO2_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(NO2_df)
p <- NO2_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2", x="Day")+
  theme_bw()
p
ggsave("NO2 Daily Mean May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- NO2_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="day")+
  theme_bw()###needs some time
p
ggsave("NO2 3 day Boxplot May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(NO2_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- NO2_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()
p
ggsave("NO2 Weekly Mean May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- NO2_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()###needs some time
p
ggsave("NO2 Weekly Boxplot May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#######################################################################################
#######################################################################################

Sound_df <- Mean_lists_day[5]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO


anyNA(Sound_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(Sound_df)
p <- Sound_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound", x="Day")+
  theme_bw()
p
ggsave("Sound Daily Mean May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- Sound_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="day")+
  theme_bw()###needs some time
p
ggsave("Sound 3 day Boxplot May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(Sound_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- Sound_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="Week")+
  theme_bw()
p
ggsave("Sound Weekly Mean May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- Sound_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="Week")+
  theme_bw()###needs some time
p
ggsave("Sound Weekly Boxplot May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#######################################################################################
#######################################################################################

Temp_df <- Mean_lists_day[6]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########


anyNA(Temp_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(Temp_df)
p <- Temp_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature", x="Day")+
  theme_bw()
p
ggsave("Temp Daily Mean May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- Temp_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="day")+
  theme_bw()###needs some time
p
ggsave("Temp 3 day Boxplot May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(Temp_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- Temp_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()
p
ggsave("Temp Weekly Mean May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- Temp_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()###needs some time
p
ggsave("Temp Weekly Boxplot May .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#
#
#
#
#
#
#
#
#
#
#
#######################################################################################

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Jun")###this is the location of your files
Air_Quality <- read.csv("RVI_Jun_Clean.csv", header= TRUE)
library(tidyr)
#########
############
summary(Air_Quality)

str(Air_Quality)

###we have Timestamp not Date_Time
##make new column Date_Time
Air_Quality$Date_Time <-dmy_hm(Air_Quality$Timestamp)
str(Air_Quality) ##ok now

##remove unwanted column
Air_Quality<-Air_Quality[-c(1,2)]

str(Air_Quality)
mydf_list <- split( Air_Quality , f = Air_Quality$Variable )##save each parameter as a separate df with its name in mydf_list

library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)
library(rio)
library(lubridate)
str(Air_Quality)
str(mydf_list)

#######calculating mean of values by day and putting in Mean_lists_day list
Mean_lists_day = list()
for (i in 1:length(mydf_list)) {
  
  m <-mydf_list[[i]]%>%
    mutate(day = round_date(Date_Time, "day")) %>%
    group_by(day) %>%
    summarySE(measurevar="Value", groupvars=c("day"))
  
  listname <- paste("Mean_by_day_", names(mydf_list[i]))
  
  Mean_lists_day[[listname]] <-m
  
}


################################corrected down .....we can means od days for day, week and month plots
CO_df <- Mean_lists_day[1]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(CO_df$Value)
boxplot(CO_df$Value,plot=FALSE)$out
outliers <- boxplot(CO_df$Value,plot=FALSE)$out
print(outliers)

CO_df<-CO_df[-which(CO_df$Value %in% outliers),]
boxplot(CO_df$Value)
str(CO_df)

anyNA(CO_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(CO_df)
p <- CO_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO", x="Day")+
  theme_bw()
p
ggsave("CO Daily Mean Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- CO_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="day")+
  theme_bw()###needs some time
p
ggsave("CO 3 day Boxplot Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(CO_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- CO_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()
p
ggsave("CO Weekly Mean Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- CO_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()###needs some time
p
ggsave("CO Weekly Boxplot Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- CO_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= month, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("month"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="month")+
  theme_bw()
p
ggsave("CO month Mean Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- CO_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="month")+
  theme_bw()###needs some time
p
ggsave("CO month Boxplot Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file



######END of my correction .....you can repeat as above for other parameters

###you can repeat line 56-168 for each parameter
####################################



######i removed other parameters..  please correct as above for each parameter...line 56-168.






#############################################################################################
#############################################################################################
################################corrected down .....we can means od days for day, week and month plots

Hum_df <- Mean_lists_day[2]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO


anyNA(Hum_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(Hum_df)
p <- Hum_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity", x="Day")+
  theme_bw()
p
ggsave("Hum Daily Mean Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- Hum_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="day")+
  theme_bw()###needs some time
p
ggsave("Hum 3 day Boxplot Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(Hum_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- Hum_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()
p
ggsave("Humidity Weekly Mean Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- Hum_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()###needs some time
p
ggsave("Humidity Weekly Boxplot Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#######################################################################################


NO_df <- Mean_lists_day[3]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(NO_df$Value)
boxplot(NO_df$Value,plot=FALSE)$out
outliers <- boxplot(NO_df$Value,plot=FALSE)$out
print(outliers)

NO_df<-NO_df[-which(NO_df$Value %in% outliers),]
boxplot(NO_df$Value)
str(NO_df)

anyNA(NO_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(NO_df)
p <- NO_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO", x="Day")+
  theme_bw()
p
ggsave("NO Daily Mean Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- NO_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="day")+
  theme_bw()###needs some time
p
ggsave("NO 3 day Boxplot Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(NO_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- NO_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()
p
ggsave("NO Weekly Mean Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- NO_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()###needs some time
p
ggsave("NO Weekly Boxplot Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#######################################################################################

NO2_df <- Mean_lists_day[4]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(NO2_df$Value)
boxplot(NO2_df$Value,plot=FALSE)$out
outliers <- boxplot(NO2_df$Value,plot=FALSE)$out
print(outliers)

NO2_df<-NO2_df[-which(NO2_df$Value %in% outliers),]
boxplot(NO2_df$Value)
str(NO2_df)

anyNA(NO2_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(NO2_df)
p <- NO2_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2", x="Day")+
  theme_bw()
p
ggsave("NO2 Daily Mean Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- NO2_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="day")+
  theme_bw()###needs some time
p
ggsave("NO2 3 day Boxplot Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(NO2_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- NO2_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()
p
ggsave("NO2 Weekly Mean Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- NO2_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()###needs some time
p
ggsave("NO2 Weekly Boxplot Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#######################################################################################
#######################################################################################

Sound_df <- Mean_lists_day[5]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########


anyNA(Sound_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(Sound_df)
p <- Sound_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound", x="Day")+
  theme_bw()
p
ggsave("Sound Daily Mean Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- Sound_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="day")+
  theme_bw()###needs some time
p
ggsave("Sound 3 day Boxplot Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(Sound_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- Sound_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="Week")+
  theme_bw()
p
ggsave("Sound Weekly Mean Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- Sound_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="Week")+
  theme_bw()###needs some time
p
ggsave("Sound Weekly Boxplot Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#######################################################################################
#######################################################################################

Temp_df <- Mean_lists_day[6]%>%
  map_df(as_tibble)##to convert any list to data tibble
##

anyNA(Temp_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(Temp_df)
p <- Temp_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature", x="Day")+
  theme_bw()
p
ggsave("Temp Daily Mean Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- Temp_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="day")+
  theme_bw()###needs some time
p
ggsave("Temp 3 day Boxplot Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(Temp_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- Temp_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()
p
ggsave("Temp Weekly Mean Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- Temp_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()###needs some time
p
ggsave("Temp Weekly Boxplot Jun .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#
#
#
#
#
#
#
#
#
#
#
#######################################################################################

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Jul")###this is the location of your files
Air_Quality <- read.csv("RVI_Jul_Clean.csv", header= TRUE)
library(tidyr)
#########
############
summary(Air_Quality)

str(Air_Quality)

###we have Timestamp not Date_Time
##make new column Date_Time
Air_Quality$Date_Time <-dmy_hm(Air_Quality$Timestamp)
str(Air_Quality) ##ok now

##remove unwanted column
Air_Quality<-Air_Quality[-c(1,2)]

str(Air_Quality)
mydf_list <- split( Air_Quality , f = Air_Quality$Variable )##save each parameter as a separate df with its name in mydf_list

library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)
library(rio)
library(lubridate)
str(Air_Quality)
str(mydf_list)

#######calculating mean of values by day and putting in Mean_lists_day list
Mean_lists_day = list()
for (i in 1:length(mydf_list)) {
  
  m <-mydf_list[[i]]%>%
    mutate(day = round_date(Date_Time, "day")) %>%
    group_by(day) %>%
    summarySE(measurevar="Value", groupvars=c("day"))
  
  listname <- paste("Mean_by_day_", names(mydf_list[i]))
  
  Mean_lists_day[[listname]] <-m
  
}


################################corrected down .....we can means od days for day, week and month plots
CO_df <- Mean_lists_day[1]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(CO_df$Value)
boxplot(CO_df$Value,plot=FALSE)$out
outliers <- boxplot(CO_df$Value,plot=FALSE)$out
print(outliers)

CO_df<-CO_df[-which(CO_df$Value %in% outliers),]
boxplot(CO_df$Value)
str(CO_df)

anyNA(CO_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(CO_df)
p <- CO_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO", x="Day")+
  theme_bw()
p
ggsave("CO Daily Mean Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- CO_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="day")+
  theme_bw()###needs some time
p
ggsave("CO 3 day Boxplot Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(CO_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- CO_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()
p
ggsave("CO Weekly Mean Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- CO_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()###needs some time
p
ggsave("CO Weekly Boxplot Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- CO_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= month, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("month"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="month")+
  theme_bw()
p
ggsave("CO month Mean Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- CO_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="month")+
  theme_bw()###needs some time
p
ggsave("CO month Boxplot Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file



######END of my correction .....you can repeat as above for other parameters

###you can repeat line 56-168 for each parameter
####################################



######i removed other parameters..  please correct as above for each parameter...line 56-168.






#############################################################################################
#############################################################################################
################################corrected down .....we can means od days for day, week and month plots

Hum_df <- Mean_lists_day[2]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO


anyNA(Hum_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(Hum_df)
p <- Hum_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity", x="Day")+
  theme_bw()
p
ggsave("Hum Daily Mean Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- Hum_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="day")+
  theme_bw()###needs some time
p
ggsave("Hum 3 day Boxplot Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(Hum_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- Hum_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()
p
ggsave("Humidity Weekly Mean Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- Hum_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()###needs some time
p
ggsave("Humidity Weekly Boxplot Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#######################################################################################


NO_df <- Mean_lists_day[3]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(NO_df$Value)
boxplot(NO_df$Value,plot=FALSE)$out
outliers <- boxplot(NO_df$Value,plot=FALSE)$out
print(outliers)

NO_df<-NO_df[-which(NO_df$Value %in% outliers),]
boxplot(NO_df$Value)
str(NO_df)

anyNA(NO_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(NO_df)
p <- NO_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO", x="Day")+
  theme_bw()
p
ggsave("NO Daily Mean Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- NO_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="day")+
  theme_bw()###needs some time
p
ggsave("NO 3 day Boxplot Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(NO_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- NO_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()
p
ggsave("NO Weekly Mean Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- NO_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()###needs some time
p
ggsave("NO Weekly Boxplot Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#######################################################################################

NO2_df <- Mean_lists_day[4]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(NO2_df$Value)
boxplot(NO2_df$Value,plot=FALSE)$out
outliers <- boxplot(NO2_df$Value,plot=FALSE)$out
print(outliers)

NO2_df<-NO2_df[-which(NO2_df$Value %in% outliers),]
boxplot(NO2_df$Value)
str(NO2_df)

anyNA(NO2_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(NO2_df)
p <- NO2_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2", x="Day")+
  theme_bw()
p
ggsave("NO2 Daily Mean Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- NO2_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="day")+
  theme_bw()###needs some time
p
ggsave("NO2 3 day Boxplot Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(NO2_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- NO2_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()
p
ggsave("NO2 Weekly Mean Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- NO2_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()###needs some time
p
ggsave("NO2 Weekly Boxplot Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#######################################################################################
#######################################################################################

Sound_df <- Mean_lists_day[5]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO


anyNA(Sound_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(Sound_df)
p <- Sound_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound", x="Day")+
  theme_bw()
p
ggsave("Sound Daily Mean Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- Sound_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="day")+
  theme_bw()###needs some time
p
ggsave("Sound 3 day Boxplot Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(Sound_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- Sound_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="Week")+
  theme_bw()
p
ggsave("Sound Weekly Mean Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- Sound_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="Week")+
  theme_bw()###needs some time
p
ggsave("Sound Weekly Boxplot Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#######################################################################################
#######################################################################################

Temp_df <- Mean_lists_day[6]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO

anyNA(Temp_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(Temp_df)
p <- Temp_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature", x="Day")+
  theme_bw()
p
ggsave("Temp Daily Mean Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- Temp_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="day")+
  theme_bw()###needs some time
p
ggsave("Temp 3 day Boxplot Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(Temp_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- Temp_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()
p
ggsave("Temp Weekly Mean Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file



##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- Temp_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()###needs some time
p
ggsave("Temp Weekly Boxplot Jul .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#
#
#
#
#
#
#
#
#
#
#
#######################################################################################

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Aug")###this is the location of your files
Air_Quality <- read.csv("RVI_Aug_Clean.csv", header= TRUE)
library(tidyr)
#########
############
summary(Air_Quality)

str(Air_Quality)

###we have Timestamp not Date_Time
##make new column Date_Time
Air_Quality$Date_Time <-dmy_hm(Air_Quality$Timestamp)
str(Air_Quality) ##ok now

##remove unwanted column
Air_Quality<-Air_Quality[-c(1,2)]

str(Air_Quality)
mydf_list <- split( Air_Quality , f = Air_Quality$Variable )##save each parameter as a separate df with its name in mydf_list

library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)
library(rio)
library(lubridate)
str(Air_Quality)
str(mydf_list)

#######calculating mean of values by day and putting in Mean_lists_day list
Mean_lists_day = list()
for (i in 1:length(mydf_list)) {
  
  m <-mydf_list[[i]]%>%
    mutate(day = round_date(Date_Time, "day")) %>%
    group_by(day) %>%
    summarySE(measurevar="Value", groupvars=c("day"))
  
  listname <- paste("Mean_by_day_", names(mydf_list[i]))
  
  Mean_lists_day[[listname]] <-m
  
}


################################corrected down .....we can means od days for day, week and month plots
CO_df <- Mean_lists_day[1]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(CO_df$Value)
boxplot(CO_df$Value,plot=FALSE)$out
outliers <- boxplot(CO_df$Value,plot=FALSE)$out
print(outliers)

CO_df<-CO_df[-which(CO_df$Value %in% outliers),]
boxplot(CO_df$Value)
str(CO_df)

anyNA(CO_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(CO_df)
p <- CO_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO", x="Day")+
  theme_bw()
p
ggsave("CO Daily Mean Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- CO_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="day")+
  theme_bw()###needs some time
p
ggsave("CO 3 day Boxplot Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(CO_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- CO_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()
p
ggsave("CO Weekly Mean Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- CO_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()###needs some time
p
ggsave("CO Weekly Boxplot Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- CO_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= month, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("month"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="month")+
  theme_bw()
p
ggsave("CO month Mean Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- CO_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="month")+
  theme_bw()###needs some time
p
ggsave("CO month Boxplot Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file



######END of my correction .....you can repeat as above for other parameters

###you can repeat line 56-168 for each parameter
####################################



######i removed other parameters..  please correct as above for each parameter...line 56-168.






#############################################################################################
#############################################################################################
################################corrected down .....we can means od days for day, week and month plots

Hum_df <- Mean_lists_day[2]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO


anyNA(Hum_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(Hum_df)
p <- Hum_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity", x="Day")+
  theme_bw()
p
ggsave("Hum Daily Mean Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- Hum_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="day")+
  theme_bw()###needs some time
p
ggsave("Hum 3 day Boxplot Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(Hum_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- Hum_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()
p
ggsave("Humidity Weekly Mean Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- Hum_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()###needs some time
p
ggsave("Humidity Weekly Boxplot Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#######################################################################################


NO_df <- Mean_lists_day[3]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(NO_df$Value)
boxplot(NO_df$Value,plot=FALSE)$out
outliers <- boxplot(NO_df$Value,plot=FALSE)$out
print(outliers)

NO_df<-NO_df[-which(NO_df$Value %in% outliers),]
boxplot(NO_df$Value)
str(NO_df)

anyNA(NO_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(NO_df)
p <- NO_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO", x="Day")+
  theme_bw()
p
ggsave("NO Daily Mean Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- NO_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="day")+
  theme_bw()###needs some time
p
ggsave("NO 3 day Boxplot Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(NO_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- NO_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()
p
ggsave("NO Weekly Mean Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- NO_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()###needs some time
p
ggsave("NO Weekly Boxplot Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#######################################################################################

NO2_df <- Mean_lists_day[4]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(NO2_df$Value)
boxplot(NO2_df$Value,plot=FALSE)$out
outliers <- boxplot(NO2_df$Value,plot=FALSE)$out
print(outliers)

NO2_df<-NO2_df[-which(NO2_df$Value %in% outliers),]
boxplot(NO2_df$Value)
str(NO2_df)

anyNA(NO2_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(NO2_df)
p <- NO2_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2", x="Day")+
  theme_bw()
p
ggsave("NO2 Daily Mean Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- NO2_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="day")+
  theme_bw()###needs some time
p
ggsave("NO2 3 day Boxplot Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(NO2_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- NO2_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()
p
ggsave("NO2 Weekly Mean Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- NO2_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()###needs some time
p
ggsave("NO2 Weekly Boxplot Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#######################################################################################
#######################################################################################

Sound_df <- Mean_lists_day[5]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO


str(Sound_df)

anyNA(Sound_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(Sound_df)
p <- Sound_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound", x="Day")+
  theme_bw()
p
ggsave("Sound Daily Mean Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- Sound_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="day")+
  theme_bw()###needs some time
p
ggsave("Sound 3 day Boxplot Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(Sound_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- Sound_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="Week")+
  theme_bw()
p
ggsave("Sound Weekly Mean Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- Sound_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="Week")+
  theme_bw()###needs some time
p
ggsave("Sound Weekly Boxplot Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#######################################################################################
#######################################################################################

Temp_df <- Mean_lists_day[6]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO


anyNA(Temp_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(Temp_df)
p <- Temp_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature", x="Day")+
  theme_bw()
p
ggsave("Temp Daily Mean Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- Temp_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="day")+
  theme_bw()###needs some time
p
ggsave("Temp 3 day Boxplot Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(Temp_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- Temp_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()
p
ggsave("Temp Weekly Mean Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- Temp_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()###needs some time
p
ggsave("Temp Weekly Boxplot Aug .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#
#
#
#
#
#
#
#
#
#
#
#######################################################################################

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Sep")###this is the location of your files
Air_Quality <- read.csv("RVI_Sep_Clean.csv", header= TRUE)
library(tidyr)
#########
############
summary(Air_Quality)

str(Air_Quality)

###we have Timestamp not Date_Time
##make new column Date_Time
Air_Quality$Date_Time <-dmy_hm(Air_Quality$Timestamp)
str(Air_Quality) ##ok now

##remove unwanted column
Air_Quality<-Air_Quality[-c(1,2)]

str(Air_Quality)
mydf_list <- split( Air_Quality , f = Air_Quality$Variable )##save each parameter as a separate df with its name in mydf_list

library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)
library(rio)
library(lubridate)
str(Air_Quality)
str(mydf_list)

#######calculating mean of values by day and putting in Mean_lists_day list
Mean_lists_day = list()
for (i in 1:length(mydf_list)) {
  
  m <-mydf_list[[i]]%>%
    mutate(day = round_date(Date_Time, "day")) %>%
    group_by(day) %>%
    summarySE(measurevar="Value", groupvars=c("day"))
  
  listname <- paste("Mean_by_day_", names(mydf_list[i]))
  
  Mean_lists_day[[listname]] <-m
  
}


################################corrected down .....we can means od days for day, week and month plots
CO_df <- Mean_lists_day[1]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(CO_df$Value)
boxplot(CO_df$Value,plot=FALSE)$out
outliers <- boxplot(CO_df$Value,plot=FALSE)$out
print(outliers)

CO_df<-CO_df[-which(CO_df$Value %in% outliers),]
boxplot(CO_df$Value)
str(CO_df)

anyNA(CO_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(CO_df)
p <- CO_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO", x="Day")+
  theme_bw()
p
ggsave("CO Daily Mean Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- CO_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="day")+
  theme_bw()###needs some time
p
ggsave("CO 3 day Boxplot Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(CO_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- CO_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()
p
ggsave("CO Weekly Mean Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- CO_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()###needs some time
p
ggsave("CO Weekly Boxplot Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- CO_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= month, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("month"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="month")+
  theme_bw()
p
ggsave("CO month Mean Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- CO_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="month")+
  theme_bw()###needs some time
p
ggsave("CO month Boxplot Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file



######END of my correction .....you can repeat as above for other parameters

###you can repeat line 56-168 for each parameter
####################################



######i removed other parameters..  please correct as above for each parameter...line 56-168.






#############################################################################################
#############################################################################################
################################corrected down .....we can means od days for day, week and month plots

Hum_df <- Mean_lists_day[2]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO

anyNA(Hum_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(Hum_df)
p <- Hum_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity", x="Day")+
  theme_bw()
p
ggsave("Hum Daily Mean Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- Hum_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="day")+
  theme_bw()###needs some time
p
ggsave("Hum 3 day Boxplot Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(Hum_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- Hum_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()
p
ggsave("Humidity Weekly Mean Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- Hum_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()###needs some time
p
ggsave("Humidity Weekly Boxplot Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#######################################################################################


NO_df <- Mean_lists_day[3]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(NO_df$Value)
boxplot(NO_df$Value,plot=FALSE)$out
outliers <- boxplot(NO_df$Value,plot=FALSE)$out
print(outliers)

NO_df<-NO_df[-which(NO_df$Value %in% outliers),]
boxplot(NO_df$Value)
str(NO_df)

anyNA(NO_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(NO_df)
p <- NO_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO", x="Day")+
  theme_bw()
p
ggsave("NO Daily Mean Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- NO_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="day")+
  theme_bw()###needs some time
p
ggsave("NO 3 day Boxplot Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(NO_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- NO_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()
p
ggsave("NO Weekly Mean Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- NO_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()###needs some time
p
ggsave("NO Weekly Boxplot Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#######################################################################################

NO2_df <- Mean_lists_day[4]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(NO2_df$Value)
boxplot(NO2_df$Value,plot=FALSE)$out
outliers <- boxplot(NO2_df$Value,plot=FALSE)$out
print(outliers)

NO2_df<-NO2_df[-which(NO2_df$Value %in% outliers),]
boxplot(NO2_df$Value)
str(NO2_df)

anyNA(NO2_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(NO2_df)
p <- NO2_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2", x="Day")+
  theme_bw()
p
ggsave("NO2 Daily Mean Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- NO2_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="day")+
  theme_bw()###needs some time
p
ggsave("NO2 3 day Boxplot Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(NO2_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- NO2_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()
p
ggsave("NO2 Weekly Mean Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- NO2_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()###needs some time
p
ggsave("NO2 Weekly Boxplot Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#######################################################################################
#######################################################################################

Sound_df <- Mean_lists_day[5]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO

str(Sound_df)

anyNA(Sound_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(Sound_df)
p <- Sound_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound", x="Day")+
  theme_bw()
p
ggsave("Sound Daily Mean Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- Sound_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="day")+
  theme_bw()###needs some time
p
ggsave("Sound 3 day Boxplot Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(Sound_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- Sound_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="Week")+
  theme_bw()
p
ggsave("Sound Weekly Mean Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- Sound_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="Week")+
  theme_bw()###needs some time
p
ggsave("Sound Weekly Boxplot Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#######################################################################################
#######################################################################################

Temp_df <- Mean_lists_day[6]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO

str(Temp_df)

anyNA(Temp_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(Temp_df)
p <- Temp_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature", x="Day")+
  theme_bw()
p
ggsave("Temp Daily Mean Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- Temp_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="day")+
  theme_bw()###needs some time
p
ggsave("Temp 3 day Boxplot Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(Temp_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- Temp_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()
p
ggsave("Temp Weekly Mean Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- Temp_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()###needs some time
p
ggsave("Temp Weekly Boxplot Sep .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#
#
#
#
#
#
#
#
#
#
#
#######################################################################################

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Oct")###this is the location of your files
Air_Quality <- read.csv("RVI_Oct_Clean.csv", header= TRUE)
library(tidyr)
#########
############
summary(Air_Quality)

str(Air_Quality)

###we have Timestamp not Date_Time
##make new column Date_Time
Air_Quality$Date_Time <-dmy_hm(Air_Quality$Timestamp)
str(Air_Quality) ##ok now

##remove unwanted column
Air_Quality<-Air_Quality[-c(1,2)]

str(Air_Quality)
mydf_list <- split( Air_Quality , f = Air_Quality$Variable )##save each parameter as a separate df with its name in mydf_list

library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)
library(rio)
library(lubridate)
str(Air_Quality)
str(mydf_list)

#######calculating mean of values by day and putting in Mean_lists_day list
Mean_lists_day = list()
for (i in 1:length(mydf_list)) {
  
  m <-mydf_list[[i]]%>%
    mutate(day = round_date(Date_Time, "day")) %>%
    group_by(day) %>%
    summarySE(measurevar="Value", groupvars=c("day"))
  
  listname <- paste("Mean_by_day_", names(mydf_list[i]))
  
  Mean_lists_day[[listname]] <-m
  
}


################################corrected down .....we can means od days for day, week and month plots
CO_df <- Mean_lists_day[1]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(CO_df$Value)
boxplot(CO_df$Value,plot=FALSE)$out
outliers <- boxplot(CO_df$Value,plot=FALSE)$out
print(outliers)

CO_df<-CO_df[-which(CO_df$Value %in% outliers),]
boxplot(CO_df$Value)
str(CO_df)

anyNA(CO_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(CO_df)
p <- CO_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO", x="Day")+
  theme_bw()
p
ggsave("CO Daily Mean Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- CO_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="day")+
  theme_bw()###needs some time
p
ggsave("CO 3 day Boxplot Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(CO_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- CO_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()
p
ggsave("CO Weekly Mean Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- CO_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()###needs some time
p
ggsave("CO Weekly Boxplot Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- CO_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= month, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("month"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="month")+
  theme_bw()
p
ggsave("CO month Mean Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- CO_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="month")+
  theme_bw()###needs some time
p
ggsave("CO month Boxplot Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file



######END of my correction .....you can repeat as above for other parameters

###you can repeat line 56-168 for each parameter
####################################



######i removed other parameters..  please correct as above for each parameter...line 56-168.






#############################################################################################
#############################################################################################
################################corrected down .....we can means od days for day, week and month plots

Hum_df <- Mean_lists_day[2]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO

anyNA(Hum_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(Hum_df)
p <- Hum_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity", x="Day")+
  theme_bw()
p
ggsave("Hum Daily Mean Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- Hum_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="day")+
  theme_bw()###needs some time
p
ggsave("Hum 3 day Boxplot Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(Hum_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- Hum_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()
p
ggsave("Humidity Weekly Mean Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- Hum_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()###needs some time
p
ggsave("Humidity Weekly Boxplot Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#######################################################################################


NO_df <- Mean_lists_day[3]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(NO_df$Value)
boxplot(NO_df$Value,plot=FALSE)$out
outliers <- boxplot(NO_df$Value,plot=FALSE)$out
print(outliers)

NO_df<-NO_df[-which(NO_df$Value %in% outliers),]
boxplot(NO_df$Value)
str(NO_df)

anyNA(NO_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(NO_df)
p <- NO_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO", x="Day")+
  theme_bw()
p
ggsave("NO Daily Mean Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- NO_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="day")+
  theme_bw()###needs some time
p
ggsave("NO 3 day Boxplot Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(NO_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- NO_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()
p
ggsave("NO Weekly Mean Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- NO_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()###needs some time
p
ggsave("NO Weekly Boxplot Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#######################################################################################

NO2_df <- Mean_lists_day[4]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(NO2_df$Value)
boxplot(NO2_df$Value,plot=FALSE)$out
outliers <- boxplot(NO2_df$Value,plot=FALSE)$out
print(outliers)

NO2_df<-NO2_df[-which(NO2_df$Value %in% outliers),]
boxplot(NO2_df$Value)
str(NO2_df)

anyNA(NO2_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(NO2_df)
p <- NO2_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2", x="Day")+
  theme_bw()
p
ggsave("NO2 Daily Mean Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- NO2_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="day")+
  theme_bw()###needs some time
p
ggsave("NO2 3 day Boxplot Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(NO2_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- NO2_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()
p
ggsave("NO2 Weekly Mean Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- NO2_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()###needs some time
p
ggsave("NO2 Weekly Boxplot Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#######################################################################################
#######################################################################################

Sound_df <- Mean_lists_day[5]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO

anyNA(Sound_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(Sound_df)
p <- Sound_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound", x="Day")+
  theme_bw()
p
ggsave("Sound Daily Mean Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- Sound_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="day")+
  theme_bw()###needs some time
p
ggsave("Sound 3 day Boxplot Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(Sound_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- Sound_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="Week")+
  theme_bw()
p
ggsave("Sound Weekly Mean Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- Sound_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="Week")+
  theme_bw()###needs some time
p
ggsave("Sound Weekly Boxplot Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#######################################################################################
#######################################################################################

Temp_df <- Mean_lists_day[6]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO


anyNA(Temp_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(Temp_df)
p <- Temp_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature", x="Day")+
  theme_bw()
p
ggsave("Temp Daily Mean Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- Temp_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="day")+
  theme_bw()###needs some time
p
ggsave("Temp 3 day Boxplot Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(Temp_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- Temp_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()
p
ggsave("Temp Weekly Mean Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- Temp_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()###needs some time
p
ggsave("Temp Weekly Boxplot Oct .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#
#
#
#
#
#
#
#
#
#
#
#######################################################################################

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Nov")###this is the location of your files
Air_Quality <- read.csv("RVI_Nov_Clean.csv", header= TRUE)
library(tidyr)
#########
############
summary(Air_Quality)

str(Air_Quality)

###we have Timestamp not Date_Time
##make new column Date_Time
Air_Quality$Date_Time <-dmy_hm(Air_Quality$Timestamp)
str(Air_Quality) ##ok now

##remove unwanted column
Air_Quality<-Air_Quality[-c(1,2)]

str(Air_Quality)
mydf_list <- split( Air_Quality , f = Air_Quality$Variable )##save each parameter as a separate df with its name in mydf_list

library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)
library(rio)
library(lubridate)
str(Air_Quality)
str(mydf_list)

#######calculating mean of values by day and putting in Mean_lists_day list
Mean_lists_day = list()
for (i in 1:length(mydf_list)) {
  
  m <-mydf_list[[i]]%>%
    mutate(day = round_date(Date_Time, "day")) %>%
    group_by(day) %>%
    summarySE(measurevar="Value", groupvars=c("day"))
  
  listname <- paste("Mean_by_day_", names(mydf_list[i]))
  
  Mean_lists_day[[listname]] <-m
  
}


################################corrected down .....we can means od days for day, week and month plots
CO_df <- Mean_lists_day[1]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(CO_df$Value)
boxplot(CO_df$Value,plot=FALSE)$out
outliers <- boxplot(CO_df$Value,plot=FALSE)$out
print(outliers)

CO_df<-CO_df[-which(CO_df$Value %in% outliers),]
boxplot(CO_df$Value)
str(CO_df)

anyNA(CO_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(CO_df)
p <- CO_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO", x="Day")+
  theme_bw()
p
ggsave("CO Daily Mean Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- CO_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="day")+
  theme_bw()###needs some time
p
ggsave("CO 3 day Boxplot Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(CO_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- CO_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()
p
ggsave("CO Weekly Mean Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- CO_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()###needs some time
p
ggsave("CO Weekly Boxplot Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- CO_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= month, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("month"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="month")+
  theme_bw()
p
ggsave("CO month Mean Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- CO_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="month")+
  theme_bw()###needs some time
p
ggsave("CO month Boxplot Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file



######END of my correction .....you can repeat as above for other parameters

###you can repeat line 56-168 for each parameter
####################################



######i removed other parameters..  please correct as above for each parameter...line 56-168.






#############################################################################################
#############################################################################################
################################corrected down .....we can means od days for day, week and month plots

Hum_df <- Mean_lists_day[2]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO


anyNA(Hum_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(Hum_df)
p <- Hum_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity", x="Day")+
  theme_bw()
p
ggsave("Hum Daily Mean Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- Hum_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="day")+
  theme_bw()###needs some time
p
ggsave("Hum 3 day Boxplot Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(Hum_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- Hum_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()
p
ggsave("Humidity Weekly Mean Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- Hum_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()###needs some time
p
ggsave("Humidity Weekly Boxplot Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#######################################################################################


NO_df <- Mean_lists_day[3]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(NO_df$Value)
boxplot(NO_df$Value,plot=FALSE)$out
outliers <- boxplot(NO_df$Value,plot=FALSE)$out
print(outliers)

NO_df<-NO_df[-which(NO_df$Value %in% outliers),]
boxplot(NO_df$Value)
str(NO_df)

anyNA(NO_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(NO_df)
p <- NO_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO", x="Day")+
  theme_bw()
p
ggsave("NO Daily Mean Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- NO_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="day")+
  theme_bw()###needs some time
p
ggsave("NO 3 day Boxplot Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(NO_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- NO_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()
p
ggsave("NO Weekly Mean Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- NO_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()###needs some time
p
ggsave("NO Weekly Boxplot Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#######################################################################################

NO2_df <- Mean_lists_day[4]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(NO2_df$Value)
boxplot(NO2_df$Value,plot=FALSE)$out
outliers <- boxplot(NO2_df$Value,plot=FALSE)$out
print(outliers)

NO2_df<-NO2_df[-which(NO2_df$Value %in% outliers),]
boxplot(NO2_df$Value)
str(NO2_df)

anyNA(NO2_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(NO2_df)
p <- NO2_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2", x="Day")+
  theme_bw()
p
ggsave("NO2 Daily Mean Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- NO2_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="day")+
  theme_bw()###needs some time
p
ggsave("NO2 3 day Boxplot Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(NO2_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- NO2_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()
p
ggsave("NO2 Weekly Mean Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- NO2_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()###needs some time
p
ggsave("NO2 Weekly Boxplot Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#######################################################################################
#######################################################################################

Sound_df <- Mean_lists_day[5]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO


anyNA(Sound_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(Sound_df)
p <- Sound_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound", x="Day")+
  theme_bw()
p
ggsave("Sound Daily Mean Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- Sound_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="day")+
  theme_bw()###needs some time
p
ggsave("Sound 3 day Boxplot Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(Sound_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- Sound_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="Week")+
  theme_bw()
p
ggsave("Sound Weekly Mean Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- Sound_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="Week")+
  theme_bw()###needs some time
p
ggsave("Sound Weekly Boxplot Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#######################################################################################
#######################################################################################

Temp_df <- Mean_lists_day[6]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO


anyNA(Temp_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(Temp_df)
p <- Temp_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature", x="Day")+
  theme_bw()
p
ggsave("Temp Daily Mean Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- Temp_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="day")+
  theme_bw()###needs some time
p
ggsave("Temp 3 day Boxplot Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(Temp_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- Temp_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()
p
ggsave("Temp Weekly Mean Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- Temp_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()###needs some time
p
ggsave("Temp Weekly Boxplot Nov .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#
#
#
#
#
#
#
#
#
#
#
#######################################################################################

setwd("~/Dissertation/Dissertation Datasets/RVI/RVI_Dec")###this is the location of your files
Air_Quality <- read.csv("RVI_Dec_Clean.csv", header= TRUE)
library(tidyr)
#########
############
summary(Air_Quality)

str(Air_Quality)

###we have Timestamp not Date_Time
##make new column Date_Time
Air_Quality$Date_Time <-dmy_hm(Air_Quality$Timestamp)
str(Air_Quality) ##ok now

##remove unwanted column
Air_Quality<-Air_Quality[-c(1,2)]

str(Air_Quality)
mydf_list <- split( Air_Quality , f = Air_Quality$Variable )##save each parameter as a separate df with its name in mydf_list

library(Rmisc)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)
library(rio)
library(lubridate)
str(Air_Quality)
str(mydf_list)

#######calculating mean of values by day and putting in Mean_lists_day list
Mean_lists_day = list()
for (i in 1:length(mydf_list)) {
  
  m <-mydf_list[[i]]%>%
    mutate(day = round_date(Date_Time, "day")) %>%
    group_by(day) %>%
    summarySE(measurevar="Value", groupvars=c("day"))
  
  listname <- paste("Mean_by_day_", names(mydf_list[i]))
  
  Mean_lists_day[[listname]] <-m
  
}


################################corrected down .....we can means od days for day, week and month plots
CO_df <- Mean_lists_day[1]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(CO_df$Value)
boxplot(CO_df$Value,plot=FALSE)$out
outliers <- boxplot(CO_df$Value,plot=FALSE)$out
print(outliers)

CO_df<-CO_df[-which(CO_df$Value %in% outliers),]
boxplot(CO_df$Value)
str(CO_df)

anyNA(CO_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(CO_df)
p <- CO_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO", x="Day")+
  theme_bw()
p
ggsave("CO Daily Mean Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- CO_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="day")+
  theme_bw()###needs some time
p
ggsave("CO 3 day Boxplot Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(CO_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- CO_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()
p
ggsave("CO Weekly Mean Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- CO_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()###needs some time
p
ggsave("CO Weekly Boxplot Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- CO_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= month, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("month"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="month")+
  theme_bw()
p
ggsave("CO month Mean Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- CO_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="month")+
  theme_bw()###needs some time
p
ggsave("CO month Boxplot Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file



######END of my correction .....you can repeat as above for other parameters

###you can repeat line 56-168 for each parameter
####################################



######i removed other parameters..  please correct as above for each parameter...line 56-168.






#############################################################################################
#############################################################################################
################################corrected down .....we can means od days for day, week and month plots

Hum_df <- Mean_lists_day[2]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO


anyNA(Hum_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(Hum_df)
p <- Hum_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity", x="Day")+
  theme_bw()
p
ggsave("Hum Daily Mean Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- Hum_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="day")+
  theme_bw()###needs some time
p
ggsave("Hum 3 day Boxplot Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(Hum_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- Hum_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()
p
ggsave("Humidity Weekly Mean Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- Hum_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Humidity",x="Week")+
  theme_bw()###needs some time
p
ggsave("Humidity Weekly Boxplot Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#######################################################################################


NO_df <- Mean_lists_day[3]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(NO_df$Value)
boxplot(NO_df$Value,plot=FALSE)$out
outliers <- boxplot(NO_df$Value,plot=FALSE)$out
print(outliers)

NO_df<-NO_df[-which(NO_df$Value %in% outliers),]
boxplot(NO_df$Value)
str(NO_df)

anyNA(NO_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(NO_df)
p <- NO_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO", x="Day")+
  theme_bw()
p
ggsave("NO Daily Mean Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- NO_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="day")+
  theme_bw()###needs some time
p
ggsave("NO 3 day Boxplot Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(NO_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- NO_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()
p
ggsave("NO Weekly Mean Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- NO_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()###needs some time
p
ggsave("NO Weekly Boxplot Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#######################################################################################

NO2_df <- Mean_lists_day[4]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(NO2_df$Value)
boxplot(NO2_df$Value,plot=FALSE)$out
outliers <- boxplot(NO2_df$Value,plot=FALSE)$out
print(outliers)

NO2_df<-NO2_df[-which(NO2_df$Value %in% outliers),]
boxplot(NO2_df$Value)
str(NO2_df)

anyNA(NO2_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(NO2_df)
p <- NO2_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2", x="Day")+
  theme_bw()
p
ggsave("NO2 Daily Mean Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- NO2_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="day")+
  theme_bw()###needs some time
p
ggsave("NO2 3 day Boxplot Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(NO2_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- NO2_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()
p
ggsave("NO2 Weekly Mean Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- NO2_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()###needs some time
p
ggsave("NO2 Weekly Boxplot Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#######################################################################################
#######################################################################################

Sound_df <- Mean_lists_day[5]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO

anyNA(Sound_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(Sound_df)
p <- Sound_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound", x="Day")+
  theme_bw()
p
ggsave("Sound Daily Mean Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- Sound_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="day")+
  theme_bw()###needs some time
p
ggsave("Sound 3 day Boxplot Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(Sound_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- Sound_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="Week")+
  theme_bw()
p
ggsave("Sound Weekly Mean Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- Sound_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="Week")+
  theme_bw()###needs some time
p
ggsave("Sound Weekly Boxplot Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#######################################################################################
#######################################################################################

Temp_df <- Mean_lists_day[6]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO


anyNA(Temp_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(Temp_df)
p <- Temp_df %>%
  ######here we need to put day in stead of Date_Time because we no longer have it in CO_df
  ###as our current CO_df is from Mean_list_day we have day column
  ####
  ####
  mutate(day = round_date(day, "day")) %>%
  group_by(day) %>%
  ggplot(aes(x= day, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("5 days"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature", x="Day")+
  theme_bw()
p
ggsave("Temp Daily Mean Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- Temp_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="day")+
  theme_bw()###needs some time
p
ggsave("Temp 3 day Boxplot Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(Temp_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- Temp_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= week, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("week"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()
p
ggsave("Temp Weekly Mean Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- Temp_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Temperature",x="Week")+
  theme_bw()###needs some time
p
ggsave("Temp Weekly Boxplot Dec .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################

