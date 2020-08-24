setwd("~/Dissertation/Dissertation Datasets/Dinnington near Airport/Dinnington_Jan20")###this is the location of your files
Air_Quality <- read.csv("Dinnington_Jan20_Clean.csv", header= TRUE)
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
O3_df <- Mean_lists_day[4]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(O3_df$Value)
boxplot(O3_df$Value,plot=FALSE)$out
outliers <- boxplot(O3_df$Value,plot=FALSE)$out
print(outliers)

O3_df<-O3_df[-which(O3_df$Value %in% outliers),]
boxplot(O3_df$Value)
str(O3_df)

anyNA(O3_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(O3_df)
p <- O3_df %>%
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
ggsave("O3 Daily Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- O3_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="day")+
  theme_bw()###needs some time
p
ggsave("O3 3 day Boxplot Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(O3_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- O3_df %>%
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
ggsave("O3 Weekly Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- O3_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()###needs some time
p
ggsave("O3 Weekly Boxplot Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- O3_df %>%
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
ggsave("O3 month Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- O3_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="month")+
  theme_bw()###needs some time
p
ggsave("O3 month Boxplot Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#
#
#
#
NO_df <- Mean_lists_day[2]%>%
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
ggsave("NO Daily Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- NO_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="day")+
  theme_bw()###needs some time
p
ggsave("NO 3 day Boxplot Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("NO Weekly Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- NO_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()###needs some time
p
ggsave("NO Weekly Boxplot Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- NO_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= month, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("month"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="month")+
  theme_bw()
p
ggsave("NO month Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- NO_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="month")+
  theme_bw()###needs some time
p
ggsave("NO month Boxplot Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#
#
#
#
NO2_df <- Mean_lists_day[3]%>%
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
ggsave("NO2 Daily Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- NO2_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="day")+
  theme_bw()###needs some time
p
ggsave("NO2 3 day Boxplot Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("NO2 Weekly Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- NO2_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()###needs some time
p
ggsave("NO2 Weekly Boxplot Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- NO2_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= month, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("month"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="month")+
  theme_bw()
p
ggsave("NO2 month Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- NO2_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="month")+
  theme_bw()###needs some time
p
ggsave("NO2 month Boxplot Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#
#
#
#
PM4_df <- Mean_lists_day[5]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(PM4_df$Value)
boxplot(PM4_df$Value,plot=FALSE)$out
outliers <- boxplot(PM4_df$Value,plot=FALSE)$out
print(outliers)

PM4_df<-PM4_df[-which(PM4_df$Value %in% outliers),]
boxplot(PM4_df$Value)
str(PM4_df)

anyNA(PM4_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(PM4_df)
p <- PM4_df %>%
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
ggsave("PM4 Daily Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- PM4_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="day")+
  theme_bw()###needs some time
p
ggsave("PM4 3 day Boxplot Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(PM4_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- PM4_df %>%
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
ggsave("PM4 Weekly Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- PM4_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="Week")+
  theme_bw()###needs some time
p
ggsave("PM4 Weekly Boxplot Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- PM4_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= month, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("month"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="month")+
  theme_bw()
p
ggsave("PM4 month Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- PM4_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="month")+
  theme_bw()###needs some time
p
ggsave("PM4 month Boxplot Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file
#
#
#
#
PM1_df <- Mean_lists_day[6]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(PM1_df$Value)
boxplot(PM1_df$Value,plot=FALSE)$out
outliers <- boxplot(PM1_df$Value,plot=FALSE)$out
print(outliers)

PM1_df<-PM1_df[-which(PM1_df$Value %in% outliers),]
boxplot(PM1_df$Value)
str(PM1_df)

anyNA(PM1_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(PM1_df)
p <- PM1_df %>%
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
ggsave("PM1 Daily Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- PM1_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="day")+
  theme_bw()###needs some time
p
ggsave("PM1 3 day Boxplot Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(PM1_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- PM1_df %>%
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
ggsave("PM1 Weekly Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- PM1_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="Week")+
  theme_bw()###needs some time
p
ggsave("PM1 Weekly Boxplot Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- PM1_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= month, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("month"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="month")+
  theme_bw()
p
ggsave("PM1 month Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- PM1_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="month")+
  theme_bw()###needs some time
p
ggsave("PM1 month Boxplot Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file
#
#
#
#
PM10_df <- Mean_lists_day[7]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(PM10_df$Value)
boxplot(PM10_df$Value,plot=FALSE)$out
outliers <- boxplot(PM10_df$Value,plot=FALSE)$out
print(outliers)

PM10_df<-PM10_df[-which(PM10_df$Value %in% outliers),]
boxplot(PM10_df$Value)
str(PM10_df)

anyNA(PM10_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(PM10_df)
p <- PM10_df %>%
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
ggsave("PM10 Daily Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- PM4_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="day")+
  theme_bw()###needs some time
p
ggsave("PM10 3 day Boxplot Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(PM10_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- PM10_df %>%
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
ggsave("PM10 Weekly Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- PM10_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="Week")+
  theme_bw()###needs some time
p
ggsave("PM10 Weekly Boxplot Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- PM10_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= month, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("month"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="month")+
  theme_bw()
p
ggsave("PM10 month Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- PM10_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="month")+
  theme_bw()###needs some time
p
ggsave("PM10 month Boxplot Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file
#
#
#
#
PM2.5_df <- Mean_lists_day[8]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(PM2.5_df$Value)
boxplot(PM2.5_df$Value,plot=FALSE)$out
outliers <- boxplot(PM2.5_df$Value,plot=FALSE)$out
print(outliers)

PM2.5_df<-PM2.5_df[-which(PM2.5_df$Value %in% outliers),]
boxplot(PM2.5_df$Value)
str(PM2.5_df)

anyNA(PM2.5_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(PM2.5_df)
p <- PM2.5_df %>%
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
ggsave("PM2.5 Daily Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- PM2.5_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="day")+
  theme_bw()###needs some time
p
ggsave("PM2.5 3 day Boxplot Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(PM2.5_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- PM2.5_df %>%
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
ggsave("PM2.5 Weekly Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- PM2.5_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="Week")+
  theme_bw()###needs some time
p
ggsave("PM2.5 Weekly Boxplot Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- PM2.5_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= month, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("month"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="month")+
  theme_bw()
p
ggsave("PM2.5 month Mean Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- PM2.5_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="month")+
  theme_bw()###needs some time
p
ggsave("PM2.5 month Boxplot Jan20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
setwd("~/Dissertation/Dissertation Datasets/Dinnington near Airport/Dinnington_Feb20")###this is the location of your files
Air_Quality <- read.csv("Dinnington_Feb20_Clean.csv", header= TRUE)
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
O3_df <- Mean_lists_day[4]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(O3_df$Value)
boxplot(O3_df$Value,plot=FALSE)$out
outliers <- boxplot(O3_df$Value,plot=FALSE)$out
print(outliers)

O3_df<-O3_df[-which(O3_df$Value %in% outliers),]
boxplot(O3_df$Value)
str(O3_df)

anyNA(O3_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(O3_df)
p <- O3_df %>%
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
ggsave("O3 Daily Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- O3_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="day")+
  theme_bw()###needs some time
p
ggsave("O3 3 day Boxplot Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(O3_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- O3_df %>%
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
ggsave("O3 Weekly Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- O3_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()###needs some time
p
ggsave("O3 Weekly Boxplot Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- O3_df %>%
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
ggsave("O3 month Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- O3_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="month")+
  theme_bw()###needs some time
p
ggsave("O3 month Boxplot Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#
#
#
#
NO_df <- Mean_lists_day[2]%>%
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
ggsave("NO Daily Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- NO_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="day")+
  theme_bw()###needs some time
p
ggsave("NO 3 day Boxplot Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("NO Weekly Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- NO_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()###needs some time
p
ggsave("NO Weekly Boxplot Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- NO_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= month, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("month"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="month")+
  theme_bw()
p
ggsave("NO month Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- NO_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="month")+
  theme_bw()###needs some time
p
ggsave("NO month Boxplot Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#
#
#
#
NO2_df <- Mean_lists_day[3]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(NO2_df$Value)
boxplot(NO2_df$Value,plot=FALSE)$out
outliers <- boxplot(NO2_df$Value,plot=FALSE)$out
print(outliers)

NO2_df<N-O2_df[-which(NO2_df$Value %in% outliers),]
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
ggsave("NO2 Daily Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- NO2_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="day")+
  theme_bw()###needs some time
p
ggsave("NO2 3 day Boxplot Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("NO2 Weekly Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- NO2_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()###needs some time
p
ggsave("NO2 Weekly Boxplot Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- NO2_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= month, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("month"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="month")+
  theme_bw()
p
ggsave("NO2 month Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- NO2_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="month")+
  theme_bw()###needs some time
p
ggsave("NO2 month Boxplot Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#
#
#
#
PM4_df <- Mean_lists_day[5]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(PM4_df$Value)
boxplot(PM4_df$Value,plot=FALSE)$out
outliers <- boxplot(PM4_df$Value,plot=FALSE)$out
print(outliers)

PM4_df<-PM4_df[-which(PM4_df$Value %in% outliers),]
boxplot(PM4_df$Value)
str(PM4_df)

anyNA(PM4_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(PM4_df)
p <- PM4_df %>%
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
ggsave("PM4 Daily Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- PM4_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="day")+
  theme_bw()###needs some time
p
ggsave("PM4 3 day Boxplot Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(PM4_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- PM4_df %>%
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
ggsave("PM4 Weekly Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- PM4_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="Week")+
  theme_bw()###needs some time
p
ggsave("PM4 Weekly Boxplot Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- PM4_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= month, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("month"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="month")+
  theme_bw()
p
ggsave("PM4 month Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- PM4_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="month")+
  theme_bw()###needs some time
p
ggsave("PM4 month Boxplot Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file
#
#
#
#
PM1_df <- Mean_lists_day[6]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(PM1_df$Value)
boxplot(PM1_df$Value,plot=FALSE)$out
outliers <- boxplot(PM1_df$Value,plot=FALSE)$out
print(outliers)

PM1_df<-PM1_df[-which(PM1_df$Value %in% outliers),]
boxplot(PM1_df$Value)
str(PM1_df)

anyNA(PM1_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(PM1_df)
p <- PM1_df %>%
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
ggsave("PM1 Daily Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- PM1_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="day")+
  theme_bw()###needs some time
p
ggsave("PM1 3 day Boxplot Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(PM1_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- PM1_df %>%
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
ggsave("PM1 Weekly Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- PM1_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="Week")+
  theme_bw()###needs some time
p
ggsave("PM1 Weekly Boxplot Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- PM1_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= month, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("month"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="month")+
  theme_bw()
p
ggsave("PM1 month Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- PM1_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="month")+
  theme_bw()###needs some time
p
ggsave("PM1 month Boxplot Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file
#
#
#
#
PM10_df <- Mean_lists_day[7]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(PM10_df$Value)
boxplot(PM10_df$Value,plot=FALSE)$out
outliers <- boxplot(PM10_df$Value,plot=FALSE)$out
print(outliers)

PM10_df<-PM10_df[-which(PM10_df$Value %in% outliers),]
boxplot(PM10_df$Value)
str(PM10_df)

anyNA(PM10_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(PM10_df)
p <- PM10_df %>%
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
ggsave("PM10 Daily Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- PM4_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="day")+
  theme_bw()###needs some time
p
ggsave("PM10 3 day Boxplot Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(PM10_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- PM10_df %>%
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
ggsave("PM10 Weekly Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- PM10_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="Week")+
  theme_bw()###needs some time
p
ggsave("PM10 Weekly Boxplot Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- PM10_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= month, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("month"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="month")+
  theme_bw()
p
ggsave("PM10 month Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- PM10_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="month")+
  theme_bw()###needs some time
p
ggsave("PM10 month Boxplot Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file
#
#
#
#
PM2.5_df <- Mean_lists_day[8]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(PM2.5_df$Value)
boxplot(PM2.5_df$Value,plot=FALSE)$out
outliers <- boxplot(PM2.5_df$Value,plot=FALSE)$out
print(outliers)

PM2.5_df<-PM2.5_df[-which(PM2.5_df$Value %in% outliers),]
boxplot(PM2.5_df$Value)
str(PM2.5_df)

anyNA(PM2.5_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(PM2.5_df)
p <- PM2.5_df %>%
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
ggsave("PM2.5 Daily Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- PM2.5_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="day")+
  theme_bw()###needs some time
p
ggsave("PM2.5 3 day Boxplot Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(PM2.5_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- PM2.5_df %>%
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
ggsave("PM2.5 Weekly Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- PM2.5_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="Week")+
  theme_bw()###needs some time
p
ggsave("PM2.5 Weekly Boxplot Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- PM2.5_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= month, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("month"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="month")+
  theme_bw()
p
ggsave("PM2.5 month Mean Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- PM2.5_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="month")+
  theme_bw()###needs some time
p
ggsave("PM2.5 month Boxplot Feb20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
setwd("~/Dissertation/Dissertation Datasets/Dinnington near Airport/Dinnington_Mar20")###this is the location of your files
Air_Quality <- read.csv("Dinnington_Mar20_Clean.csv", header= TRUE)
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
O3_df <- Mean_lists_day[4]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(O3_df$Value)
boxplot(O3_df$Value,plot=FALSE)$out
outliers <- boxplot(O3_df$Value,plot=FALSE)$out
print(outliers)

O3_df<-O3_df[-which(O3_df$Value %in% outliers),]
boxplot(O3_df$Value)
str(O3_df)

anyNA(O3_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(O3_df)
p <- O3_df %>%
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
ggsave("O3 Daily Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- O3_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="day")+
  theme_bw()###needs some time
p
ggsave("O3 3 day Boxplot Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(O3_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- O3_df %>%
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
ggsave("O3 Weekly Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- O3_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()###needs some time
p
ggsave("O3 Weekly Boxplot Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- O3_df %>%
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
ggsave("O3 month Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- O3_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="month")+
  theme_bw()###needs some time
p
ggsave("O3 month Boxplot Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#
#
#
#
NO_df <- Mean_lists_day[2]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(NO_df$Value)
boxplot(NO_df$Value,plot=FALSE)$out
outliers <- boxplot(NO_df$Value,plot=FALSE)$out
print(outliers)

NO_df-<NO_df[-which(NO_df$Value %in% outliers),]
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
ggsave("NO Daily Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- NO_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="day")+
  theme_bw()###needs some time
p
ggsave("NO 3 day Boxplot Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("NO Weekly Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- NO_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()###needs some time
p
ggsave("NO Weekly Boxplot Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- NO_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= month, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("month"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="month")+
  theme_bw()
p
ggsave("NO month Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- NO_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="month")+
  theme_bw()###needs some time
p
ggsave("NO month Boxplot Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#
#
#
#
NO2_df <- Mean_lists_day[3]%>%
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
ggsave("NO2 Daily Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- NO2_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="day")+
  theme_bw()###needs some time
p
ggsave("NO2 3 day Boxplot Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("NO2 Weekly Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- NO2_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()###needs some time
p
ggsave("NO2 Weekly Boxplot Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- NO2_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= month, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("month"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="month")+
  theme_bw()
p
ggsave("NO2 month Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- NO2_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="month")+
  theme_bw()###needs some time
p
ggsave("NO2 month Boxplot Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#
#
#
#
PM4_df <- Mean_lists_day[5]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(PM4_df$Value)
boxplot(PM4_df$Value,plot=FALSE)$out
outliers <- boxplot(PM4_df$Value,plot=FALSE)$out
print(outliers)

PM4_df<-PM4_df[-which(PM4_df$Value %in% outliers),]
boxplot(PM4_df$Value)
str(PM4_df)

anyNA(PM4_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(PM4_df)
p <- PM4_df %>%
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
ggsave("PM4 Daily Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- PM4_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="day")+
  theme_bw()###needs some time
p
ggsave("PM4 3 day Boxplot Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(PM4_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- PM4_df %>%
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
ggsave("PM4 Weekly Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- PM4_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="Week")+
  theme_bw()###needs some time
p
ggsave("PM4 Weekly Boxplot Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- PM4_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= month, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("month"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="month")+
  theme_bw()
p
ggsave("PM4 month Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- PM4_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="month")+
  theme_bw()###needs some time
p
ggsave("PM4 month Boxplot Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file
#
#
#
#
PM1_df <- Mean_lists_day[6]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(PM1_df$Value)
boxplot(PM1_df$Value,plot=FALSE)$out
outliers <- boxplot(PM1_df$Value,plot=FALSE)$out
print(outliers)

PM1_df<-PM1_df[-which(PM1_df$Value %in% outliers),]
boxplot(PM1_df$Value)
str(PM1_df)

anyNA(PM1_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(PM1_df)
p <- PM1_df %>%
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
ggsave("PM1 Daily Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- PM1_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="day")+
  theme_bw()###needs some time
p
ggsave("PM1 3 day Boxplot Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(PM1_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- PM1_df %>%
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
ggsave("PM1 Weekly Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- PM1_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="Week")+
  theme_bw()###needs some time
p
ggsave("PM1 Weekly Boxplot Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- PM1_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= month, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("month"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="month")+
  theme_bw()
p
ggsave("PM1 month Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- PM1_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="month")+
  theme_bw()###needs some time
p
ggsave("PM1 month Boxplot Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file
#
#
#
#
PM10_df <- Mean_lists_day[7]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(PM10_df$Value)
boxplot(PM10_df$Value,plot=FALSE)$out
outliers <- boxplot(PM10_df$Value,plot=FALSE)$out
print(outliers)

PM10_df<-PM10_df[-which(PM10_df$Value %in% outliers),]
boxplot(PM10_df$Value)
str(PM10_df)

anyNA(PM10_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(PM10_df)
p <- PM10_df %>%
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
ggsave("PM10 Daily Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- PM4_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="day")+
  theme_bw()###needs some time
p
ggsave("PM10 3 day Boxplot Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(PM10_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- PM10_df %>%
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
ggsave("PM10 Weekly Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- PM10_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="Week")+
  theme_bw()###needs some time
p
ggsave("PM10 Weekly Boxplot Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- PM10_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= month, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("month"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="month")+
  theme_bw()
p
ggsave("PM10 month Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- PM10_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="month")+
  theme_bw()###needs some time
p
ggsave("PM10 month Boxplot Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file
#
#
#
#
PM2.5_df <- Mean_lists_day[8]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(PM2.5_df$Value)
boxplot(PM2.5_df$Value,plot=FALSE)$out
outliers <- boxplot(PM2.5_df$Value,plot=FALSE)$out
print(outliers)

PM2.5_df<-PM2.5_df[-which(PM2.5_df$Value %in% outliers),]
boxplot(PM2.5_df$Value)
str(PM2.5_df)

anyNA(PM2.5_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(PM2.5_df)
p <- PM2.5_df %>%
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
ggsave("PM2.5 Daily Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- PM2.5_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="day")+
  theme_bw()###needs some time
p
ggsave("PM2.5 3 day Boxplot Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(PM2.5_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- PM2.5_df %>%
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
ggsave("PM2.5 Weekly Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- PM2.5_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="Week")+
  theme_bw()###needs some time
p
ggsave("PM2.5 Weekly Boxplot Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- PM2.5_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= month, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("month"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="month")+
  theme_bw()
p
ggsave("PM2.5 month Mean Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- PM2.5_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="month")+
  theme_bw()###needs some time
p
ggsave("PM2.5 month Boxplot Mar20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
setwd("~/Dissertation/Dissertation Datasets/Dinnington near Airport/Dinnington_Apr20")###this is the location of your files
Air_Quality <- read.csv("Dinnington_Apr20_Clean.csv", header= TRUE)
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
O3_df <- Mean_lists_day[4]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(O3_df$Value)
boxplot(O3_df$Value,plot=FALSE)$out
outliers <- boxplot(O3_df$Value,plot=FALSE)$out
print(outliers)

O3_df<-O3_df[-which(O3_df$Value %in% outliers),]
boxplot(O3_df$Value)
str(O3_df)

anyNA(O3_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(O3_df)
p <- O3_df %>%
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
ggsave("O3 Daily Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- O3_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="day")+
  theme_bw()###needs some time
p
ggsave("O3 3 day Boxplot Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(O3_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- O3_df %>%
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
ggsave("O3 Weekly Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- O3_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()###needs some time
p
ggsave("O3 Weekly Boxplot Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- O3_df %>%
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
ggsave("O3 month Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- O3_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="month")+
  theme_bw()###needs some time
p
ggsave("O3 month Boxplot Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#
#
#
#
NO_df <- Mean_lists_day[2]%>%
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
ggsave("NO Daily Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- NO_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="day")+
  theme_bw()###needs some time
p
ggsave("NO 3 day Boxplot Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("NO Weekly Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- NO_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()###needs some time
p
ggsave("NO Weekly Boxplot Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- NO_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= month, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("month"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="month")+
  theme_bw()
p
ggsave("NO month Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- NO_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="month")+
  theme_bw()###needs some time
p
ggsave("NO month Boxplot Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#
#
#
#
NO2_df <- Mean_lists_day[3]%>%
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
ggsave("NO2 Daily Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- NO2_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="day")+
  theme_bw()###needs some time
p
ggsave("NO2 3 day Boxplot Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("NO2 Weekly Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- NO2_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()###needs some time
p
ggsave("NO2 Weekly Boxplot Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- NO2_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= month, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("month"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="month")+
  theme_bw()
p
ggsave("NO2 month Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- NO2_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="month")+
  theme_bw()###needs some time
p
ggsave("NO2 month Boxplot Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#
#
#
#
PM4_df <- Mean_lists_day[5]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(PM4_df$Value)
boxplot(PM4_df$Value,plot=FALSE)$out
outliers <- boxplot(PM4_df$Value,plot=FALSE)$out
print(outliers)

PM4_df<-PM4_df[-which(PM4_df$Value %in% outliers),]
boxplot(PM4_df$Value)
str(PM4_df)

anyNA(PM4_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(PM4_df)
p <- PM4_df %>%
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
ggsave("PM4 Daily Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- PM4_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="day")+
  theme_bw()###needs some time
p
ggsave("PM4 3 day Boxplot Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(PM4_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- PM4_df %>%
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
ggsave("PM4 Weekly Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- PM4_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="Week")+
  theme_bw()###needs some time
p
ggsave("PM4 Weekly Boxplot Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- PM4_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= month, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("month"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="month")+
  theme_bw()
p
ggsave("PM4 month Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- PM4_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="month")+
  theme_bw()###needs some time
p
ggsave("PM4 month Boxplot Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file
#
#
#
#
PM1_df <- Mean_lists_day[6]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(PM1_df$Value)
boxplot(PM1_df$Value,plot=FALSE)$out
outliers <- boxplot(PM1_df$Value,plot=FALSE)$out
print(outliers)

PM1_df<-PM1_df[-which(PM1_df$Value %in% outliers),]
boxplot(PM1_df$Value)
str(PM1_df)

anyNA(PM1_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(PM1_df)
p <- PM1_df %>%
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
ggsave("PM1 Daily Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- PM1_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="day")+
  theme_bw()###needs some time
p
ggsave("PM1 3 day Boxplot Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(PM1_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- PM1_df %>%
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
ggsave("PM1 Weekly Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- PM1_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="Week")+
  theme_bw()###needs some time
p
ggsave("PM1 Weekly Boxplot Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- PM1_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= month, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("month"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="month")+
  theme_bw()
p
ggsave("PM1 month Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- PM1_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="month")+
  theme_bw()###needs some time
p
ggsave("PM1 month Boxplot Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file
#
#
#
#
PM10_df <- Mean_lists_day[7]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(PM10_df$Value)
boxplot(PM10_df$Value,plot=FALSE)$out
outliers <- boxplot(PM10_df$Value,plot=FALSE)$out
print(outliers)

PM10_df<-PM10_df[-which(PM10_df$Value %in% outliers),]
boxplot(PM10_df$Value)
str(PM10_df)

anyNA(PM10_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(PM10_df)
p <- PM10_df %>%
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
ggsave("PM10 Daily Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- PM4_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="day")+
  theme_bw()###needs some time
p
ggsave("PM10 3 day Boxplot Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(PM10_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- PM10_df %>%
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
ggsave("PM10 Weekly Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- PM10_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="Week")+
  theme_bw()###needs some time
p
ggsave("PM10 Weekly Boxplot Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- PM10_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= month, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("month"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="month")+
  theme_bw()
p
ggsave("PM10 month Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- PM10_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="month")+
  theme_bw()###needs some time
p
ggsave("PM10 month Boxplot Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file
#
#
#
#
PM2.5_df <- Mean_lists_day[8]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(PM2.5_df$Value)
boxplot(PM2.5_df$Value,plot=FALSE)$out
outliers <- boxplot(PM2.5_df$Value,plot=FALSE)$out
print(outliers)

PM2.5_df<-PM2.5_df[-which(PM2.5_df$Value %in% outliers),]
boxplot(PM2.5_df$Value)
str(PM2.5_df)

anyNA(PM2.5_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(PM2.5_df)
p <- PM2.5_df %>%
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
ggsave("PM2.5 Daily Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- PM2.5_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="day")+
  theme_bw()###needs some time
p
ggsave("PM2.5 3 day Boxplot Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(PM2.5_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- PM2.5_df %>%
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
ggsave("PM2.5 Weekly Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- PM2.5_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="Week")+
  theme_bw()###needs some time
p
ggsave("PM2.5 Weekly Boxplot Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- PM2.5_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= month, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("month"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="month")+
  theme_bw()
p
ggsave("PM2.5 month Mean Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- PM2.5_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="month")+
  theme_bw()###needs some time
p
ggsave("PM2.5 month Boxplot Apr20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
setwd("~/Dissertation/Dissertation Datasets/Dinnington near Airport/Dinnington_May20")###this is the location of your files
Air_Quality <- read.csv("Dinnington_May20_Clean.csv", header= TRUE)
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
O3_df <- Mean_lists_day[4]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(O3_df$Value)
boxplot(O3_df$Value,plot=FALSE)$out
outliers <- boxplot(O3_df$Value,plot=FALSE)$out
print(outliers)

O3_df<-O3_df[-which(O3_df$Value %in% outliers),]
boxplot(O3_df$Value)
str(O3_df)

anyNA(O3_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(O3_df)
p <- O3_df %>%
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
ggsave("O3 Daily Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- O3_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="day")+
  theme_bw()###needs some time
p
ggsave("O3 3 day Boxplot May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(O3_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- O3_df %>%
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
ggsave("O3 Weekly Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- O3_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="Week")+
  theme_bw()###needs some time
p
ggsave("O3 Weekly Boxplot May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- O3_df %>%
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
ggsave("O3 month Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- O3_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="CO",x="month")+
  theme_bw()###needs some time
p
ggsave("O3 month Boxplot May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#
#
#
#
NO_df <- Mean_lists_day[2]%>%
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
ggsave("NO Daily Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- NO_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="day")+
  theme_bw()###needs some time
p
ggsave("NO 3 day Boxplot May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("NO Weekly Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- NO_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="Week")+
  theme_bw()###needs some time
p
ggsave("NO Weekly Boxplot May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- NO_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= month, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("month"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="month")+
  theme_bw()
p
ggsave("NO month Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- NO_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO",x="month")+
  theme_bw()###needs some time
p
ggsave("NO month Boxplot May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#
#
#
#
NO2_df <- Mean_lists_day[3]%>%
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
ggsave("NO2 Daily Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- NO2_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="day")+
  theme_bw()###needs some time
p
ggsave("NO2 3 day Boxplot May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


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
ggsave("NO2 Weekly Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- NO2_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="Week")+
  theme_bw()###needs some time
p
ggsave("NO2 Weekly Boxplot May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- NO2_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= month, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("month"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="month")+
  theme_bw()
p
ggsave("NO2 month Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- NO2_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="NO2",x="month")+
  theme_bw()###needs some time
p
ggsave("NO2 month Boxplot May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#
#
#
#
PM4_df <- Mean_lists_day[5]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(PM4_df$Value)
boxplot(PM4_df$Value,plot=FALSE)$out
outliers <- boxplot(PM4_df$Value,plot=FALSE)$out
print(outliers)

PM4_df<-PM4_df[-which(PM4_df$Value %in% outliers),]
boxplot(PM4_df$Value)
str(PM4_df)

anyNA(PM4_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(PM4_df)
p <- PM4_df %>%
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
ggsave("PM4 Daily Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- PM4_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="day")+
  theme_bw()###needs some time
p
ggsave("PM4 3 day Boxplot May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(PM4_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- PM4_df %>%
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
ggsave("PM4 Weekly Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- PM4_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="Week")+
  theme_bw()###needs some time
p
ggsave("PM4 Weekly Boxplot May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- PM4_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= month, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("month"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="month")+
  theme_bw()
p
ggsave("PM4 month Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- PM4_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="month")+
  theme_bw()###needs some time
p
ggsave("PM4 month Boxplot May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file
#
#
#
#
PM1_df <- Mean_lists_day[6]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(PM1_df$Value)
boxplot(PM1_df$Value,plot=FALSE)$out
outliers <- boxplot(PM1_df$Value,plot=FALSE)$out
print(outliers)

PM1_df<-PM1_df[-which(PM1_df$Value %in% outliers),]
boxplot(PM1_df$Value)
str(PM1_df)

anyNA(PM1_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(PM1_df)
p <- PM1_df %>%
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
ggsave("PM1 Daily Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- PM1_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="day")+
  theme_bw()###needs some time
p
ggsave("PM1 3 day Boxplot May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(PM1_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- PM1_df %>%
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
ggsave("PM1 Weekly Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- PM1_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="Week")+
  theme_bw()###needs some time
p
ggsave("PM1 Weekly Boxplot May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- PM1_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= month, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("month"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="month")+
  theme_bw()
p
ggsave("PM1 month Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- PM1_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="month")+
  theme_bw()###needs some time
p
ggsave("PM1 month Boxplot May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file
#
#
#
#
PM10_df <- Mean_lists_day[7]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(PM10_df$Value)
boxplot(PM10_df$Value,plot=FALSE)$out
outliers <- boxplot(PM10_df$Value,plot=FALSE)$out
print(outliers)

PM10_df<-PM10_df[-which(PM10_df$Value %in% outliers),]

boxplot(PM10_df$Value)
str(PM10_df)

anyNA(PM10_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(PM10_df)
p <- PM10_df %>%
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
ggsave("PM10 Daily Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- PM4_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="day")+
  theme_bw()###needs some time
p
ggsave("PM10 3 day Boxplot May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(PM10_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- PM10_df %>%
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
ggsave("PM10 Weekly Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- PM10_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="Week")+
  theme_bw()###needs some time
p
ggsave("PM10 Weekly Boxplot May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- PM10_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= month, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("month"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="month")+
  theme_bw()
p
ggsave("PM10 month Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- PM10_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="month")+
  theme_bw()###needs some time
p
ggsave("PM10 month Boxplot May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file
#
#
#
#
PM2.5_df <- Mean_lists_day[8]%>%
  map_df(as_tibble)##to convert any list to data tibble
###########
###remove outliers for CO
boxplot(PM2.5_df$Value)
boxplot(PM2.5_df$Value,plot=FALSE)$out
outliers <- boxplot(PM2.5_df$Value,plot=FALSE)$out
print(outliers)

PM2.5_df<-PM2.5_df[-which(PM2.5_df$Value %in% outliers),]
boxplot(PM2.5_df$Value)
str(PM2.5_df)

anyNA(PM2.5_df)

############################our CO_df  is ready for plots
##ggplot mean+sd,se,ci by day
pd <- position_dodge(0.4) # move them .05 to the left and right

####NOTE: you lready have mean values by day so you don't need 
#to put summarySE(measurevar="Value", groupvars=c("day"))%>% in the following code

########################day################################################
str(PM2.5_df)
p <- PM2.5_df %>%
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
ggsave("PM2.5 Daily Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

##NOTE: we can make box plot of every 3 days
p<- PM2.5_df %>%
  mutate(day = round_date(day, "3 day")) %>%
  group_by(day) %>%
  ggplot(aes(x= as.factor (day), y=Value,colour=as.factor (day) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="day")+
  theme_bw()###needs some time
p
ggsave("PM2.5 3 day Boxplot May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


####by week
#############################################################week###################################
##ggplot mean+sd,se,ci by week from previous mean rounded by day of CO_df

str(PM2.5_df)
pd <- position_dodge(0.4) # move them .05 to the left and right

###Error Here in CO_df we dont have  Date_Time i replaced it with day 

p<- PM2.5_df %>%
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
ggsave("PM2.5 Weekly Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##or ggplot of box plot by week note: for boxplot x-axis need to be factor

p<- PM2.5_df %>%
  mutate(week = round_date(day, "week")) %>%
  group_by(week) %>%
  ggplot(aes(x= as.factor (week), y=Value,colour=as.factor (week) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="Week")+
  theme_bw()###needs some time
p
ggsave("PM2.5 Weekly Boxplot May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

#######################################################################################
#you can do the same by month for CO
##########
p<- PM2.5_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  summarize(Value = mean(Value), sd = mean(sd), se = mean(se), ci = mean(ci))%>%
  ggplot(aes(x= month, y=Value,colour=Variable ))+
  geom_errorbar(aes(ymin=Value-sd, ymax=Value+sd), colour="black", width=0.4,position=pd) +
  geom_point(position=pd, size=7, color = "red")+
  scale_x_datetime(date_labels = "%d %B %y",breaks = date_breaks("month"))+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="month")+
  theme_bw()
p
ggsave("PM2.5 month Mean May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file


##ggplot  box plot by month note: for boxplot x-axis need to be factor

p<- PM2.5_df %>%
  mutate(month = round_date(day, "month")) %>%
  group_by(month) %>%
  ggplot(aes(x= as.factor (month), y=Value,colour=as.factor (month) ))+
  geom_boxplot(aes()) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+labs(y="Sound",x="month")+
  theme_bw()###needs some time
p
ggsave("PM2.5 month Boxplot May20 .jpg", p, width = 24, height = 14,dpi = 300 )#save to file

