setwd("~/Dissertation/Dissertation Datasets/Haymarket")
COH<-read.csv('CO_Haymarket.csv')
setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station")
COG<-read.csv('CO_Gateshead.csv')
setwd("~/Dissertation/Dissertation Datasets/RVI")
COR<-read.csv('CO_RVI.csv')

CO<-rbind(COG,COH)
CO<-rbind(CO,COR)

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

setwd("~/Dissertation/Dissertation Datasets/Haymarket")
NO2H<-read.csv('NO2_Haymarket.csv')
setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station")
NO2G<-read.csv('NO2_Gateshead.csv')
setwd("~/Dissertation/Dissertation Datasets/RVI")
NO2R<-read.csv('NO2_RVI.csv')
setwd("~/Dissertation/Dissertation Datasets/Dinnington near Airport")
NO2D<-read.csv('NO2_Dinnington.csv')

NO2<-rbind(NO2G,NO2H)
NO2<-rbind(NO2,NO2R)
NO2<-rbind(NO2,NO2D)

setwd("~/Dissertation/Dissertation Datasets/Haymarket")
O3H<-read.csv('O3_Haymarket.csv')
setwd("~/Dissertation/Dissertation Datasets/Dinnington near Airport")
O3D<-read.csv('O3_Dinnington.csv')

O3<-rbind(O3D,O3H)

setwd("~/Dissertation/Dissertation Datasets/Haymarket")
PM4H<-read.csv('PM4_Haymarket.csv')
setwd("~/Dissertation/Dissertation Datasets/Dinnington near Airport")
PM4D<-read.csv('PM4_Dinnington.csv')

PM4<-rbind(PM4D,PM4H)

setwd("~/Dissertation/Dissertation Datasets/Haymarket")
PM1H<-read.csv('PM1_Haymarket.csv')
setwd("~/Dissertation/Dissertation Datasets/Dinnington near Airport")
PM1D<-read.csv('PM1_Dinnington.csv')
setwd("~/Dissertation/Dissertation Datasets/Nafferton Farm")
PM1N<-read.csv('PM1_Nafferton.csv')

PM1<-rbind(PM1D,PM1H)
PM1<-rbind(PM1,PM1N)


setwd("~/Dissertation/Dissertation Datasets/Haymarket")
PM10H<-read.csv('PM10_Haymarket.csv')
setwd("~/Dissertation/Dissertation Datasets/Dinnington near Airport")
PM10D<-read.csv('PM10_Dinnington.csv')
setwd("~/Dissertation/Dissertation Datasets/Nafferton Farm")
PM10N<-read.csv('PM10_Nafferton.csv')

PM10<-rbind(PM10D,PM10H)
PM10<-rbind(PM10,PM10N)


setwd("~/Dissertation/Dissertation Datasets/Haymarket")
PM2.5H<-read.csv('PM2.5_Haymarket.csv')
setwd("~/Dissertation/Dissertation Datasets/Dinnington near Airport")
PM2.5D<-read.csv('PM2.5_Dinnington.csv')
setwd("~/Dissertation/Dissertation Datasets/Nafferton Farm")
PM2.5N<-read.csv('PM2.5_Nafferton.csv')

PM2.5<-rbind(PM2.5D,PM2.5H)
PM2.5<-rbind(PM2.5,PM2.5N)


setwd("~/Dissertation/Dissertation Datasets/Gateshead Bus Station")
SG<-read.csv('Sound_Gateshead.csv')
setwd("~/Dissertation/Dissertation Datasets/RVI")
SR<-read.csv('Sound_RVI.csv')

Sound<-rbind(SG,SR)

setwd("~/Dissertation/Dissertation Datasets/Haymarket")
VH<-read.csv('Vehicles_Haymarket.csv')



Sound <- rename(Sound,Avg_Sound = Avg_O3)
PM4 <- rename(PM4,Avg_PM4 = Avg_O3)
PM2.5 <- rename(PM2.5,Avg_PM2.5 = Avg_O3)
PM10 <- rename(PM10,Avg_PM10 = Avg_O3)
PM1 <- rename(PM1,Avg_PM1 = Avg_O3)



PM4H <- rename(PM4H,Avg_PM4 = Avg_O3)
PM2.5H <- rename(PM2.5H,Avg_PM2.5 = Avg_O3)
PM10H <- rename(PM10H,Avg_PM10 = Avg_O3)
PM1H <- rename(PM1H,Avg_PM1 = Avg_O3)
VH <- rename(VH,Avg_Vehicles = Avg_CO)



Haymarket <- merge(COH,NOH,by="date.Date_Time.",all=T)
Haymarket <- merge(Haymarket,NO2H,by="date.Date_Time.",all=T)
Haymarket <- merge(Haymarket,O3H,by="date.Date_Time.",all=T)
Haymarket <- merge(Haymarket,PM10H,by="date.Date_Time.",all=T)
Haymarket <- merge(Haymarket,PM1H,by="date.Date_Time.",all=T)
Haymarket <- merge(Haymarket,PM4H,by="date.Date_Time.",all=T)
Haymarket <- merge(Haymarket,PM2.5H,by="date.Date_Time.",all=T)
Haymarket <- merge(Haymarket,VH,by="date.Date_Time.",all=T)

Haymarket <- Haymarket[ -c(3,5,7,9,11,13,15,17,19) ]

SG <- rename(SG,Avg_Sound = Avg_O3)

Gateshead <- merge(COG,NOG, by="date.Date_Time.",all=T)
Gateshead <- merge(Gateshead,NO2G, by="date.Date_Time.",all=T)
Gateshead <- merge(Gateshead,SG, by="date.Date_Time.",all=T)

Gateshead <- Gateshead[ -c(3,5,7,9)]


PM4D <- rename(PM4D,Avg_PM4 = Avg_O3)
PM2.5D <- rename(PM2.5D,Avg_PM2.5 = Avg_O3)
PM10D <- rename(PM10D,Avg_PM10 = Avg_O3)
PM1D <- rename(PM1D,Avg_PM1 = Avg_O3)

Dinnington <- merge(NO2D,NOD,by="date.Date_Time.",all=T)
Dinnington <- merge(Dinnington,O3D,by="date.Date_Time.",all=T)
Dinnington <- merge(Dinnington,PM10D,by="date.Date_Time.",all=T)
Dinnington <- merge(Dinnington,PM2.5D,by="date.Date_Time.",all=T)
Dinnington <- merge(Dinnington,PM4D,by="date.Date_Time.",all=T)
Dinnington <- merge(Dinnington,PM1D,by="date.Date_Time.",all=T)

Dinnington <- Dinnington[ -c(3,5,7,9,11,13,15)]


PM2.5N <- rename(PM2.5N,Avg_PM2.5 = Avg_O3)
PM10N <- rename(PM10N,Avg_PM10 = Avg_O3)
PM1N <- rename(PM1N,Avg_PM1 = Avg_O3)


Nafferton <- merge(PM10N,PM2.5N, by="date.Date_Time.",all=T)
Nafferton <- merge(Nafferton,PM1N,by="date.Date_Time.",all=T)

Nafferton <- Nafferton[ -c(3,5,7)]

SR <- rename(SR,Avg_Sound = Avg_O3)

RVI <- merge(COR,NO2R,by="date.Date_Time.",all=T)
RVI <- merge(RVI,NOR,by="date.Date_Time.",all=T)
RVI <- merge(RVI,SR,by="date.Date_Time.",all=T)

RVI <- RVI[ -c(3,5,7,9)]


library(ggcorrplot)

str(Haymarket)
Haymarket$date.Date_Time. <- as.Date(Haymarket$date.Date_Time.)

str(RVI)
RVI$date.Date_Time. <- as.Date(RVI$date.Date_Time.)

str(Nafferton)
Nafferton$date.Date_Time. <- as.Date(Nafferton$date.Date_Time.)

str(Dinnington)
Dinnington$date.Date_Time. <- as.Date(Dinnington$date.Date_Time.)

str(Gateshead)
Gateshead$date.Date_Time. <- as.Date(Gateshead$date.Date_Time.)

pdf(file = "~/Dissertation/Dissertation Datasets/Haymarket/Haymarket Correlation.pdf",   # The directory you want to save the file in
    width = 10, # The width of the plot in inches
    height = 10) # The height of the plot in inches

Haymarketnd <- Haymarket[-c(1)]

corr <- round(cor(Haymarketnd), 1)
head(corr[, 1:9])

p.mat <- cor_pmat(Haymarketnd)
head(p.mat[, 1:9])

ggcorrplot(corr,method="circle")

dev.off()
pdf(file = "~/Dissertation/Dissertation Datasets/RVI/RVI Correlation.pdf",   # The directory you want to save the file in
    width = 10, # The width of the plot in inches
    height = 10) # The height of the plot in inches
RVInd <- RVI[-c(1)]

corr <- round(cor(RVInd), 1)
head(corr[, 1:4])

p.mat <- cor_pmat(RVInd)
head(p.mat[, 1:4])

ggcorrplot(corr,method="circle")
dev.off()

pdf(file = "~/Dissertation/Dissertation Datasets/Dinnington near Airport/Dinnington Correlation.pdf",   # The directory you want to save the file in
    width = 10, # The width of the plot in inches
    height = 10) # The height of the plot in inches

Dinningtonnd <- Dinnington[-c(1)]

corr <- round(cor(Dinningtonnd), 1)
head(corr[, 1:7])

p.mat <- cor_pmat(Dinningtonnd)
head(p.mat[, 1:7])

ggcorrplot(corr,method="circle")
dev.off()

pdf(file = "~/Dissertation/Dissertation Datasets/Gateshead Bus Station/Gateshead Correlation.pdf",   # The directory you want to save the file in
    width = 10, # The width of the plot in inches
    height = 10) # The height of the plot in inches


Gatesheadnd <- Gateshead[-c(1)]

corr <- round(cor(Gatesheadnd), 1)
head(corr[, 1:4])

p.mat <- cor_pmat(Gatesheadnd)
head(p.mat[, 1:4])

ggcorrplot(corr,method="circle")
dev.off()

pdf(file = "~/Dissertation/Dissertation Datasets/Nafferton Farm/Nafferton Correlation.pdf",   # The directory you want to save the file in
    width = 10, # The width of the plot in inches
    height = 10) # The height of the plot in inches


Naffertonnd <- Nafferton[-c(1)]

corr <- round(cor(Naffertonnd), 1)
head(corr[, 1:3])

p.mat <- cor_pmat(Naffertonnd)
head(p.mat[, 1:3])

ggcorrplot(corr,method="circle")
dev.off()
