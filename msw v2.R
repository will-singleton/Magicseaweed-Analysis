####explaination of data####
#day - used instead of date for ease of analysis 
#f.time - how many days in the future the forecast is predicting (such that 0 is the final day of forecasting)
#date - not used in analysis 
#surf - primary swell height in ft
#period - primary swell period in seconds
#w.speed - magicseaweed wind speed in mph
#w.direc - magicseaweed wind direction catagorised into offshore (off), cross offshore (c.off), cross shore (cross), cross onshore (c.on) and onshore (on)
#w.rating - magicseaweed wind rating combing wind direction and speed (see Supplementary Material)
#prob - magicseaweed model probability but not used
#g.speed - windguru wind speed in mph
#g.direc - windguru wind direction catagorised into offshore (off), cross offshore (c.off), cross shore (cross), cross onshore (c.on) and onshore (on)
#g.rating - windguru wind rating combing wind direction and speed (see Supplementary Material)
####setting up####
msw<-read.csv("msw5.csv",header=TRUE)
msw$day<-as.factor(msw$day)
levels(msw$final)
attach(msw)
library(ggplot2)
library(car)
library(tidyverse)
library(dplyr)
library(broom)
library(rstatix)
install.packages("rstatix")
library(MASS)
dev.off()
dev.new()
par(mfrow=c(1,2))
####graphs####
par(mfrow=c(1,1))

##windspeed
#msw
ggplot(msw, aes(x=f.time, y=w.speed))+
  geom_point(shape=1)+
  geom_smooth(method=lm,  
              se=T)+
  scale_x_reverse()+
  xlab("Length of forecast prediction (days)")+
  ylab("Wind speed forecasted (mph)")+
  theme_bw()+
  ggtitle("a) Magicseaweed")+
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))
#averages
days<-c(6:0)
m.speed.avg<-c(mean(w.speed[f.time=="6"&!is.na(w.speed)]),
               mean(w.speed[f.time=="5"&!is.na(w.speed)]),
               mean(w.speed[f.time=="4"&!is.na(w.speed)]),
               mean(w.speed[f.time=="3"&!is.na(w.speed)]),
               mean(w.speed[f.time=="2"&!is.na(w.speed)]),
               mean(w.speed[f.time=="1"&!is.na(w.speed)]),
               mean(w.speed[f.time=="0"&!is.na(w.speed)]))
msw.w.speed.avg<-data.frame(days,m.speed.avg)

ggplot(msw.w.speed.avg, aes(x=days, y=m.speed.avg))+
  geom_point(colour="white")+
  scale_x_reverse()+
  xlab("Length of forecast prediction (days)")+
  ylab("Mean forecasted wind speed (mph)")+
  theme_bw()+
  ggtitle("a) Magicseaweed")+
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  geom_abline(slope=0.7188, intercept=15.7)+
  ylim(10,16)+
  geom_abline(slope=0, intercept=13.54)


#wg  
ggplot(msw, aes(x=f.time, y=g.speed))+
  geom_point(shape=1)+
  geom_smooth(method=lm,  
              se=T)+
  scale_x_reverse()+
  xlab("Length of forecast prediction (days)")+
  ylab("Wind speed forecasted (mph)")+
  theme_bw()+
  ggtitle("b) WindGuru")+
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))

#averages
g.speed.avg<-c(mean(g.speed[f.time=="6"&!is.na(g.speed)]),
               mean(g.speed[f.time=="5"&!is.na(g.speed)]),
               mean(g.speed[f.time=="4"&!is.na(g.speed)]),
               mean(g.speed[f.time=="3"&!is.na(g.speed)]),
               mean(g.speed[f.time=="2"&!is.na(g.speed)]),
               mean(g.speed[f.time=="1"&!is.na(g.speed)]),
               mean(g.speed[f.time=="0"&!is.na(g.speed)]))
wg.w.speed.avg<-data.frame(days,g.speed.avg)

ggplot(wg.w.speed.avg, aes(x=days, y=g.speed.avg))+
  geom_point(colour="white")+
  scale_x_reverse()+
  xlab("Length of forecast prediction (days)")+
  ylab("Wind speed forecasted (mph)")+
  theme_bw()+
  ggtitle("b) Windguru")+
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  geom_abline(slope=0.2782, intercept=14.73)+
  ylim(10,16)+
geom_abline(slope=0, intercept=13.89)


##ratings
#msw
ggplot(msw, aes(x=f.time, y=w.rating))+
  geom_point(shape=1)+
  geom_smooth(method=lm,  
              se=T)+
  scale_x_reverse()+
  xlab("Length of forecast prediction (days)")+
  ylab("Wind rating forecasted")+
  theme_bw()+
  ggtitle("a) Magicseaweed")+
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))

#averages
m.rating.avg<-c(mean(w.rating[f.time=="6"&!is.na(w.rating)]),
               mean(w.rating[f.time=="5"&!is.na(w.rating)]),
               mean(w.rating[f.time=="4"&!is.na(w.rating)]),
               mean(w.rating[f.time=="3"&!is.na(w.rating)]),
               mean(w.rating[f.time=="2"&!is.na(w.rating)]),
               mean(w.rating[f.time=="1"&!is.na(w.rating)]),
               mean(w.rating[f.time=="0"&!is.na(w.rating)]))
msw.w.rating.avg<-data.frame(days,m.speed.avg)

ggplot(msw.w.rating.avg, aes(x=days, y=m.rating.avg))+
  geom_point()+
  scale_x_reverse()+
  xlab("Length of forecast prediction (days)")+
  ylab("Mean forecasted wind rating")+
  theme_bw()+
  ggtitle("a) Magicseaweed")+
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  geom_smooth(method=lm,se=F, colour="black")+
  ylim(1.5,2.5)

#wg  
ggplot(msw, aes(x=f.time, y=g.rating))+
  geom_point(shape=1)+
  geom_smooth(method=lm,se=T)+
  scale_x_reverse()+
  xlab("Length of forecast prediction (days)")+
  ylab("Wind rating forecasted")+
  theme_bw()+
  ggtitle("b) Windguru")+
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))
#averages
g.rating.avg<-c(mean(g.rating[f.time=="6"&!is.na(g.rating)]),
                mean(g.rating[f.time=="5"&!is.na(g.rating)]),
                mean(g.rating[f.time=="4"&!is.na(g.rating)]),
                mean(g.rating[f.time=="3"&!is.na(g.rating)]),
                mean(g.rating[f.time=="2"&!is.na(g.rating)]),
                mean(g.rating[f.time=="1"&!is.na(g.rating)]),
                mean(g.rating[f.time=="0"&!is.na(g.rating)]))
wg.w.rating.avg<-data.frame(days,g.speed.avg)

ggplot(wg.w.rating.avg, aes(x=days, y=g.rating.avg))+
  geom_point()+
  scale_x_reverse()+
  xlab("Length of forecast prediction (days)")+
  ylab("Mean forecasted wind rating")+
  theme_bw()+
  ggtitle("b) Windguru")+
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black'))+
  geom_smooth(method=lm,se=F, colour="black")+
  ylim(1.5,2.5)
#swell
ggplot(msw, aes(x=f.time, y=surf))+
  geom_point(shape=1)+
  geom_smooth(method=lm,  
              se=T)+
  scale_x_reverse()

#period 
ggplot(msw, aes(x=f.time, y=period))+
  geom_point(shape=1)+
  geom_smooth(method=lm,  
              se=T)+
  scale_x_reverse()

#power
ggplot(msw, aes(x=f.time, y=surf*period))+
  geom_point(shape=1)+
  geom_smooth(method=lm,  
              se=T)+
  scale_x_reverse()

#analysis
##windspeed####

###msw###

#assumption testing
hist(w.speed) #norm
bartlett.test(w.speed~f.time, data=msw) #homo
plot(s.msw.mod.1)

#lm
s.msw.mod.1<-lm(w.speed~f.time, data=msw)
summary(s.msw.mod.1) #simple model dy/dx -0.72 p<0.001 int=15.7 r^2 = 0.065

#means 
mean(w.speed[f.time=="6"&!is.na(w.speed)])-
mean(w.speed[f.time=="0"&!is.na(w.speed)])

###wg###

#assumption testing
hist(g.speed) #norm
bartlett.test(g.speed~f.time, data=msw) #homo
plot(s.wg.mod.1)

#lm 

s.wg.mod.1<-lm(g.speed~f.time, data=msw) 
summary(s.wg.mod.1) #f.time not signif 


####rating####

###msw###

#assumption testing
hist(w.rating, breaks=10) #not norm 
bartlett.test(w.rating~f.time, data=msw) #homo

#sp cor
cor.test(w.rating, f.time, method="spearman", use="complete.obs") #signif p=0.0036 rho=0.20

#means

mean(w.rating[f.time=="6"&!is.na(w.rating)])
  mean(w.rating[f.time=="0"&!is.na(w.rating)])

###wg###

#assumption testing
hist(g.rating, breaks=10) #not norm 
bartlett.test(g.rating~f.time, data=msw) #homo

#sp cor
cor.test(g.rating, f.time, method="spearman") #not signif

####swell#####

#assumption testing
hist(surf, breaks=5) #norm
bartlett.test(surf~f.time, data=msw) #homo
plot(surf.mod.1)

#Lm
plot(surf.mod.1)
hist(sqrt(surf))
surf.mod.1<-lm(sqrt(surf)~f.time, data=msw) 
summary(surf.mod.1) #f.time not signif 

mean(surf[f.time=="6"&!is.na(surf)])-
mean(surf[f.time=="0"&!is.na(surf)])

####period#####

#assumption testing
hist(period, breaks=5) #norm
bartlett.test(period~f.time, data=msw) #homo
plot(period.mod.1)

#Lm

period.mod.1<-lm(period~f.time, data=msw) 
summary(period.mod.1) #f.time not signif 

mean(period[f.time=="6"&!is.na(surf)])-
  mean(period[f.time=="0"&!is.na(surf)])

####off/onshore#### 
#not enough data - not reporting 
#msw - no signif
direc.msw<-table(f.time, w.direc)
direc.msw
chisq.test(direc.msw)
#combining c.on and c.off
msw2<-msw
msw2$w.direc[msw2$w.direc=="c.on"&!is.na(msw2$w.direc)]<-"on"
msw2$w.direc[msw2$w.direc=="c.off"&!is.na(msw2$w.direc)]<-"off"
msw2$w.direc<-factor(msw2$w.direc)
levels(msw2$w.direc)
direc.msw2<-table(msw2$f.time, msw2$w.direc)
direc.msw2
chisq.test(direc.msw2)

#wg - no signif
direc.wg<-table(f.time, g.direc)
direc.wg
chisq.test(direc.wg)
#combining c.on and c.off
msw2$g.direc[msw2$g.direc=="c.on"&!is.na(msw2$g.direc)]<-"on"
msw2$g.direc[msw2$g.direc=="c.off"&!is.na(msw2$g.direc)]<-"off"
msw2$g.direc<-factor(msw2$g.direc)
levels(msw2$g.direc)
direc.wg2<-table(msw2$f.time, msw2$g.direc)
direc.wg2
chisq.test(direc.wg2)
