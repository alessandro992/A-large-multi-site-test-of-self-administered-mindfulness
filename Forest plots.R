# Required Libraries

library(BayesFactor)
library(dplyr)
library(tidyverse)
library(readxl)
library(readr)
library(janitor)
library(qualtRics)
library(lavaan)
library(semTools)
library(psych)
library(MBESS)
library(tm)
library(tidyr)
library(metafor)

set.seed(10)

#Note that you need to run the mainscript to make this script works 

# saving row means of the groups
f=aggregate(Stai$Stai_average, list(Stai$group), mean)
Mean_control=f[5,2]
Mean_BS=f[4,2]
Mean_LK=f[3,2]
Mean_MB=f[2,2]
Mean_MW=f[1,2]

# saving sd of the groups
f2=aggregate(Stai$Stai_average, list(Stai$group), sd)
Sd_control=f2[5,2]
Sd_BS=f2[4,2]
Sd_LK=f2[3,2]
Sd_MB=f2[2,2]
Sd_MW=f2[1,2]

ff=cbind(f,f2)
ff=ff[,-3]
colnames(ff)=c("gruppo","mean","sd")
#ff

f=aggregate(Stai$Stai_average, list(Stai$site,Stai$group), mean,na.rm=T)
f2=aggregate(Stai$Stai_average, list(Stai$site,Stai$group), sd,na.rm=T)
ffx=cbind(f,f2)
ffx=ffx[,-c(4,5)]
colnames(ffx)=c("Site","Group","Mean","Sd")
#Next line of code is just if you run this analysis with all the sites
#ffx=ffx[-c(4,21,22,37,54,55,70,87,88,89,92,106,123,124,125,135,141,158),]

ffx %>%
  ggplot(aes(x=Mean, y=Site,size=Sd,col=Group)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 6), name="Standard Deviation")+
  labs(title="means and standard deviations of all test conditions")


#Control vs Body scan

St=Stai[Stai$group=="condition5bookchapter"|Stai$group=="condition4bodyscan",]
f=aggregate(St$Stai_average, list(St$site,St$group), mean,na.rm=T)
f2=aggregate(St$Stai_average, list(St$site,St$group), sd,na.rm=T)


ff1=cbind(f,f2)
ff1=ff1[,-c(4,5)]
colnames(ff1)=c("Site","Group","Mean","Sd")
gg=as.data.frame(table(St$site,St$group))
colnames(gg)=c("Site","Group","Freq")
ff11=merge(x=ff1,y=gg,by=c("Site","Group"))
#ff11=ff11[-c(43:46,65),]
#ff11

#Forest plot and test of heterogeneity for body scan vs control
#ff11=ff11[-c(41,42,43,50),]

Bookchapter <- ff11 %>% filter (Group == "condition5bookchapter")
Bodyscan <- ff11 %>% filter (Group == "condition4bodyscan")

Bodyscan = Bodyscan [-c(4,34),]
Bookchapter = Bookchapter [-c(),]
  
Bodyscan <- escalc(measure = "SMD", m1i = Bookchapter$Mean, m2i = Bodyscan$Mean, sd1i = Bookchapter$Sd, sd2i = Bodyscan$Sd, n1i = Bookchapter$Freq, n2i = Bodyscan$Freq, var.names = c("Es_Bodyscan", "Se_Bodyscan"), data = Bodyscan)

m_smd <- rma.uni(yi = Es_Bodyscan,
                 vi = Se_Bodyscan,
                 data = Bodyscan)

summary(m_smd)
forest(m_smd,
       slab = Bodyscan$Site)


ff11 %>%
  ggplot(aes(x=Mean, y=Site,size=Sd,col=Group)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 9), name="Standard Deviation")+
  labs(title="Control vs body scan")


#Control vs Loving-kindness

St=Stai[Stai$group=="condition5bookchapter"|Stai$group=="condition3lovingkindness",]
f=aggregate(St$Stai_average, list(St$site,St$group), mean)
f2=aggregate(St$Stai_average, list(St$site,St$group), sd)

ff2=cbind(f,f2)
ff2=ff2[,-c(4,5)]
colnames(ff2)=c("Site","Group","Mean","Sd")
gg=as.data.frame(table(St$site,St$group))
colnames(gg)=c("Site","Group","Freq")
ff22=merge(x=ff2,y=gg,by=c("Site","Group"))
#ff22=ff22[-c(41,42,43,50),]
ff22

ff22 %>%
  ggplot(aes(x=Mean, y=Site,size=Sd,col=Group)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 9), name="Standard Deviation")+
  labs(title="Control vs loving-kindness")

#Forest plot and test of heterogeneity for Loving-kindness vs control 

Bookchapter <- ff11 %>% filter (Group == "condition5bookchapter")
Lovingkindness <- ff22 %>% filter (Group == "condition3lovingkindness")

Lovingkindness = Lovingkindness [-c(26),]


Lovingkindness <- escalc(measure = "SMD", m1i = Bookchapter$Mean, m2i = Lovingkindness$Mean, sd1i = Bookchapter$Sd, sd2i = Lovingkindness$Sd, n1i = Bookchapter$Freq, n2i = Lovingkindness$Freq, var.names = c("Es_lovingkindness", "Se_lovingkindness"), data = Lovingkindness)

m_smd <- rma.uni(yi = Es_lovingkindness,
                 vi = Se_lovingkindness,
                 data = Lovingkindness)

summary(m_smd)
forest(m_smd,
       slab = Lovingkindness$Site)

#Control vs mindful breathing
St=Stai[Stai$group=="condition5bookchapter"|Stai$group=="condition2mindfulbreathing",]
f=aggregate(St$Stai_average, list(St$site,St$group), mean)
f2=aggregate(St$Stai_average, list(St$site,St$group), sd)

ff3=cbind(f,f2)
ff3=ff3[,-c(4,5)]
colnames(ff3)=c("Site","Group","Mean","Sd")
gg=as.data.frame(table(St$site,St$group))
colnames(gg)=c("Site","Group","Freq")
ff33=merge(x=ff3,y=gg,by=c("Site","Group"))
#ff33=ff33[-c(7,8,42,43),]
ff33

ff33 %>%
  ggplot(aes(x=Mean, y=Site,size=Sd,col=Group)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 9), name="Standard Deviation")+
  labs(title="Control vs mindful breathing")

#Forest plot and test of heterogeneity for mindful breathing vs control 

Bookchapter <- ff11 %>% filter (Group == "condition5bookchapter")
Mindfulbreathing <- ff33 %>% filter (Group == "condition2mindfulbreathing")

Bookchapter = Bookchapter [-c(21),]



Mindfulbreathing <- escalc(measure = "SMD", m1i = Bookchapter$Mean, m2i = Mindfulbreathing$Mean, sd1i = Bookchapter$Sd, sd2i = Mindfulbreathing$Sd, n1i = Bookchapter$Freq, n2i = Mindfulbreathing$Freq, var.names = c("Es_mindfulbreathing", "Se_mindfulbreathing"), data = Mindfulbreathing)

m_smd <- rma.uni(yi = Es_mindfulbreathing ,
                 vi = Se_mindfulbreathing ,
                 data = Mindfulbreathing)

summary(m_smd)
forest(m_smd,
       slab = Mindfulbreathing$Site)


# Controllo vs mindful walking
St=Stai[Stai$group=="condition5bookchapter"|Stai$group=="condition1mindfulwalking",]
f=aggregate(St$Stai_average, list(St$site,St$group), mean)
f2=aggregate(St$Stai_average, list(St$site,St$group), sd)

ff4=cbind(f,f2)
ff4=ff4[,-c(4,5)]
colnames(ff4)=c("Site","Group","Mean","Sd")
gg=as.data.frame(table(St$site,St$group))
colnames(gg)=c("Site","Group","Freq")
ff44=merge(x=ff4,y=gg,by=c("Site","Group"))
#ff44=ff44[-c(7,8,41,42),]

ff44

ff44 %>%
  ggplot(aes(x=Mean, y=Site,size=Sd,col=Group)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 9), name="Standard Deviation")+
  labs(title="Control vs mindful walking")

#Forest plot and test of heterogeneity for mindful walking vs control 

Bookchapter <- ff11 %>% filter (Group == "condition5bookchapter")
Mindfulwalking <- ff44 %>% filter (Group == "condition1mindfulwalking")

Bookchapter = Bookchapter [-c(21),]

Mindfulwalking  <- escalc(measure = "SMD", m1i = Bookchapter$Mean, m2i = Mindfulwalking$Mean, sd1i = Bookchapter$Sd, sd2i = Mindfulwalking$Sd, n1i = Bookchapter$Freq, n2i = Mindfulwalking$Freq, var.names = c("Es_mindfulwalking", "Se_mindfulwalking"), data = Mindfulwalking)

m_smd <- rma.uni(yi = Es_mindfulwalking ,
                 vi = Se_mindfulwalking ,
                 data = Mindfulwalking)

summary(m_smd)
forest(m_smd,
       slab = Mindfulwalking$Site)

