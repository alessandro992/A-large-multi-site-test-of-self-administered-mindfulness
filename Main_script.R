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

set.seed(10)

Df <- read_survey("test3.csv") %>%
  clean_names()

names(Df) <- tolower(names(Df))
Df <- Df %>% rename(responseid = response_id, site = user_language, group = fl_13_do) 

#Creating different datasets from the csv file: ipip, stai,sam_arousal,
#sam_control,sam_emotional,age,gender. For each scales we computed the sum, 
#the means, the median and SDs.

ipip <- Df %>% 
  select(responseid, group, starts_with("ipip"),site) 

ipip <- ipip %>% 
  transmute(responseid = responseid,
            group = group,
            site = site,
            ipip1 = ipip1,
            ipip2 = ipip2,
            ipip3 = ipip3,
            ipip4 = ipip4,
            ipip5 = ipip5,
            ipip6 = ipip6,
            ipip7 = ipip7,
            ipip8 = ipip8,
            ipip9 = ipip9,
            ipip10 = ipip10,
            ipip11_r = 6 - ipip11,
            ipip12_r = 6 - ipip12,
            ipip13_r = 6 - ipip13,
            ipip14_r = 6 - ipip14,
            ipip15_r = 6 - ipip15,
            ipip16_r = 6 - ipip16,
            ipip17_r = 6 - ipip17,
            ipip18_r = 6 - ipip18,
            ipip19_r = 6 - ipip19,
            ipip20_r = 6 - ipip20,) %>%
  mutate(ipip_tot = rowSums(select(., c(4:23))))

ipip$ipip_average <- rowMeans(cbind(ipip$ipip1,
                                    ipip$ipip2,
                                    ipip$ipip3,
                                    ipip$ipip4,
                                    ipip$ipip5,
                                    ipip$ipip6,
                                    ipip$ipip7,
                                    ipip$ipip8,
                                    ipip$ipip9,
                                    ipip$ipip10,
                                    ipip$ipip11_r,
                                    ipip$ipip12_r,
                                    ipip$ipip13_r,
                                    ipip$ipip14_r,
                                    ipip$ipip15_r,
                                    ipip$ipip16_r,
                                    ipip$ipip17_r,
                                    ipip$ipip18_r,
                                    ipip$ipip19_r,
                                    ipip$ipip20_r), na.rm = TRUE)

ipip$ipip_median <- median(cbind(ipip$ipip1,
                                 ipip$ipip2,
                                 ipip$ipip3,
                                 ipip$ipip4,
                                 ipip$ipip5,
                                 ipip$ipip6,
                                 ipip$ipip7,
                                 ipip$ipip8,
                                 ipip$ipip9,
                                 ipip$ipip10,
                                 ipip$ipip11_r,
                                 ipip$ipip12_r,  
                                 ipip$ipip13_r,
                                 ipip$ipip14_r,
                                 ipip$ipip15_r,
                                 ipip$ipip16_r,
                                 ipip$ipip17_r,
                                 ipip$ipip18_r,
                                 ipip$ipip19_r,
                                 ipip$ipip20_r), na.rm = TRUE)

ipip$ipip_sd <- sd(cbind(ipip$ipip1,
                         ipip$ipip2,
                         ipip$ipip3,
                         ipip$ipip4,
                         ipip$ipip5,
                         ipip$ipip6,
                         ipip$ipip7,
                         ipip$ipip8,
                         ipip$ipip9,
                         ipip$ipip10,
                         ipip$ipip11_r,
                         ipip$ipip12_r,
                         ipip$ipip13_r,
                         ipip$ipip14_r,
                         ipip$ipip15_r,
                         ipip$ipip16_r,
                         ipip$ipip17_r,
                         ipip$ipip18_r,
                         ipip$ipip19_r,
                         ipip$ipip20_r), na.rm = TRUE)

Stai <- Df %>% 
  select(responseid, group, starts_with("Stai"),site) 

Stai <- Stai %>% 
  transmute(responseid = responseid,
            group = group,
            site = site,
            Stai1_r = 5 - stai1,
            Stai2_r = 5 - stai2,
            Stai3 = stai3,
            Stai4 =  stai4,
            Stai5_r = 5 - stai5,
            Stai6 = stai6,
            Stai7 = stai7,
            Stai8_r = 5 - stai8,
            Stai9 = stai9,
            Stai10_r = 5 - stai10,
            Stai11_r = 5  - stai11,
            Stai12 = stai12,
            Stai13 = stai13,
            Stai14 = stai14,
            Stai15_r = 5 - stai15,
            Stai16_r = 5 - stai16,
            Stai17 = stai17,
            Stai18 = stai18,
            Stai19_r = 5 - stai19,
            Stai20_r = 5 - stai20,) %>%
  mutate(Stai_tot = rowSums(select(., c(4:23))))

Stai$Stai_average <- rowMeans(cbind(Stai$Stai1_r,
                                    Stai$Stai2_r,
                                    Stai$Stai3,
                                    Stai$Stai4,
                                    Stai$Stai5_r,        
                                    Stai$Stai6,
                                    Stai$Stai7,
                                    Stai$Stai8_r,
                                    Stai$Stai9,
                                    Stai$Stai10_r,
                                    Stai$Stai11_r,
                                    Stai$Stai12,
                                    Stai$Stai13,
                                    Stai$Stai14,
                                    Stai$Stai15_r,
                                    Stai$Stai16_r,
                                    Stai$Stai17,
                                    Stai$Stai18,
                                    Stai$Stai19_r,
                                    Stai$Stai20_r), na.rm = TRUE)

Stai$Stai_median <- median(cbind(Stai$Stai1_r,
                                 Stai$Stai2_r,
                                 Stai$Stai3,
                                 Stai$Stai4,
                                 Stai$Stai5_r,                                    
                                 Stai$Stai6,
                                 Stai$Stai7,
                                 Stai$Stai8_r,
                                 Stai$Stai9,
                                 Stai$Stai10_r,
                                 Stai$Stai11_r,
                                 Stai$Stai12,
                                 Stai$Stai13,
                                 Stai$Stai14,
                                 Stai$Stai15_r,
                                 Stai$Stai16_r,
                                 Stai$Stai17,
                                 Stai$Stai18,
                                 Stai$Stai19_r,
                                 Stai$Stai20_r), na.rm = TRUE)

Stai$Stai_sd <- sd(cbind(Stai$Stai1_r,
                         Stai$Stai2_r,
                         Stai$Stai3,
                         Stai$Stai4,
                         Stai$Stai5_r,
                         Stai$Stai6,
                         Stai$Stai7,
                         Stai$Stai8_r,
                         Stai$Stai9,
                         Stai$Stai10_r,
                         Stai$Stai11_r,
                         Stai$Stai12,
                         Stai$Stai13,
                         Stai$Stai14,
                         Stai$Stai15_r,
                         Stai$Stai16_r,
                         Stai$Stai17,
                         Stai$Stai18,
                         Stai$Stai19_r,
                         Stai$Stai20_r), na.rm = TRUE)

sam_emotional <- Df %>% 
  select(responseid, group, starts_with("Sam1"),site) 

sam_arousal <- Df %>% 
  select(responseid, group, starts_with("Sam2"),site) 

sam_control <- Df %>% 
  select(responseid, group, starts_with("Sam3"),site) 

gender <- Df %>%
  select(responseid, gender,site)

eng_level <- Df %>%
  select(responseid, eng_level)

age <- Df %>%
  select(responseid, age,site) %>%
  mutate(mean = mean(age),
         median = median(age),
         sd = sd(age))


#We have to check that on a descriptive level the level of stress in
#the control group is higher than in the other experimental groups.

Stai %>%
  group_by(group) %>%
  summarise_at(vars(Stai_average), list(name = mean))

#We set a minimum of 50 participants for groups to avoid risk of misleading evidences and we 
#For now we set 600 as the maximum number of participants for each group (it will be fixed when we will have the total number of sites)

n=50
N=600

c1=filter(Stai, group == "Condition5:Bookchapter" ) 
if(nrow(c1) < n){
  control_s=NA
}  else if (nrow(c1) >= n) {
  control_s=c1$Stai_average
} 

c2=filter(Stai, group == "Condition4:BodyScan" )
if(nrow(c2) < n){
  bs=NA
}  else if (nrow(c2) >= n) {
  bs=c2$Stai_average
} 

c3=filter(Stai, group == "Condition3:Loving-kindness" )
if(nrow(c3) < n){
  lk=NA
}  else if (nrow(c3) >= n) {
  lk=c3$Stai_average
} 

c4=filter(Stai, group == "Condition1:Mindfulwalking" ) 
if(nrow(c4) < n){
  mm=c4$Stai_average
}  else if (nrow(c4) > N) {
  mm=c4$Stai_average
} else
  mm=NA

c5=filter(Stai, group == "Condition2:Mindfulbreathing" )
if(nrow(c5) > n & nrow(c5)<N){
  mb=c5$Stai_average
}  else if (nrow(c5) > N) {
  mb=c5$Stai_average
} else
  mb=NA

#Where sample size is sufficiently big we carry out the 4 BF test. If a group has not exceeded size 50 
#we get an error message. 

s1=ttestBF(x = control_s, y=mb,paired=F)

s2=ttestBF(x = control_s, y=lk,paired=F)

s3=ttestBF(x = control_s, y=mm,paired=F)

s4=ttestBF(x = control_s, y=bs,paired=F)


#We fixed the minimum sample size per group n=50 and the maximum N=600
#If the observations in the group are less than 50 the stress vector is not generated, 
#if they are between 50 and 600 then the available observations are taken


#We identify the experimental groups that have a BF of 10 (in favor of H1) or a BF 
#of 0.1 (in Favor of H0)

groups=c("Condition2:Mindfulbreathing",
         "Condition3:Loving-kindness",
         "Condition1:Mindfulwalking",
         "Condition4:BodyScan",
         "Condition5:Bookchapter")
value=c(extractBF(s1)$bf[1],extractBF(s2)$bf[1],extractBF(s3)$bf[1],extractBF(s4)$bf[1],11)
Size=c(length(mb),length(lk),length(mm),length(bs),length(control_s))
data=data.frame(groups,value,Size)
name=data[data$value>10 | data$value < 0.10 | Size>=N  ,"groups"]

#If a group exceeds BF=10 in stress, we compute the BF test also for sam_arousal, sam_control,
#sam_emotional as well, otherwise we obtain NA. Being this an exploratory analysis we do not commit
#the data collection to the threshold of 10 for the self-assessment manikin scale

if(("Condition1:Mindful-Breathing " %in% name)==T){
  t1=ttestBF(x = filter(sam_arousal, group == "Condition5:Bookchapter" )$sam2_1,
             y=filter(sam_arousal, group == "Condition2:Mindfulbreathing" )$sam2_1,
             paired=F)
  t1b=ttestBF(x = filter(sam_control, group == "Condition5:Bookchapter" )$sam3_1,
              y=filter(sam_control, group == "Condition2:Mindfulbreathing" )$sam3_1,
              paired=F)
  t1c=ttestBF(x = filter(sam_emotional, group == "Condition5:Bookchapter" )$sam2_1,
              y=filter(sam_emotional, group == "Condition2:Mindfulbreathing" )$sam1_1,
              paired=F)
}  else
  t1=t1b=t1c=NA

if(("Condition3:Loving-kindness " %in% name)==T){
  t2=ttestBF(x = filter(sam_arousal, group == "Condition5:Bookchapter" )$sam2_1,
             y=filter(sam_arousal, group == "Condition3:Loving-kindness" )$sam2_1,
             paired=F)
  t2b=ttestBF(x = filter(sam_control, group == "Condition5:Bookchapter" )$sam3_1,
              y=filter(sam_control, group == "Condition3:Loving-kindness" )$sam3_1,
              paired=F)
  t2c=ttestBF(x = filter(sam_emotional, group == "Condition5:Bookchapter" )$sam2_1,
              y=filter(sam_emotional, group == "Condition3:Loving-kindness" )$sam1_1,
              paired=F)
}  else
  t2=t2b=t2c=NA

if(("Condition2:Mindful-Movements " %in% name)==T){
  t3=ttestBF(x = filter(sam_arousal, group == "Condition5:Bookchapter")$sam2_1,
             y=filter(sam_arousal, group == "Condition1:Mindfulwalking")$sam2_1,
             paired=F)
  t3b=ttestBF(x = filter(sam_control, group == "Condition5:Bookchapter")$sam3_1,
              y=filter(sam_control, group == "Condition1:Mindfulwalking")$sam3_1,
              paired=F)
  t3c=ttestBF(x = filter(sam_emotional, group == "Condition5:Bookchapter")$sam2_1,
              y=filter(sam_emotional, group == "Condition1:Mindfulwalking")$sam1_1,
              paired=F)
}  else
  t3=t3b=t3c=NA

if(("Condition4:BodyScan" %in% name)==T){
  t4=ttestBF(x = filter(sam_arousal, group == "Condition5:Bookchapter")$sam2_1,
             y=filter(sam_arousal, group == "Condition4:BodyScan")$sam2_1,
             paired=F)
  t4b=ttestBF(x = filter(sam_control, group == "Condition5:Bookchapter")$sam3_1,
              y=filter(sam_control, group == "Condition4:BodyScan")$sam3_1,
              paired=F)
  t4c=ttestBF(x = filter(sam_emotional, group == "Condition5:Bookchapter")$sam2_1,
              y=filter(sam_emotional, group == "Condition4:BodyScan")$sam1_1,
              paired=F)
}  else
  t4=t4b=t4c=NA


Stai2=Stai %>%
  filter(group %in% name)

ipip2=ipip %>%
  filter(group %in% name)

sam_arousal2=sam_arousal %>%
  filter(group %in% name)

sam_control2=sam_control %>%
  filter(group %in% name)

sam_emotional2=sam_emotional %>%
  filter(group %in% name)

gender2=gender %>%
  filter(group %in% name)

age2=age %>%
  filter(group %in% name)


results<-merge(x=Stai2,y=ipip2,by="responseid",all.x=TRUE)

#The first question we can ask ourselves is: is the interaction
#between neuroticism and the experimental group reasonable?

model=lm(Stai_average ~ group.x + ipip_average + group.x:ipip_average, data=results)
summary(model)

#At this point I build a Bayesian model with and without interaction and check whether the one with 
#the interaction is preferable and observe the parameter estimates. I continue the data collection untill a BF of 10
#is reached for the model with the interaction or until the total maximum sample size is reach. 
#the total maximum sample size is 130 (maximun n requested to each site) * n (number of sites that participate in the study). 
#If the threshold of 10 (for the model with the interaction) is not reached or if the total maximum sample size is not reached 
#data collection will be stopped 3 months after the beginning of the data collection.
#the model with the interaction) is not reached or if the total maximum sample size is not reached data 
#collection will be stopped 3 months after the beginning of the data collection. 

full <- lmBF(Stai_average ~ group.x +
               ipip_average +
               group.x:ipip_average +
               site,
             whichRandom = "site", data=results)

noInteraction <- lmBF(Stai_average ~ group.x +
                        ipip_average +
                        site,
                      whichRandom = "site",data=results)

allBFs <- c(full, noInteraction)
allBFs

full/noInteraction

chainsFull <- posterior(full, iterations = 10000)
summary(chainsFull[,1:7])






