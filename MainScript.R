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

set.seed(10)

#load the final dataset

Df <- read_csv2("finaldata.csv") 

#count participants that did not fit inclusion criteria
#dfb <- Df %>% count(q110)
#dfa <- Df %>% count(mental)
#dfc <- Df %>% count(meditation_exp)
#dfd <- Df %>% count(eng_level)
#dfe <- Df %>% count(gender)
#dff <- Df %>% count(student)

# remove rows with missing RESPONSEID 
Df <- Df[!(is.na(Df$responseid)==T),]

# remove rows with missing GROUP
Df <- Df[!(is.na(Df$group)==T),]

# remove rows with AGE=999
Df <- Df[Df$age != "999",]
Df <- Df[Df$age != 999,]

# check observations for site overall (including careless respondents):
d2=as.data.frame(table(Df$site))
colnames(d2)=c("Site","Participants")

# remove duplicates in RESPONSEID
Df[!duplicated(Df$responseid),]

# remove individuals with 50% items responses missing
Df2=Df %>% 
  select(starts_with("ipip"),starts_with("stai"))
Df2$check=rowSums(is.na(Df2))/40
check=Df2$check
Df=cbind(Df,check)
Df=Df[Df$check < 0.5,]

# remove individuals with 10 or more consecutive equal responses in ipip or stai items
Dd=Df %>% 
  select(starts_with("ipip"))
De=Df %>% 
  select(starts_with("stai"))
CK=c()
CH=c()
for (i in 1:nrow(Df)){
  a=max(table(Dd[i,])/length(Dd[i,]))
  if (a>0.5) {CH[i]=1} else {CH[i]=0}
}
for (i in 1:nrow(De)){
  b=max(table(De[i,])/length(De[i,]))
  if (b>0.5) {CK[i]=1} else {CK[i]=0}
}
Df=cbind(Df,CH,CK)
Df=Df[Df$CH==0 & Df$CK==0,]

# Cleaning of group and story 
Df$group=as.character(Df$group)
Df$group=tolower(Df$group)
Df$group=trimws(Df$group, whitespace = "[\\h\\v\\t ]")
Df$group=trimws(Df$group)
Df$group=gsub(" ","",Df$group)
Df$group=removePunctuation(Df$group)

Df$story=as.character(Df$story)
Df$story=tolower(Df$story)
Df$story=trimws(Df$story, whitespace = "[\\h\\v\\t ]")
Df$story=trimws(Df$story)
Df$story=gsub(" ","",Df$story)
Df$story=removePunctuation(Df$story)

# where story is NA becomes "absent"
Df$story[is.na(Df$story)]="absent"

# where site is NA becomes "absent"
Df$site[is.na(Df$site)]="absent"

Df$site=as.factor(Df$site)
Df$story=as.factor(Df$story)

#no need for these datasets anymore
rm(d2)
rm(Dd)
rm(De)
rm(Df2)

# Creating different datasets from the csv file: ipip, stai,sam_arousal,
# sam_control,sam_emotional,age,gender. For each scales we computed the sum, 
# the means, the median and SDs.

ipip <- Df %>% 
  select(responseid, group, starts_with("ipip"),site,story) 

ipip <- ipip %>% 
  transmute(responseid = responseid,
            group = group,
            site = site,
            story=story,
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
  mutate(ipip_tot = rowSums(select(., c(5:23))))

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

ipip$ipip_median <- apply(cbind(ipip$ipip1,
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
                                ipip$ipip20_r),1,median, na.rm = TRUE)

ipip$ipip_sd <- apply(cbind(ipip$ipip1,
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
                            ipip$ipip20_r),1,sd, na.rm = TRUE)


Stai <- Df %>% 
  select(responseid, group, starts_with("Stai"),site,story) 

Stai <- Stai %>% 
  transmute(responseid = responseid,
            group = group,
            site = site,
            story=story,
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
  mutate(Stai_tot = rowSums(select(., c(5:23))))

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

Stai$Stai_median <- apply(cbind(Stai$Stai1_r,
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
                                Stai$Stai20_r),1,median, na.rm = TRUE)

Stai$Stai_sd <- apply(cbind(Stai$Stai1_r,
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
                            Stai$Stai20_r),1,sd, na.rm = TRUE)

sam_emotional <- Df %>% 
  select(responseid, group,site,story, starts_with("Sam1")) 
sam_emotional=na.omit(sam_emotional)

sam_arousal <- Df %>% 
  select(responseid, group,site,story, starts_with("Sam2")) 
sam_arousal=na.omit(sam_arousal)

sam_control <- Df %>% 
  select(responseid, group,site, story, starts_with("Sam3")) 
sam_control=na.omit(sam_control)

gender <- Df %>%
  select(responseid, gender,site)

Df$age=as.numeric(Df$age)
age <- Df %>%
  select(responseid, age,site) %>%
  mutate(mean = mean(age),
         median = median(age),
         sd = sd(age))

# group means for STAI_AVERAGE 
Stai %>%
  group_by(group) %>%
  summarise_at(vars(Stai_average), list(name = mean))


# selecting scores STAI for each group if n > 100 otherwise it will be a Nas
n=100

c5=filter(Stai, group == "condition5bookchapter" ) 
if(nrow(c5) < n){
  control_s=NA
}  else if (nrow(c5) >= n) {
  control_s=c5$Stai_average
  control_story=c5$story
  control_site=c5$site
} 

c4=filter(Stai, group == "condition4bodyscan" )
if(nrow(c4) < n){
  bs=NA
}  else if (nrow(c4) >= n) {
  bs=c4$Stai_average
  bs_story=c4$story
  bs_site=c4$site
} 

c3=filter(Stai, group == "condition3lovingkindness" )
if(nrow(c3) < n){
  lk=NA
}  else if (nrow(c3) >= n) {
  lk=c3$Stai_average
  lk_story=c3$story
  lk_site=c3$site
} 

c1=filter(Stai, group == "condition1mindfulwalking" ) 
if(nrow(c1) < n){
  mw=NA
}  else if (nrow(c1) >= n) {
  mw=c1$Stai_average
  mw_story=c1$story
  mw_site=c1$site
} 

c2=filter(Stai, group == "condition2mindfulbreathing" )
if(nrow(c2)<n){
  mb=NA
}  else if (nrow(c2) >= n) {
  mb=c2$Stai_average
  mb_story=c2$story
  mb_site=c2$site
} 


# computing the lmBF function for each experimental condition and the control group with sites and story as random factor for STAI
data1=data.frame(Stress=c(control_s,mw),
                 Group=c(rep("Control",length(control_s)),rep("MW",length(mw))),
                 Site=c(as.character(control_site),as.character(mw_site)),
                 Story=c(as.character(control_story),as.character(mw_story)))
colnames(data1)=c("Stress","Group","Site","Story")
data1$Group=as.factor(data1$Group)
data1$Site=as.factor(data1$Site)
data1$Story=as.factor(data1$Story)
s1=lmBF(Stress ~ Group + Site + Story ,data1,whichRandom=c("Site","Story"))


data2=data.frame(Stress=c(control_s,mb),
                 Group=c(rep("Control",length(control_s)),rep("MB",length(mb))),
                 Site=c(as.character(control_site), as.character(mb_site)),
                 Story=c(as.character(control_story),as.character(mb_story)))
data2$Group=as.factor(data2$Group)
data2$Site=as.factor(data2$Site)
data2$Story=as.factor(data2$Story)
s2=lmBF(Stress ~ Group + Site + Story ,data2,whichRandom=c("site","story"))

data3=data.frame(Stress=c(control_s,lk),
                 Group=c(rep("Control",length(control_s)),rep("LK",length(lk))),
                 Site=c(as.character(control_site),as.character(lk_site)),
                 Story=c(as.character(control_story),as.character(lk_story)))
data3$Group=as.factor(data3$Group)
data3$Site=as.factor(data3$Site)
data3$Story=as.factor(data3$Story)
s3=lmBF(Stress ~ Group + Site + Story ,data3,whichRandom=c("site","story"))


data4=data.frame(Stress=c(control_s,bs),
                 Group=c(rep("Control",length(control_s)),rep("BS",length(bs))),
                 Site=c(as.character(control_site),as.character(bs_site)),
                 Story=c(as.character(control_story),as.character(bs_story)))
data4$Group=as.factor(data4$Group)
data4$Site=as.factor(data4$Site)
data4$Story=as.factor(data4$Story)
s4=lmBF(Stress ~ Group + Site + Story ,data4,whichRandom=c("site","story"))

# verify groups with (BF > 10 or BF < 0.1 or N > Nmax) 
N_max=4800
groups=c("condition1mindfulwalking","condition2mindfulbreathing",
         "condition3lovingkindness","condition4bodyscan",
         "condition5bookchapter")
value=c(extractBF(s1)$bf[1],extractBF(s2)$bf[1],extractBF(s3)$bf[1],extractBF(s4)$bf[1],11)
Size=c(length(mw),length(mb),length(lk),length(bs),length(control_s))
data=data.frame(groups,value,Size)
name=data[data$value>10 | data$value < 0.10 | Size>=N_max  ,"groups"]

# computing the lmBF function for each experimental condition and the control group with sites and story as random factor for SAM
if(("condition1mindfulwalking" %in% name)==T){
  
  D1=sam_emotional[sam_emotional$group == "condition5bookchapter" | sam_emotional$group == "condition1mindfulwalking",]
  t1=lmBF(sam1 ~ group +site+story,D1,whichRandom=c("site","story"))
  
  
  D2=sam_arousal[sam_arousal$group == "condition5bookchapter" | sam_arousal$group == "condition1mindfulwalking",]
  t1b=lmBF(sam2 ~ group +site+story,D2,whichRandom=c("site","story"))
  
  
  D3=sam_control[sam_control$group == "condition5bookchapter" | sam_control$group == "condition1mindfulwalking",]
  t1c=lmBF(sam3 ~ group +site+story,D3,whichRandom=c("site","story"))
  
}  else
  t1=t1b=t1c=NA

if(("condition2mindfulbreathing" %in% name)==T){
  
  D1=sam_emotional[sam_emotional$group == "condition5bookchapter" | sam_emotional$group == "condition2mindfulbreathing",]
  t2=lmBF(sam1 ~ group +site+story,D1,whichRandom=c("site","story"))
  
  D2=sam_arousal[sam_arousal$group == "condition5bookchapter" | sam_arousal$group == "condition2mindfulbreathing",]
  t2b=lmBF(sam2 ~ group +site+story,D2,whichRandom=c("site","story"))
  
  D3=sam_control[sam_control$group == "condition5bookchapter" | sam_control$group == "condition2mindfulbreathing",]
  t2c=lmBF(sam3 ~ group +site+story,D3,whichRandom=c("site","story"))
  
  
}  else
  t2=t2b=t2c=NA

if(("condition3lovingkindness" %in% name)==T){
  
  D1=sam_emotional[sam_emotional$group == "condition5bookchapter" | sam_emotional$group == "condition3lovingkindness",]
  t3=lmBF(sam1 ~ group +site+story,D1,whichRandom=c("site","story"))
  
  D2=sam_arousal[sam_arousal$group == "condition5bookchapter" | sam_arousal$group == "condition3lovingkindness",]
  t3b=lmBF(sam2 ~ group +site+story,D2,whichRandom=c("site","story"))
  
  D3=sam_control[sam_control$group == "condition5bookchapter" | sam_control$group == "condition3lovingkindness",]
  t3c=lmBF(sam3 ~ group +site+story,D3,whichRandom=c("site","story"))
  
}  else
  t3=t3b=t3c=NA

if(("condition4bodyscan" %in% name)==T){
  
  D1=sam_emotional[sam_emotional$group == "condition5bookchapter" | sam_emotional$group == "condition4bodyscan",]
  t4=lmBF(sam1 ~ group +site+story,D1,whichRandom=c("site","story"))
  
  D2=sam_arousal[sam_arousal$group == "condition5bookchapter" | sam_arousal$group == "condition4bodyscan",]
  t4b=lmBF(sam2 ~ group +site+story,D2,whichRandom=c("site","story"))
  
  D3=sam_control[sam_control$group == "condition5bookchapter" | sam_control$group == "condition4bodyscan",]
  t4c=lmBF(sam3 ~ group +site+story,D3,whichRandom=c("site","story"))
  
}  else
  t4=t4b=t4c=NA

# BF of STAI & SAM dimensions for the 4 conditions test
Comparison=c("Control vs  Mindful Walking",
             "Control vs  Mindful Breathing",
             "Control vs  loving Kindness",
             "Control vs  Body Scan")
Stress_BF=c(value[1],value[2],value[3],value[4])

Sam_arousal_BF=c()
if(is.na(t1b)==F){Sam_arousal_BF[1]=extractBF(t1b)$bf[1]} else Sam_arousal_BF[1]==NA
if(is.na(t2b)==F){Sam_arousal_BF[2]=extractBF(t2b)$bf[1]} else Sam_arousal_BF[2]==NA
if(is.na(t3b)==F){Sam_arousal_BF[3]=extractBF(t3b)$bf[1]} else Sam_arousal_BF[3]==NA
if(is.na(t4b)==F){Sam_arousal_BF[4]=extractBF(t4b)$bf[1]} else Sam_arousal_BF[4]==NA

Sam_control_BF=c()
if(is.na(t1c)==F){Sam_control_BF[1]=extractBF(t1c)$bf[1]} else Sam_control_BF[1]==NA
if(is.na(t2c)==F){Sam_control_BF[2]=extractBF(t2c)$bf[1]} else Sam_control_BF[2]==NA
if(is.na(t3c)==F){Sam_control_BF[3]=extractBF(t3c)$bf[1]} else Sam_control_BF[3]==NA
if(is.na(t4c)==F){Sam_control_BF[4]=extractBF(t4c)$bf[1]} else Sam_control_BF[4]==NA


Sam_emotional_BF=c()
if(is.na(t1)==F){Sam_emotional_BF[1]=extractBF(t1)$bf[1]} else Sam_emotional_BF[1]==NA
if(is.na(t2)==F){Sam_emotional_BF[2]=extractBF(t2)$bf[1]} else Sam_emotional_BF[2]==NA
if(is.na(t3)==F){Sam_emotional_BF[3]=extractBF(t3)$bf[1]} else Sam_emotional_BF[3]==NA
if(is.na(t4)==F){Sam_emotional_BF[4]=extractBF(t4)$bf[1]} else Sam_emotional_BF[4]==NA


data=data.frame(Comparison,Stress_BF, Sam_arousal_BF, Sam_control_BF, Sam_emotional_BF)
data


################################################################################
# Non-confirmatory analyses########################################
################################################################################

#neuroticism
results<-merge(x=Stai,y=ipip,by="responseid",all.x=TRUE)
results=results[,c(1,2,3,4,26,33,53)]
colnames(results)=c("responseid","group","site","story","Stai_average","eng_level","ipip_average")


full <- lmBF(Stai_average ~ group +
               ipip_average +
               group:ipip_average +
               site,
             whichRandom=c("site","story"),
             data=results)

noInteraction <- lmBF(Stai_average ~ group +
                        ipip_average +
                        site,
                      whichRandom=c("site","story"),
                      data=results)

allBFs <- c(full, noInteraction)
allBFs

full/noInteraction

chainsFull <- posterior(full, iterations = 10000)
summary(chainsFull[,1:7])


#merging the groups to achieve more power

results$G[results$group=="condition1mindfulwalking"]= "mindfulnessgroups"
results$G[results$group=="condition2mindfulbreathing"]= "mindfulnessgroups"
results$G[results$group=="condition3lovingkindness"]= "mindfulnessgroups"
results$G[results$group=="condition4bodyscan"]= "mindfulnessgroups"
results$G[results$group=="condition5bookchapter"]= "controlgroup"

full <- lmBF(Stai_average ~ G +
               ipip_average +
               G:ipip_average +
               site,
             whichRandom=c("site","story"),
             data=results)

noInteraction <- lmBF(Stai_average ~ G +
                        ipip_average +
                        site,
                      whichRandom=c("site","story"),
                      data=results)

allBFs <- c(full, noInteraction)
allBFs

full/noInteraction

chainsFull <- posterior(full, iterations = 10000)
summary(chainsFull[,1:7])

#moderation for english level

full <- lmBF(Stai_average ~ group +
               eng_level +
               group:eng_level +
               site,
             whichRandom=c("site","story"),
             data=results)

noInteraction <- lmBF(Stai_average ~ group +
                        eng_level +
                        site,
                      whichRandom=c("site","story"),
                      data=results)

allBFs <- c(full, noInteraction)
allBFs

full/noInteraction

chainsFull <- posterior(full, iterations = 10000)
summary(chainsFull[,1:7])

#merging the groups together to achieve more power

table(Df$eng_level)

full <- lmBF(Stai_average ~ G +
               eng_level +
               G:eng_level +
               site,
             whichRandom=c("site","story"),
             data=results)

noInteraction <- lmBF(Stai_average ~ G +
                        eng_level +
                        site,
                      whichRandom=c("site","story"),
                      data=results)

allBFs <- c(full, noInteraction)
allBFs

full/noInteraction

chainsFull <- posterior(full, iterations = 10000)
summary(chainsFull[,1:7])

################################################################################
# Sequential bayesian graphs #
################################################################################

Stai=Stai[,c(1:4,26)]
name=c("condition2mindfulbreathing","condition5bookchapter")
Stai1=Stai[(Stai$group %in% name)==T,]
colnames(Stai1)=c("responseid","Group","Site","Story","Stress")
BF=c()
x=seq(0,890,10)
for (i in 1:length(x)){
  data=Stai1[1:(10+x[i]),]
  s=lmBF(Stress ~ Group + Site + Story ,data,whichRandom=c("site","story"))
  BF[i]=s@bayesFactor$bf
}
v=x+10

BFF=exp(BF) 
bf=log10(BFF)
dat=data.frame(bf,v)
ggplot(data=dat, aes(x=v, y=bf)) +
  geom_line()+
  geom_hline(yintercept=1, color = "red", size=0.5) +
  geom_hline(yintercept=-1, color = "red", size=0.5) +
  labs(x="Sample size",y="log10(Bayes Factor)",title="Control vs Mindful Breathing")


Stai=Stai[,c(1:4,26)]
name=c("condition1mindfulwalking","condition5bookchapter")
Stai1=Stai[(Stai$group %in% name)==T,]
colnames(Stai1)=c("responseid","Group","Site","Story","Stress")
BF=c()
x=seq(0,830,10)
for (i in 1:length(x)){
  data=Stai1[1:(10+x[i]),]
  s=lmBF(Stress ~ Group + Site + Story ,data,whichRandom=c("site","story"))
  BF[i]=s@bayesFactor$bf
}
v=x+10

BFF=exp(BF) 
bf=log10(BFF) 
dat=data.frame(bf,v)
ggplot(data=dat, aes(x=v, y=bf)) +
  geom_line()+
  geom_hline(yintercept=1, color = "red", size=0.5) +
  geom_hline(yintercept=-1, color = "red", size=0.5) +
  labs(x="Sample size",y="log10(Bayes Factor)",title="Control vs Mindful Walking")


Stai=Stai[,c(1:4,26)]
name=c("condition3lovingkindness","condition5bookchapter")
Stai1=Stai[(Stai$group %in% name)==T,]
colnames(Stai1)=c("responseid","Group","Site","Story","Stress")
BF=c()
x=seq(0,840,10)
for (i in 1:length(x)){
  data=Stai1[1:(10+x[i]),]
  s=lmBF(Stress ~ Group + Site + Story ,data,whichRandom=c("site","story"))
  BF[i]=s@bayesFactor$bf
}
v=x+10

BFF=exp(BF) 
bf=log10(BFF) 
dat=data.frame(bf,v)
ggplot(data=dat, aes(x=v, y=bf)) +
  geom_line()+
  geom_hline(yintercept=1, color = "red", size=0.5) +
  geom_hline(yintercept=-1, color = "red", size=0.5) +
  labs(x="Sample size",y="log10(Bayes Factor)",title="Control vs Loving-Kindness")


Stai=Stai[,c(1:4,26)]
name=c("condition4bodyscan","condition5bookchapter")
Stai1=Stai[(Stai$group %in% name)==T,]
colnames(Stai1)=c("responseid","Group","Site","Story","Stress")
BF=c()
x=seq(0,870,10)
for (i in 1:length(x)){
  data=Stai1[1:(10+x[i]),]
  s=lmBF(Stress ~ Group + Site + Story ,data,whichRandom=c("site","story"))
  BF[i]=s@bayesFactor$bf
}
v=x+10

BFF=exp(BF) 
bf=log10(BFF) 
dat=data.frame(bf,v)
ggplot(data=dat, aes(x=v, y=bf)) +
  geom_line()+
  geom_hline(yintercept=1, color = "red", size=0.5) +
  geom_hline(yintercept=-1, color = "red", size=0.5) +
  labs(x="Sample size",y="log10(Bayes Factor)",title="Control vs Body Scan")


#Following some lines of codes that can be useful 

# data collection tracker
# check how many participants for each groups
#d1=as.data.frame(table(Df$group))
#colnames(d1)=c("Group","Participants")

# check how many participants for site
#d2=as.data.frame(table(Df$site))
#colnames(d2)=c("Site","Participants")

#use the following row to see Bayes Factor as scientific notation
#format(3.440854e-05, scientific = FALSE)
