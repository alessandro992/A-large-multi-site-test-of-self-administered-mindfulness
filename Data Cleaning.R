##########################################
## A large multi-site study on SAM      ##
## Code by Sparacio, Giorgini           ##
##########################################

#That's only a test#

## install.packages("qualtRics")
library(qualtRics)
## install.packages("tidyverse")
library(tidyverse)
## install.packages("janitor")
library(janitor)
## install.packages("psych")
library(psych)
## install.packages("dataMaid")
library(dataMaid)
## install.packages("skimr")
library(skimr)
## install.packages("dplyr")
library(dplyr)
## install.packages("reshape2")
library(reshape2)
## install.packages("DT")
library(DT)  
## install.packages("tinytex")
library(tinytex)


# Load data ---------------------------
Df <- read_survey("A large, multi-site test of SAM.csv") %>%
  clean_names()

names(Df) <- tolower(names(Df))
Df <- Df %>% rename(responseid = response_id) %>% 
  rename(group = fl_13_do) 

##############################################
## IPIP-5 NEO Domains Neuroticism 20 items ##
############################################
## Response options: 1 - very inaccurate	2 - Moderately Inaccurate 3 - Neither Inaccurate nor Accurate 4 - Moderately Accurate 5 - Very Accurate 
## scoring can be found here: https://ipip.ori.org/newNEOKey.htm#Neuroticism

ipip <- Df %>% 
  select(responseid, group, starts_with("ipip")) 

## reverse item 11,12,13,14,15,16,17,18,19,20
ipip <- ipip %>% 
  transmute(responseid = responseid,
            group = group,
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
mutate(ipip_tot = rowSums(select(., c(3:22))))

#compute the mean for ipip Neuroticism scale
ipip$ipip_average <- rowMeans(cbind(ipip$ipip1,ipip$ipip2,ipip$ipip3,ipip$ipip4,ipip$ipip5,ipip$ipip6,
                              ipip$ipip7,ipip$ipip8,ipip$ipip9,ipip$ipip10,ipip$ipip11_r,ipip$ipip12_r,
                              ipip$ipip13_r,ipip$ipip14_r,ipip$ipip15_r,ipip$ipip16_r,ipip$ipip17_r,
                              ipip$ipip18_r,ipip$ipip19_r,ipip$ipip20_r), na.rm = TRUE)

#compute the mean for ipip Neuroticism divided by groups
group_by(ipip, group) %>%
  summarise(ipip_average = mean(ipip_average, na.rm = TRUE))

#compute the median for ipip Neuroticism scale
ipip$ipip_median <- median(cbind(ipip$ipip1,ipip$ipip2,ipip$ipip3,ipip$ipip4,ipip$ipip5,ipip$ipip6,
                                       ipip$ipip7,ipip$ipip8,ipip$ipip9,ipip$ipip10,ipip$ipip11_r,ipip$ipip12_r,
                                       ipip$ipip13_r,ipip$ipip14_r,ipip$ipip15_r,ipip$ipip16_r,ipip$ipip17_r,
                                       ipip$ipip18_r,ipip$ipip19_r,ipip$ipip20_r), na.rm = TRUE)

#compute the median for ipip Neuroticism divided by groups
group_by(ipip, group) %>%
  summarise(ipip_average = median(ipip_average, na.rm = TRUE))

#compute the SD for ipip Neuroticism scale
ipip$ipip_sd <- sd(cbind(ipip$ipip1,ipip$ipip2,ipip$ipip3,ipip$ipip4,ipip$ipip5,ipip$ipip6,
                          ipip$ipip7,ipip$ipip8,ipip$ipip9,ipip$ipip10,ipip$ipip11_r,ipip$ipip12_r,
                          ipip$ipip13_r,ipip$ipip14_r,ipip$ipip15_r,ipip$ipip16_r,ipip$ipip17_r,
                          ipip$ipip18_r,ipip$ipip19_r,ipip$ipip20_r), na.rm = TRUE)

#compute the sd for ipip Neuroticism divided by groups
group_by(ipip, group) %>%
  summarise(ipip_average = sd(ipip_average, na.rm = TRUE))

#####################
## STAY Form  Y-1 ##
###################
## Response options: 1 - not at all	2 - somewhat 3 - Moderately so 4 - Very much so 
## scoring can be found here: https://oml.eular.org/sysModules/obxOML/docs/id_150/State-Trait-Anxiety-Inventory.pdf

Stai <- Df %>% 
  select(responseid, group, starts_with("Stai")) 

## reverse item 1,2,5,8,10,11,15,16,19,20.
Stai <- Stai %>% 
  transmute(responseid = responseid,
            group = group,
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
  mutate(Stai_tot = rowSums(select(., c(3:22))))

#compute the mean for Stay Form Y-1
Stai$Stai_average <- rowMeans(cbind(Stai$Stai1_r,Stai$Stai2_r,Stai$Stai3,Stai$Stai4,Stai$Stai5_r,
                                    Stai$Stai6,Stai$Stai7,Stai$Stai8_r,Stai$Stai9,Stai$Stai10_r,Stai$Stai11_r,
                                    Stai$Stai12,Stai$Stai13,Stai$Stai14,Stai$Stai15_r,Stai$Stai16_r,Stai$Stai17,
                                    Stai$Stai18,Stai$Stai19_r,Stai$Stai20_r), na.rm = TRUE)

group_by(Stai, group) %>%
  summarise(Stai_average = mean(Stai_average, na.rm = TRUE))


#compute the median for Stay Form Y-1
Stai$Stai_median <- median(cbind(Stai$Stai1_r,Stai$Stai2_r,Stai$Stai3,Stai$Stai4,Stai$Stai5_r,
                                    Stai$Stai6,Stai$Stai7,Stai$Stai8_r,Stai$Stai9,Stai$Stai10_r,Stai$Stai11_r,
                                    Stai$Stai12,Stai$Stai13,Stai$Stai14,Stai$Stai15_r,Stai$Stai16_r,Stai$Stai17,
                                    Stai$Stai18,Stai$Stai19_r,Stai$Stai20_r), na.rm = TRUE)

#compute the median for Stay Form Y-1 divided by groups
group_by(Stai, group) %>%
  summarise(Stai_median = median(Stai_average, na.rm = TRUE))

#compute the SD for Stay Form Y-1
Stai$Stai_sd <- sd(cbind(Stai$Stai1_r,Stai$Stai2_r,Stai$Stai3,Stai$Stai4,Stai$Stai5_r,
                         Stai$Stai6,Stai$Stai7,Stai$Stai8_r,Stai$Stai9,Stai$Stai10_r,Stai$Stai11_r,
                         Stai$Stai12,Stai$Stai13,Stai$Stai14,Stai$Stai15_r,Stai$Stai16_r,Stai$Stai17,
                         Stai$Stai18,Stai$Stai19_r,Stai$Stai20_r), na.rm = TRUE)

#compute the SD for Stay Form Y-1 divided by groups
group_by(Stai, group) %>%
  summarise(Stai_sd = mean(Stai_sd, na.rm = TRUE))

#############################
## Self-Assessment Manikin ##
#############################
## Emotional Valence (1 = Very unpleasant ; 9 = Very pleasant).
## Arousal (1 = Very apathetic ; 9 = Very intense).
## Control (1 = Very submissive and being controlled ; 9 = Very powerful and in control)
## scoring can be found here: https://pubmed.ncbi.nlm.nih.gov/7962581/

sam_emotional <- Df %>% 
  select(responseid, group, starts_with("Sam1")) 

group_by(sam_emotional, group) %>%
  summarise(sam_emotional, na.rm = TRUE)

sam_arousal <- Df %>% 
  select(responseid, group, starts_with("Sam2")) 

group_by(sam_arousal, group) %>%
  summarise(sam_arousal, na.rm = TRUE)

sam_control <- Df %>% 
  select(responseid, group, starts_with("Sam3")) 

group_by(sam_control, group) %>%
  summarise(sam_control, na.rm = TRUE)

######################################
## Demographics ##--------------------
######################################
## gender
## options: male (1), female (2), other (3) prefer not to say (4)

gender <- Df %>%
  select(responseid, gender)

## age
# Compute the mean, median, and conf interval:
age <- Df %>%
  select(responseid, age) %>%
  mutate(mean = mean(age),
        median = median(age),
        sd = sd(age)
  )









