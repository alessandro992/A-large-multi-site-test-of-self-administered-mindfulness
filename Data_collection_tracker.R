## Data collection tracker ##

#We check how many participants sites are collecting

DfSitesEN <- Df %>%
  filter(site == "EN") 
nrow(DfSitesEN)

#We print how many observations each site collected 
print (DfSitesEN)

#We repeat this for each site
#When the full list of sites will be available you will be able to modify the script

DfSitesLA <- Df %>%
  filter(site == "LA") 
nrow(DfSitesLA)



#We can check how many participants were randomized across conditions (and that come from any site)

Dfmindfulwalking <- Df %>%
  filter(group == "Condition1:Mindfulwalking") 
nrow(Dfmindfulwalking)

Dfmindfulbreathing <- Df %>%
  filter(group == "Condition2:Mindfulbreathing") 
nrow(Dfmindfulbreathing)

Dflovingkindness <- Df %>%
  filter(group == "Condition3:Loving-kindness") 
nrow(Dflovingkindness)

DfBodyScan <- Df %>%
  filter(group == "Condition4:BodyScan") 
nrow(DfBodyScan)

DfBookchapter <- Df %>%
  filter(group == "Condition5:Bookchapter") 
nrow(DfBookchapter)