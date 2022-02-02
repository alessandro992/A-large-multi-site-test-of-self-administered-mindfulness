## Data collection tracker ##

#We check how many participants sites are collecting

Df$site=as.factor(Df$site)
d=as.data.frame(table(Df$site))
colnames(d)=c("Site","Frequency")
d

#We can check how many participants were randomized across conditions (and that come from any site)

Df$group=as.factor(Df$group)
d1=as.data.frame(table(Df$group))
colnames(d1)=c("Group","Frequency")
d1
