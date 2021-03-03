# Read in data
#install.packages(c("ggplot2","dplyr","ggthemes","fBasics","RColorBrewer","reshape"))
#install.packages("waffle")

library(ggplot2)
library(ggthemes)
library(dplyr)
library(waffle)

#read data
dat <- read.csv('selected_txt_data.csv')

#head(dat)
# Check types of columns
str(dat)

library(fBasics)
stat <- basicStats(victim)

#group number of attacks by year
byyear <- dat %>% group_by(iyear) %>% 
  summarise(frequency=n()) 

#plot the number of attack over time
p<-ggplot(data=byyear, aes(x=iyear, y=frequency, fill=iyear)) +
  geom_bar(stat="identity") + labs(x= 'year', y= 'frequency')
p

#Test plot stacked frequency
#byyear1 <- dat %>% group_by(iyear) %>% 
 # summarise(frequency=n()) %>% 
#  mutate(scsfreq=sum(success))%>%
#  merge(dat) 

#calc number of attacks by regions
byregion <- dat %>% group_by(region_txt) %>% 
  summarise(number=n()) %>%
    mutate(percent = number / sum(number) * 100)

#barplot of number of attack in different regions
p1<- byregion %>%
  arrange(desc(percent)) %>% 
  ggplot(aes(x=reorder(region_txt,percent),y=number,fill=region_txt))+
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle=90,size=8,vjust=1, hjust=1)) +
  labs(x= 'Attacked Regions', y= 'Frequency') +
  coord_flip()
p1

#plot pie chart of percent attacks in diff regions
p2<-ggplot(byregion, aes(x="", y=percent,fill=region_txt)) +
  geom_bar(width = 1, stat="identity")+ coord_polar("y", start=0)
p2

#group number of attacks by types
bytype <- dat %>% group_by(attacktype1_txt) %>% 
  summarise(number=n()) %>% mutate(percent = number/sum(number) *100)

#plot the number of different type of attacks
p3 <- bytype %>%
  arrange(desc(percent)) %>% 
  ggplot(aes(x=reorder(attacktype1_txt,percent),y=number,fill=attacktype1_txt))+
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle=90,size=8,vjust=1, hjust=1)) +
  labs(x= 'Attack Type', y= 'Frequency') +
  coord_flip()
p3

#plot pie chart of percent attacks in diff types
p4<-ggplot(bytype, aes(x="", y=percent,fill=attacktype1_txt)) +
  geom_bar(width=1, stat="identity")+ coord_polar("y", start=0)
p4


#plot the number of attack by group name
dat %>%
  group_by(gname)%>% # group by gname
  count() %>% # count the number of times a gname appear
  arrange(desc(n)) %>% # subset by rows based on condition
  head(n=10) %>%
  ggplot(aes(x=reorder(gname,n),y=n, fill=gname))+
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle=90,size=8,vjust=1, hjust=1)) +
  labs(x= 'Group Name', y= 'Frequency') +
  coord_flip()



#plot the top 10 most attacked countries
dat %>%
  group_by(country_txt)%>% 
  count() %>%
  arrange(desc(n)) %>% 
  head(n=10) %>%
  ggplot(aes(x=reorder(country_txt,n),y=n, fill=country_txt))+
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle=90,size=8,vjust=1, hjust=1)) +
  labs(x= 'Countries', y= 'Frequency') +
  coord_flip()




#library(plyr) #call dplyr after plyr to prevent rlang error
#library(readxl)
#library(fBasics)

#calc type of attacks per regions





#library(reshape2)
#df <- as.data.frame(victim[,c(1,7:9)])
#ggplot(df, aes(x = frequency, y = victim, group=, fill = iyear)) + geom_area(position = 'stack')

# Plot total attacks and succesful rate
plot(x = victim$iyear,y = victim$frequency, type = 'l', col = 'blue', xlab = 'Date', ylab = 'Number of attacks', main = 'Number of attacks from 1970 - 2016') 
lines(x = victim$iyear, y = victim$scsfreq, col = 'red')
legend('topleft', c('Number of Total attacks','Number of succesful attacks'), col = c('blue', 'red'), lty = 1)


#, group=success, fill=success
# Plot numbers of victims
plot(x = victim$iyear,y = victim$victim, type = 'l', col = 'blue', xlab = 'Date', ylab = 'Number of wounded victims', main = 'Personel Damage') 
lines(x = victim$iyear, y = victim$kill, col = 'red')
lines(x = victim$iyear, y = victim$wound, col = 'black')
legend('topleft', c('Total victims','Number of killed persons','Number of wounded persons'), col = c('blue', 'red', 'black'), lty = 1)


#plot number of attacks by regions over year



library(tidyverse)

P <- ggplot(victim1, aes(x=frequency, y=victim)) +
  geom_point()
P





#extract 2014 data
y2014 <- dat[c(125150:142009),c(10,11,18,24,33,35)]

y2014 <- y2014 %>%  
  dplyr::filter(!is.na(nwound)) %>%
  dplyr::filter(!is.na(nkill)) %>%
  group_by(country_txt)%>% mutate(wound=sum(nwound)) %>% 
  mutate(kill=sum(nkill)) %>%
  mutate(victim=sum(nwound,nkill)) %>%
  arrange(desc(victim))

y2011 <- dat[c(71578:73484),c(10,11,18,24,33,35)]

y2011 <- y2001 %>%  
  dplyr::filter(!is.na(nwound)) %>%
  dplyr::filter(!is.na(nkill)) %>%
  group_by(country_txt)%>% mutate(wound=sum(nwound)) %>% 
  mutate(kill=sum(nkill)) %>%
  mutate(victim=sum(nwound,nkill)) %>%
  arrange(desc(victim))


y2001 <- dat[c(71578:73484),c(10,11,18,24,33,35)]

y2001 <- y2001 %>%  
  dplyr::filter(!is.na(nwound)) %>%
  dplyr::filter(!is.na(nkill)) %>%
  group_by(country_txt)%>% mutate(wound=sum(nwound)) %>% 
  mutate(kill=sum(nkill)) %>%
  mutate(victim=sum(nwound,nkill)) %>%
  arrange(desc(victim))





bywp <- dat %>% group_by(weapsubtype1_txt) %>% 
  summarise(freq=n()) 
p1<- bywp %>%
  arrange(desc(freq)) %>% 
  ggplot(aes(x=reorder(weapsubtype1_txt,freq),y=freq,fill=region_txt))+
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle=90,size=8,vjust=1, hjust=1)) +
  labs(x= 'Attacked Regions', y= 'Frequency') +
  coord_flip()
p1


dat$weapsubtype1_txt[dat$weapsubtype1_txt=='']=NA

#plot attacks by subwp
dat %>%
  group_by(weapsubtype1_txt)%>% 
  dplyr::filter(!is.na(weapsubtype1_txt)) %>%
  count() %>%
  arrange(desc(n)) %>% 
  ggplot(aes(x=reorder(weapsubtype1_txt,n),y=n, fill=weapsubtype1_txt))+
  geom_bar(stat = "identity") +
  labs(x= 'Countries', y= 'Frequency') +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()
