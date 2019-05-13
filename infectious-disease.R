##librairies needed
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
###
cenus<- read.csv("DEC_10_SF1_GCTPH1.ST05_with_ann.csv",header=T)
head(cenus)
colnames(cenus)<- c("ID","ID2","Geog","Geoid1","Geoid2","Geographicarea",
                    "county","Population","Housing units","totalarea","waterarea",
                    "leandarea","popdens","housedens")

##
ggplot(cenus)+
  geom_col(aes(county,Population, fill=county))+
  ylim(0,1000000)+
  ggpubr::theme_pubr()+
  xlab("Counties")+
  ylab("Population")+
  theme(axis.text.x = element_text(size=16,hjust=45))+
  theme(axis.title.x = element_text(size=16))+
  theme(axis.text.y = element_text(size=16))+
  theme(axis.title.y = element_text(size=16))+
  labs(title = "Population of california counties")
######
############
dis<- read.csv("izb_odp_final_03262019.csv")
head(dis)
dis2<- dis %>% 
  spread(year,count)
head(dis2)    
ggplot(dis)+
  geom_line(aes(year,count, col=county))+
  facet_wrap(~disease,scales = "free")+
  ggpubr::theme_pubr()+
  xlab("Disease")+
  ylab("Number of cases")+
  theme(axis.text.x = element_text(size=16))+
  theme(axis.title.x = element_text(size=16))+
  theme(axis.text.y = element_text(size=16))+
  theme(axis.title.y = element_text(size=16))+
  labs(title = "Number of cases of infectious diseases in california counties")
  ###########
###################
#split the data for county from census
head(cenus)
census<- cenus %>% 
  na.omit() %>% 
  separate("county",into=c("county","just "))
head(census)
tail(census)
tail(dis)
##merge by county
dem.dis<- full_join(census,dis,by="county")
head(dem.dis)
tail(dem.dis)
??tidyr::separate
colnames(cenus)
de.dis.2010<-dem.dis %>% 
  filter(year==2010) %>% 
  select(county,year,Population,disease,count,popdens) %>% 
  group_by(disease) %>% 
  summarise(n=sum(count))
  #spread(disease,count) %>% 
tail(de.dis.2010)
head(de.dis.2010)

###does the cases depend on the population
dem.dis2<-dem.dis %>% filter(year==2010) %>% group_by(disease) %>% 
  select(county,disease,Population,count,popdens)
###pertusis
de.dis.2010.per<-dem.dis %>% 
  filter(year==2010) %>% 
  select(county,year,Population,disease,count,popdens) %>% 
 filter(disease== "Pertussis")

dem.dis.2010.per<- as.data.frame(de.dis.2010.per)
m1<-glm(count~county,data=dem.dis.2010.per)
m2<-glm(count~Population,data=dem.dis.2010.per)
m3<-glm(count~county+Population,data=dem.dis.2010.per)
m4<-glm(count~popdens,data=dem.dis.2010.per)
### relation between population and cases of pertusis
ggplot(de.dis.2010.per, aes(Population,count)) +
  geom_smooth(method=lm, forumla =count~Population,se=T,col="red",fill="orchid")+
  xlim(0,1000000)+
  ggpubr::theme_pubr()+
  xlab("Population")+
  ylab("Number of cases")+
  theme(axis.text.x = element_text(size=16))+
  theme(axis.title.x = element_text(size=16))+
  theme(axis.text.y = element_text(size=16))+
  theme(axis.title.y = element_text(size=16))+
  labs(title="Relation between population and number of pertussis cases ")
### relation between population and cases of all diseases
ggplot(dem.dis2, aes(Population,count)) +
  geom_smooth(method=lm, forumla =count~Population,se=T,col="orange",fill="yellow")+
  xlim(0,1000000)+
  ggpubr::theme_pubr()+
  xlab("Population")+
  ylab("Number of cases")+
  theme(axis.text.x = element_text(size=16))+
  theme(axis.title.x = element_text(size=16))+
  theme(axis.text.y = element_text(size=16))+
  theme(axis.title.y = element_text(size=16))+
  labs(title="Relation between population and number of all disease cases ")
##############3
###################densiyt
### relation between population and cases of pertusis
ggplot(de.dis.2010.per, aes(popdens,count)) +
  geom_smooth(method=lm, forumla =count~popdens,se=T,col="purple",fill="light green")+
  #xlim(0,1000000)+
  ggpubr::theme_pubr()+
  xlab("Population density")+
  ylab("Number of cases")+
  theme(axis.text.x = element_text(size=16))+
  theme(axis.title.x = element_text(size=16))+
  theme(axis.text.y = element_text(size=16))+
  theme(axis.title.y = element_text(size=16))+
  labs(title="Relation between population density and number of pertussis cases ")
### relation between population and cases of all diseases
ggplot(dem.dis2, aes(popdens,count)) +
  geom_smooth(method=lm, forumla =count~podens,se=T,col="navy blue",fill="ligh tblue")+
  #xlim(0,1000000)+
  ggpubr::theme_pubr()+
  xlab("Population density")+
  ylab("Number of cases")+
  theme(axis.text.x = element_text(size=16))+
  theme(axis.title.x = element_text(size=16))+
  theme(axis.text.y = element_text(size=16))+
  theme(axis.title.y = element_text(size=16))+
  labs(title="Relation between population and number of all disease cases ")

