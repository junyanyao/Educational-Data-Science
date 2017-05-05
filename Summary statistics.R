library(foreign)
edat<- read.csv("~/Desktop/edsproject/data_cleaned.csv")
dat<- read.dta("~/Desktop/edsproject/china_univ_by_municipality.dta")

#merge two data frames by year and cityid
total<- merge(edat, dat, by=c("cityid", "year"), all.x=TRUE, all.y=TRUE)

#drop unnecessary variables
total<- total[,-c(3,4,8,15,16,17,18,19,20,21,22,23,30,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47)]
#generate two periods- 0 is preexpansion; 1 is expansion;
peroid<- 0
total<- cbind(total, peroid)
total$period[total$year<= 1999]=0
total$period[total$year>= 2000]=1
#generate the change of number of campuses
require(dplyr)
#create nc_year3
df<- data.frame(id=total$cityid, year=total$year, value= total$ncampus)
year3<- df %>% filter(!is.na(year)) %>% mutate(year= year+3, lag_nc_year3= value, value=NULL)
df %>% left_join(year3)
df3<- cbind(df,df %>% left_join(year3))
df3<-df[,-c(1,2,3)]
nc_year3<- df3$value- df3$lag_nc_year3
df<-cbind(df, nc_year3)
#create nc_year4
year4<- df %>% filter(!is.na(year)) %>% mutate(year= year+4, lag_nc_year4= value, value=NULL)
df %>% left_join(year4)
df4<- cbind(df,df %>% left_join(year4))
df4<-df4[,-c(1,2,3)]
nc_year4<- df4$value- df4$lag_nc_year4
#create nc_year5
year5<- df %>% filter(!is.na(year)) %>% mutate(year= year+5, lag_nc_year5= value, value=NULL)
df %>% left_join(year5)
df5<- cbind(df,df %>% left_join(year5))
df5<-df5[,-c(1,2,3)]
nc_year5<- df5$value- df5$lag_nc_year5
#create nc_year6
year6<- df %>% filter(!is.na(year)) %>% mutate(year= year+6, lag_nc_year6= value, value=NULL)
df %>% left_join(year6)
df6<- cbind(df,df %>% left_join(year6))
df6<-df6[,-c(1,2,3)]
nc_year6<- df6$value- df6$lag_nc_year6
#create nc_yeat7
year7<- df %>% filter(!is.na(year)) %>% mutate(year= year+7, lag_nc_year7= value, value=NULL)
df %>% left_join(year7)
df7<- cbind(df,df %>% left_join(year7))
df7<-df7[,-c(1,2,3)]
nc_year7<- df7$value- df7$lag_nc_year7
#create nc_year8
year8<- df %>% filter(!is.na(year)) %>% mutate(year= year+8, lag_nc_year8= value, value=NULL)
df %>% left_join(year8)
df8<- cbind(df,df %>% left_join(year8))
df8<-df8[,-c(1,2,3)]
nc_year8<- df8$value- df8$lag_nc_year8
#create nc_year9
year9<- df %>% filter(!is.na(year)) %>% mutate(year= year+9, lag_nc_year9= value, value=NULL)
df %>% left_join(year9)
df9<- cbind(df,df %>% left_join(year9))
df9<-df9[,-c(1,2,3)]
nc_year9<- df9$value- df9$lag_nc_year9
#create nc_year10
year10<- df %>% filter(!is.na(year)) %>% mutate(year= year+10, lag_nc_year10= value, value=NULL)
df %>% left_join(year10)
df10<- cbind(df,df %>% left_join(year10))
df10<-df10[,-c(1,2,3)]
nc_year10<- df10$value- df10$lag_nc_year10















#generate regions;
#1- North; 2- Northeast; 3- East; 4-South Central; 5- Southwest; 6- Northwest;
region<- as.integer(total$cityid/100000)
total<- cbind(total,region)

#generate city type(by population)
#citytype<-0
#data<- cbind(data, citytype)

library(ggplot2)
library(gganimate)

#animate real total GDP, measured in 100 million yuan;
#calculate real GDP( GDP deflator use 2000 as the base year)
rgdptota<- total$gdptota/total$gdpdef
pa<- ggplot(total, aes(rgdptota, ncampus, size = poptota, frame = year)) +
  geom_point()+geom_smooth(aes(group = year), method = "lm", show.legend = FALSE) +
  facet_wrap(~region, scales = "free")
gganimate(pa)


rgdptotc<- total$gdptotc/total$gdpdef
pc<- ggplot(total, aes(rgdptotc, ncampus, size = poptotc, frame = year)) +
  geom_point()+geom_smooth(aes(group = year), method = "lm", show.legend = FALSE) +
  facet_wrap(~region, scales = "free")
gganimate(pc)


library(glmnet)
# glmnet can also deal with penalized linear reg, binary and multinomial
#   logistic reg, Poisson reg. 
# It can also use the elastic net (or ridge) penalty.








