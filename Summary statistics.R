library(foreign)
edat<- read.csv("~/Desktop/edsproject/data_cleaned.csv")
dat<- read.dta("~/Desktop/edsproject/china_univ_by_municipality.dta")

#merge two data frames by year and cityid
total<- merge(edat, dat, by=c("cityid", "year"), all.x=TRUE, all.y=TRUE)

#drop unnecessary variables
data<- total[,-c(3,4,8,15,16,17,18,19,20,21,22,23,30,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47)]
#generate two periods- 0 is preexpansion; 1 is expansion;
peroid<- 0
data<- cbind(data, peroid)
data$period[data$year<= 1999]=0
data$period[data$year>= 2000]=1

#generate regions;
#1- North; 2- Northeast; 3- East; 4-South Central; 5- Southwest; 6- Northwest;
region<- as.integer(data$cityid/100000)
data<- cbind(data,region)

#generate city type(by population)
#citytype<-0
#data<- cbind(data, citytype)

library(ggplot2)
library(gganimate)

#animate real total GDP, measured in 100 million yuan;
#calculate real GDP( GDP deflator use 2000 as the base year)
rgdptota<- total$gdptota/total$gdpdef
pa<- ggplot(data, aes(rgdptota, ncampus, size = poptota, frame = year)) +
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








