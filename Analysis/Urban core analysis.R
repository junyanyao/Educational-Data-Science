#Urban core Analysis
library(foreign)
edat<- read.csv("~/Desktop/edsproject/data_cleaned.csv")
dat<- read.dta("~/Desktop/edsproject/china_univ_by_municipality.dta")

#merge two data frames by year and cityid
total<- merge(edat, dat, by=c("cityid", "year"), all.x=TRUE, all.y=TRUE)

#Create subsets
pref<- total[which(total$urban==0),]
urb<-total[which(total$urban==1),]

#generate log values of variables(rescale)
l.poptotc<- log(urban$poptotc)
l.gdptotc<- log(urban$gdptotc/urban$gdpdef)
urban<- cbind(urban, lpoptotc, l.gdptotc)

#OLS regression with fixed effect
library(plm)
U1<- plm(ncampus ~ lpoptotc + log(fditotc+1)+ l.gdptotc+log(empavec),data = urban, index=c("cityid", "year"), effect= "individual",model="within")
print(summary(U1))

#Probt analysis
U2<- glm(dcampus ~ l.poptotc+log(fditotc+1)+l.gdptotc+log(empavec), data=urban,family = "binomial")
summary(U2)




