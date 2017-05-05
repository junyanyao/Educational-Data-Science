#Prefecture level analysis

library(foreign)
edat<- read.csv("~/Desktop/edsproject/data_cleaned.csv")
dat<- read.dta("~/Desktop/edsproject/china_univ_by_municipality.dta")

#merge two data frames by year and cityid
total<- merge(edat, dat, by=c("cityid", "year"), all.x=TRUE, all.y=TRUE)

#Create subsets
pref<- total[which(total$urban==0),]
urb<-total[which(total$urban==1),]

#coplot(ncampus ~ year|provcat, typle ="l", data=pref)
#View(prefecture[,c("year","provcat")])


#generate log values of variables(rescale)
l.poptota<- log(pref$poptota)
l.fditota<- log((pref$fditota+1)/pref$gdpdef)
l.gdptota<- log(pref$gdptota/pref$gdpdef)
l.empavea<- log(pref$empavea/pref$gdpdef)

#generate quartic terms of variables
psqua<- l.poptota*l.poptota
fsqua<- l.fditota*l.fditota
gsqua<- l.gdptota*l.gdptota
esqua<-pref$empavea*pref$empavea

pref<- cbind(pref, l.poptota, l.fditota, l.gdptota,l.empavea, psqua, fsqua,gsqua,esqua)

#look at the scatterplot for variables.
plot(pref$ncampus~pref$l.poptota, xlab="log of population in the prefecture level", ylab = "number of campuses in the prefecture level")

#not run #lines(lowess(pref$l.poptota,pref$ncampus),col="red")

plot(pref$ncampus~pref$l.gdptota, xlab="log of real GDP per capita in the prefecture level", ylab = "number of campuses in the prefecture level")
#not run #temp<-loess(pref$ncampus~pref$l.gdptota)
#not run#j<-order(pref$l.gdptota)
#not run# lines(pref$l.gdptota[j],temp$fitted[j],col="red")


plot(pref$ncampus~pref$empavea, xlab="Average wage in the prefecture level", ylab = "number of campuses in the prefecture level")

plot(pref$ncampus~pref$fditota, xlab="Foreign direct investment per capita in the prefecture level", ylab = "number of campuses in the prefecture level")


##OLS fixed effect model
library(plm)
M1<- plm(ncampus ~ l.poptota + l.fditota+l.gdptota+l.empavea+gsqua+fsqua+esqua+psqua,data = pref, index=c("cityid", "year"), effect= "individual", model="within")
print(summary(M1))


G2<- glm(ncampus ~ l.poptota+log(fditota+1)+l.gdptota+l.empavea, data=pref,family =poisson(link=log), weights = pref$phat)
summary(M2)


#####This one did not work###
#library(glmmML)
#M1<- glmmML(ncampus~l.poptota, data=prefecture, cluster=cityid)
#error: yvalues must between 0 and 1;

#using lfe package;
#library(lfe)
#MM1<- felm(ncampus ~ l.poptota + l.gdptota+ fditota, data = pref,weights = pref$phat, na.action = TRUE)
#print(summary(MM1))

#using lmer package
#library(lme4)
#MMM1<- lmer(ncampus ~ l.poptota + (l.poptota|cityid),data = pref,na.action = TRUE)
#print(summary(MMM1))

#using GLM to apply poisson;
#library(MASS)
#MMMM1<- glm(ncampus ~ l.poptota+ l.fditota+ l.gdptota + l.empavea, data = pref,na.action = TRUE)


##Logit model for panel data
#library(pglm)
#M2<- pglm(dcampus ~ lpoptota+fditota+l.gdptota+empavea, data=pref, effect="individual",family= binomial('probit'), model="within")
#print(summary(M2))

#using lme4 package, treat this model as mixed structure model.
#library(dplyr)
#pref2 <- pref[!is.na(pref$dcampus) & !is.na(pref$l.poptota) & !is.na(pref$l.fditota) & !is.na(pref$l.gdptota) & !is.na(pref$l.empavea),]
#View(pref2)
M2<- glm(dcampus ~ l.poptota+log(fditota+1)+l.gdptota+l.empavea, data=pref,family = "binomial")
summary(M2)

#MM2<- nlmer(dcampus ~ l.poptota|cityid+ log(fditota+1)+l.gdptota+l.empavea, data=pref,family = binomial)
