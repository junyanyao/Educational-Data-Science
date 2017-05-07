library(foreign)
edat<- read.csv("~/Desktop/edsproject/data_cleaned.csv")
dat<- read.dta("~/Desktop/edsproject/china_univ_by_municipality.dta")
#merge two data frames by year and cityid
total<- merge(edat, dat, by=c("cityid", "year"), all.x=TRUE, all.y=TRUE)

#drop unnecessary variables
total<- total[,-c(3,4,8,15,16,17,18,19,20,21,22,23,30,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47)]
#caret package
#streamline the model building and evaluation process
#use train function to evaluate, using  resampling, the effect of model tuning parameters on performance;
#choose the optimal model across these parameters;
#estimate model performance from a training set;


#Looks like `createDataPartition` split your data into smaller pieces, 
#but allows for the same example to appear in different splits. 
#`createFolds` doesn't allow different examples to appear in different splits of the folds. 

library(caret)
set.seed(100)
fold<- createFolds(newdat$cityid, k=10, list = TRUE, returnTrain = TRUE) #this split the data into 10 folds and the integer represents the row number.
training1<- newdat[fold$Fold01,]
training2<- newdat[fold$Fold02,]
training3<- newdat[fold$Fold03,]
training4<- newdat[fold$Fold04,]
training5<- newdat[fold$Fold05,]
training6<- newdat[fold$Fold06,]
training7<- newdat[fold$Fold07,]
training8<- newdat[fold$Fold08,]
training9<- newdat[fold$Fold09,]
training10<- newdat[fold$Fold10,]
#fit predictive models over different tuning parameters
#use glmnet method: tuning parameters: alpha=1, lambda
ctr1<- trainControl(method = "repeatedcv", repeats = 10)
fit1<- train(ncampus~lgdptota+fditota+wgavea+poptota+nc_year3+nc_year4+nc_year5+nc_year6+nc_year7+nc_year8+nc_year9+nc_year10, data = training1, method="glmnet",trControl=ctr1, metric="ROC", preProc=c("center","scale"), tuneGrid= expand.grid(.alpha=1, .lambda= seq(0.05,10, length.out = 10)))


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








