
#y Dependent variable
#x Independent variable
#d Control variable
#sed  A random seed
#data Data

dml_boosting <- function(y,x,d,data,sed=123) {

#split the sample into two parts
#library(caret)
set.seed(sed)
trainindex=createDataPartition(d,p=0.5,list=F)
data1=data[trainindex,]
data2=data[-trainindex,]
y1=y[trainindex]
y2=y[-trainindex]
d1=d[trainindex]
d2=d[-trainindex]

formula.1 <- as.formula(paste("y1~",x))
formula.2 <- as.formula(paste("d1~",x))

formula.3 <- as.formula(paste("y2~",x))
formula.4 <- as.formula(paste("d2~",x))

#########################################################
# Method 3: boosting
#library(gbm)
set.seed(sed)
#step 1
boost.aq=gbm(formula.1,data=data1,distribution="gaussian",n.trees=100,interaction.depth=4,shrinkage=0.1)
summary(boost.aq)
yhat.aq3=predict(boost.aq,newdata=data2,n.trees=100)
ylhat31=y2-yhat.aq3
#step 2
boost.d=gbm(formula.2,data=data1,distribution="gaussian",n.trees=100,interaction.depth=4,shrinkage=0.1)
summary(boost.d)
yhat.d3=predict(boost.d,newdata=data2,n.trees=100)
vhat31=d2-yhat.d3
#step 3
boost.aq=gbm(formula.3,data=data2,distribution="gaussian",n.trees=100,interaction.depth=4,shrinkage=0.1)
summary(boost.aq)
yhat.aq3=predict(boost.aq,newdata=data1,n.trees=100)
ylhat32=y1-yhat.aq3
#step 4
boost.d=gbm(formula.4,data=data2,distribution="gaussian",n.trees=100,interaction.depth=4,shrinkage=0.1)
summary(boost.d)
yhat.d3=predict(boost.d,newdata=data1,n.trees=100)
vhat32=d1-yhat.d3
#step5: reg ylhat vhat
lm.fit1=lm(ylhat31~vhat31)
summary(lm.fit1)

lm.fit2=lm(ylhat32~vhat32)
summary(lm.fit2)

# DML2: combine and reg
dim1=length(ylhat31)
dim2=length(ylhat32)
dim=dim1+dim2

dim3=dim1+1
yhat=rep(NA,dim)
yhat[1:dim1]=ylhat31
yhat[dim3:dim]=ylhat32

vhat=rep(NA,dim)
vhat[1:dim1]=vhat31
vhat[dim3:dim]=vhat32

lm.all=lm(yhat~vhat)
# compute robust standard error

#install.packages("lmtest")
#install.packages("sandwich")
#library(lmtest)
#library(sandwich)
#est1 = coeftest(lm.fit1 )
est1 = coeftest(lm.fit1)

est2 = coeftest(lm.fit2)

b1 = est1[2,1]
b2 = est2[2,1]
be = (b1+b2)/2

se1 = est1[2,2]
se2 = est2[2,2]
sig2 =(se1^2+se2^2 +(b1-be)^2+(b2-be)^2)/2
se =sqrt(sig2)
t =be/se



# combined reg
estall = coeftest(lm.all)
beAll=estall[2,1]
seAll=estall[2,2]
tAll=beAll/seAll


# ouput the estimation results
message("-----------------------------------------------------------","\n")
message("Double machine learning 1 (boosting, 2-folds):","\n")
message("Estimate, s.e., t-statistic, p.value, 95%lower, 95%upper","\n")
message(cbind(theta=be,se.robust=se,t.value=t,pvalue=round(2*(1-pnorm(abs(be/se))),5),
            be-1.96*se,be+1.96*se),digits=4)

message("t-statistic critial values: 90%=1.65, 95%=1.96, 99%=2.58","\n")
message("-----------------------------------------------------------","\n")




message("-----------------------------------------------------------","\n")
message("Double machine learning 2 (boosting, 2-folds):","\n")
message("Estimate, s.e., t-statistic, p.value, 95%lower, 95%upper","\n")
message(cbind(theta=beAll,se.robust=seAll,t.value=tAll,pvalue=round(2*(1-pnorm(abs(beAll/seAll))),5),
            beAll-1.96*seAll,beAll+1.96*seAll),digits=4)

message("t-statistic critial values: 90%=1.65, 95%=1.96, 99%=2.58","\n")
message("-----------------------------------------------------------","\n")

}

dml_neural_network <- function( y,x,d,data,sed=123) {
  #split the sample into two parts

  set.seed(sed)
  #library(caret)
  trainindex=createDataPartition(d,p=0.5,list=F)
  data1=data[trainindex,]
  data2=data[-trainindex,]
  y1=y[trainindex]
  y2=y[-trainindex]
  d1=d[trainindex]
  d2=d[-trainindex]


  formula.1 <- as.formula(paste("y1~",x))
  formula.2 <- as.formula(paste("d1~",x))

  formula.3 <- as.formula(paste("y2~",x))
  formula.4 <- as.formula(paste("d2~",x))


  #########################################################

  # method 4: nuetral networks
  ##library(neuralnet)
  #library(nnet)
  set.seed(sed)
  #step 1
  #net.aq <- neuralnet(formula.1, data = data1,  hidden = 1, linear.output = FALSE)
  net.aq <- nnet(formula.1,data = data1,maxit=300,size=1,linout=T)
  summary(net.aq)
  yhat.aq4 = predict(net.aq, newdata = data2)
  ylhat41=y2-yhat.aq4
  #step 2
  #net.d <- neuralnet(formula.2, data = data1,  hidden = 1, linear.output = FALSE)
  net.d <- nnet(formula.2,data = data1,maxit=300,size=1,linout=T)
  summary(net.d)
  yhat.d4=predict(net.d,newdata=data2)
  vhat41=d2-yhat.d4

  #step 3
  #net.aq <- neuralnet(formula.3, data = data2,  hidden = 1, linear.output = FALSE)
  net.aq <- nnet(formula.3,data = data2,maxit=300,size=1,linout=T)
  summary(net.aq)
  yhat.aq4 = predict(net.aq, newdata = data1)
  ylhat42=y1-yhat.aq4
  #step 4
  #net.d <- neuralnet(formula.4, data = data2,  hidden = 1, linear.output = FALSE)
  net.d <- nnet(formula.4,data = data2,maxit=300,size=1,linout=T)
  summary(net.d)
  yhat.d4=predict(net.d,newdata=data1)
  vhat42=d1-yhat.d4


  #step5: reg ylhat vhat
  lm.fit1=lm(ylhat41~vhat41)
  summary(lm.fit1)

  lm.fit2=lm(ylhat42~vhat42)
  summary(lm.fit2)

  # DML2: combine and reg
  dim1=length(ylhat41)
  dim2=length(ylhat42)
  dim=dim1+dim2

  dim3=dim1+1
  yhat=rep(NA,dim)
  yhat[1:dim1]=ylhat41
  yhat[dim3:dim]=ylhat42

  vhat=rep(NA,dim)
  vhat[1:dim1]=vhat41
  vhat[dim3:dim]=vhat42

  lm.all=lm(yhat~vhat)
  # compute robust standard error

  #install.packages("lmtest")
  #install.packages("sandwich")
  #library(lmtest)
  #library(sandwich)
  est1 = coeftest(lm.fit1)
  est2 = coeftest(lm.fit2)

  b1 = est1[2,1]
  b2 = est2[2,1]
  be = (b1+b2)/2

  se1 = est1[2,2]
  se2 = est2[2,2]
  sig2 =(se1^2+se2^2 +(b1-be)^2+(b2-be)^2)/2
  se =sqrt(sig2)
  t =be/se



  # combined reg
  estall = coeftest(lm.all)
  beAll=estall[2,1]
  seAll=estall[2,2]
  tAll=beAll/seAll


  # ouput the estimation results
  message("-----------------------------------------------------------","\n")
  message("Double machine learning 1 (nuetral networks , 2-folds):","\n")
  message("Estimate, s.e., t-statistic, p.value, 95%lower, 95%upper","\n")
  message(cbind(theta=be,se.robust=se,t.value=t,pvalue=round(2*(1-pnorm(abs(be/se))),5),
              be-1.96*se,be+1.96*se),digits=4)

  message("t-statistic critial values: 90%=1.65, 95%=1.96, 99%=2.58","\n")
  message("-----------------------------------------------------------","\n")




  message("-----------------------------------------------------------","\n")
  message("Double machine learning 2 (nuetral networks, 2-folds):","\n")
  message("Estimate, s.e., t-statistic, p.value, 95%lower, 95%upper","\n")
  message(cbind(theta=beAll,se.robust=seAll,t.value=tAll,pvalue=round(2*(1-pnorm(abs(beAll/seAll))),5),
              beAll-1.96*seAll,beAll+1.96*seAll),digits=4)

  message("t-statistic critial values: 90%=1.65, 95%=1.96, 99%=2.58","\n")
  message("-----------------------------------------------------------","\n")
}

dml_random_forest<- function(y,x,d,data,sed=123) {
  #split the sample into two parts
  #library(caret)
  set.seed(sed)
  trainindex=createDataPartition(d,p=0.5,list=F)
  data1=data[trainindex,]
  data2=data[-trainindex,]
  y1=y[trainindex]
  y2=y[-trainindex]
  d1=d[trainindex]
  d2=d[-trainindex]

  formula.1 <- as.formula(paste("y1~",x))
  formula.2 <- as.formula(paste("d1~",x))

  formula.3 <- as.formula(paste("y2~",x))
  formula.4 <- as.formula(paste("d2~",x))

  #########################################################
  # Method 2: random forecast
  #library(randomForest)
  set.seed(sed)
  #step 1
  rf.aq2=randomForest(formula.1,data=data1,ntree=25,importance=TRUE)
  summary(rf.aq2)
  yhat.aq2 = predict(rf.aq2, newdata = data2)
  ylhat21=y2-yhat.aq2
  #step 2
  rf.d2=randomForest(formula.2,data=data1,ntree=25,importance=TRUE)
  summary(rf.d2)
  yhat.d2 = predict(rf.d2, newdata = data2)
  vhat21 = d2-yhat.d2
  #step 3
  rf.aq2=randomForest(formula.3,data=data2,ntree=25,importance=TRUE)
  summary(rf.aq2)
  yhat.aq2 = predict(rf.aq2, newdata = data1)
  ylhat22 = y1-yhat.aq2
  #step 4
  rf.d2=randomForest(formula.4,data=data2,ntree=25,importance=TRUE)
  summary(rf.d2)
  yhat.d2 = predict(rf.d2, newdata = data1)
  vhat22 = d1-yhat.d2

  #step5: reg ylhat vhat
  lm.fit1=lm(ylhat21~vhat21)
  summary(lm.fit1)

  lm.fit2=lm(ylhat22~vhat22)
  summary(lm.fit2)

  # DML2: combine and reg
  dim1=length(ylhat21)
  dim2=length(ylhat22)
  dim=dim1+dim2

  dim3=dim1+1
  yhat=rep(NA,dim)
  yhat[1:dim1]=ylhat21
  yhat[dim3:dim]=ylhat22

  vhat=rep(NA,dim)
  vhat[1:dim1]=vhat21
  vhat[dim3:dim]=vhat22

  lm.all=lm(yhat~vhat)
  # compute robust standard error

  #install.packages("lmtest")
  #install.packages("sandwich")
  #library(lmtest)
  #library(sandwich)
  est1 = coeftest(lm.fit1)
  est2 = coeftest(lm.fit2)

  b1 = est1[2,1]
  b2 = est2[2,1]
  be = (b1+b2)/2

  se1 = est1[2,2]
  se2 = est2[2,2]
  sig2 =(se1^2+se2^2 +(b1-be)^2+(b2-be)^2)/2
  se =sqrt(sig2)
  t =be/se



  # combined reg
  estall = coeftest(lm.all)
  beAll=estall[2,1]
  seAll=estall[2,2]
  tAll=beAll/seAll


  # ouput the estimation results
  message("-----------------------------------------------------------","\n")
  message("Double machine learning 1 (random forest , 2-folds):","\n")
  message("Estimate, s.e., t-statistic, p.value, 95%lower, 95%upper","\n")
  message(cbind(theta=be,se.robust=se,t.value=t,pvalue=round(2*(1-pnorm(abs(be/se))),5),
              be-1.96*se,be+1.96*se),digits=4)

  message("t-statistic critial values: 90%=1.65, 95%=1.96, 99%=2.58","\n")
  message("-----------------------------------------------------------","\n")




  message("-----------------------------------------------------------","\n")
  message("Double machine learning 2 (random forest, 2-folds):","\n")
  message("Estimate, s.e., t-statistic, p.value, 95%lower, 95%upper","\n")
  message(cbind(theta=beAll,se.robust=seAll,t.value=tAll,pvalue=round(2*(1-pnorm(abs(beAll/seAll))),5),
              beAll-1.96*seAll,beAll+1.96*seAll),digits=4)

  message("t-statistic critial values: 90%=1.65, 95%=1.96, 99%=2.58","\n")
  message("-----------------------------------------------------------","\n")

}

dml_bagging <- function(y,x,d,data,sed=123) {
  #split the sample into two parts
  #library(caret)
  set.seed(sed)
  trainindex=createDataPartition(d,p=0.5,list=F)
  data1=data[trainindex,]
  data2=data[-trainindex,]
  y1=y[trainindex]
  y2=y[-trainindex]
  d1=d[trainindex]
  d2=d[-trainindex]

  formula.1 <- as.formula(paste("y1~",x))
  formula.2 <- as.formula(paste("d1~",x))

  formula.3 <- as.formula(paste("y2~",x))
  formula.4 <- as.formula(paste("d2~",x))

  #########################################################

  # Method 1: bagging

  #library(randomForest)
  set.seed(sed)
  #step 1
  bag.aq=randomForest(formula.1,data=data1,mtry=6,importance=TRUE)
  summary(bag.aq)
  yhat.aq = predict(bag.aq, newdata = data2)
  ylhat1=y2-yhat.aq
  #step 2
  bag.d=randomForest(formula.2,data=data1,mtry=6,importance=TRUE)
  summary(bag.d)
  yhat.d = predict(bag.d, newdata = data2)
  vhat1=d2-yhat.d
  #step 3
  bag.aq=randomForest(formula.3,data=data2,mtry=6,importance=TRUE)
  summary(bag.aq)
  yhat.aq = predict(bag.aq, newdata = data1)
  ylhat2=y1-yhat.aq
  #step 4
  bag.d=randomForest(formula.4,data=data2,mtry=6,importance=TRUE)
  summary(bag.d)
  yhat.d = predict(bag.d, newdata = data1)
  vhat2=d1-yhat.d
  #step5: reg ylhat vhat
  lm.fit1=lm(ylhat1~vhat1)
  summary(lm.fit1)

  lm.fit2=lm(ylhat2~vhat2)
  summary(lm.fit2)

  # DML2: combine and reg
  dim1=length(ylhat1)
  dim2=length(ylhat2)
  dim=dim1+dim2

  dim3=dim1+1
  yhat=rep(NA,dim)
  yhat[1:dim1]=ylhat1
  yhat[dim3:dim]=ylhat2

  vhat=rep(NA,dim)
  vhat[1:dim1]=vhat1
  vhat[dim3:dim]=vhat2

  lm.all=lm(yhat~vhat)
  # compute robust standard error

  #install.packages("lmtest")
  #install.packages("sandwich")
  #library(lmtest)
  #library(sandwich)
  est1 = coeftest(lm.fit1 )
  est2 = coeftest(lm.fit2 )

  b1 = est1[2,1]
  b2 = est2[2,1]
  be = (b1+b2)/2

  se1 = est1[2,2]
  se2 = est2[2,2]
  sig2 =(se1^2+se2^2 +(b1-be)^2+(b2-be)^2)/2
  se =sqrt(sig2)
  t =be/se



  # combined reg
  estall = coeftest(lm.all )
  beAll=estall[2,1]
  seAll=estall[2,2]
  tAll=beAll/seAll


  # ouput the estimation results
  message("-----------------------------------------------------------","\n")
  message("Double machine learning 1 (bagging, 2-folds):","\n")
  message("Estimate, s.e., t-statistic, p.value, 95%lower, 95%upper","\n")
  message(cbind(theta=be,se.robust=se,t.value=t,pvalue=round(2*(1-pnorm(abs(be/se))),5),
              be-1.96*se,be+1.96*se),digits=4)

  message("t-statistic critial values: 90%=1.65, 95%=1.96, 99%=2.58","\n")
  message("-----------------------------------------------------------","\n")




  message("-----------------------------------------------------------","\n")
  message("Double machine learning 2 (bagging, 2-folds):","\n")
  message("Estimate, s.e., t-statistic, p.value, 95%lower, 95%upper","\n")
  message(cbind(theta=beAll,se.robust=seAll,t.value=tAll,pvalue=round(2*(1-pnorm(abs(beAll/seAll))),5),
              beAll-1.96*seAll,beAll+1.96*seAll),digits=4)

  message("t-statistic critial values: 90%=1.65, 95%=1.96, 99%=2.58","\n")
  message("-----------------------------------------------------------","\n")

}

dml_ensemble_lm <- function(y,x,d,data,sed=123) {
  #split the sample into two parts
  set.seed(sed)
  #library(caret)
  trainindex=createDataPartition(d,p=0.5,list=F)
  data1=data[trainindex,]
  data2=data[-trainindex,]
  y1=y[trainindex]
  y2=y[-trainindex]
  d1=d[trainindex]
  d2=d[-trainindex]

  formula.1 <- as.formula(paste("y1~",x))
  formula.2 <- as.formula(paste("d1~",x))

  formula.3 <- as.formula(paste("y2~",x))
  formula.4 <- as.formula(paste("d2~",x))
  #########################################################
  # method 5: ensemble learning
  set.seed(sed)
  #########################
  #step 1: E(Y|X) leanrning in sample 1; residuals in sample 2
  #library(randomForest)

  #bagging
  bag.aq=randomForest( formula.1,data=data1,mtry=6,importance=TRUE)
  yhat.aq = predict(bag.aq, newdata = data1)

  #randomForest
  rf.aq=randomForest( formula.1,data=data1,ntree=25,importance=TRUE)
  yhat.aq2 = predict(rf.aq, newdata = data1)

  # boosting
  #library(gbm)

  boost.aq=gbm( formula.1,data=data1,distribution="gaussian",
                n.trees=100,interaction.depth=4,shrinkage=0.1)
  yhat.aq3=predict(boost.aq,newdata=data1,n.trees=100)

  #nuetral networks
  #library(nnet)
  #net.aq <- neuralnet( formula.1, data = data1,  hidden = 1, linear.output = FALSE)
  net.aq <- nnet(formula.1,data = data1,maxit=300,size=1,linout=T)
  yhat.aq4=predict(net.aq,newdata=data1)
  #all train
  all.train=data.frame(yhat.aq,yhat.aq2,yhat.aq3,yhat.aq4)
  all.train=data.frame(cbind(all.train, y1))

  ensemble.model=lm( y1~.,data=all.train)

  #in test
  yhat.aq = predict(bag.aq, newdata = data2)
  yhat.aq2 = predict(rf.aq, newdata = data2)
  yhat.aq3=predict(boost.aq,newdata=data2,n.trees=100)
  yhat.aq4=predict(net.aq,newdata=data2)
  all.test=data.frame(yhat.aq,yhat.aq2,yhat.aq3,yhat.aq4)
  all.test=data.frame(cbind(all.test, y2))

  fit1 = predict(ensemble.model, all.test)
  ylhat51=y2-fit1

  ##########################
  #step 2: E(D|X) leanrning in sample 1; residuals in sample 2

  #bagging
  bag.d=randomForest(formula.2,data=data1,mtry=6,importance=TRUE)
  yhat.d = predict(bag.d, newdata = data1)

  #randomForest
  rf.d=randomForest(formula.2,data=data1,ntree=25,importance=TRUE)
  yhat.d2 = predict(rf.d, newdata = data1)

  # boosting
  boost.d=gbm(formula.2,data=data1,distribution="gaussian",
              n.trees=100,interaction.depth=4,shrinkage=0.1)
  yhat.d3=predict(boost.d,newdata=data1,n.trees=100)

  #nuetral networks
  #net.d <- neuralnet(formula.2, data = data1, hidden = 1, linear.output = FALSE)
  net.d <- nnet(formula.2,data = data1,maxit=300,size=1,linout=T)
  yhat.d4=predict(net.d,newdata=data1)
  #all train
  all.train=data.frame(yhat.d,yhat.d2,yhat.d3,yhat.d4)
  all.train=data.frame(cbind(all.train,d1))


  ensemble.model=lm(d1~.,data=all.train)
  #in test
  yhat.d = predict(bag.d, newdata = data2)
  yhat.d2 = predict(rf.d, newdata = data2)
  yhat.d3=predict(boost.d,newdata=data2,n.trees=100)
  yhat.d4=predict(net.d,newdata=data2)
  all.test=data.frame(yhat.d,yhat.d2,yhat.d3,yhat.d4)
  all.test=data.frame(cbind(all.test,d2))

  fit2 = predict(ensemble.model, all.test)
  vhat51=d2-fit2

  ###########################
  #step 3: E(Y|X) leanrning in sample 2; residuals in sample 1

  #bagging
  bag.aq=randomForest( formula.3,data=data2,mtry=6,importance=TRUE)
  yhat.aq = predict(bag.aq, newdata = data2)

  #randomForest
  rf.aq=randomForest( formula.3,data=data2,ntree=25,importance=TRUE)
  yhat.aq2 = predict(rf.aq, newdata = data2)

  # boosting
  boost.aq=gbm( formula.3,data=data2,distribution="gaussian",
                n.trees=100,interaction.depth=4,shrinkage=0.1)
  yhat.aq3=predict(boost.aq,newdata=data2,n.trees=100)

  #nuetral networks
  #net.aq <- neuralnet( formula.3, data = data2, hidden = 1, linear.output = FALSE)
  net.aq <- nnet(formula.3,data = data2,maxit=300,size=1,linout=T)
  yhat.aq4=predict(net.aq,newdata=data2)
  #all train
  all.train=data.frame(yhat.aq,yhat.aq2,yhat.aq3,yhat.aq4)
  all.train=data.frame(cbind(all.train,y2))
  #model
  ensemble.model=lm( y2~.,data=all.train)

  #in test
  yhat.aq = predict(bag.aq, newdata = data1)
  yhat.aq2 = predict(rf.aq, newdata = data1)
  yhat.aq3=predict(boost.aq,newdata=data1,n.trees=100)
  yhat.aq4=predict(net.aq,newdata=data1)
  all.test=data.frame(yhat.aq,yhat.aq2,yhat.aq3,yhat.aq4)
  all.test=data.frame(cbind(all.test, y1))

  fit3 = predict(ensemble.model, all.test)
  ylhat52=y1-fit3
  ##############################
  #step 4: E(D|X) leanrning in sample 2; residuals in sample 1

  #bagging
  bag.d=randomForest(formula.4,data=data2,mtry=6,importance=TRUE)
  yhat.d = predict(bag.d, newdata = data2)

  #randomForest
  rf.d=randomForest(formula.4,data=data2,ntree=25,importance=TRUE)
  yhat.d2 = predict(rf.d, newdata = data2)

  # boosting
  boost.d=gbm(formula.4,data=data2,distribution="gaussian",
              n.trees=100,interaction.depth=4,shrinkage=0.1)
  yhat.d3=predict(boost.d,newdata=data2,n.trees=100)

  #nuetral networks
  #net.d <- neuralnet(formula.4, data = data2, hidden = 1, linear.output = FALSE)
  net.d <- nnet(formula.4,data = data2,maxit=300,size=1,linout=T)
  yhat.d4<-predict(net.d,newdata=data2)

  #all train
  all.train=data.frame(yhat.d,yhat.d2,yhat.d3,yhat.d4)
  all.train=data.frame(cbind(all.train,d2))

  ensemble.model=lm(d2~.,data=all.train)

  #in test
  yhat.d = predict(bag.d, newdata = data1)
  yhat.d2 = predict(rf.d, newdata = data1)
  yhat.d3=predict(boost.d,newdata=data1,n.trees=100)
  yhat.d4=predict(net.d,newdata=data1)
  all.test=data.frame(yhat.d,yhat.d2,yhat.d3,yhat.d4)
  all.test=data.frame(cbind(all.test,d1))
  fit4 = predict(ensemble.model, all.test)
  vhat52=d1-fit4

  #step5: reg ylhat vhat
  lm.fit1=lm(ylhat51~vhat51)
  summary(lm.fit1)

  lm.fit2=lm(ylhat52~vhat52)
  summary(lm.fit2)

  # DML2: combine and reg
  dim1=length(ylhat51)
  dim2=length(ylhat52)
  dim=dim1+dim2

  dim3=dim1+1
  yhat=rep(NA,dim)
  yhat[1:dim1]=ylhat51
  yhat[dim3:dim]=ylhat52

  vhat=rep(NA,dim)
  vhat[1:dim1]=vhat51
  vhat[dim3:dim]=vhat52

  lm.all=lm(yhat~vhat)
  # compute robust standard error

  #install.packages("lmtest")
  #install.packages("sandwich")
  ##library(lmtest)
  ##library(sandwich)
  est1 = coeftest(lm.fit1 )
  est2 = coeftest(lm.fit2 )

  b1 = est1[2,1]
  b2 = est2[2,1]
  be = (b1+b2)/2

  se1 = est1[2,2]
  se2 = est2[2,2]
  sig2 =(se1^2+se2^2 +(b1-be)^2+(b2-be)^2)/2
  se =sqrt(sig2)
  t =be/se



  # combined reg
  estall = coeftest(lm.all )
  beAll=estall[2,1]
  seAll=estall[2,2]
  tAll=beAll/seAll


  # ouput the estimation results
  message("-----------------------------------------------------------","\n")
  message("Double machine learning 1 (ensemble learning , 2-folds):","\n")
  message("Estimate, s.e., t-statistic, p.value, 95%lower, 95%upper","\n")
  message(cbind(theta=be,se.robust=se,t.value=t,pvalue=round(2*(1-pnorm(abs(be/se))),5),
              be-1.96*se,be+1.96*se),digits=4)

  message("t-statistic critial values: 90%=1.65, 95%=1.96, 99%=2.58","\n")
  message("-----------------------------------------------------------","\n")




  message("-----------------------------------------------------------","\n")
  message("Double machine learning 2 (ensemble learning, 2-folds):","\n")
  message("Estimate, s.e., t-statistic, p.value, 95%lower, 95%upper","\n")
  message(cbind(theta=beAll,se.robust=seAll,t.value=tAll,pvalue=round(2*(1-pnorm(abs(beAll/seAll))),5),
              beAll-1.96*seAll,beAll+1.96*seAll),digits=4)

  message("t-statistic critial values: 90%=1.65, 95%=1.96, 99%=2.58","\n")
  message("-----------------------------------------------------------","\n")

}

dml_ensemble_rf<- function(y,x,d,data,sed=123) {
  #split the sample into two parts
  set.seed(sed)
  #library(caret)
  trainindex=createDataPartition(d,p=0.5,list=F)
  data1=data[trainindex,]
  data2=data[-trainindex,]
  y1=y[trainindex]
  y2=y[-trainindex]
  d1=d[trainindex]
  d2=d[-trainindex]

  formula.1 <- as.formula(paste("y1~",x))
  formula.2 <- as.formula(paste("d1~",x))

  formula.3 <- as.formula(paste("y2~",x))
  formula.4 <- as.formula(paste("d2~",x))
  #########################################################
  # method 5: ensemble learning
  set.seed(sed)
  #########################
  #step 1: E(Y|X) leanrning in sample 1; residuals in sample 2
  #library(randomForest)

  #bagging
  bag.aq=randomForest( formula.1,data=data1,mtry=6,importance=TRUE)
  yhat.aq = predict(bag.aq, newdata = data1)

  #randomForest
  rf.aq=randomForest( formula.1,data=data1,ntree=25,importance=TRUE)
  yhat.aq2 = predict(rf.aq, newdata = data1)

  # boosting
  #library(gbm)

  boost.aq=gbm( formula.1,data=data1,distribution="gaussian",
                n.trees=100,interaction.depth=4,shrinkage=0.1)
  yhat.aq3=predict(boost.aq,newdata=data1,n.trees=100)

  #nuetral networks
  #library(nnet)
  #net.aq <- neuralnet( formula.1, data = data1,  hidden = 1, linear.output = FALSE)
  net.aq <- nnet(formula.1,data = data1,maxit=300,size=1,linout=T)
  yhat.aq4=predict(net.aq,newdata=data1)
  #all train
  all.train=data.frame(yhat.aq,yhat.aq2,yhat.aq3,yhat.aq4)
  all.train=data.frame(cbind(all.train, y1))

  ensemble.model=randomForest(y1~.,data=all.train,ntree=25,importance=TRUE)

  #in test
  yhat.aq = predict(bag.aq, newdata = data2)
  yhat.aq2 = predict(rf.aq, newdata = data2)
  yhat.aq3=predict(boost.aq,newdata=data2,n.trees=100)
  yhat.aq4=predict(net.aq,newdata=data2)
  all.test=data.frame(yhat.aq,yhat.aq2,yhat.aq3,yhat.aq4)
  all.test=data.frame(cbind(all.test, y2))

  fit1 = predict(ensemble.model, all.test)
  ylhat51=y2-fit1

  ##########################
  #step 2: E(D|X) leanrning in sample 1; residuals in sample 2

  #bagging
  bag.d=randomForest(formula.2,data=data1,mtry=6,importance=TRUE)
  yhat.d = predict(bag.d, newdata = data1)

  #randomForest
  rf.d=randomForest(formula.2,data=data1,ntree=25,importance=TRUE)
  yhat.d2 = predict(rf.d, newdata = data1)

  # boosting
  boost.d=gbm(formula.2,data=data1,distribution="gaussian",
              n.trees=100,interaction.depth=4,shrinkage=0.1)
  yhat.d3=predict(boost.d,newdata=data1,n.trees=100)

  #nuetral networks
  #net.d <- neuralnet(formula.2, data = data1, hidden = 1, linear.output = FALSE)
  net.d <- nnet(formula.2,data = data1,maxit=300,size=1,linout=T)
  yhat.d4=predict(net.d,newdata=data1)
  #all train
  all.train=data.frame(yhat.d,yhat.d2,yhat.d3,yhat.d4)
  all.train=data.frame(cbind(all.train,d1))


  ensemble.model=randomForest(d1~.,data=all.train,ntree=25,importance=TRUE)
  #in test
  yhat.d = predict(bag.d, newdata = data2)
  yhat.d2 = predict(rf.d, newdata = data2)
  yhat.d3=predict(boost.d,newdata=data2,n.trees=100)
  yhat.d4=predict(net.d,newdata=data2)
  all.test=data.frame(yhat.d,yhat.d2,yhat.d3,yhat.d4)
  all.test=data.frame(cbind(all.test,d2))

  fit2 = predict(ensemble.model, all.test)
  vhat51=d2-fit2

  ###########################
  #step 3: E(Y|X) leanrning in sample 2; residuals in sample 1

  #bagging
  bag.aq=randomForest( formula.3,data=data2,mtry=6,importance=TRUE)
  yhat.aq = predict(bag.aq, newdata = data2)

  #randomForest
  rf.aq=randomForest( formula.3,data=data2,ntree=25,importance=TRUE)
  yhat.aq2 = predict(rf.aq, newdata = data2)

  # boosting
  boost.aq=gbm( formula.3,data=data2,distribution="gaussian",
                n.trees=100,interaction.depth=4,shrinkage=0.1)
  yhat.aq3=predict(boost.aq,newdata=data2,n.trees=100)

  #nuetral networks
  #net.aq <- neuralnet( formula.3, data = data2, hidden = 1, linear.output = FALSE)
  net.aq <- nnet(formula.3,data = data2,maxit=300,size=1,linout=T)
  yhat.aq4=predict(net.aq,newdata=data2)
  #all train
  all.train=data.frame(yhat.aq,yhat.aq2,yhat.aq3,yhat.aq4)
  all.train=data.frame(cbind(all.train,y2))
  #model
  ensemble.model=randomForest(y2~.,data=all.train,ntree=25,importance=TRUE)

  #in test
  yhat.aq = predict(bag.aq, newdata = data1)
  yhat.aq2 = predict(rf.aq, newdata = data1)
  yhat.aq3=predict(boost.aq,newdata=data1,n.trees=100)
  yhat.aq4=predict(net.aq,newdata=data1)
  all.test=data.frame(yhat.aq,yhat.aq2,yhat.aq3,yhat.aq4)
  all.test=data.frame(cbind(all.test, y1))

  fit3 = predict(ensemble.model, all.test)
  ylhat52=y1-fit3
  ##############################
  #step 4: E(D|X) leanrning in sample 2; residuals in sample 1

  #bagging
  bag.d=randomForest(formula.4,data=data2,mtry=6,importance=TRUE)
  yhat.d = predict(bag.d, newdata = data2)

  #randomForest
  rf.d=randomForest(formula.4,data=data2,ntree=25,importance=TRUE)
  yhat.d2 = predict(rf.d, newdata = data2)

  # boosting
  boost.d=gbm(formula.4,data=data2,distribution="gaussian",
              n.trees=100,interaction.depth=4,shrinkage=0.1)
  yhat.d3=predict(boost.d,newdata=data2,n.trees=100)

  #nuetral networks
  #net.d <- neuralnet(formula.4, data = data2, hidden = 1, linear.output = FALSE)
  net.d <- nnet(formula.4,data = data2,maxit=300,size=1,linout=T)
  yhat.d4<-predict(net.d,newdata=data2)

  #all train
  all.train=data.frame(yhat.d,yhat.d2,yhat.d3,yhat.d4)
  all.train=data.frame(cbind(all.train,d2))

  ensemble.model=randomForest(d2~.,data=all.train,ntree=25,importance=TRUE)

  #in test
  yhat.d = predict(bag.d, newdata = data1)
  yhat.d2 = predict(rf.d, newdata = data1)
  yhat.d3=predict(boost.d,newdata=data1,n.trees=100)
  yhat.d4=predict(net.d,newdata=data1)
  all.test=data.frame(yhat.d,yhat.d2,yhat.d3,yhat.d4)
  all.test=data.frame(cbind(all.test,d1))
  fit4 = predict(ensemble.model, all.test)
  vhat52=d1-fit4

  #step5: reg ylhat vhat
  lm.fit1=lm(ylhat51~vhat51)
  summary(lm.fit1)

  lm.fit2=lm(ylhat52~vhat52)
  summary(lm.fit2)

  # DML2: combine and reg
  dim1=length(ylhat51)
  dim2=length(ylhat52)
  dim=dim1+dim2

  dim3=dim1+1
  yhat=rep(NA,dim)
  yhat[1:dim1]=ylhat51
  yhat[dim3:dim]=ylhat52

  vhat=rep(NA,dim)
  vhat[1:dim1]=vhat51
  vhat[dim3:dim]=vhat52

  lm.all=lm(yhat~vhat)
  # compute robust standard error

  #install.packages("lmtest")
  #install.packages("sandwich")
  #library(lmtest)
  #library(sandwich)
  est1 = coeftest(lm.fit1 )
  est2 = coeftest(lm.fit2 )

  b1 = est1[2,1]
  b2 = est2[2,1]
  be = (b1+b2)/2

  se1 = est1[2,2]
  se2 = est2[2,2]
  sig2 =(se1^2+se2^2 +(b1-be)^2+(b2-be)^2)/2
  se =sqrt(sig2)
  t =be/se



  # combined reg
  estall = coeftest(lm.all )
  beAll=estall[2,1]
  seAll=estall[2,2]
  tAll=beAll/seAll


  # ouput the estimation results
  message("-----------------------------------------------------------","\n")
  message("Double machine learning 1 (ensemble learning , 2-folds):","\n")
  message("Estimate, s.e., t-statistic, p.value, 95%lower, 95%upper","\n")
  message(cbind(theta=be,se.robust=se,t.value=t,pvalue=round(2*(1-pnorm(abs(be/se))),5),
              be-1.96*se,be+1.96*se),digits=4)

  message("t-statistic critial values: 90%=1.65, 95%=1.96, 99%=2.58","\n")
  message("-----------------------------------------------------------","\n")




  message("-----------------------------------------------------------","\n")
  message("Double machine learning 2 (ensemble learning, 2-folds):","\n")
  message("Estimate, s.e., t-statistic, p.value, 95%lower, 95%upper","\n")
  message(cbind(theta=beAll,se.robust=seAll,t.value=tAll,pvalue=round(2*(1-pnorm(abs(beAll/seAll))),5),
              beAll-1.96*seAll,beAll+1.96*seAll),digits=4)

  message("t-statistic critial values: 90%=1.65, 95%=1.96, 99%=2.58","\n")
  message("-----------------------------------------------------------","\n")

}





