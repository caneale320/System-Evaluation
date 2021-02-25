################
##Reading Data##
################
library(readxl)

teamData = read_excel("TeamData.xlsx")
playerData = read_excel("PlayerData.xlsx")

##see what variables are in our data
str(teamData)
str(playerData)


#######
##EDA##
#######

pairs(teamData[,3:17], lower.panel = NULL, main="Scatterplot of Quantitative Variables")


cor(teamData[,3:17], method ="spearman")[,14]

roundedCor = round(cor(teamData[,3:17]),3)
roundedCor[14,]

##############
##Regression##
##############
modelData = teamData[,3:16]
result<-lm(data=modelData, Wins~.)

###############
##Diagnostics##
###############

par(mfrow=c(2,1))
plot(result)

library(MASS)
boxcox(result)

#############
##Inference##
#############

summary(result)

##consider removing insignificant predictors
reduced<-lm(data=modelData, Wins~`At Bats`+`Runs Scored`+`Runs Batted In`)

summary(reduced)
##partial F test
anova(reduced,result)

confint(reduced,level = 0.95)


# predict(reduced,newdata,level=0.95, interval="prediction")

names(reduced)
reduced$coefficients

#########
##LASSO##
#########

library(glmnet)
library(caret)

##check need for smaller threshold with multicollinearity. the coeffs should be the same as lm
colnames(modelData)= c('gamesPlayed', 'atBats', 'runsScored', 'hits', 'doubles', 'triples', 'homeRuns', 'totalBases', 'runBattedIn', 'battingAverage', 'onbasePercentage', 'sluggingPercentage', 'onBaseAndSlugging', 'Wins')

x = model.matrix(Wins~., modelData)[,-1]
y = as.matrix(modelData[,14])

lasso.r<-glmnet(x,y,alpha=1, lambda=0,thresh = 1e-14, nfolds=30)
coefficients(lasso.r)


##use CV to find optimal lambda based on training set
cv.out<-cv.glmnet(x,y,alpha=1)
plot(cv.out)
bestlam<-cv.out$lambda.min
bestlam

##fit lasso using training data and bestlam
lasso.mod<-glmnet(x,y,alpha=1,lambda=bestlam,thresh = 1e-14)

coefficients(lasso.mod)

lasso.pred<-predict(lasso.mod,s=bestlam,newx=x)
mean(abs(lasso.pred-y))


##############################
##Bagging and Random Forests##
##############################
library(randomForest) 


##bagging is special case of random forest when mtry = number of predictors
bag.reg<-randomForest(Wins~., data=modelData, mtry=13, importance=TRUE)
bag.reg ##note with classification tree OOB estimates are provided

##importance measures of predictors
importance(bag.reg)
##graphical version
varImpPlot(bag.reg)


library(gbm)
boost<-gbm(Wins~., data=modelData, distribution="gaussian", n.trees=500, n.minobsinnode=5)

summary(boost)


#####################
##Comparing Results##
#####################
train_control = trainControl(method = "LOOCV")
lasso.cv = train(Wins~., data=modelData, trControl=train_control, method="glmnet", lambda=bestlam)

lasso.cv

lasso.pred<-predict(lasso.mod,s=bestlam,newx=x)
mean((lasso.pred-y)^2)

result.pred <- predict(result, newx= x)
mean((result.pred-y)^2)

reduced.pred <- predict(reduced, newx= x)
mean((reduced.pred-y)^2)

bag.reg.pred <- predict(bag.reg, newx= x)
mean((bag.reg.pred-y)^2)


boost.err <- numeric(0)

for (k in 1:nrow(modelData)){
  boost<-gbm(Wins~., data=modelData[-k,], distribution="gaussian", n.trees=500, n.minobsinnode=5)
  boost.pred <- predict(boost, newx=teamData[k,])
  MSE <- mean((boost.pred-y)^2)
  boost.err <- rbind(boost.err, MSE)
}

hist(boost.err)
mean(boost.err)

lasso.err <- numeric(0)

for (k in 1:nrow(modelData)){
  xtest = as.matrix(modelData[k,-14])

  lasso.mod<-glmnet(x[-k,],y[-k,],alpha=1,lambda=bestlam,thresh = 1e-14)
  lasso.pred<-predict(lasso.mod, newx=xtest)
  MSE <- abs(lasso.pred-y[k])
  lasso.err <- rbind(lasso.err, MSE)
}
mean(lasso.err)
hist(lasso.err)

boost.pred <- predict(boost, newx=x)
mean((boost.pred-y)^2)

summary(boost)

coefficients(lasso.mod)

summary(result)

summary(reduced)



#####################
##Creating Rankings##
#####################

colnames(playerData)= c('playerCost', 'playerNum', 'atBats', 'runsScored', 'hits', 'doubles', 'triples', 'homeRuns', 'runBattedIn', 'stolenBases', 'caughtStealing', 'baseOnBallswalks', 'battingAverage', 'onbasePercentage', 'sluggingPercentage')

playerData %>% mutate(lasso_ranking = )

lassoCoefs = coefficients(lasso.mod)

test[7]




