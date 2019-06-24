rm(list=ls())

x= Data
G = x[,9] # subset with categories
N = dim(x)[1]
xs = x[,3:8] # subset with features

#############
### functions 
#############

# bootstrap
indicesTV = function(N, fracIS = 0.7){
  nIS = round(N*fracIS)
  iShuffle = sample(N)
  i= list(train = iShuffle[1:nIS], valid = iShuffle[(nIS+1):N])
  
  return(i) 
}

# evaluation
evalTV = function(Ghat, G, i){ # G = categories
  cmT = table(Ghat[i$train], G[i$train])
  cmV = table(Ghat[i$valid], G[i$valid])
  accT = sum(diag(cmT))/ sum(cmT)
  accV = sum(diag(cmV))/ sum(cmV)
  return(c(accT, accV))
}

############################
## artificial neural networks 
############################

library(nnet) 
 

accExp = matrix(NA, 100, 2)
for ( e in 1:dim(accExp)[1]){
  
  i= indicesTV(N)
  nn_fit = nnet(G~ ., data = x[i$train, ], size=c(2))
  nn_pred = predict(nn_fit, x, type = "class")
  accExp[e,] = evalTV(nn_pred, G, i) 
  
}
boxplot(accExp) 

###########################
######## SVM ##############
###########################

library(e1071)

svm_fit= svm(G~., data = x[i$train, ])
svm_fit
svm_pred = predict(svm_fit, newdata = x)
table(svm_pred,G)
evalTV(svm_pred, G, i)

############################
######## classification trees
############################


library(rpart) 
library(rpart.plot)

i = indicesTV(N)
rpart_fit = rpart(G~., data = x, minsplit = 10) 
prp(rpart_fit, extra = 104) 

plot(jitter(rpart_pred))

rpart_pred = predict(rpart_fit, xDF) 
Ghat = levels(G)[apply(rpart_pred, 1, which.max)] #estimated group membership
evalTV(Ghat, G, i)


############################
######## random forest 
############################

library(randomForest) 

rf_fit = randomForest(xs, G, mtry = 4) 
rf_fit 

rf_fit$predicted
table(rf_fit$predicted, G)

