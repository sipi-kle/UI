#Setup 

###########################################
########### finalTestiranje modelov ############
###########################################
mae <- function(observed, predicted)
{
	mean(abs(observed - predicted))
}

rmae <- function(observed, predicted, mean.val) 
{  
	sum(abs(observed - predicted)) / sum(abs(observed - mean.val))
}

mse <- function(observed, predicted)
{
	mean((observed - predicted)^2)
}

rmse <- function(observed, predicted, mean.val)
{  
	sum((observed - predicted)^2)/sum((observed - mean.val)^2)
}

#Pravilne vrednosti
observed <- finalTest$DIFF


#Knjižnice
library(CORElearn)
library(rpart)
library(kknn)
library(nnet)


#################### --ZAČETEK REGRESIJ-- #########################

#Regresijsko drevo
rt.model <- rpart(DIFF ~ ., learn)
predicted <- predict(rt.model, finalTest)


mae(observed, predicted)                            #6.473853       #Test final: 6.165406
rmae(observed,predicted,mean(learn$DIFF))           #0.5761694      #Test final: 0.5736165
mse(observed,predicted)                             #71.08853       #Test final: 62.75234
rmse(observed,predicted,mean(learn$DIFF))           #0.3617341      #Test final: 0.3543155


#Naključni gozd
rf.model <- randomForest(DIFF ~ ., learn)
predicted <- predict(rf.model, finalTest)

mae(observed, predicted)                            #6.501852       #Test final: 6.461351
rmae(observed,predicted,mean(learn$DIFF))           #0.5786613      #Test final: 0.6011506
mse(observed,predicted)                             #71.3672        #Test final: 66.66973
rmse(observed,predicted,mean(learn$DIFF))           #0.3631521      #Test final: 0.3764341

#N-naključnih sosedov
knn.model <- kknn(DIFF ~ ., learn, finalTest, k =60)
predicted <- fitted(knn.model)


mae(observed, predicted)                            #6.923056       #Test final: 6.821811 
rmae(observed,predicted,mean(learn$DIFF))           #0.6161482      #Test final: 0.634687
mse(observed,predicted)                             #90.02221       #Test final: 84.51727 
rmse(observed,predicted,mean(learn$DIFF))           #0.4580782      #Test final: 0.4772058

#Nevronska mreža
nn.model <- nnet(DIFF ~ ., learn, size = 5, decay = 0.0001, maxit = 10000, linout = T)
predicted <- predict(nn.model, finalTest)

mae(observed, predicted)                            #6.470851       #Test final: 6.817467
rmae(observed,predicted,mean(learn$DIFF))           #0.5759022      #Test final: 0.6342828
mse(observed,predicted)                             #71.13953       #Test final: 82.75671
rmse(observed,predicted,mean(learn$DIFF))           #0.3619937      #Test final: 0.4672652
