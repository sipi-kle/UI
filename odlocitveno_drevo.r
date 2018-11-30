 install.packages(c("ipred", "prodlim", "CORElearn", "e1071", "randomForest", "kernlab", "nnet","knn"))
###############################################################################################
#vecinski klasifikator
majority.class <- names(which.max(table(learn$W)))
majority.class
sum(test$W == majority.class) / length(test$W)

odlocitveno drevo 1
########################################
library(rpart)
dt <- rpart(W ~ HPTS+H2PM+H3PM+HFTM+APTS+A2PM+A3PM+AFTM, data = learn)

plot(dt)
text(dt, pretty = 0)

observed <- test$W

predicted <- predict(dt, test, type = "class")
CA(observed,predicted)

##############################################
CA <- function(prave, napovedane)
{
	t <- table(prave, napovedane)

	sum(diag(t)) / sum(t)
}

brier.score <- function(observedMatrix, predictedMatrix)
{
	sum((observedMatrix - predictedMatrix) ^ 2) / nrow(predictedMatrix)
}
#################################################


################################################# nakljucni gozd
library(randomForest)

rf <- randomForest(W~., data=learn, ntree=100, proximity=T)
predicted <- predict(rf, test, type="class")
CA(observed, predicted)





obsMat <- model.matrix(~W-1, test)
predMat <- predict(rf, test, type = "prob")
brier.score(obsMat, predMat)

mypredict.rf <- function(object, newdata){predict(object, newdata, type = "class")}
errorest(W~., data=learn, model = randomForest, predict = mypredict.generic)
#######################################################
######################################################k naj sosedi
library(CORElearn)
cm.knn <- CoreModel(W ~ ., data = learn, model="knn", kInNN = 40)
predicted <- predict(cm.knn, test, type="class")
CA(observed, predicted)
#######################################################