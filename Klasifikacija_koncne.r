#Setup
# Funkcija za izracun klasifikacijske tocnosti
CA <- function(observed, predicted)
{
	t <- table(observed, predicted)

	sum(diag(t)) / sum(t)
}

# Funkcija za izracun Brierjeve mere
brier.score <- function(observedMatrix, predictedMatrix)
{
	sum((observedMatrix - predictedMatrix) ^ 2) / nrow(predictedMatrix)
}

#Pravilni rezultati
observed <- finalTest$W
obsMat <- model.matrix(~W-1, finalTest)


#Knjižnice
library(rpart)
library(nnet)
library(CORElearn)
library(randomForest)
#############################---ZAČETEK KLASIFIKACIJ---########################################

#Odločitveno drevo
dt <- rpart(W ~ ., data = learn)
predicted <- predict(dt, finalTest, type = "class")
predMat <- predict(dt, finalTest, type = "prob")

CA(observed,predicted)                          #0.7990012      #Final test: 0.7615481
brier.score(obsMat, predMat)                    #0.2396166      #Final test: 0.2872681


#Random forest
rf <- randomForest(W~., data=learn, ntree=181, proximity=T)
predicted <- predict(rf, finalTest, type="class")
predMat <- predict(rf, finalTest, type = "prob")

CA(observed,predicted)                          #0.7915106      #Final test: 0.7652934
brier.score(obsMat, predMat)                    #0.2613955      #Final test: 0.2998844

#N-tih sosedov
cm.knn <- CoreModel(W ~ ., data = learn, model="knn", kInNN = 36)
predicted <- predict(cm.knn, finalTest, type="class")
predMat <- predict(cm.knn, finalTest, type = "prob")

CA(observed, predicted)                         #0.7315855      #Final test: 0.6916355
brier.score(obsMat, predMat)                    #0.3625946      #Final test: 0.3947284


#Nevronska mreža
norm.data <- scale.data(rbind(learn,finalTest))
norm.learn <- norm.data[1:nrow(learn),]
norm.finalTest <- norm.data[-(1:nrow(learn)),]

scale.data <- function(data)
{
	norm.data <- data

	for (i in 1:ncol(data))
	{
		if (!is.factor(data[,i]))
			norm.data[,i] <- scale(data[,i])
	}
	
	norm.data
}

nn <- nnet(W ~ ., data = norm.learn, size = 3, decay = 0.0002, maxit = 10000)
predicted <- predict(nn, norm.finalTest, type = "class")
pm <- predict(nn, norm.finalTest, type = "raw")
predMat <- cbind(1-pm, pm)

CA(observed, predicted)                         #0.7240949      #Final test: 0.7153558
brier.score(obsMat, predMat)                    #0.3838815      #Final test: 0.3530902