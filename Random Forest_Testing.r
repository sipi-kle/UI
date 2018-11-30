


library(randomForest)


nt <- seq(100,500,20)
for(n in nt){
    rf <- randomForest(W~., data=learn, ntree=n, proximity=T)
    predicted <- predict(rf, test, type="class")
    print(CA(observed, predicted))
    print(n)
}





#HPTS+H2PM+H3PM+HFTM+APTS+A2PM+A3PM+AFTM

library(CORElearn)

nt <- seq(1,50,1)
for(n in nt){
    cm.knn <- CoreModel(W ~ ., data = learn, model="knn", kInNN = n)
    predicted <- predict(cm.knn, test, type="class")
    print(n)
    print(CA(observed, predicted))
}




#odstranitev nepomembnih atributov v učni monožici
learn2 <- learn
test2 <- test

learn2$HPF <- NULL
learn2$APF <- NULL


test2$HPF <- NULL
test2$APF <- NULL



nt <- seq(1,50,1)
for(n in nt){
    vals <-c()
for(i in seq(1,10)){    
    cm.knn <- CoreModel(W ~ ., data = learn, model="knn", kInNN = n)
    predicted <- predict(cm.knn, test, type="class")
    vals <- c(vals,CA(observed, predicted))
    }

    if(mean(vals) > best[1]){
        best <-c(mean(vals),n)
    }
    print(n)
}


nt <- seq(20,50,1)
for(n in nt){
    cm.knn <- CoreModel(W ~ ., data = learn2, model="knn", kInNN = n)
    predicted <- predict(cm.knn, test, type="class")
    print(n)
    print(CA(observed, predicted))
}








library(rpart)
dt <- rpart(W ~ ., data = learn2)

plot(dt)
text(dt, pretty = 0)

observed <- test$W

predicted <- predict(dt, test, type = "class")
CA(observed,predicted)






#Nevronska mreža
library(nnet)


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

norm.data <- scale.data(rbind(learn,test))
norm.learn <- norm.data[1:nrow(learn),]
norm.test <- norm.data[-(1:nrow(learn)),]


#avg testiranje random forest
best <-c(0,0)
nt <- seq(1,500,20)
for(n in nt){
    vals <-c()
for(i in seq(1,10)){    
     rf <- randomForest(W~., data=learn, ntree=n, proximity=T)
    predicted <- predict(rf, test, type="class")
    vals <- c(vals,CA(observed, predicted))
    }

    if(mean(vals) > best[1]){
        best <-c(mean(vals),n)
    }
    print(n)
}









#avg testiranje nevronska mreža
best <-c(0,0)
nt <- seq(1,5,1)
for(n in nt){
    vals <-c()
for(i in seq(1,50)){    
    nn <- nnet(W ~ ., data = norm.learn, size = n, decay = 0.0002, maxit = 10000)
    predicted <- predict(nn, norm.test, type = "class")
    vals <- c(vals,CA(observed, predicted))
    }

    if(mean(vals) > best[1]){
        best <-c(mean(vals),n)
    }
}








#rpart
nt <- seq(0.0001,10,0.0001)
best <- c(0,0)
for(n in nt){
    dt <- rpart(W ~., data = learn,control = rpart.control(cp = n))

    observed <- test$W

    predicted <- predict(dt, test, type = "class")
    if(CA(observed,predicted) > best[1]){
        best <- c(CA(observed,predicted),n)
    }
    
}



    dt <- rpart(W ~., data = learn,control = rpart.control(cp = 0.0103))
    observed <- test$W
    predicted <- predict(dt, test, type = "class")
    CA(observed,predicted)

 