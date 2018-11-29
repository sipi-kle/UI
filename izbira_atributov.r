install.packages("CORElearn") 
library(CORElearn)

# Funkcija "attrEval" oceni kvaliteto atributov glede na ciljno spremenljivko
sort(attrEval(W ~ ., learn, "Relief"), decreasing = TRUE)
sort(attrEval(W ~ ., learn, "ReliefFequalK"), decreasing = TRUE)
sort(attrEval(W ~ ., learn, "ReliefFexpRank"), decreasing = TRUE)


# Odlocitveno drevo z uporabo informacijskega prispevka
dt <- CoreModel(W ~ ., learn, model="tree", selectionEstimator = "ReliefFexpRank")
plot(dt, learn)

library(rpart)
dt <- rpart(W ~ ., data = learn,selectionEstimator = "ReliefFexpRank")

observed <- test$W
predicted <- predict(dt, test, type = "class")
CA(observed,predicted)