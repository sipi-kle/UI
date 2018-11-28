games <- read.table("regular.txt", sep=",", header=TRUE)

stTrojk=0
stDvojk=0
for(i in 1:nrow(games))
	{
        stTrojk=games[i,]$H3PA+games[i,]$A3PA+stTrojk
        stDvojk=games[i,]$H2PA+games[i,]$A2PA+stDvojk
    }

tocke=c(stDvojk,stTrojk)
labels=c("Dvojke","Trojke")
procenti=round(tocke/sum(tocke)*100)
labels=paste(labels,procenti)
labels=paste(labels,"%",sep="")

#install.packages("plotrix")
library(plotrix)
pie3D(tocke, labels=labels,main="Razmerje med trojkami in dvojkami",explode=0.1)

