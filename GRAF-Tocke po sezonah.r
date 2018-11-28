games <- read.table("regular.txt", sep=",", header=TRUE)
ekipe <- levels(games[1,]$HOME)
sezona <- games[1,]$SEASON
tockPoSez <-c()
tockPoSezIndex <- 1
tockPoSez[tockPoSezIndex] <- 0
for(i in 1:nrow(games))
	{
		#Zamenjava sezon
		if(sezona != games[i,]$SEASON)
		{
			sezona <- games[i,]$SEASON
			tockPoSezIndex=tockPoSezIndex+1
			tockPoSez[tockPoSezIndex] <- 0

		}

		tockPoSez[tockPoSezIndex] <- games[i,]$HPTS + games[i,]$APTS + tockPoSez[tockPoSezIndex] 
	}	
install.packages("plotrix")
library(plotrix)
pie3D(tockPoSez, labels=levels(sezona),main="Število doseženih točk po sezonah",explode=0.1)