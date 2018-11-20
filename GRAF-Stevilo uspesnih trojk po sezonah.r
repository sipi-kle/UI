games <- read.table("regular.txt", sep=",", header=TRUE)
ekipe <- levels(games[1,]$HOME)
sezona <- games[1,]$SEASON
UspTroj <-c()
UspTrojIndex <- 1
UspTroj[UspTrojIndex] <- 0
for(i in 1:nrow(games))
	{
		#Zamenjava sezon
		if(sezona != games[i,]$SEASON)
		{
			sezona <- games[i,]$SEASON
			UspTrojIndex=UspTrojIndex+1
			UspTroj[UspTrojIndex] <- 0

		}

		UspTroj[UspTrojIndex] <- games[i,]$H3PM + games[i,]$A3PM + UspTroj[UspTrojIndex] 
	}	

barplot(UspTroj, names=levels(sezona),main="Število uspešnih metov za 3 po sezonah")
   