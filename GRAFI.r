############################################################################################
#Naj ekipa razmerje zmag porazi                                                            #
############################################################################################
games <- read.table("regular.txt", sep=",", header=TRUE)
ekipe <- levels(games[1,]$HOME)
razmerje <- c()
for(stevec in 1:length(ekipe))
{
    steviloZmag <- 0
    steviloPorazov <- 0
    for (i in 1:nrow(games))
    {
        if (ekipe[games[i,]$HOME] == ekipe[stevec])
        {
             if (games[i,]$HPTS > games[i,]$APTS)
             {
                  steviloZmag <- steviloZmag + 1
             }
             else if (games[i,]$HPTS < games[i,]$APTS)
             {       
                  steviloPorazov <- steviloPorazov + 1
             }
         }else if (ekipe[games[i,]$AWAY] == ekipe[stevec])
         {
             if (games[i,]$APTS > games[i,]$HPTS)
             {
                  steviloZmag <- steviloZmag + 1
             } else if (games[i,]$APTS < games[i,]$HPTS)
             {
                  steviloPorazov <- steviloPorazov + 1
             }
         }
    }
    razmerje <- c(razmerje, (steviloZmag/steviloPorazov))  
}
barplot(razmerje, names=ekipe, main="Najuspešnejše ekipe glede na razmerje zmag in porazov")
abline(h=max(razmerje))
abline(h=min(razmerje))
############################################################################################
#Razmerje med vrženimi dvojkami in trojkami                                                #
############################################################################################
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

install.packages("plotrix")
library(plotrix)
pie3D(tocke, labels=labels,main="Razmerje med vrženimi trojkami in dvojkami",explode=0.1)
############################################################################################
#Št zadetih trojk po sezonah                                                               #
############################################################################################
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
############################################################################################
#Št točk po sezonah                                                                        #
############################################################################################
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
#install.packages("plotrix")
library(plotrix)
pie3D(tockPoSez, labels=levels(sezona),main="Število doseženih točk po sezonah",explode=0.1)   