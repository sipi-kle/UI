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