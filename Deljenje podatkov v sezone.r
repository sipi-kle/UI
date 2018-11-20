#Preberemo podatke
vsiPodatki <- read.table("regular.txt", sep=",", header=TRUE)

#Uredimo podatke po vsaki sezoni (za vsako sezono svoj dataframe)
s14 <- md[md$SEASON == "2014-15",]
s15 <- md[md$SEASON == "2015-16",]
s16 <- md[md$SEASON == "2016-17",]

#Podatke zapišemo v datoteke
write.csv(s14, file = "s14-15.txt")
write.csv(s15, file = "s15-16.txt")
write.csv(s16, file = "s16-17.txt")