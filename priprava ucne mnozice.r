podatki = read.table("regular.txt",sep = ",", header = TRUE)
ekipe = levels(podatki[1,]$HOME)
ppts = c()
p2pm = c()
p3pm = c()
pftm = c()
porb = c()
pdrb = c()
past = c()
pstl = c()
ptov = c()
pblk = c()
ppf = c()
num = c()
first = c()
for (ekipa in ekipe){
    for(i in 1:nrow(podatki)){
        if(ekipa == ekipe[podatki[i,]$HOME]){
            ppts = c(ppts,(podatki[i,]$HPTS))
            p2pm = c(p2pm,(podatki[i,]$H2PM/podatki[i,]$H2PA))
            p3pm = c(p3pm,(podatki[i,]$H3PM/podatki[i,]$H3PA))
            pftm = c(pftm,(podatki[i,]$HFTM/podatki[i,]$HFTA))
            porb = c(porb,(podatki[i,]$HORB))
            pdrb = c(pdrb,(podatki[i,]$HDRB))
            past = c(past,(podatki[i,]$HAST))
            pstl = c(pstl,(podatki[i,]$HSTL))
            ptov = c(ptov,(podatki[i,]$HTOV))
            pblk = c(pblk,(podatki[i,]$HBLK))
            ppf = c(ppf,(podatki[i,]$HPF))
            num = c(num,1)
            first = c(first, TRUE)
            break()
        }
        else if(ekipa == ekipe[podatki[i,]$AWAY]){
            ppts = c(ppts,(podatki[i,]$APTS))
            p2pm = c(p2pm,(podatki[i,]$A2PM/podatki[i,]$A2PA))
            p3pm = c(p3pm,(podatki[i,]$A3PM/podatki[i,]$A3PA))
            pftm = c(pftm,(podatki[i,]$AFTM/podatki[i,]$AFTA))
            porb = c(porb,(podatki[i,]$AORB))
            pdrb = c(pdrb,(podatki[i,]$ADRB))
            past = c(past,(podatki[i,]$AAST))
            pstl = c(pstl,(podatki[i,]$ASTL))
            ptov = c(ptov,(podatki[i,]$ATOV))
            pblk = c(pblk,(podatki[i,]$ABLK))
            ppf = c(ppf,(podatki[i,]$APF))
            num = c(num,1)
            first = c(first, TRUE)
            break()
        }
    }
}
temp.data = data.frame(ekipe,ppts,p2pm,p3pm,pftm,porb,pdrb,past,pstl,ptov,pblk,ppf,num,first)



pod = data.frame(matrix(ncol = 25,nrow = 1),stringsAsFactors = FALSE)
names(pod) = c("HOME","HPTS","H2PM","H3PM","HFTM","HORB","HDRB","HAST","HSTL","HTOV","HBLK","HPF","AWAY","APTS","A2PM","A3PM","AFTM","AORB","ADRB","AAST","ASTL","ATOV","ABLK","APF","W")
for(i in 1:nrow(podatki)){
    if(podatki[i,]$HPTS > podatki[i,]$APTS){
        WIN = "H"
    }
    else{
        WIN = "A"
    }
    
    pod = rbind(pod,list(
        podatki[i,]$HOME,
        temp.data[temp.data$ekipe == podatki[i,]$HOME,]$ppts/temp.data[temp.data$ekipe == podatki[i,]$HOME,]$num,
        temp.data[temp.data$ekipe == podatki[i,]$HOME,]$p2pm/temp.data[temp.data$ekipe == podatki[i,]$HOME,]$num,
        temp.data[temp.data$ekipe == podatki[i,]$HOME,]$p3pm/temp.data[temp.data$ekipe == podatki[i,]$HOME,]$num,
        temp.data[temp.data$ekipe == podatki[i,]$HOME,]$pftm/temp.data[temp.data$ekipe == podatki[i,]$HOME,]$num,
        temp.data[temp.data$ekipe == podatki[i,]$HOME,]$porb/temp.data[temp.data$ekipe == podatki[i,]$HOME,]$num,
        temp.data[temp.data$ekipe == podatki[i,]$HOME,]$pdrb/temp.data[temp.data$ekipe == podatki[i,]$HOME,]$num,
        temp.data[temp.data$ekipe == podatki[i,]$HOME,]$past/temp.data[temp.data$ekipe == podatki[i,]$HOME,]$num,
        temp.data[temp.data$ekipe == podatki[i,]$HOME,]$pstl/temp.data[temp.data$ekipe == podatki[i,]$HOME,]$num,
        temp.data[temp.data$ekipe == podatki[i,]$HOME,]$ptov/temp.data[temp.data$ekipe == podatki[i,]$HOME,]$num,
        temp.data[temp.data$ekipe == podatki[i,]$HOME,]$pblk/temp.data[temp.data$ekipe == podatki[i,]$HOME,]$num,
        temp.data[temp.data$ekipe == podatki[i,]$HOME,]$ppf/temp.data[temp.data$ekipe == podatki[i,]$HOME,]$num,
        podatki[i,]$AWAY,
        temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$ppts/temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$num,
        temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$p2pm/temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$num,
        temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$p3pm/temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$num,
        temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$pftm/temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$num,
        temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$porb/temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$num,
        temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$pdrb/temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$num,
        temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$past/temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$num,
        temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$pstl/temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$num,
        temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$ptov/temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$num,
        temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$pblk/temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$num,
        temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$ppf/temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$num,
        WIN))
    
    if(temp.data[temp.data$ekipe == podatki[i,]$HOME,]$first == TRUE){
        temp.data[temp.data$ekipe == podatki[i,]$HOME,]$first = FALSE
        temp.data[temp.data$ekipe == podatki[i,]$HOME,]$num = temp.data[temp.data$ekipe == podatki[i,]$HOME,]$num - 1
    }
    
    if(temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$first == TRUE){
        temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$first = FALSE
        temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$num = temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$num - 1
    }
    
    temp.data[temp.data$ekipe == podatki[i,]$HOME,]$num = temp.data[temp.data$ekipe == podatki[i,]$HOME,]$num + 1
    temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$num = temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$num + 1
    
    if(temp.data[temp.data$ekipe == podatki[i,]$HOME,]$num != 1){
        temp.data[temp.data$ekipe == podatki[i,]$HOME,]$ppts = (temp.data[temp.data$ekipe == podatki[i,]$HOME,]$ppts + podatki[i,]$HPTS)
        temp.data[temp.data$ekipe == podatki[i,]$HOME,]$p2pm = temp.data[temp.data$ekipe == podatki[i,]$HOME,]$p2pm + (podatki[i,]$H2PM/podatki[i,]$H2PA)
        temp.data[temp.data$ekipe == podatki[i,]$HOME,]$p3pm = temp.data[temp.data$ekipe == podatki[i,]$HOME,]$p3pm + (podatki[i,]$H3PM/podatki[i,]$H3PA)
        temp.data[temp.data$ekipe == podatki[i,]$HOME,]$pftm = temp.data[temp.data$ekipe == podatki[i,]$HOME,]$pftm + (podatki[i,]$HFTM/podatki[i,]$HFTA)
        temp.data[temp.data$ekipe == podatki[i,]$HOME,]$porb = temp.data[temp.data$ekipe == podatki[i,]$HOME,]$porb + podatki[i,]$HORB
        temp.data[temp.data$ekipe == podatki[i,]$HOME,]$pdrb = temp.data[temp.data$ekipe == podatki[i,]$HOME,]$pdrb + podatki[i,]$HDRB
        temp.data[temp.data$ekipe == podatki[i,]$HOME,]$past = temp.data[temp.data$ekipe == podatki[i,]$HOME,]$past + podatki[i,]$HAST
        temp.data[temp.data$ekipe == podatki[i,]$HOME,]$pstl = temp.data[temp.data$ekipe == podatki[i,]$HOME,]$pstl + podatki[i,]$HSTL
        temp.data[temp.data$ekipe == podatki[i,]$HOME,]$ptov = temp.data[temp.data$ekipe == podatki[i,]$HOME,]$ptov + podatki[i,]$HTOV
        temp.data[temp.data$ekipe == podatki[i,]$HOME,]$pblk = temp.data[temp.data$ekipe == podatki[i,]$HOME,]$pblk + podatki[i,]$HBLK
        temp.data[temp.data$ekipe == podatki[i,]$HOME,]$ppf = temp.data[temp.data$ekipe == podatki[i,]$HOME,]$ppf + podatki[i,]$HPF
    }
    if(temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$num != 1){
        temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$ppts = (temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$ppts + podatki[i,]$APTS)
        temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$p2pm = temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$p2pm + (podatki[i,]$A2PM/podatki[i,]$A2PA)
        temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$p3pm = temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$p3pm + (podatki[i,]$A3PM/podatki[i,]$A3PA)
        temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$pftm = temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$pftm + (podatki[i,]$AFTM/podatki[i,]$AFTA)
        temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$porb = temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$porb + podatki[i,]$AORB
        temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$pdrb = temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$pdrb + podatki[i,]$ADRB
        temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$past = temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$past + podatki[i,]$AAST
        temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$pstl = temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$pstl + podatki[i,]$ASTL
        temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$ptov = temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$ptov + podatki[i,]$ATOV
        temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$pblk = temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$pblk + podatki[i,]$ABLK
        temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$ppf = temp.data[temp.data$ekipe == podatki[i,]$AWAY,]$ppf + podatki[i,]$APF
    }
    
    
}
pod = pod[-1,]



learn <- pod[1:2900,]
test <- pod[2900:3690,]

learn$HOME <- NULL
learn$AWAY <- NULL

test$HOME <- NULL
test$AWAY <- NULL

learn$W <- as.factor(learn$W)
test$W <- as.factor(test$W)

pod$HOME <- NULL
pod$AWAY <- NULL
