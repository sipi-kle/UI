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
one = c()
two = c()
tri = c()
four = c()
five = c()
for(ekipa in ekipe){
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
            all = c(0)
            one = c(0)
            two = c(0)
            tri = c(0)
            four = c(0)
            five = c(0)
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
            all = c(0)
            one = c(0)
            two = c(0)
            tri = c(0)
            four = c(0)
            five = c(0)            
            break()
        }
    }
}
temp.data = data.frame(ekipe,ppts,p2pm,p3pm,pftm,porb,pdrb,past,pstl,ptov,pblk,ppf,num,first,one,two,tri,four,five,all)


pod = data.frame(matrix(ncol = 29,nrow = 1),stringsAsFactors = FALSE)
names(pod) = c("HOME","HPTS","H2PM","H3PM","HFTM","HORB","HDRB","HAST","HSTL","HTOV","HBLK","HPF","HL5","HALL","AWAY","APTS","A2PM","A3PM","AFTM","AORB","ADRB","AAST","ASTL","ATOV","ABLK","APF","AL5","AALL","W")
for(i in 1:nrow(podatki)){
    doma = temp.data$ekipe == podatki[i,]$HOME
    gost = temp.data$ekipe == podatki[i,]$AWAY

    temp.data[doma,]$five =  temp.data[doma,]$four
    temp.data[doma,]$four =  temp.data[doma,]$tri
    temp.data[doma,]$tri =  temp.data[doma,]$two
    temp.data[doma,]$htwo =  temp.data[doma,]$hone
    temp.data[gost,]$five =  temp.data[gost,]$four
    temp.data[gost,]$four =  temp.data[gost,]$tri
    temp.data[gost,]$tri =  temp.data[gost,]$two
    temp.data[gost,]$two =  temp.data[gost,]$one




    if(podatki[i,]$HPTS > podatki[i,]$APTS){
        WIN = "H"
        temp.data[doma,]$all = temp.data[doma,]$all +1
        temp.data[gost,]$all = temp.data[gost,]$all -1
        temp.data[doma,]$one =  1
        temp.data[gost,]$one =  0
    }
    else{
        WIN = "A"
        temp.data[doma,]$all = temp.data[doma,]$all -1
        temp.data[gost,]$all = temp.data[gost,]$all +1
        temp.data[doma,]$one =  0
        temp.data[gost,]$one =  1
    }

    pod = rbind(pod,list(
        podatki[i,]$HOME,
        temp.data[doma,]$ppts/temp.data[doma,]$num,
        temp.data[doma,]$p2pm/temp.data[doma,]$num,
        temp.data[doma,]$p3pm/temp.data[doma,]$num,
        temp.data[doma,]$pftm/temp.data[doma,]$num,
        temp.data[doma,]$porb/temp.data[doma,]$num,
        temp.data[doma,]$pdrb/temp.data[doma,]$num,
        temp.data[doma,]$past/temp.data[doma,]$num,
        temp.data[doma,]$pstl/temp.data[doma,]$num,
        temp.data[doma,]$ptov/temp.data[doma,]$num,
        temp.data[doma,]$pblk/temp.data[doma,]$num,
        temp.data[doma,]$ppf/temp.data[doma,]$num,
        sum(temp.data[doma,]$five,temp.data[doma,]$four,temp.data[doma,]$tri,temp.data[doma,]$two,temp.data[doma,]$one),
        temp.data[doma,]$all,
        podatki[i,]$AWAY,
        temp.data[gost,]$ppts/temp.data[gost,]$num,
        temp.data[gost,]$p2pm/temp.data[gost,]$num,
        temp.data[gost,]$p3pm/temp.data[gost,]$num,
        temp.data[gost,]$pftm/temp.data[gost,]$num,
        temp.data[gost,]$porb/temp.data[gost,]$num,
        temp.data[gost,]$pdrb/temp.data[gost,]$num,
        temp.data[gost,]$past/temp.data[gost,]$num,
        temp.data[gost,]$pstl/temp.data[gost,]$num,
        temp.data[gost,]$ptov/temp.data[gost,]$num,
        temp.data[gost,]$pblk/temp.data[gost,]$num,
        temp.data[gost,]$ppf/temp.data[gost,]$num,
        sum(temp.data[gost,]$five,temp.data[gost,]$four,temp.data[gost,]$tri,temp.data[gost,]$two,temp.data[gost,]$one),
        temp.data[gost,]$all,
        WIN))
    
    if(temp.data[doma,]$first == TRUE){
        temp.data[doma,]$first = FALSE
        temp.data[doma,]$num = temp.data[doma,]$num - 1
    }
    
    if(temp.data[gost,]$first == TRUE){
        temp.data[gost,]$first = FALSE
        temp.data[gost,]$num = temp.data[gost,]$num - 1
    }
    
    temp.data[doma,]$num = temp.data[doma,]$num + 1
    temp.data[gost,]$num = temp.data[gost,]$num + 1
    
    if(temp.data[doma,]$num != 1){
        temp.data[doma,]$ppts = (temp.data[doma,]$ppts + podatki[i,]$HPTS)
        temp.data[doma,]$p2pm = temp.data[doma,]$p2pm + (podatki[i,]$H2PM/podatki[i,]$H2PA)
        temp.data[doma,]$p3pm = temp.data[doma,]$p3pm + (podatki[i,]$H3PM/podatki[i,]$H3PA)
        temp.data[doma,]$pftm = temp.data[doma,]$pftm + (podatki[i,]$HFTM/podatki[i,]$HFTA)
        temp.data[doma,]$porb = temp.data[doma,]$porb + podatki[i,]$HORB
        temp.data[doma,]$pdrb = temp.data[doma,]$pdrb + podatki[i,]$HDRB
        temp.data[doma,]$past = temp.data[doma,]$past + podatki[i,]$HAST
        temp.data[doma,]$pstl = temp.data[doma,]$pstl + podatki[i,]$HSTL
        temp.data[doma,]$ptov = temp.data[doma,]$ptov + podatki[i,]$HTOV
        temp.data[doma,]$pblk = temp.data[doma,]$pblk + podatki[i,]$HBLK
        temp.data[doma,]$ppf = temp.data[doma,]$ppf + podatki[i,]$HPF
    }
    if(temp.data[gost,]$num != 1){
        temp.data[gost,]$ppts = (temp.data[gost,]$ppts + podatki[i,]$APTS)
        temp.data[gost,]$p2pm = temp.data[gost,]$p2pm + (podatki[i,]$A2PM/podatki[i,]$A2PA)
        temp.data[gost,]$p3pm = temp.data[gost,]$p3pm + (podatki[i,]$A3PM/podatki[i,]$A3PA)
        temp.data[gost,]$pftm = temp.data[gost,]$pftm + (podatki[i,]$AFTM/podatki[i,]$AFTA)
        temp.data[gost,]$porb = temp.data[gost,]$porb + podatki[i,]$AORB
        temp.data[gost,]$pdrb = temp.data[gost,]$pdrb + podatki[i,]$ADRB
        temp.data[gost,]$past = temp.data[gost,]$past + podatki[i,]$AAST
        temp.data[gost,]$pstl = temp.data[gost,]$pstl + podatki[i,]$ASTL
        temp.data[gost,]$ptov = temp.data[gost,]$ptov + podatki[i,]$ATOV
        temp.data[gost,]$pblk = temp.data[gost,]$pblk + podatki[i,]$ABLK
        temp.data[gost,]$ppf = temp.data[gost,]$ppf + podatki[i,]$APF
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
