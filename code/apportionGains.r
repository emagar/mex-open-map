rm(list=ls())

datdir <- c("~/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/")
#datdir <- c("d:/01/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/")

dfdf <- read.csv( paste(datdir, "dfdf1997-2012.csv", sep="/"), header=TRUE, stringsAsFactors=FALSE)

# replace NAs with zeroes in vote columns
tmp <- dfdf[,16:35]
tmp[is.na(tmp)] <- 0
dfdf[,16:35] <- tmp

# who won
dfdf$win <- rep(NA, times=nrow(dfdf))
wcoal <- dfdf[,16:35]
#
tmp <- dfdf$pri+dfdf$pvem+dfdf$pri.pvem
wcoal$pricoal[(dfdf$yr==2009 | dfdf$yr==2012) & dfdf$pri.pvem>0] <- tmp[(dfdf$yr==2009 | dfdf$yr==2012) & dfdf$pri.pvem>0] # adds coalition vote where needed
tmp <- dfdf$prd+dfdf$pt+dfdf$conve+dfdf$prd.pt+dfdf$prd.mc+dfdf$pt.mc+dfdf$prd.pt.c
wcoal$prdcoal[dfdf$yr==2012 & dfdf$prd.pt.c>0] <- tmp[dfdf$yr==2012 & dfdf$prd.pt.c>0] # adds coalition vote where needed
tmp <- dfdf$pt+dfdf$conve+dfdf$pt.mc
wcoal$ptcoal[dfdf$yr==2009] <- tmp[dfdf$yr==2009] # adds coalition vote where needed
#
wcoal[is.na(wcoal)] <- 0
#
max <- apply( wcoal, 1, max)

dfdf$win <- rep(NA, times=nrow(dfdf))
#
dfdf$win[ dfdf$yr!=2000                  & wcoal$pan==max] <- "pan"
dfdf$win[ dfdf$yr==2000                  & wcoal$pan.pvem==max] <- "pan.pvem"
#
dfdf$win[ dfdf$yr<=2000                  & wcoal$pri==max] <- "pri"
dfdf$win[ dfdf$yr==2003                  & wcoal$pri==max] <- "pri"
dfdf$win[ dfdf$yr==2003                  & wcoal$pri.pvem==max] <- "pri.pvem"
dfdf$win[ dfdf$yr==2006                  & wcoal$pri.pvem==max] <- "pri.pvem"
dfdf$win[(dfdf$yr==2009 | dfdf$yr==2012) & wcoal$pri==max & wcoal$pri.pvem==0] <- "pri"
dfdf$win[(dfdf$yr==2009 | dfdf$yr==2012) & wcoal$pricoal==max & wcoal$pri.pvem>0] <- "pri.pvem"
#
dfdf$win[ dfdf$yr<2000                   & wcoal$prd==max] <- "prd"
dfdf$win[ dfdf$yr==2000                  & wcoal$apm==max] <- "apm"
dfdf$win[(dfdf$yr==2003 | dfdf$yr==2009) & wcoal$prd==max] <- "prd"
dfdf$win[ dfdf$yr==2006                  & wcoal$prd.pt.c==max] <- "prd.pt.c"
dfdf$win[ dfdf$yr==2012                  & wcoal$prdcoal==max] <- "prd.pt.c"
#
dfdf$win[(dfdf$yr==1997 | dfdf$yr==2003) & wcoal$pt==max] <- "pt"
dfdf$win[ dfdf$yr==2009                  & wcoal$ptcoal==max] <- "pt.c"
#
dfdf$win[ dfdf$yr==1997                  & wcoal$pvem==max] <- "pvem"
dfdf$win[(dfdf$yr==2003 | dfdf$yr==2009 | dfdf$yr==2012) & wcoal$pri.pvem==0 & wcoal$pvem==max] <- "pvem"
#
dfdf$win[ dfdf$yr==2003                  & wcoal$conve==max ] <- "conve"
#
dfdf$win[ wcoal$panal==max ] <- "panal"

# simplifica: victorias de coalición se contabilizan como del partido mayor
dfdf$dpanw <- dfdf$dpriw <- dfdf$dprdw <- dfdf$dothw <- rep(0, times=nrow(dfdf))
dfdf$dpanw[dfdf$win=="pan" | dfdf$win=="pan.pvem"] <- 1
dfdf$dpriw[dfdf$win=="pri" | dfdf$win=="pri.pvem"] <- 1
dfdf$dprdw[dfdf$win=="prd" | dfdf$win=="apm" | dfdf$win=="prd.pt.c"] <- 1
dfdf$dothw[dfdf$win=="pt" | dfdf$win=="pt.c" | dfdf$win=="pvem"] <- 1

# triunfos uninominales por estado
# así se hace en R un by yr mo: egen tmp=sum(invested) de stata
dfdf$npanw <- ave(dfdf$dpanw, as.factor(dfdf$yr + dfdf$edon/100), FUN=sum, na.rm=TRUE)
dfdf$npriw <- ave(dfdf$dpriw, as.factor(dfdf$yr + dfdf$edon/100), FUN=sum, na.rm=TRUE)
dfdf$nprdw <- ave(dfdf$dprdw, as.factor(dfdf$yr + dfdf$edon/100), FUN=sum, na.rm=TRUE)
dfdf$nothw <- ave(dfdf$dothw, as.factor(dfdf$yr + dfdf$edon/100), FUN=sum, na.rm=TRUE)
dfdf$nallw <- dfdf$npanw + dfdf$npriw + dfdf$nprdw + dfdf$nothw
#
dfdf$shpanw <- dfdf$npanw / dfdf$nallw
dfdf$shpriw <- dfdf$npriw / dfdf$nallw
dfdf$shprdw <- dfdf$nprdw / dfdf$nallw

tmp <- rep(NA, times=nrow(dfdf)); tmp[1] <- 1
i <- 2
for (i in 2:nrow(dfdf)){
    tmp[i] <- ifelse(dfdf$edon[i]==dfdf$edon[i-1], 0, 1)
}
edo.wins <- dfdf[tmp==1, c("edon", "edoemm", "yr", "npanw", "npriw", "nprdw", "shpanw", "shpriw", "shprdw")]

tmp <- rep(NA, 32); nine.yr.avg <- data.frame(pan=tmp, pri=tmp, prd=tmp)
nine.yr.avg$pan <- (edo.wins[edo.wins$yr==2006, "shpanw"] + edo.wins[edo.wins$yr==2009, "shpanw"] + edo.wins[edo.wins$yr==2012, "shpanw"]) / 3
nine.yr.avg$pri <- (edo.wins[edo.wins$yr==2006, "shpriw"] + edo.wins[edo.wins$yr==2009, "shpriw"] + edo.wins[edo.wins$yr==2012, "shpriw"]) / 3
nine.yr.avg$prd <- (edo.wins[edo.wins$yr==2006, "shprdw"] + edo.wins[edo.wins$yr==2009, "shprdw"] + edo.wins[edo.wins$yr==2012, "shprdw"]) / 3

apportionment.marquez <- data.frame(
    dis.05=c(3, 8, 2, 2, 7, 2, 12, 9, 27, 4, 14, 9, 7, 19, 40, 12, 5, 3, 12, 11, 16, 4, 3, 7, 8, 7, 6, 8, 3, 21, 5, 4),
    ptote= c(3, 8, 2, 2, 7, 2, 13, 9, 24, 4, 15, 9, 7, 20, 41, 12, 5, 3, 12, 10, 15, 5, 4, 7, 7, 7, 6, 9, 3, 20, 5, 4),
    ptot=  c(3, 8, 2, 2, 7, 2, 13, 9, 23, 4, 15, 9, 7, 20, 41, 12, 5, 3, 12, 10, 16, 5, 4, 7, 7, 7, 6, 9, 3, 20, 5, 4),
    p18e=  c(3, 8, 2, 2, 7, 2, 12, 9, 26, 4, 14, 8, 7, 20, 41, 11, 5, 3, 13, 10, 15, 5, 4, 7, 7, 7, 6, 9, 3, 21, 5, 4),
    p18=   c(3, 8, 2, 2, 7, 2, 12, 9, 26, 4, 14, 8, 7, 20, 41, 11, 5, 3, 13, 10, 15, 5, 4, 7, 7, 7, 6, 9, 3, 21, 5, 4),
    lisnom=c(3, 9, 2, 2, 7, 2, 11, 9, 27, 4, 15, 9, 7, 20, 39, 12, 5, 3, 13, 10, 15, 5, 3, 7, 7, 7, 6, 9, 3, 20, 5, 4)
    )

dif <- data.frame(
    ptote=apportionment.marquez$ptote - apportionment.marquez$dis.05,
    ptot=apportionment.marquez$ptot - apportionment.marquez$dis.05,
    p18e=apportionment.marquez$p18e - apportionment.marquez$dis.05,
    p18=apportionment.marquez$p18 - apportionment.marquez$dis.05,
    lisnom=apportionment.marquez$lisnom - apportionment.marquez$dis.05
    )

apply(dif*nine.yr.avg$pan, 2, sum)
apply(dif*nine.yr.avg$pri, 2, sum)
apply(dif*nine.yr.avg$prd, 2, sum)

head(edo.wins)

#ptot y p18 por estado 2000

ptot.2000 <- c(944285, 2487367, 424041, 690689, 2298070, 542627, 3920892, 3052907, 8605239, 1448661, 4663032, 3079649, 2235591, 6322002, 13096686, 3985667, 1555296, 920185, 3834141, 3438765, 5076686, 1404306, 874963, 2299360, 2536844, 2216969, 1891829, 2753222, 962646, 6908975, 1658210, 1353610)

p18.2000 <- c(534188, 1394094, 259839, 398158, 1389959, 309393, 2016925, 1796933, 5760324, 820262, 2596418, 1630207, 1276510, 3697508, 7492461, 2215038, 898506, 538397, 2431091, 1885175, 2782993, 792367, 507644, 1289362, 1501733, 1350299, 1075920, 1701066, 555311, 4064590, 996374, 759789)

FALTA PTOT P18 2010: ~/Dropbox/data/mapas/entidad/estados2013/

lisnom.2003 <- c(630563, 1769264, 287018, 430828, 1544905, 366070, 2312319, 2183966, 6712664, 955312, 3075635, 1907079, 1461268, 4269184, 8259141, 2669685, 1068880, 613087, 2677341, 2108313, 3078860, 909830, 562217, 1432428, 1614239, 1510547, 1201551, 1994744, 630645, 4500029, 1064181, 908803)

lisnom.2012 <- c(812673, 2323235, 430342, 563812, 1901824, 471895, 3016603, 2500641, 7216743, 1145130, 3860238, 2343145, 1861638, 5260351, 10396508, 3133883, 1311365, 749692, 3324155, 2586212, 3915969, 1280215, 925089, 1765189, 1912288, 1872322, 1530142, 2445046, 813608, 5330347, 1357419, 1075452)

p18.2010 <- 118074, 301083, 68716, 81699, 300306, 71059, 261553, 304226, 1498598, 135783, 347952, 231144, 200247, 731038, 1389577, 290369, 171736, 103314, 572868, 227469, 465277, 184513, 118254, 215579, 343252, 290698, 201472, 349801, 103232, 630861, 169680, 101995)

ptot.2010 <- c(1184996, 3155070, 637026, 822441, 2748391, 650555, 4796580, 3406465, 8851080, 1632934, 5486372, 3388768, 2665018, 7350682, 15175862, 4351037, 1777227, 1084979, 4653458, 3801962, 5779829, 1827937, 1325578, 2585518, 2767761, 2662480, 2238603, 3268554, 1169936, 7643194, 1955577, 1490668)
