rm(list = ls())

wd <- "/home/eric/Desktop/data/elecs/MXelsCalendGovt/redistrict/git-repo/mex-open-map/data/ptyReactions2escenarios/"
setwd(wd)


# all 1/2 Escenario party counteroffers
files <- list.files()
#files <- files[grep("[a-z]+1.*.csv", files)] # keep only files for 1erEscen --- eg. ags1panloc.csv
files <- files[grep("[a-z]+2.*.csv", files)] # keep only files for 2oEscen  --- eg. ags2panloc.csv
#
dat <- data.frame()
for (i in 1:length(files)){
    #i <- 108 # debug
    tmp <- read.csv(file = files[i], stringsAsFactors = FALSE, sep = "\t")
    colnames(tmp) <- c("edon","disn","munn","seccion")
    #tmp$edo <- gsub("(.*)1(.*).csv", "\\1", files[i])
    #tmp$pty <- gsub("(.*)1(.*).csv", "\\2", files[i])
    tmp$edo <- gsub("(.*)2(.*).csv", "\\1", files[i])
    tmp$pty <- gsub("(.*)2(.*).csv", "\\2", files[i])
    dat <- rbind(dat, tmp)
}
rm(tmp, i)

# 2oEscenario (prepared by AT by deduction from party counteroffers and winner's list)
dat2 <- read.csv(file = "2oEscenario.csv", stringsAsFactors = FALSE)
head(dat2)

# get all secciones map into districts
#allSec <- read.csv(file = "/home/eric/Desktop/data/elecs/MXelsCalendGovt/redistrict/ifeRedist2013/equivSecc/tablaEquivalenciasSeccionales1994-2010.2013.csv", stringsAsFactors = FALSE)
allSec <- read.csv(file = "/home/eric/Desktop/data/elecs/MXelsCalendGovt/redistrict/ifeRedist2013/equivSecc/tablaEquivalenciasSeccionales1994-2014.csv", stringsAsFactors = FALSE)

##################################
##################################
###                            ###
###  Primer escenario comments ###
###                            ###
##################################
##################################
# panloc
allSec$work <- allSec$dis2013.1
tmp <- dat[dat$pty=="panloc" & dat$seccion==0,] # municipios that belong complete to a district
for (i in 1:nrow(tmp)){
    #i <- i+1 # debug
    message(sprintf("loop %s of %s", i, nrow(tmp)))
    sel <- which(allSec$edon == tmp$edon[i] & allSec$munn == tmp$munn[i])
    allSec$work[sel] <- tmp$disn[i]
}
tmp <- dat[dat$pty=="panloc" & dat$seccion!=0,] # rest
for (i in 1:nrow(tmp)){
    #i <- 2 # debug
    message(sprintf("loop %s of %s", i, nrow(tmp)))
    sel <- which(allSec$edon == tmp$edon[i] & allSec$seccion == tmp$seccion[i])
    if (length(sel)==0) next
    allSec$work[sel] <- tmp$disn[i]
}
colnames(allSec)[grep("work", colnames(allSec))] <- "dis2013.1panloc" # rename column

# pannac
allSec$work <- allSec$dis2013.1
tmp <- dat[dat$pty=="pannac" & dat$seccion==0,] # municipios that belong complete to a district
for (i in 1:nrow(tmp)){
    #i <- 1 # debug
    message(sprintf("loop %s of %s", i, nrow(tmp)))
    sel <- which(allSec$edon == tmp$edon[i] & allSec$munn == tmp$munn[i])
    allSec$work[sel] <- tmp$disn[i]
}
tmp <- dat[dat$pty=="pannac" & dat$seccion!=0,] # rest
for (i in 1:nrow(tmp)){
    #i <- 1 # debug
    message(sprintf("loop %s of %s", i, nrow(tmp)))
    sel <- which(allSec$edon == tmp$edon[i] & allSec$seccion == tmp$seccion[i])
    if (length(sel)==0) next
    allSec$work[sel] <- tmp$disn[i]
}
colnames(allSec)[grep("work", colnames(allSec))] <- "dis2013.1pannac" # rename column

# priloc
allSec$work <- allSec$dis2013.1
tmp <- dat[dat$pty=="priloc" & dat$seccion==0,] # municipios that belong complete to a district
for (i in 1:nrow(tmp)){
    #i <- 1 # debug
    message(sprintf("loop %s of %s", i, nrow(tmp)))
    sel <- which(allSec$edon == tmp$edon[i] & allSec$munn == tmp$munn[i])
    allSec$work[sel] <- tmp$disn[i]
}
tmp <- dat[dat$pty=="priloc" & dat$seccion!=0,] # rest
for (i in 1:nrow(tmp)){
    #i <- 1 # debug
    message(sprintf("loop %s of %s", i, nrow(tmp)))
    sel <- which(allSec$edon == tmp$edon[i] & allSec$seccion == tmp$seccion[i])
    if (length(sel)==0) next
    allSec$work[sel] <- tmp$disn[i]
}
colnames(allSec)[grep("work", colnames(allSec))] <- "dis2013.1priloc" # rename column

## # prinac NO CASES
## allSec$work <- allSec$dis2013.1
## tmp <- dat[dat$pty=="prinac" & dat$seccion==0,] # municipios that belong complete to a district
## for (i in 1:nrow(tmp)){
##     #i <- 1 # debug
##     message(sprintf("loop %s of %s", i, nrow(tmp)))
##     sel <- which(allSec$edon == tmp$edon[i] & allSec$munn == tmp$munn[i])
##     allSec$work[sel] <- tmp$disn[i]
## }
## tmp <- dat[dat$pty=="prinac" & dat$seccion!=0,] # rest
## for (i in 1:nrow(tmp)){
##     #i <- 1 # debug
##     message(sprintf("loop %s of %s", i, nrow(tmp)))
##     sel <- which(allSec$edon == tmp$edon[i] & allSec$seccion == tmp$seccion[i])
##     if (length(sel)==0) next
##     allSec$work[sel] <- tmp$disn[i]
## }
## colnames(allSec)[grep("work", colnames(allSec))] <- "dis2013.1prinac" # rename column

# prdloc
allSec$work <- allSec$dis2013.1
tmp <- dat[dat$pty=="prdloc" & dat$seccion==0,] # municipios that belong complete to a district
for (i in 1:nrow(tmp)){
    #i <- 1 # debug
    message(sprintf("loop %s of %s", i, nrow(tmp)))
    sel <- which(allSec$edon == tmp$edon[i] & allSec$munn == tmp$munn[i])
    allSec$work[sel] <- tmp$disn[i]
}
tmp <- dat[dat$pty=="prdloc" & dat$seccion!=0,] # rest
for (i in 1:nrow(tmp)){
    #i <- 1 # debug
    message(sprintf("loop %s of %s", i, nrow(tmp)))
    sel <- which(allSec$edon == tmp$edon[i] & allSec$seccion == tmp$seccion[i])
    if (length(sel)==0) next
    allSec$work[sel] <- tmp$disn[i]
}
colnames(allSec)[grep("work", colnames(allSec))] <- "dis2013.1prdloc" # rename column

# prdnac
allSec$work <- allSec$dis2013.1
tmp <- dat[dat$pty=="prdnac" & dat$seccion==0,] # municipios that belong complete to a district
for (i in 1:nrow(tmp)){
    #i <- 1 # debug
    message(sprintf("loop %s of %s", i, nrow(tmp)))
    sel <- which(allSec$edon == tmp$edon[i] & allSec$munn == tmp$munn[i])
    allSec$work[sel] <- tmp$disn[i]
}
tmp <- dat[dat$pty=="prdnac" & dat$seccion!=0,] # rest
for (i in 1:nrow(tmp)){
    #i <- 1 # debug
    message(sprintf("loop %s of %s", i, nrow(tmp)))
    sel <- which(allSec$edon == tmp$edon[i] & allSec$seccion == tmp$seccion[i])
    if (length(sel)==0) next
    allSec$work[sel] <- tmp$disn[i]
}
colnames(allSec)[grep("work", colnames(allSec))] <- "dis2013.1prdnac" # rename column
#

###########################
###########################
###                     ###
###  Segundo escenario  ###
###                     ###
###########################
###########################
# 
allSec$work <- allSec$dis2013.1
tmp <- dat2[dat2$seccion==0,] # municipios that belong complete to a district
for (i in 1:nrow(tmp)){
    #i <- i+1 # debug
    message(sprintf("loop %s of %s", i, nrow(tmp)))
    sel <- which(allSec$edon == tmp$edon[i] & allSec$munn == tmp$munn[i])
    allSec$work[sel] <- tmp$disn[i]
}
tmp <- dat2[dat2$seccion!=0,] # rest
for (i in 1:nrow(tmp)){
    #i <- i+1 # debug
    message(sprintf("loop %s of %s", i, nrow(tmp)))
    sel <- which(allSec$edon == tmp$edon[i] & allSec$seccion == tmp$seccion[i])
    if (length(sel)==0) next
    allSec$work[sel] <- tmp$disn[i]
}
colnames(allSec)[grep("work", colnames(allSec))] <- "dis2013.2" # rename column

###################################
###################################
###                             ###
###  Segundo escenario comments ###
###                             ###
###################################
###################################
# panloc
allSec$work <- allSec$dis2013.2
tmp <- dat[dat$pty=="panloc" & dat$seccion==0,] # municipios that belong complete to a district
for (i in 1:nrow(tmp)){
    #i <- i+1 # debug
    message(sprintf("loop %s of %s", i, nrow(tmp)))
    sel <- which(allSec$edon == tmp$edon[i] & allSec$munn == tmp$munn[i])
    allSec$work[sel] <- tmp$disn[i]
}
tmp <- dat[dat$pty=="panloc" & dat$seccion!=0,] # rest
for (i in 1:nrow(tmp)){
    #i <- 2 # debug
    message(sprintf("loop %s of %s", i, nrow(tmp)))
    sel <- which(allSec$edon == tmp$edon[i] & allSec$seccion == tmp$seccion[i])
    if (length(sel)==0) next
    allSec$work[sel] <- tmp$disn[i]
}
colnames(allSec)[grep("work", colnames(allSec))] <- "dis2013.2panloc" # rename column

# pannac
allSec$work <- allSec$dis2013.1
tmp <- dat[dat$pty=="pannac" & dat$seccion==0,] # municipios that belong complete to a district
for (i in 1:nrow(tmp)){
    #i <- 1 # debug
    message(sprintf("loop %s of %s", i, nrow(tmp)))
    sel <- which(allSec$edon == tmp$edon[i] & allSec$munn == tmp$munn[i])
    allSec$work[sel] <- tmp$disn[i]
}
tmp <- dat[dat$pty=="pannac" & dat$seccion!=0,] # rest
for (i in 1:nrow(tmp)){
    #i <- 1 # debug
    message(sprintf("loop %s of %s", i, nrow(tmp)))
    sel <- which(allSec$edon == tmp$edon[i] & allSec$seccion == tmp$seccion[i])
    if (length(sel)==0) next
    allSec$work[sel] <- tmp$disn[i]
}
colnames(allSec)[grep("work", colnames(allSec))] <- "dis2013.2pannac" # rename column

# priloc
allSec$work <- allSec$dis2013.1
tmp <- dat[dat$pty=="priloc" & dat$seccion==0,] # municipios that belong complete to a district
for (i in 1:nrow(tmp)){
    #i <- 1 # debug
    message(sprintf("loop %s of %s", i, nrow(tmp)))
    sel <- which(allSec$edon == tmp$edon[i] & allSec$munn == tmp$munn[i])
    allSec$work[sel] <- tmp$disn[i]
}
tmp <- dat[dat$pty=="priloc" & dat$seccion!=0,] # rest
for (i in 1:nrow(tmp)){
    #i <- 1 # debug
    message(sprintf("loop %s of %s", i, nrow(tmp)))
    sel <- which(allSec$edon == tmp$edon[i] & allSec$seccion == tmp$seccion[i])
    if (length(sel)==0) next
    allSec$work[sel] <- tmp$disn[i]
}
colnames(allSec)[grep("work", colnames(allSec))] <- "dis2013.2priloc" # rename column

# prinac
allSec$work <- allSec$dis2013.1
tmp <- dat[dat$pty=="prinac" & dat$seccion==0,] # municipios that belong complete to a district
for (i in 1:nrow(tmp)){
    #i <- 1 # debug
    message(sprintf("loop %s of %s", i, nrow(tmp)))
    sel <- which(allSec$edon == tmp$edon[i] & allSec$munn == tmp$munn[i])
    allSec$work[sel] <- tmp$disn[i]
}
tmp <- dat[dat$pty=="prinac" & dat$seccion!=0,] # rest
for (i in 1:nrow(tmp)){
    #i <- 1 # debug
    message(sprintf("loop %s of %s", i, nrow(tmp)))
    sel <- which(allSec$edon == tmp$edon[i] & allSec$seccion == tmp$seccion[i])
    if (length(sel)==0) next
    allSec$work[sel] <- tmp$disn[i]
}
colnames(allSec)[grep("work", colnames(allSec))] <- "dis2013.2prinac" # rename column

# prdloc
allSec$work <- allSec$dis2013.1
tmp <- dat[dat$pty=="prdloc" & dat$seccion==0,] # municipios that belong complete to a district
for (i in 1:nrow(tmp)){
    #i <- 1 # debug
    message(sprintf("loop %s of %s", i, nrow(tmp)))
    sel <- which(allSec$edon == tmp$edon[i] & allSec$munn == tmp$munn[i])
    allSec$work[sel] <- tmp$disn[i]
}
tmp <- dat[dat$pty=="prdloc" & dat$seccion!=0,] # rest
for (i in 1:nrow(tmp)){
    #i <- 1 # debug
    message(sprintf("loop %s of %s", i, nrow(tmp)))
    sel <- which(allSec$edon == tmp$edon[i] & allSec$seccion == tmp$seccion[i])
    if (length(sel)==0) next
    allSec$work[sel] <- tmp$disn[i]
}
colnames(allSec)[grep("work", colnames(allSec))] <- "dis2013.2prdloc" # rename column

# prdnac
allSec$work <- allSec$dis2013.1
tmp <- dat[dat$pty=="prdnac" & dat$seccion==0,] # municipios that belong complete to a district
for (i in 1:nrow(tmp)){
    #i <- 1 # debug
    message(sprintf("loop %s of %s", i, nrow(tmp)))
    sel <- which(allSec$edon == tmp$edon[i] & allSec$munn == tmp$munn[i])
    allSec$work[sel] <- tmp$disn[i]
}
tmp <- dat[dat$pty=="prdnac" & dat$seccion!=0,] # rest
for (i in 1:nrow(tmp)){
    #i <- 1 # debug
    message(sprintf("loop %s of %s", i, nrow(tmp)))
    sel <- which(allSec$edon == tmp$edon[i] & allSec$seccion == tmp$seccion[i])
    if (length(sel)==0) next
    allSec$work[sel] <- tmp$disn[i]
}
colnames(allSec)[grep("work", colnames(allSec))] <- "dis2013.2prdnac" # rename column
#


# select columns to export
sel <- c(2:4,28:33)
colnames(allSec)[sel]

write.csv(allSec[,sel], file = "/home/eric/Desktop/data/elecs/MXelsCalendGovt/redistrict/ifeRedist2013/equivSecc/tmp.csv", row.names = FALSE)

