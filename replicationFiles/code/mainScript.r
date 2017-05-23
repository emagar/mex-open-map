##################################################################################
## This is an abridged version of analizaEscenarios2013.r for replication files ##
##################################################################################

# NOTE:
# Maps are named after the first congressional election they were used: the 1979, 1997, and 2006 maps; the 2013 proposal that was never adopted for 2015 election is the 2013 map.
# Short names for objects in the analysis of the 2013 proposals are d0 (the 2006 map or status quo), d1 (initial IFE proposal), d2 (second proposal, with party feedback), and d3 (third and final proposal, with more party feedback). 

options(width = 140) # emacs screen size
rm(list=ls())
#
if(length(grep('mainScript.r',dir(),fixed=TRUE))!=1) stop ("Please set R's working directory to the base directory on the location of the unzipped directories from replicationFiles.zip")

wd <- getwd()
dd <- c("../data/") # data directory


library(R2jags)
library(Cairo)
library(tikzDevice)


#
#
###################################################################################
## *Bloc 1*:                                                                     ##
##IMPORTS OBJECT eq TO MAP SECCIONES TO 1979, 1997, 2006 DISTRICTS AND TO TWO 2013 ##
##REDISTRICTING PROPOSALS.                                                         ##
##SCRIPT eqPrep.r MANIPULATES equivalencias FILE, ORIGINALLY DISTRIBUTED BY IFE,   ##
##WHICH HAS BEEN UPDATED TO INCLUDE MORE MAPS AND DEAL WITH MOST SECCION CHANGES   ##
##("RESECCIONAMIENTO"). MISSING SECCIONES REMAIN, WITH SMALL POPULATIONS, MOSTLY   ##
##DUE TO RARE AND MINOR COURT-ORDERED DELIMITATION CHANGES BETWEEN MUNICIPALITIES; ##
##FIXING THEM IS METICULOUS AND WOULD RESULT IN NEGLIGIBLE VOTE/POPULATION CHANGES.##
###################################################################################
source(file = "eqPrep.r", echo = TRUE)

##########################################################################################
## *Bloc 2*:                                                                            ##
## READ SECCION-LEVEL ELECTION DATA AND COMPUTE DISTRICT-LEVEL RETURNS                  ##
## d1 and d3 stand for 1st and 3rd scenarios of 2013 map, d0 for 2006 map (null change) ##
## s1, s3, and s0 are state-level aggregates                                            ##
##########################################################################################
#
## adds 2015 votes
tmp <- read.csv( paste(dd, "dfSeccion2015.csv", sep=""), header=TRUE)
tmp <- tmp[order(tmp$edon, tmp$disn, tmp$seccion),]
# compute effective vote (without void ballots)
tmp$efec <- tmp$pan + tmp$pri + tmp$prd + tmp$pvem + tmp$pt + tmp$mc + tmp$panal + tmp$morena + tmp$ph + tmp$ps + tmp$pri_pvem + tmp$prd_pt + tmp$indep1 + tmp$indep2 
# aggregates coalition votes where needed
tmp$dcoalpri <- rep(0, times=nrow(tmp))
tmp$dcoalpri[tmp$pri_pvem>0] <- 1 # misses sections with coalition but no joint pri-pvem vote... next lines fix this
#table(tmp$dcoalpri) # debug
tmp1 <- ave(tmp$dcoalpri, as.factor(tmp$edon*100+tmp$disn), FUN=sum, na.rm=TRUE)
tmp2 <- ave(rep(1,length(tmp$dcoalpri)), as.factor(tmp$edon*100+tmp$disn), FUN=sum, na.rm=TRUE)
tmp3 <- tmp1/tmp2
tmp$dcoalpri[tmp3>=.5] <- 1
rm(tmp1,tmp2,tmp3)
tmp$pric <- tmp$pri+tmp$pvem+tmp$pri_pvem
#
tmp$dcoalprd <- rep(0, times=nrow(tmp))
tmp$dcoalprd[tmp$prd_pt>0] <- 1 # misses sections with coalition but no joint pri-pvem vote... next lines fix this
#table(tmp$dcoalprd) # debug
tmp1 <- ave(tmp$dcoalprd, as.factor(tmp$edon*100+tmp$disn), FUN=sum, na.rm=TRUE)
tmp2 <- ave(rep(1,length(tmp$dcoalprd)), as.factor(tmp$edon*100+tmp$disn), FUN=sum, na.rm=TRUE)
tmp3 <- tmp1/tmp2
tmp$dcoalprd[tmp3>0] <- 1 # rule for pt must be different than pvem: will not get vote in many secciones where it teamed with prd
rm(tmp1,tmp2,tmp3)
tmp$prdc <- tmp$prd+tmp$pt+tmp$prd_pt
#
#
# extracts 2013 districts to re-map 2006 districts
dis2013 <-       eq[,c("edon","dis2015","munn","seccion","dis2013.3","dis2013.1")]
colnames(dis2013) <- c("edon","disn","munn","seccion","dis3er13","dis1er13")
#
# unassigned secciones need to be removed: these remain unassigned even after eqPrep's work
sel <- which(dis2013$disn==0 & dis2013$dis1er13==0); sel
## dis2013[sel,]
dis2013 <- dis2013[-which(dis2013$disn==0 | dis2013$dis1er13==0 | dis2013$dis3er13==0),]
#
# maybe unnecessary (IFE's municipal codes slightly differ from census codes)
dis2013$ife <- dis2013$edon*1000 + dis2013$munn
dis2013 <- dis2013[,c("edon","disn","munn","seccion","ife","dis3er13","dis1er13")] # subset data columns
#
tmp$edon.secn <- tmp$edon*10000 + tmp$seccion
dis2013$edon.secn <- dis2013$edon*10000 + dis2013$seccion
tmp1 <- dis2013[,c("edon.secn","dis1er13","dis3er13")]
tmp <- merge(x = tmp, y = tmp1, by = "edon.secn", all.x = TRUE)
colnames(tmp)[which(colnames(tmp)=="dis1er13")] <- "dis13.1"
colnames(tmp)[which(colnames(tmp)=="dis3er13")] <- "dis13.3"
rm(tmp1)
#
colnames(tmp)
## Aggregates 2015 results by district
df2015d0 <- tmp[order(tmp$edon, tmp$disn),] # resultados con distritos reales (adoptados en 2005)
# as? se hace en R un by yr mo: egen tmp=sum(invested) de stata
df2015d0$pan <- ave(df2015d0$pan, as.factor(df2015d0$edon*100+df2015d0$disn), FUN=sum, na.rm=TRUE)
df2015d0$pri <- ave(df2015d0$pri, as.factor(df2015d0$edon*100+df2015d0$disn), FUN=sum, na.rm=TRUE)
df2015d0$pric <- ave(df2015d0$pric, as.factor(df2015d0$edon*100+df2015d0$disn), FUN=sum, na.rm=TRUE)
df2015d0$prd <- ave(df2015d0$prd, as.factor(df2015d0$edon*100+df2015d0$disn), FUN=sum, na.rm=TRUE)
df2015d0$prdc <- ave(df2015d0$prdc, as.factor(df2015d0$edon*100+df2015d0$disn), FUN=sum, na.rm=TRUE)
df2015d0$pt <- ave(df2015d0$pt, as.factor(df2015d0$edon*100+df2015d0$disn), FUN=sum, na.rm=TRUE)
df2015d0$pvem <- ave(df2015d0$pvem, as.factor(df2015d0$edon*100+df2015d0$disn), FUN=sum, na.rm=TRUE)
df2015d0$mc <- ave(df2015d0$mc, as.factor(df2015d0$edon*100+df2015d0$disn), FUN=sum, na.rm=TRUE)
df2015d0$panal <- ave(df2015d0$panal, as.factor(df2015d0$edon*100+df2015d0$disn), FUN=sum, na.rm=TRUE)
df2015d0$morena <- ave(df2015d0$morena, as.factor(df2015d0$edon*100+df2015d0$disn), FUN=sum, na.rm=TRUE)
df2015d0$ph <- ave(df2015d0$ph, as.factor(df2015d0$edon*100+df2015d0$disn), FUN=sum, na.rm=TRUE)
df2015d0$ps <- ave(df2015d0$ps, as.factor(df2015d0$edon*100+df2015d0$disn), FUN=sum, na.rm=TRUE)
df2015d0$indep1 <- ave(df2015d0$indep1, as.factor(df2015d0$edon*100+df2015d0$disn), FUN=sum, na.rm=TRUE)
df2015d0$indep2 <- ave(df2015d0$indep2, as.factor(df2015d0$edon*100+df2015d0$disn), FUN=sum, na.rm=TRUE)
df2015d0$pri_pvem <- ave(df2015d0$pri_pvem, as.factor(df2015d0$edon*100+df2015d0$disn), FUN=sum, na.rm=TRUE)
df2015d0$prd_pt <- ave(df2015d0$prd_pt, as.factor(df2015d0$edon*100+df2015d0$disn), FUN=sum, na.rm=TRUE)
df2015d0$nr <- ave(df2015d0$nr, as.factor(df2015d0$edon*100+df2015d0$disn), FUN=sum, na.rm=TRUE)
df2015d0$nul <- ave(df2015d0$nul, as.factor(df2015d0$edon*100+df2015d0$disn), FUN=sum, na.rm=TRUE)
df2015d0$efec <- ave(df2015d0$efec, as.factor(df2015d0$edon*100+df2015d0$disn), FUN=sum, na.rm=TRUE)
df2015d0$lisnom <- ave(df2015d0$lisnom, as.factor(df2015d0$edon*100+df2015d0$disn), FUN=sum, na.rm=TRUE)
df2015d0$tmp <- rep(1, times = nrow(df2015d0))
df2015d0$tmp <- ave(df2015d0$tmp, as.factor(df2015d0$edon*100+df2015d0$disn), FUN=sum, na.rm=TRUE)
df2015d0$dcoalpri <- ave(df2015d0$dcoalpri, as.factor(df2015d0$edon*100+df2015d0$disn), FUN=sum, na.rm=TRUE)
df2015d0$dcoalpri <- df2015d0$dcoalpri/df2015d0$tmp # share of secciones with pri coalition in district
colnames(df2015d0)[which(colnames(df2015d0)=="dcoalpri")] <- "shSecCoalPri"
df2015d0$dcoalprd <- ave(df2015d0$dcoalprd, as.factor(df2015d0$edon*100+df2015d0$disn), FUN=sum, na.rm=TRUE)
df2015d0$dcoalprd <- df2015d0$dcoalprd/df2015d0$tmp # share of secciones with prd coalition in district
colnames(df2015d0)[which(colnames(df2015d0)=="dcoalprd")] <- "shSecCoalPrd"
df2015d0$tmp <- NULL
df2015d0 <- df2015d0[duplicated(df2015d0$edon*100+df2015d0$disn)==FALSE,]
## table(df2015d0$shSecCoalPri) ## debug: should now be 0 or 1 after fix used above
## table(df2015d0$shSecCoalPrd) ## debug: should now be 0 or 1 after fix used above
df2015d0$pri_pvem[df2015d0$shSecCoalPri==0] # debug: no need to assign, all == 0
df2015d0$pric[df2015d0$shSecCoalPri==0] <- 0
df2015d0$pri[df2015d0$shSecCoalPri==1] <- 0
df2015d0$pvem[df2015d0$shSecCoalPri==1] <- 0
#df2015d0$pri_pvem <- NULL
#
df2015d0$prd_pt[df2015d0$shSecCoalPrd==0] # debug: no need to assign, all == 0
df2015d0$prdc[df2015d0$shSecCoalPrd==0] <- 0
df2015d0$prd[df2015d0$shSecCoalPrd==1] <- 0
df2015d0$pt[df2015d0$shSecCoalPrd==1] <- 0
#df2015d0$prd_pt <- NULL
df2015d0[df2015d0$edon==9,]
#
# VOTES WITH DISTRICTS PROPOSED IN 2013 (scen. 1)
df2015d1 <- tmp[is.na(tmp$dis13.1)==FALSE,] # elimina las secciones no asignadas a distrito (posiblemente por reseccionamiento, habr? que recuperarlas)
df2015d1$pan <- ave(df2015d1$pan, as.factor(df2015d1$edon*100+df2015d1$dis13.1), FUN=sum, na.rm=TRUE)
df2015d1$pri <- ave(df2015d1$pri, as.factor(df2015d1$edon*100+df2015d1$dis13.1), FUN=sum, na.rm=TRUE)
df2015d1$pric <- ave(df2015d1$pric, as.factor(df2015d1$edon*100+df2015d1$dis13.1), FUN=sum, na.rm=TRUE)
df2015d1$prd <- ave(df2015d1$prd, as.factor(df2015d1$edon*100+df2015d1$dis13.1), FUN=sum, na.rm=TRUE)
df2015d1$prdc <- ave(df2015d1$prdc, as.factor(df2015d1$edon*100+df2015d1$dis13.1), FUN=sum, na.rm=TRUE)
df2015d1$pt <- ave(df2015d1$pt, as.factor(df2015d1$edon*100+df2015d1$dis13.1), FUN=sum, na.rm=TRUE)
df2015d1$pvem <- ave(df2015d1$pvem, as.factor(df2015d1$edon*100+df2015d1$dis13.1), FUN=sum, na.rm=TRUE)
df2015d1$mc <- ave(df2015d1$mc, as.factor(df2015d1$edon*100+df2015d1$dis13.1), FUN=sum, na.rm=TRUE)
df2015d1$panal <- ave(df2015d1$panal, as.factor(df2015d1$edon*100+df2015d1$dis13.1), FUN=sum, na.rm=TRUE)
df2015d1$morena <- ave(df2015d1$morena, as.factor(df2015d1$edon*100+df2015d1$dis13.1), FUN=sum, na.rm=TRUE)
df2015d1$ph <- ave(df2015d1$ph, as.factor(df2015d1$edon*100+df2015d1$dis13.1), FUN=sum, na.rm=TRUE)
df2015d1$ps <- ave(df2015d1$ps, as.factor(df2015d1$edon*100+df2015d1$dis13.1), FUN=sum, na.rm=TRUE)
df2015d1$indep1 <- ave(df2015d1$indep1, as.factor(df2015d1$edon*100+df2015d1$dis13.1), FUN=sum, na.rm=TRUE)
df2015d1$indep2 <- ave(df2015d1$indep2, as.factor(df2015d1$edon*100+df2015d1$dis13.1), FUN=sum, na.rm=TRUE)
df2015d1$pri_pvem <- ave(df2015d1$pri_pvem, as.factor(df2015d1$edon*100+df2015d1$dis13.1), FUN=sum, na.rm=TRUE)
df2015d1$prd_pt <- ave(df2015d1$prd_pt, as.factor(df2015d1$edon*100+df2015d1$dis13.1), FUN=sum, na.rm=TRUE)
df2015d1$nr <- ave(df2015d1$nr, as.factor(df2015d1$edon*100+df2015d1$dis13.1), FUN=sum, na.rm=TRUE)
df2015d1$nul <- ave(df2015d1$nul, as.factor(df2015d1$edon*100+df2015d1$dis13.1), FUN=sum, na.rm=TRUE)
df2015d1$efec <- ave(df2015d1$efec, as.factor(df2015d1$edon*100+df2015d1$dis13.1), FUN=sum, na.rm=TRUE)
df2015d1$lisnom <- ave(df2015d1$lisnom, as.factor(df2015d1$edon*100+df2015d1$dis13.1), FUN=sum, na.rm=TRUE)
df2015d1$tmp <- rep(1, times = nrow(df2015d1))
df2015d1$tmp <- ave(df2015d1$tmp, as.factor(df2015d1$edon*100+df2015d1$dis13.1), FUN=sum, na.rm=TRUE)
df2015d1$dcoalpri <- ave(df2015d1$dcoalpri, as.factor(df2015d1$edon*100+df2015d1$dis13.1), FUN=sum, na.rm=TRUE)
df2015d1$dcoalpri <- df2015d1$dcoalpri/df2015d1$tmp # share of secciones with pri coalition in district
colnames(df2015d1)[which(colnames(df2015d1)=="dcoalpri")] <- "shSecCoalPri"
df2015d1$dcoalprd <- ave(df2015d1$dcoalprd, as.factor(df2015d1$edon*100+df2015d1$dis13.1), FUN=sum, na.rm=TRUE)
df2015d1$dcoalprd <- df2015d1$dcoalprd/df2015d1$tmp # share of secciones with prd coalition in district
colnames(df2015d1)[which(colnames(df2015d1)=="dcoalprd")] <- "shSecCoalPrd"
df2015d1$tmp <- NULL
df2015d1 <- df2015d1[duplicated(df2015d1$edon*100+df2015d1$dis13.1)==FALSE,]
df2015d1$shSecCoalPri[df2015d1$edon==9] ## debug: should have some districts with incomplete coalition...
#
# VOTES WITH DISTRICTS PROPOSED IN 2013 (scen. 3)
df2015d3 <- tmp[is.na(tmp$dis13.3)==FALSE,] # elimina las secciones no asignadas a distrito (posiblemente por reseccionamiento, habr? que recuperarlas)
df2015d3 <- df2015d3[order(df2015d3$edon, df2015d3$dis13.3),]
df2015d3$pan <- ave(df2015d3$pan, as.factor(df2015d3$edon*100+df2015d3$dis13.3), FUN=sum, na.rm=TRUE)
df2015d3$pri <- ave(df2015d3$pri, as.factor(df2015d3$edon*100+df2015d3$dis13.3), FUN=sum, na.rm=TRUE)
df2015d3$pric <- ave(df2015d3$pric, as.factor(df2015d3$edon*100+df2015d3$dis13.3), FUN=sum, na.rm=TRUE)
df2015d3$prd <- ave(df2015d3$prd, as.factor(df2015d3$edon*100+df2015d3$dis13.3), FUN=sum, na.rm=TRUE)
df2015d3$prdc <- ave(df2015d3$prdc, as.factor(df2015d3$edon*100+df2015d3$dis13.3), FUN=sum, na.rm=TRUE)
df2015d3$pt <- ave(df2015d3$pt, as.factor(df2015d3$edon*100+df2015d3$dis13.3), FUN=sum, na.rm=TRUE)
df2015d3$pvem <- ave(df2015d3$pvem, as.factor(df2015d3$edon*100+df2015d3$dis13.3), FUN=sum, na.rm=TRUE)
df2015d3$mc <- ave(df2015d3$mc, as.factor(df2015d3$edon*100+df2015d3$dis13.3), FUN=sum, na.rm=TRUE)
df2015d3$panal <- ave(df2015d3$panal, as.factor(df2015d3$edon*100+df2015d3$dis13.3), FUN=sum, na.rm=TRUE)
df2015d3$morena <- ave(df2015d3$morena, as.factor(df2015d3$edon*100+df2015d3$dis13.3), FUN=sum, na.rm=TRUE)
df2015d3$ph <- ave(df2015d3$ph, as.factor(df2015d3$edon*100+df2015d3$dis13.3), FUN=sum, na.rm=TRUE)
df2015d3$ps <- ave(df2015d3$ps, as.factor(df2015d3$edon*100+df2015d3$dis13.3), FUN=sum, na.rm=TRUE)
df2015d3$indep1 <- ave(df2015d3$indep1, as.factor(df2015d3$edon*100+df2015d3$dis13.3), FUN=sum, na.rm=TRUE)
df2015d3$indep2 <- ave(df2015d3$indep2, as.factor(df2015d3$edon*100+df2015d3$dis13.3), FUN=sum, na.rm=TRUE)
df2015d3$pri_pvem <- ave(df2015d3$pri_pvem, as.factor(df2015d3$edon*100+df2015d3$dis13.3), FUN=sum, na.rm=TRUE)
df2015d3$prd_pt <- ave(df2015d3$prd_pt, as.factor(df2015d3$edon*100+df2015d3$dis13.3), FUN=sum, na.rm=TRUE)
df2015d3$nr <- ave(df2015d3$nr, as.factor(df2015d3$edon*100+df2015d3$dis13.3), FUN=sum, na.rm=TRUE)
df2015d3$nul <- ave(df2015d3$nul, as.factor(df2015d3$edon*100+df2015d3$dis13.3), FUN=sum, na.rm=TRUE)
df2015d3$efec <- ave(df2015d3$efec, as.factor(df2015d3$edon*100+df2015d3$dis13.3), FUN=sum, na.rm=TRUE)
df2015d3$lisnom <- ave(df2015d3$lisnom, as.factor(df2015d3$edon*100+df2015d3$dis13.3), FUN=sum, na.rm=TRUE)
df2015d3$tmp <- rep(1, times = nrow(df2015d3))
df2015d3$tmp <- ave(df2015d3$tmp, as.factor(df2015d3$edon*100+df2015d3$dis13.3), FUN=sum, na.rm=TRUE)
df2015d3$dcoalpri <- ave(df2015d3$dcoalpri, as.factor(df2015d3$edon*100+df2015d3$dis13.3), FUN=sum, na.rm=TRUE)
df2015d3$dcoalpri <- df2015d3$dcoalpri/df2015d3$tmp # share of secciones with pri coalition in district
colnames(df2015d3)[which(colnames(df2015d3)=="dcoalpri")] <- "shSecCoalPri"
df2015d3$dcoalprd <- ave(df2015d3$dcoalprd, as.factor(df2015d3$edon*100+df2015d3$dis13.3), FUN=sum, na.rm=TRUE)
df2015d3$dcoalprd <- df2015d3$dcoalprd/df2015d3$tmp # share of secciones with prd coalition in district
colnames(df2015d3)[which(colnames(df2015d3)=="dcoalprd")] <- "shSecCoalPrd"
df2015d3$tmp <- NULL
df2015d3 <- df2015d3[duplicated(df2015d3$edon*100+df2015d3$dis13.3)==FALSE,]
df2015d3$shSecCoalPri[df2015d3$edon==9] ## debug: should have some districts with incomplete coalition...
#
# removes redundant columns
df2015d0 <- df2015d0[,c("edon","disn"   ,"pan","pri","pric","prd","prdc","pt","pvem","mc","panal","morena","ph","ps","pri_pvem","prd_pt","indep1","indep2","efec","nr","nul","lisnom","shSecCoalPri","shSecCoalPrd")]
df2015d1 <- df2015d1[,c("edon","dis13.1","pan","pri","pric","prd","prdc","pt","pvem","mc","panal","morena","ph","ps","pri_pvem","prd_pt","indep1","indep2","efec","nr","nul","lisnom","shSecCoalPri","shSecCoalPrd")]
df2015d3 <- df2015d3[,c("edon","dis13.3","pan","pri","pric","prd","prdc","pt","pvem","mc","panal","morena","ph","ps","pri_pvem","prd_pt","indep1","indep2","efec","nr","nul","lisnom","shSecCoalPri","shSecCoalPrd")]
#
colnames(df2015d0)[2] <- colnames(df2015d1)[2] <- colnames(df2015d3)[2] <- "disn"
#
dim(df2015d0)
dim(df2015d1)
dim(df2015d3)
head(df2015d0[df2015d0$edon==6,])
head(df2015d1)
head(df2015d3)
#
# agrega votos de coalicion pri donde las hubo con base en la proporcion de secciones con coalicion en el nuevo distrito
## df2015d0$pric[df2015d0$shSecCoalPri<.5] <- 0
df2015d0$pri_pvem <- NULL
df2015d0$prd_pt <- NULL
## df2015d0$pri[df2015d0$shSecCoalPri>=.5] <- 0
## df2015d0$pvem[df2015d0$shSecCoalPri>=.5] <- 0
#
#df2015d1$pripvem[df2015d1$shSecCoalPri<.5] # debug: need to assign >0 to pri and pvem in new districts coded as having no coalition
df2015d1$pric[df2015d1$shSecCoalPri<.5] <- 0
shrPri <- df2015d1$pri[df2015d1$shSecCoalPri<.5] / ( df2015d1$pri[df2015d1$shSecCoalPri<.5] + df2015d1$pvem[df2015d1$shSecCoalPri<.5] )
df2015d1$pri[df2015d1$shSecCoalPri<.5] <- df2015d1$pri[df2015d1$shSecCoalPri<.5] + shrPri * df2015d1$pri_pvem[df2015d1$shSecCoalPri<.5]
df2015d1$pvem[df2015d1$shSecCoalPri<.5] <- df2015d1$pvem[df2015d1$shSecCoalPri<.5] + (1-shrPri) * df2015d1$pri_pvem[df2015d1$shSecCoalPri<.5]
df2015d1$pri_pvem <- NULL
df2015d1$pri[df2015d1$shSecCoalPri>=.5] <- 0
df2015d1$pvem[df2015d1$shSecCoalPri>=.5] <- 0
#
df2015d1$prdc[df2015d1$shSecCoalPrd<.5] <- 0
shrPrd <- df2015d1$prd[df2015d1$shSecCoalPrd<.5] / ( df2015d1$prd[df2015d1$shSecCoalPrd<.5] + df2015d1$pt[df2015d1$shSecCoalPrd<.5] )
df2015d1$prd[df2015d1$shSecCoalPrd<.5] <- df2015d1$prd[df2015d1$shSecCoalPrd<.5] + shrPrd * df2015d1$prd_pt[df2015d1$shSecCoalPrd<.5]
df2015d1$pt[df2015d1$shSecCoalPrd<.5] <- df2015d1$pt[df2015d1$shSecCoalPrd<.5] + (1-shrPrd) * df2015d1$prd_pt[df2015d1$shSecCoalPrd<.5]
df2015d1$prd_pt <- NULL
df2015d1$prd[df2015d1$shSecCoalPrd>=.5] <- 0
df2015d1$pt[df2015d1$shSecCoalPrd>=.5] <- 0
#
df2015d3$pric[df2015d3$shSecCoalPri<.5] <- 0
shrPri <- df2015d3$pri[df2015d3$shSecCoalPri<.5] / ( df2015d3$pri[df2015d3$shSecCoalPri<.5] + df2015d3$pvem[df2015d3$shSecCoalPri<.5] )
df2015d3$pri[df2015d3$shSecCoalPri<.5] <- df2015d3$pri[df2015d3$shSecCoalPri<.5] + shrPri * df2015d3$pri_pvem[df2015d3$shSecCoalPri<.5]
df2015d3$pvem[df2015d3$shSecCoalPri<.5] <- df2015d3$pvem[df2015d3$shSecCoalPri<.5] + (1-shrPri) * df2015d3$pri_pvem[df2015d3$shSecCoalPri<.5]
df2015d3$pri_pvem <- NULL
df2015d3$pri[df2015d3$shSecCoalPri>=.5] <- 0
df2015d3$pvem[df2015d3$shSecCoalPri>=.5] <- 0
#
df2015d3$prdc[df2015d3$shSecCoalPrd<.5] <- 0
shrPrd <- df2015d3$prd[df2015d3$shSecCoalPrd<.5] / ( df2015d3$prd[df2015d3$shSecCoalPrd<.5] + df2015d3$pt[df2015d3$shSecCoalPrd<.5] )
df2015d3$prd[df2015d3$shSecCoalPrd<.5] <- df2015d3$prd[df2015d3$shSecCoalPrd<.5] + shrPrd * df2015d3$prd_pt[df2015d3$shSecCoalPrd<.5]
df2015d3$pt[df2015d3$shSecCoalPrd<.5] <- df2015d3$pt[df2015d3$shSecCoalPrd<.5] + (1-shrPrd) * df2015d3$prd_pt[df2015d3$shSecCoalPrd<.5]
df2015d3$prd_pt <- NULL
df2015d3$prd[df2015d3$shSecCoalPrd>=.5] <- 0
df2015d3$pt[df2015d3$shSecCoalPrd>=.5] <- 0
rm(shrPri,shrPrd)
df2015d1[df2015d1$edon==9,]
#
## winner (more compact method than used in 2012 and earlier)
# handy function to sort one data frame by order of another, matching data frame
sortBy <- function(target, By){
    t <- target; b <- By;
    do.call(rbind, lapply(seq_len(nrow(b)), 
            function(i) as.character(unlist(t[i,])[order(unlist(-b[i,]))]))) # change to -b if decreasing wished
}
## # example
## v <- data.frame(c1=c(30,15,3), c2=c(10,25,2), c3=c(20,35,4))
## w <- data.frame(c1=c("thirty","fifteen","three"), c2=c("ten","twenty-five","two"), c3=c("twenty","thirty-five","four"))
## v.sorted <- t(apply(v, 1, function(x) sort(x, decreasing = TRUE))) # sort each row of df -- http://stackoverflow.com/questions/6063881/sorting-rows-alphabetically
## w.sorted <- sortBy(target = w, By = v)
## sortBy(target = v, By = v)
#
## this sorts matrix rows faster than function above
# vot <- t(apply(vot, 1, function(x) sort(x, decreasing = TRUE)))
#colnames(df2015d0)
tmp <- c("pan", "pri", "pric", "prd", "prdc", "pt", "pvem", "mc", "panal", "morena", "ph", "ps","indep1","indep2")
tmpv <- df2015d0[,tmp]
tmpl <- matrix(rep(tmp,300), byrow=TRUE, nrow=300)
tmpl <- sortBy(target=tmpl, By=tmpv)
tmpv <- t(apply(tmpv, 1, function(x) sort(x, decreasing = TRUE)))
df2015d0$win <- tmpl[,1]
#table(tmpl[,1])
df2015d0$panw <- df2015d0$priw <- df2015d0$pricw <- df2015d0$prdw <- df2015d0$prdcw <- df2015d0$morenaw <- df2015d0$mcw <- df2015d0$panalw <- df2015d0$indepw <- rep(0, times=300)
df2015d0$panw[df2015d0$win=="pan"] <- 1
df2015d0$priw[df2015d0$win=="pri"] <- 1
df2015d0$pricw[df2015d0$win=="pric"] <- 1
df2015d0$prdw[df2015d0$win=="prd"] <- 1
df2015d0$prdcw[df2015d0$win=="prdc"] <- 1
df2015d0$morenaw[df2015d0$win=="morena"] <- 1
df2015d0$mcw[df2015d0$win=="mc"] <- 1
df2015d0$panalw[df2015d0$win=="panal"] <- 1
df2015d0$indepw[df2015d0$win=="indep1"] <- 1
## winner's margin
df2015d0$winmg <- (tmpv[,1] - tmpv[,2])/df2015d0$efec
#
colnames(df2015d1)
tmp <- c("pan", "pri", "pric", "prd", "prdc", "pt", "pvem", "mc", "panal", "morena", "ph", "ps","indep1","indep2")
tmpv <- df2015d1[,tmp]
tmpl <- matrix(rep(tmp,300), byrow=TRUE, nrow=300)
tmpl <- sortBy(target=tmpl, By=tmpv)
tmpv <- t(apply(tmpv, 1, function(x) sort(x, decreasing = TRUE)))
df2015d1$win <- tmpl[,1]
#table(tmpl[,1])
df2015d1$panw <- df2015d1$priw <- df2015d1$pricw <- df2015d1$prdw <- df2015d1$prdcw <- df2015d1$morenaw <- df2015d1$mcw <- df2015d1$panalw <- df2015d1$indepw <- rep(0, times=300)
df2015d1$panw[df2015d1$win=="pan"] <- 1
df2015d1$priw[df2015d1$win=="pri"] <- 1
df2015d1$pricw[df2015d1$win=="pric"] <- 1
df2015d1$prdw[df2015d1$win=="prd"] <- 1
df2015d1$prdcw[df2015d1$win=="prdc"] <- 1
df2015d1$morenaw[df2015d1$win=="morena"] <- 1
df2015d1$mcw[df2015d1$win=="mc"] <- 1
df2015d1$panalw[df2015d1$win=="panal"] <- 1
df2015d1$indepw[df2015d1$win=="indep1"] <- 1
## winner's margin
df2015d1$winmg <- (tmpv[,1] - tmpv[,2])/df2015d1$efec
#
tmp <- c("pan", "pri", "pric", "prd", "prdc", "pt", "pvem", "mc", "panal", "morena", "ph", "ps","indep1","indep2")
tmpv <- df2015d3[,tmp]
tmpl <- matrix(rep(tmp,300), byrow=TRUE, nrow=300)
tmpl <- sortBy(target=tmpl, By=tmpv)
tmpv <- t(apply(tmpv, 1, function(x) sort(x, decreasing = TRUE)))
df2015d3$win <- tmpl[,1]
#table(tmpl[,1])
df2015d3$panw <- df2015d3$priw <- df2015d3$pricw <- df2015d3$prdw <- df2015d3$prdcw <- df2015d3$morenaw <- df2015d3$mcw <- df2015d3$panalw <- df2015d3$indepw <- rep(0, times=300)
df2015d3$panw[df2015d3$win=="pan"] <- 1
df2015d3$priw[df2015d3$win=="pri"] <- 1
df2015d3$pricw[df2015d3$win=="pric"] <- 1
df2015d3$prdw[df2015d3$win=="prd"] <- 1
df2015d3$prdcw[df2015d3$win=="prdc"] <- 1
df2015d3$morenaw[df2015d3$win=="morena"] <- 1
df2015d3$mcw[df2015d3$win=="mc"] <- 1
df2015d3$panalw[df2015d3$win=="panal"] <- 1
df2015d3$indepw[df2015d3$win=="indep1"] <- 1
## winner's margin
df2015d3$winmg <- (tmpv[,1] - tmpv[,2])/df2015d3$efec
#
rm(tmp,tmpl,tmpv)
#
# debug
df2015d0[df2015d0$edon==6,]  # colima
df2015d1[df2015d1$edon==24,] # sinaloa
df2015d3[df2015d3$edon==24,] # sinaloa
#
# state aggregates statistics
df2015s0 <- df2015d0
df2015s0 <- df2015s0[order(df2015s0$edon),]
df2015s0$pan <- ave(df2015s0$pan, as.factor(df2015s0$edon), FUN=sum, na.rm=TRUE)
df2015s0$pri <- ave(df2015s0$pri, as.factor(df2015s0$edon), FUN=sum, na.rm=TRUE)
df2015s0$pric <- ave(df2015s0$pric, as.factor(df2015s0$edon), FUN=sum, na.rm=TRUE)
df2015s0$prd <- ave(df2015s0$prd, as.factor(df2015s0$edon), FUN=sum, na.rm=TRUE)
df2015s0$prdc <- ave(df2015s0$prdc, as.factor(df2015s0$edon), FUN=sum, na.rm=TRUE)
df2015s0$pt <- ave(df2015s0$pt, as.factor(df2015s0$edon), FUN=sum, na.rm=TRUE)
df2015s0$pvem <- ave(df2015s0$pvem, as.factor(df2015s0$edon), FUN=sum, na.rm=TRUE)
df2015s0$mc <- ave(df2015s0$mc, as.factor(df2015s0$edon), FUN=sum, na.rm=TRUE)
df2015s0$panal <- ave(df2015s0$panal, as.factor(df2015s0$edon), FUN=sum, na.rm=TRUE)
df2015s0$morena <- ave(df2015s0$morena, as.factor(df2015s0$edon), FUN=sum, na.rm=TRUE)
df2015s0$ph <- ave(df2015s0$ph, as.factor(df2015s0$edon), FUN=sum, na.rm=TRUE)
df2015s0$ps <- ave(df2015s0$ps, as.factor(df2015s0$edon), FUN=sum, na.rm=TRUE)
df2015s0$indep1 <- ave(df2015s0$indep1, as.factor(df2015s0$edon), FUN=sum, na.rm=TRUE)
df2015s0$indep2 <- ave(df2015s0$indep2, as.factor(df2015s0$edon), FUN=sum, na.rm=TRUE)
df2015s0$efec <- ave(df2015s0$efec, as.factor(df2015s0$edon), FUN=sum, na.rm=TRUE)
df2015s0$ndis <- rep(1, 300)
df2015s0$ndis <- ave(df2015s0$ndis, as.factor(df2015s0$edon), FUN=sum, na.rm=TRUE)
df2015s0$panw <- ave(df2015s0$panw, as.factor(df2015s0$edon), FUN=sum, na.rm=TRUE)
df2015s0$priw <- ave(df2015s0$priw, as.factor(df2015s0$edon), FUN=sum, na.rm=TRUE)
df2015s0$pricw <- ave(df2015s0$pricw, as.factor(df2015s0$edon), FUN=sum, na.rm=TRUE)
df2015s0$prdw <- ave(df2015s0$prdw, as.factor(df2015s0$edon), FUN=sum, na.rm=TRUE)
df2015s0$prdcw <- ave(df2015s0$prdcw, as.factor(df2015s0$edon), FUN=sum, na.rm=TRUE)
df2015s0$morenaw <- ave(df2015s0$morenaw, as.factor(df2015s0$edon), FUN=sum, na.rm=TRUE)
df2015s0$mcw <- ave(df2015s0$mcw, as.factor(df2015s0$edon), FUN=sum, na.rm=TRUE)
df2015s0$panalw <- ave(df2015s0$panalw, as.factor(df2015s0$edon), FUN=sum, na.rm=TRUE)
df2015s0$indepw <- ave(df2015s0$indepw, as.factor(df2015s0$edon), FUN=sum, na.rm=TRUE)
df2015s0 <- df2015s0[duplicated(df2015s0$edon)==FALSE,]
tmp <- df2015s0; tmp[, grep("disn|win|shSec", colnames(tmp))] <- NULL; df2015s0 <- tmp # drop useless columns
#
# shares
df2015s0$pansh   <- df2015s0$pan/df2015s0$efec
df2015s0$prish   <- df2015s0$pri/df2015s0$efec
df2015s0$pricsh  <- df2015s0$pric/df2015s0$efec
df2015s0$prdsh   <- df2015s0$prd/df2015s0$efec
df2015s0$prdcsh  <- df2015s0$prdc/df2015s0$efec
df2015s0$ptsh    <- df2015s0$pt/df2015s0$efec
df2015s0$pvemsh  <- df2015s0$pvem/df2015s0$efec
df2015s0$mcsh    <- df2015s0$mc/df2015s0$efec
df2015s0$panalsh <- df2015s0$panal/df2015s0$efec
df2015s0$morenash  <- df2015s0$morena/df2015s0$efec
df2015s0$phsh  <- df2015s0$ph/df2015s0$efec
df2015s0$pssh  <- df2015s0$ps/df2015s0$efec
df2015s0$indep1sh  <- df2015s0$indep1/df2015s0$efec
df2015s0$indep2sh  <- df2015s0$indep2/df2015s0$efec
#
df2015s0$panw   <- df2015s0$panw/df2015s0$ndis
df2015s0$priw   <- df2015s0$priw/df2015s0$ndis
df2015s0$pricw  <- df2015s0$pricw/df2015s0$ndis
df2015s0$prdw   <- df2015s0$prdw/df2015s0$ndis
df2015s0$prdcw  <- df2015s0$prdcw/df2015s0$ndis
df2015s0$mcw    <- df2015s0$mcw/df2015s0$ndis
df2015s0$panalw <- df2015s0$panalw/df2015s0$ndis
df2015s0$morenaw   <- df2015s0$morenaw/df2015s0$ndis
df2015s0$indepw   <- df2015s0$indepw/df2015s0$ndis
#df2015s0$shDisCoalPri <-  df2015s0$shDisCoalPri/df2015s0$ndis
#df2015s0$shDisCoalPrd <-  df2015s0$shDisCoalPrd/df2015s0$ndis
head(df2015s0)
#
df2015s1 <- df2015d1
df2015s1 <- df2015s1[order(df2015s1$edon),]
## # WILL DROP THIS STEP THAT FORCES STATES WITH PARTIAL COALITION TO REPORT ALL VOTES IN PRI OR IN PRIC... MAYBE NEEDED TO COMPUTE PRI BIAS?
## colnames(df2015s1)[which(colnames(df2015s1)=="shSecCoalPri")] <- "shDisCoalPri"
## df2015s1$shDisCoalPri <- rep(0, 300)
## df2015s1$shDisCoalPri[df2015s1$pric>0] <- 1
## df2015s1$shDisCoalPri <- ave(df2015s1$shDisCoalPri, as.factor(df2015s1$edon), FUN=sum, na.rm=TRUE)
df2015s1$pan <- ave(df2015s1$pan, as.factor(df2015s1$edon), FUN=sum, na.rm=TRUE)
df2015s1$pri <- ave(df2015s1$pri, as.factor(df2015s1$edon), FUN=sum, na.rm=TRUE)
df2015s1$pric <- ave(df2015s1$pric, as.factor(df2015s1$edon), FUN=sum, na.rm=TRUE)
df2015s1$prd <- ave(df2015s1$prd, as.factor(df2015s1$edon), FUN=sum, na.rm=TRUE)
df2015s1$prdc <- ave(df2015s1$prdc, as.factor(df2015s1$edon), FUN=sum, na.rm=TRUE)
df2015s1$pt <- ave(df2015s1$pt, as.factor(df2015s1$edon), FUN=sum, na.rm=TRUE)
df2015s1$pvem <- ave(df2015s1$pvem, as.factor(df2015s1$edon), FUN=sum, na.rm=TRUE)
df2015s1$mc <- ave(df2015s1$mc, as.factor(df2015s1$edon), FUN=sum, na.rm=TRUE)
df2015s1$panal <- ave(df2015s1$panal, as.factor(df2015s1$edon), FUN=sum, na.rm=TRUE)
df2015s1$morena <- ave(df2015s1$morena, as.factor(df2015s1$edon), FUN=sum, na.rm=TRUE)
df2015s1$ph <- ave(df2015s1$ph, as.factor(df2015s1$edon), FUN=sum, na.rm=TRUE)
df2015s1$ps <- ave(df2015s1$ps, as.factor(df2015s1$edon), FUN=sum, na.rm=TRUE)
df2015s1$indep1 <- ave(df2015s1$indep1, as.factor(df2015s1$edon), FUN=sum, na.rm=TRUE)
df2015s1$indep2 <- ave(df2015s1$indep2, as.factor(df2015s1$edon), FUN=sum, na.rm=TRUE)
df2015s1$efec <- ave(df2015s1$efec, as.factor(df2015s1$edon), FUN=sum, na.rm=TRUE)
df2015s1$ndis <- rep(1, 300)
df2015s1$ndis <- ave(df2015s1$ndis, as.factor(df2015s1$edon), FUN=sum, na.rm=TRUE)
df2015s1$panw <- ave(df2015s1$panw, as.factor(df2015s1$edon), FUN=sum, na.rm=TRUE)
df2015s1$priw <- ave(df2015s1$priw, as.factor(df2015s1$edon), FUN=sum, na.rm=TRUE)
df2015s1$pricw <- ave(df2015s1$pricw, as.factor(df2015s1$edon), FUN=sum, na.rm=TRUE)
df2015s1$prdw <- ave(df2015s1$prdw, as.factor(df2015s1$edon), FUN=sum, na.rm=TRUE)
df2015s1$prdcw <- ave(df2015s1$prdcw, as.factor(df2015s1$edon), FUN=sum, na.rm=TRUE)
df2015s1$morenaw <- ave(df2015s1$morenaw, as.factor(df2015s1$edon), FUN=sum, na.rm=TRUE)
df2015s1$mcw <- ave(df2015s1$mcw, as.factor(df2015s1$edon), FUN=sum, na.rm=TRUE)
df2015s1$panalw <- ave(df2015s1$panalw, as.factor(df2015s1$edon), FUN=sum, na.rm=TRUE)
df2015s1$indepw <- ave(df2015s1$indepw, as.factor(df2015s1$edon), FUN=sum, na.rm=TRUE)
df2015s1 <- df2015s1[duplicated(df2015s1$edon)==FALSE,]
tmp <- df2015s1; tmp[, grep("disn|win|shSec", colnames(tmp))] <- NULL; df2015s1 <- tmp # drop useless columns
#
# shares
df2015s1$pansh   <- df2015s1$pan/df2015s1$efec
df2015s1$prish   <- df2015s1$pri/df2015s1$efec
df2015s1$pricsh  <- df2015s1$pric/df2015s1$efec
df2015s1$prdsh   <- df2015s1$prd/df2015s1$efec
df2015s1$prdcsh  <- df2015s1$prdc/df2015s1$efec
df2015s1$ptsh    <- df2015s1$pt/df2015s1$efec
df2015s1$pvemsh  <- df2015s1$pvem/df2015s1$efec
df2015s1$mcsh    <- df2015s1$mc/df2015s1$efec
df2015s1$panalsh <- df2015s1$panal/df2015s1$efec
df2015s1$morenash  <- df2015s1$morena/df2015s1$efec
df2015s1$phsh  <- df2015s1$ph/df2015s1$efec
df2015s1$pssh  <- df2015s1$ps/df2015s1$efec
df2015s1$indep1sh  <- df2015s1$indep1/df2015s1$efec
df2015s1$indep2sh  <- df2015s1$indep2/df2015s1$efec
#
df2015s1$panw   <- df2015s1$panw/df2015s1$ndis
df2015s1$priw   <- df2015s1$priw/df2015s1$ndis
df2015s1$pricw  <- df2015s1$pricw/df2015s1$ndis
df2015s1$prdw   <- df2015s1$prdw/df2015s1$ndis
df2015s1$prdcw  <- df2015s1$prdcw/df2015s1$ndis
df2015s1$mcw    <- df2015s1$mcw/df2015s1$ndis
df2015s1$panalw <- df2015s1$panalw/df2015s1$ndis
df2015s1$morenaw   <- df2015s1$morenaw/df2015s1$ndis
df2015s1$indepw   <- df2015s1$indepw/df2015s1$ndis
#df2015s1$shDisCoalPri <-  df2015s1$shDisCoalPri/df2015s1$ndis
#df2015s1$shDisCoalPrd <-  df2015s1$shDisCoalPrd/df2015s1$ndis
head(df2015s1)
#
df2015s3 <- df2015d3
df2015s3 <- df2015s3[order(df2015s3$edon),]
df2015s3$pan <- ave(df2015s3$pan, as.factor(df2015s3$edon), FUN=sum, na.rm=TRUE)
df2015s3$pri <- ave(df2015s3$pri, as.factor(df2015s3$edon), FUN=sum, na.rm=TRUE)
df2015s3$pric <- ave(df2015s3$pric, as.factor(df2015s3$edon), FUN=sum, na.rm=TRUE)
df2015s3$prd <- ave(df2015s3$prd, as.factor(df2015s3$edon), FUN=sum, na.rm=TRUE)
df2015s3$prdc <- ave(df2015s3$prdc, as.factor(df2015s3$edon), FUN=sum, na.rm=TRUE)
df2015s3$pt <- ave(df2015s3$pt, as.factor(df2015s3$edon), FUN=sum, na.rm=TRUE)
df2015s3$pvem <- ave(df2015s3$pvem, as.factor(df2015s3$edon), FUN=sum, na.rm=TRUE)
df2015s3$mc <- ave(df2015s3$mc, as.factor(df2015s3$edon), FUN=sum, na.rm=TRUE)
df2015s3$panal <- ave(df2015s3$panal, as.factor(df2015s3$edon), FUN=sum, na.rm=TRUE)
df2015s3$morena <- ave(df2015s3$morena, as.factor(df2015s3$edon), FUN=sum, na.rm=TRUE)
df2015s3$ph <- ave(df2015s3$ph, as.factor(df2015s3$edon), FUN=sum, na.rm=TRUE)
df2015s3$ps <- ave(df2015s3$ps, as.factor(df2015s3$edon), FUN=sum, na.rm=TRUE)
df2015s3$indep1 <- ave(df2015s3$indep1, as.factor(df2015s3$edon), FUN=sum, na.rm=TRUE)
df2015s3$indep2 <- ave(df2015s3$indep2, as.factor(df2015s3$edon), FUN=sum, na.rm=TRUE)
df2015s3$efec <- ave(df2015s3$efec, as.factor(df2015s3$edon), FUN=sum, na.rm=TRUE)
df2015s3$ndis <- rep(1, 300)
df2015s3$ndis <- ave(df2015s3$ndis, as.factor(df2015s3$edon), FUN=sum, na.rm=TRUE)
df2015s3$panw <- ave(df2015s3$panw, as.factor(df2015s3$edon), FUN=sum, na.rm=TRUE)
df2015s3$priw <- ave(df2015s3$priw, as.factor(df2015s3$edon), FUN=sum, na.rm=TRUE)
df2015s3$pricw <- ave(df2015s3$pricw, as.factor(df2015s3$edon), FUN=sum, na.rm=TRUE)
df2015s3$prdw <- ave(df2015s3$prdw, as.factor(df2015s3$edon), FUN=sum, na.rm=TRUE)
df2015s3$prdcw <- ave(df2015s3$prdcw, as.factor(df2015s3$edon), FUN=sum, na.rm=TRUE)
df2015s3$morenaw <- ave(df2015s3$morenaw, as.factor(df2015s3$edon), FUN=sum, na.rm=TRUE)
df2015s3$mcw <- ave(df2015s3$mcw, as.factor(df2015s3$edon), FUN=sum, na.rm=TRUE)
df2015s3$panalw <- ave(df2015s3$panalw, as.factor(df2015s3$edon), FUN=sum, na.rm=TRUE)
df2015s3$indepw <- ave(df2015s3$indepw, as.factor(df2015s3$edon), FUN=sum, na.rm=TRUE)
df2015s3 <- df2015s3[duplicated(df2015s3$edon)==FALSE,]
tmp <- df2015s3; tmp[, grep("disn|win|shSec", colnames(tmp))] <- NULL; df2015s3 <- tmp # drop useless columns
#
# shares
df2015s3$pansh   <- df2015s3$pan/df2015s3$efec
df2015s3$prish   <- df2015s3$pri/df2015s3$efec
df2015s3$pricsh  <- df2015s3$pric/df2015s3$efec
df2015s3$prdsh   <- df2015s3$prd/df2015s3$efec
df2015s3$prdcsh  <- df2015s3$prdc/df2015s3$efec
df2015s3$ptsh    <- df2015s3$pt/df2015s3$efec
df2015s3$pvemsh  <- df2015s3$pvem/df2015s3$efec
df2015s3$mcsh    <- df2015s3$mc/df2015s3$efec
df2015s3$panalsh <- df2015s3$panal/df2015s3$efec
df2015s3$morenash  <- df2015s3$morena/df2015s3$efec
df2015s3$phsh  <- df2015s3$ph/df2015s3$efec
df2015s3$pssh  <- df2015s3$ps/df2015s3$efec
df2015s3$indep1sh  <- df2015s3$indep1/df2015s3$efec
df2015s3$indep2sh  <- df2015s3$indep2/df2015s3$efec
#
df2015s3$panw   <- df2015s3$panw/df2015s3$ndis
df2015s3$priw   <- df2015s3$priw/df2015s3$ndis
df2015s3$pricw  <- df2015s3$pricw/df2015s3$ndis
df2015s3$prdw   <- df2015s3$prdw/df2015s3$ndis
df2015s3$prdcw  <- df2015s3$prdcw/df2015s3$ndis
df2015s3$mcw    <- df2015s3$mcw/df2015s3$ndis
df2015s3$panalw <- df2015s3$panalw/df2015s3$ndis
df2015s3$morenaw   <- df2015s3$morenaw/df2015s3$ndis
df2015s3$indepw   <- df2015s3$indepw/df2015s3$ndis
#df2015s3$shDisCoalPri <-  df2015s3$shDisCoalPri/df2015s3$ndis
#df2015s3$shDisCoalPrd <-  df2015s3$shDisCoalPrd/df2015s3$ndis
head(df2015s3)
## hasta aqu? 11may16 ##
#
##############
# 2012 votes #
##############
tmp <- read.csv( paste(dd, "dfSeccion2012.csv", sep=""), header=TRUE)
tmp <- tmp[order(tmp$edon, tmp$disn, tmp$seccion),]
# compute effective vote (without void ballots)
tmp$efec <- tmp$pan + tmp$pri + tmp$prd + tmp$pvem + tmp$pt + tmp$mc + tmp$panal + tmp$pripvem + tmp$prdptmc + tmp$prdpt + tmp$prdmc + tmp$ptmc
head(tmp[tmp$edon==1,])
# aggregates coalition votes where needed
tmp$prdc <- tmp$prd+tmp$pt+tmp$mc+tmp$prdptmc+tmp$prdpt+tmp$prdmc+tmp$ptmc
tmp$prd <- tmp$pt <- tmp$mc <- tmp$prdptmc <- tmp$prdpt <- tmp$prdmc <- tmp$ptmc <- NULL # UNLIKE PRI's AND PVEM's PARTIAL COALITIONS, PRD HAD NATIONAL COALITION AND THESE ARE REDUNDANT
#
tmp$dcoalpri <- rep(0, times=nrow(tmp))
tmp$dcoalpri[tmp$pripvem>0] <- 1 # misses sections with coalition but no joint pri-pvem vote... next lines fix this
#table(tmp$dcoalpri) # debug
tmp1 <- ave(tmp$dcoalpri, as.factor(tmp$edon*100+tmp$disn), FUN=sum, na.rm=TRUE)
tmp2 <- ave(rep(1,length(tmp$dcoalpri)), as.factor(tmp$edon*100+tmp$disn), FUN=sum, na.rm=TRUE)
tmp3 <- tmp1/tmp2
tmp$dcoalpri[tmp3>.5] <- 1
rm(tmp1,tmp2,tmp3)
tmp$pric <- tmp$pri+tmp$pvem+tmp$pripvem
#
#
tmp$edon.secn <- tmp$edon*10000 + tmp$seccion
dis2013$edon.secn <- dis2013$edon*10000 + dis2013$seccion
tmp1 <- dis2013[,c("edon.secn","dis1er13","dis3er13")]
tmp <- merge(x = tmp, y = tmp1, by = "edon.secn", all.x = TRUE)
colnames(tmp)[which(colnames(tmp)=="dis1er13")] <- "dis13.1"
colnames(tmp)[which(colnames(tmp)=="dis3er13")] <- "dis13.3"
rm(tmp1)
#
# OJO: some secciones used in 2012 do not appear listed in IFE's 2013 redistricting scenarios. List of such secciones follows
# 11may16: fixed
select <- which(dis2013$dis1er13>0 & dis2013$dis3er13==0)
data.frame(edon=dis2013$edon[select], seccion=dis2013$seccion[select])
#data.frame(edon=dis2013$edon[is.na(dis2013$dis3er13)==TRUE], seccion=dis2013$seccion[is.na(dis2013$dis3er13)==TRUE])
dim(tmp); dim(dis2013)
#
colnames(tmp)
## Aggregates 2012 results by district
df2012d0 <- tmp[order(tmp$edon, tmp$disn),] # resultados con distritos reales (2006 map)
df2012d0$pan <- ave(df2012d0$pan, as.factor(df2012d0$edon*100+df2012d0$disn), FUN=sum, na.rm=TRUE)
df2012d0$pri <- ave(df2012d0$pri, as.factor(df2012d0$edon*100+df2012d0$disn), FUN=sum, na.rm=TRUE)
df2012d0$pric <- ave(df2012d0$pric, as.factor(df2012d0$edon*100+df2012d0$disn), FUN=sum, na.rm=TRUE)
df2012d0$prdc <- ave(df2012d0$prdc, as.factor(df2012d0$edon*100+df2012d0$disn), FUN=sum, na.rm=TRUE)
df2012d0$pvem <- ave(df2012d0$pvem, as.factor(df2012d0$edon*100+df2012d0$disn), FUN=sum, na.rm=TRUE)
df2012d0$panal <- ave(df2012d0$panal, as.factor(df2012d0$edon*100+df2012d0$disn), FUN=sum, na.rm=TRUE)
df2012d0$pripvem <- ave(df2012d0$pripvem, as.factor(df2012d0$edon*100+df2012d0$disn), FUN=sum, na.rm=TRUE)
df2012d0$nr <- ave(df2012d0$nr, as.factor(df2012d0$edon*100+df2012d0$disn), FUN=sum, na.rm=TRUE)
df2012d0$nul <- ave(df2012d0$nul, as.factor(df2012d0$edon*100+df2012d0$disn), FUN=sum, na.rm=TRUE)
df2012d0$efec <- ave(df2012d0$efec, as.factor(df2012d0$edon*100+df2012d0$disn), FUN=sum, na.rm=TRUE)
df2012d0$lisnom <- ave(df2012d0$lisnom, as.factor(df2012d0$edon*100+df2012d0$disn), FUN=sum, na.rm=TRUE)
df2012d0$tmp <- rep(1, times = nrow(df2012d0))
df2012d0$tmp <- ave(df2012d0$tmp, as.factor(df2012d0$edon*100+df2012d0$disn), FUN=sum, na.rm=TRUE)
df2012d0$dcoalpri <- ave(df2012d0$dcoalpri, as.factor(df2012d0$edon*100+df2012d0$disn), FUN=sum, na.rm=TRUE)
df2012d0$dcoalpri <- df2012d0$dcoalpri/df2012d0$tmp # share of secciones with pri coalition in district
colnames(df2012d0)[which(colnames(df2012d0)=="dcoalpri")] <- "shSecCoalPri"
df2012d0$tmp <- NULL
df2012d0 <- df2012d0[duplicated(df2012d0$edon*100+df2012d0$disn)==FALSE,]
# df2012d0$shSecCoalPri ## debug: should now be 0 or 1 after fix used above
df2012d0$pripvem[df2012d0$shSecCoalPri==0] # debug: no need to assign, all == 0
df2012d0$pric[df2012d0$shSecCoalPri==0] <- 0
df2012d0$pri[df2012d0$shSecCoalPri==1] <- 0
df2012d0$pvem[df2012d0$shSecCoalPri==1] <- 0
#df2012d0$pripvem <- NULL
df2012d0[df2012d0$edon==24,]
#
# RESULTADOS CON LOS DISTRITOS PROPUESTOS EN 2013 (escen. 1)
df2012d1 <- tmp[is.na(tmp$dis13.1)==FALSE,] # elimina las secciones no asignadas a distrito (posiblemente por reseccionamiento, habr? que recuperarlas)
df2012d1 <- df2012d1[order(df2012d1$edon, df2012d1$dis13.1),]
df2012d1$pan <- ave(df2012d1$pan, as.factor(df2012d1$edon*100+df2012d1$dis13.1), FUN=sum, na.rm=TRUE)
df2012d1$pri <- ave(df2012d1$pri, as.factor(df2012d1$edon*100+df2012d1$dis13.1), FUN=sum, na.rm=TRUE)
df2012d1$pric <- ave(df2012d1$pric, as.factor(df2012d1$edon*100+df2012d1$dis13.1), FUN=sum, na.rm=TRUE)
df2012d1$prdc <- ave(df2012d1$prdc, as.factor(df2012d1$edon*100+df2012d1$dis13.1), FUN=sum, na.rm=TRUE)
df2012d1$pvem <- ave(df2012d1$pvem, as.factor(df2012d1$edon*100+df2012d1$dis13.1), FUN=sum, na.rm=TRUE)
df2012d1$panal <- ave(df2012d1$panal, as.factor(df2012d1$edon*100+df2012d1$dis13.1), FUN=sum, na.rm=TRUE)
df2012d1$pripvem <- ave(df2012d1$pripvem, as.factor(df2012d1$edon*100+df2012d1$dis13.1), FUN=sum, na.rm=TRUE)
df2012d1$nr <- ave(df2012d1$nr, as.factor(df2012d1$edon*100+df2012d1$dis13.1), FUN=sum, na.rm=TRUE)
df2012d1$nul <- ave(df2012d1$nul, as.factor(df2012d1$edon*100+df2012d1$dis13.1), FUN=sum, na.rm=TRUE)
df2012d1$efec <- ave(df2012d1$efec, as.factor(df2012d1$edon*100+df2012d1$dis13.1), FUN=sum, na.rm=TRUE)
df2012d1$lisnom <- ave(df2012d1$lisnom, as.factor(df2012d1$edon*100+df2012d1$dis13.1), FUN=sum, na.rm=TRUE)
df2012d1$tmp <- rep(1, times = nrow(df2012d1))
df2012d1$tmp <- ave(df2012d1$tmp, as.factor(df2012d1$edon*100+df2012d1$dis13.1), FUN=sum, na.rm=TRUE)
df2012d1$dcoalpri <- ave(df2012d1$dcoalpri, as.factor(df2012d1$edon*100+df2012d1$dis13.1), FUN=sum, na.rm=TRUE)
df2012d1$dcoalpri <- df2012d1$dcoalpri/df2012d1$tmp # share of secciones with pri coalition in district
## table(df2012d1$dcoalpri[df2012d1$edon==24]) # debug
## data.frame(coal=df2012d1$dcoalpri[df2012d1$edon==24], dis0=df2012d1$disn[df2012d1$edon==24], dis1=df2012d1$dis13.1[df2012d1$edon==24]) # debug
colnames(df2012d1)[which(colnames(df2012d1)=="dcoalpri")] <- "shSecCoalPri"
df2012d1$tmp <- NULL
df2012d1 <- df2012d1[duplicated(df2012d1$edon*100+df2012d1$dis13.1)==FALSE,]
df2012d1$shSecCoalPri[df2012d1$edon==24] ## debug: should have some districts with incomplete coalition...
#
# RESULTADOS CON LOS DISTRITOS PROPUESTOS EN 2013 (escen. 3)
df2012d3 <- tmp[is.na(tmp$dis13.3)==FALSE,] # elimina las secciones no asignadas a distrito (posiblemente por reseccionamiento, habr? que recuperarlas)
df2012d3 <- df2012d3[order(df2012d3$edon, df2012d3$dis13.3),]
df2012d3$pan <- ave(df2012d3$pan, as.factor(df2012d3$edon*100+df2012d3$dis13.3), FUN=sum, na.rm=TRUE)
df2012d3$pri <- ave(df2012d3$pri, as.factor(df2012d3$edon*100+df2012d3$dis13.3), FUN=sum, na.rm=TRUE)
df2012d3$pric <- ave(df2012d3$pric, as.factor(df2012d3$edon*100+df2012d3$dis13.3), FUN=sum, na.rm=TRUE)
df2012d3$prdc <- ave(df2012d3$prdc, as.factor(df2012d3$edon*100+df2012d3$dis13.3), FUN=sum, na.rm=TRUE)
df2012d3$pvem <- ave(df2012d3$pvem, as.factor(df2012d3$edon*100+df2012d3$dis13.3), FUN=sum, na.rm=TRUE)
df2012d3$panal <- ave(df2012d3$panal, as.factor(df2012d3$edon*100+df2012d3$dis13.3), FUN=sum, na.rm=TRUE)
df2012d3$pripvem <- ave(df2012d3$pripvem, as.factor(df2012d3$edon*100+df2012d3$dis13.3), FUN=sum, na.rm=TRUE)
df2012d3$nr <- ave(df2012d3$nr, as.factor(df2012d3$edon*100+df2012d3$dis13.3), FUN=sum, na.rm=TRUE)
df2012d3$nul <- ave(df2012d3$nul, as.factor(df2012d3$edon*100+df2012d3$dis13.3), FUN=sum, na.rm=TRUE)
df2012d3$efec <- ave(df2012d3$efec, as.factor(df2012d3$edon*100+df2012d3$dis13.3), FUN=sum, na.rm=TRUE)
df2012d3$lisnom <- ave(df2012d3$lisnom, as.factor(df2012d3$edon*100+df2012d3$dis13.3), FUN=sum, na.rm=TRUE)
df2012d3$tmp <- rep(1, times = nrow(df2012d3))
df2012d3$tmp <- ave(df2012d3$tmp, as.factor(df2012d3$edon*100+df2012d3$dis13.3), FUN=sum, na.rm=TRUE)
df2012d3$dcoalpri <- ave(df2012d3$dcoalpri, as.factor(df2012d3$edon*100+df2012d3$dis13.3), FUN=sum, na.rm=TRUE)
df2012d3$dcoalpri <- df2012d3$dcoalpri/df2012d3$tmp # share of secciones with pri coalition in district
df2012d3$dcoalpri[df2012d3$edon==24]
# debug
## data.frame(a=df2012d3$disn[df2012d3$edon==24], b=df2012d3$dis13.1[df2012d3$edon==24], c=df2012d3$dis13.3[df2012d3$edon==24], d=df2012d3$dcoalpri[df2012d3$edon==24])
## table(df2012d3$disn[df2012d3$edon==24], df2012d3$dcoalpri[df2012d3$edon==24])
## table(df2012d3$dis13.1[df2012d3$edon==24], df2012d3$dcoalpri[df2012d3$edon==24])
## table(df2012d3$dis13.3[df2012d3$edon==24], df2012d3$dcoalpri[df2012d3$edon==24])
colnames(df2012d3)[which(colnames(df2012d3)=="dcoalpri")] <- "shSecCoalPri"
df2012d3$tmp <- NULL
df2012d3 <- df2012d3[duplicated(df2012d3$edon*100+df2012d3$dis13.3)==FALSE,]
df2012d3$shSecCoalPri ## debug: should have some districts with incomplete coalition...
df2012d3[df2012d3$edon==24,] # debug
#
# removes redundant columns
df2012d0 <- df2012d0[,c("edon","disn"   ,"pan","pri","pric","prdc","pvem","panal","pripvem","efec","nr","nul","lisnom","shSecCoalPri")]
df2012d1 <- df2012d1[,c("edon","dis13.1","pan","pri","pric","prdc","pvem","panal","pripvem","efec","nr","nul","lisnom","shSecCoalPri")]
df2012d3 <- df2012d3[,c("edon","dis13.3","pan","pri","pric","prdc","pvem","panal","pripvem","efec","nr","nul","lisnom","shSecCoalPri")]
#
colnames(df2012d0)[2] <- colnames(df2012d1)[2] <- colnames(df2012d3)[2] <- "disn"
#
dim(df2012d0)
dim(df2012d1)
dim(df2012d3)
head(df2012d0[df2012d0$edon==6,])
head(df2012d1)
head(df2012d3)
#
# agrega votos de coalicion pri donde las hubo con base en la proporcion de secciones con coalicion en el nuevo distrito
## df2012d0$pric[df2012d0$shSecCoalPri<.5] <- 0
df2012d0$pripvem <- NULL
## df2012d0$pri[df2012d0$shSecCoalPri>=.5] <- 0
## df2012d0$pvem[df2012d0$shSecCoalPri>=.5] <- 0
#
#df2012d1$pripvem[df2012d1$shSecCoalPri<.5] # debug: need to assign >0 to pri and pvem in new districts coded as having no coalition
df2012d1$pric[df2012d1$shSecCoalPri<.5] <- 0
shrPri <- df2012d1$pri[df2012d1$shSecCoalPri<.5] / ( df2012d1$pri[df2012d1$shSecCoalPri<.5] + df2012d1$pvem[df2012d1$shSecCoalPri<.5] )
df2012d1$pri[df2012d1$shSecCoalPri<.5] <- df2012d1$pri[df2012d1$shSecCoalPri<.5] + shrPri * df2012d1$pripvem[df2012d1$shSecCoalPri<.5]
df2012d1$pvem[df2012d1$shSecCoalPri<.5] <- df2012d1$pvem[df2012d1$shSecCoalPri<.5] + (1-shrPri) * df2012d1$pripvem[df2012d1$shSecCoalPri<.5]
df2012d1$pripvem <- NULL
df2012d1$pri[df2012d1$shSecCoalPri>=.5] <- 0
df2012d1$pvem[df2012d1$shSecCoalPri>=.5] <- 0
#
df2012d3$pric[df2012d3$shSecCoalPri<.5] <- 0
shrPri <- df2012d3$pri[df2012d3$shSecCoalPri<.5] / ( df2012d3$pri[df2012d3$shSecCoalPri<.5] + df2012d3$pvem[df2012d3$shSecCoalPri<.5] )
df2012d3$pri[df2012d3$shSecCoalPri<.5] <- df2012d3$pri[df2012d3$shSecCoalPri<.5] + shrPri * df2012d3$pripvem[df2012d3$shSecCoalPri<.5]
df2012d3$pvem[df2012d3$shSecCoalPri<.5] <- df2012d3$pvem[df2012d3$shSecCoalPri<.5] + (1-shrPri) * df2012d3$pripvem[df2012d3$shSecCoalPri<.5]
df2012d3$pripvem <- NULL
df2012d3$pri[df2012d3$shSecCoalPri>=.5] <- 0
df2012d3$pvem[df2012d3$shSecCoalPri>=.5] <- 0
rm(shrPri)
df2012d3[df2012d3$edon==24,]
#
## winner
tmp <- rep(0, times=300)
df2012d0$panw <- df2012d0$priw <- df2012d0$pricw <- df2012d0$prdcw <- df2012d0$pvemw <- df2012d0$panalw <- tmp
tmp <- apply( df2012d0[,c("pan", "pri", "pric", "prdc", "pvem", "panal")], MARGIN = 1, max)
df2012d0$panw[df2012d0$pan==tmp] <- 1
df2012d0$priw[df2012d0$pri==tmp] <- 1
df2012d0$pricw[df2012d0$pric==tmp] <- 1
df2012d0$prdcw[df2012d0$prdc==tmp] <- 1
df2012d0$pvemw[df2012d0$pvem==tmp] <- 1
df2012d0$panalw[df2012d0$panal==tmp] <- 1
## winner's margin
tmp <- apply( df2012d0[,c("pan", "pri", "pric", "prdc", "pvem", "panal")], MARGIN = 1, max) - apply( df2012d0[,c("pan", "pri", "pric", "prdc", "pvem", "panal")], MARGIN = 1, function(x) sort(x,partial=length(x)-1)[length(x)-1])
df2012d0$winmg <- tmp/df2012d0$efec
#
tmp <- rep(0, times=300)
df2012d1$panw <- df2012d1$priw <- df2012d1$pricw <- df2012d1$prdcw <- df2012d1$pvemw <- df2012d1$panalw <- tmp
tmp <- apply( df2012d1[,c("pan", "pri", "pric", "prdc", "pvem", "panal")], MARGIN = 1, max)
df2012d1$panw[df2012d1$pan==tmp] <- 1
df2012d1$priw[df2012d1$pri==tmp] <- 1
df2012d1$pricw[df2012d1$pric==tmp] <- 1
df2012d1$prdcw[df2012d1$prdc==tmp] <- 1
df2012d1$pvemw[df2012d1$pvem==tmp] <- 1
df2012d1$panalw[df2012d1$panal==tmp] <- 1
## winner's margin
tmp <- apply( df2012d1[,c("pan", "pri", "pric", "prdc", "pvem", "panal")], MARGIN = 1, max) - apply( df2012d1[,c("pan", "pri", "pric", "prdc", "pvem", "panal")], MARGIN = 1, function(x) sort(x,partial=length(x)-1)[length(x)-1])
df2012d1$winmg <- tmp/df2012d1$efec
#
## table(df2012d3$disn) # there are disn==0... i'll drop them but they need fixing ... 11may16 no more
## df2012d3 <- df2012d3[-which(df2012d3$disn==0),]
tmp <- rep(0, times=300)
df2012d3$panw <- df2012d3$priw <- df2012d3$pricw <- df2012d3$prdcw <- df2012d3$pvemw <- df2012d3$panalw <- tmp
tmp <- apply( df2012d3[,c("pan", "pri", "pric", "prdc", "pvem", "panal")], MARGIN = 1, max)
df2012d3$panw[df2012d3$pan==tmp] <- 1
df2012d3$priw[df2012d3$pri==tmp] <- 1
df2012d3$pricw[df2012d3$pric==tmp] <- 1
df2012d3$prdcw[df2012d3$prdc==tmp] <- 1
df2012d3$pvemw[df2012d3$pvem==tmp] <- 1
df2012d3$panalw[df2012d3$panal==tmp] <- 1
## winner's margin
tmp <- apply( df2012d3[,c("pan", "pri", "pric", "prdc", "pvem", "panal")], MARGIN = 1, max) - apply( df2012d3[,c("pan", "pri", "pric", "prdc", "pvem", "panal")], MARGIN = 1, function(x) sort(x,partial=length(x)-1)[length(x)-1])
df2012d3$winmg <- tmp/df2012d3$efec
#
# debug
df2012d0[df2012d0$edon==6,]
df2012d1[df2012d1$edon==24,]
df2012d3[df2012d3$edon==24,]
#
# state aggregates statistics
df2012s0 <- df2012d0
df2012s0 <- df2012s0[order(df2012s0$edon),]
## # WILL DROP THIS STEP THAT FORCES STATES WITH PARTIAL COALITION TO REPORT ALL VOTES IN PRI OR IN PRIC... MAYBE NEEDED TO COMPUTE PRI BIAS?
## colnames(df2012s0)[which(colnames(df2012s0)=="shSecCoalPri")] <- "shDisCoalPri"
## df2012s0$shDisCoalPri <- rep(0, 300)
## df2012s0$shDisCoalPri[df2012s0$pric>0] <- 1
## df2012s0$shDisCoalPri <- ave(df2012s0$shDisCoalPri, as.factor(df2012s0$edon), FUN=sum, na.rm=TRUE)
df2012s0$pan <- ave(df2012s0$pan, as.factor(df2012s0$edon), FUN=sum, na.rm=TRUE)
df2012s0$pri <- ave(df2012s0$pri, as.factor(df2012s0$edon), FUN=sum, na.rm=TRUE)
df2012s0$pric <- ave(df2012s0$pric, as.factor(df2012s0$edon), FUN=sum, na.rm=TRUE)
df2012s0$prdc <- ave(df2012s0$prdc, as.factor(df2012s0$edon), FUN=sum, na.rm=TRUE)
df2012s0$pvem <- ave(df2012s0$pvem, as.factor(df2012s0$edon), FUN=sum, na.rm=TRUE)
df2012s0$panal <- ave(df2012s0$panal, as.factor(df2012s0$edon), FUN=sum, na.rm=TRUE)
df2012s0$efec <- ave(df2012s0$efec, as.factor(df2012s0$edon), FUN=sum, na.rm=TRUE)
df2012s0$ndis <- rep(1, 300)
df2012s0$ndis <- ave(df2012s0$ndis, as.factor(df2012s0$edon), FUN=sum, na.rm=TRUE)
df2012s0$panw <- ave(df2012s0$panw, as.factor(df2012s0$edon), FUN=sum, na.rm=TRUE)
df2012s0$priw <- ave(df2012s0$priw, as.factor(df2012s0$edon), FUN=sum, na.rm=TRUE)
df2012s0$pricw <- ave(df2012s0$pricw, as.factor(df2012s0$edon), FUN=sum, na.rm=TRUE)
df2012s0$prdcw <- ave(df2012s0$prdcw, as.factor(df2012s0$edon), FUN=sum, na.rm=TRUE)
df2012s0$pvemw <- ave(df2012s0$pvemw, as.factor(df2012s0$edon), FUN=sum, na.rm=TRUE)
df2012s0$panalw <- ave(df2012s0$panalw, as.factor(df2012s0$edon), FUN=sum, na.rm=TRUE)
df2012s0 <- df2012s0[duplicated(df2012s0$edon)==FALSE,]
tmp <- df2012s0; tmp[, grep("disn|win|shSec", colnames(tmp))] <- NULL; df2012s0 <- tmp # drop useless columns
#
# shares
df2012s0$pansh   <- df2012s0$pan/df2012s0$efec
df2012s0$prish   <- df2012s0$pri/df2012s0$efec
df2012s0$pricsh  <- df2012s0$pric/df2012s0$efec
df2012s0$prdcsh  <- df2012s0$prdc/df2012s0$efec
df2012s0$pvemsh  <- df2012s0$pvem/df2012s0$efec
df2012s0$panalsh <- df2012s0$panal/df2012s0$efec
#
df2012s0$panw   <- df2012s0$panw/df2012s0$ndis
df2012s0$priw   <- df2012s0$priw/df2012s0$ndis
df2012s0$pricw  <- df2012s0$pricw/df2012s0$ndis
df2012s0$prdcw  <- df2012s0$prdcw/df2012s0$ndis
df2012s0$pvemw  <- df2012s0$pvemw/df2012s0$ndis
df2012s0$panalw <- df2012s0$panalw/df2012s0$ndis
#df2012s0$shDisCoalPri <-  df2012s0$shDisCoalPri/df2012s0$ndis
head(df2012s0)
df2012s1 <- df2012d1
df2012s1 <- df2012s1[order(df2012s1$edon),]
df2012s1$pan <- ave(df2012s1$pan, as.factor(df2012s1$edon), FUN=sum, na.rm=TRUE)
df2012s1$pri <- ave(df2012s1$pri, as.factor(df2012s1$edon), FUN=sum, na.rm=TRUE)
df2012s1$pric <- ave(df2012s1$pric, as.factor(df2012s1$edon), FUN=sum, na.rm=TRUE)
df2012s1$prdc <- ave(df2012s1$prdc, as.factor(df2012s1$edon), FUN=sum, na.rm=TRUE)
df2012s1$pvem <- ave(df2012s1$pvem, as.factor(df2012s1$edon), FUN=sum, na.rm=TRUE)
df2012s1$panal <- ave(df2012s1$panal, as.factor(df2012s1$edon), FUN=sum, na.rm=TRUE)
df2012s1$efec <- ave(df2012s1$efec, as.factor(df2012s1$edon), FUN=sum, na.rm=TRUE)
df2012s1$ndis <- rep(1, 300)
df2012s1$ndis <- ave(df2012s1$ndis, as.factor(df2012s1$edon), FUN=sum, na.rm=TRUE)
df2012s1$panw <- ave(df2012s1$panw, as.factor(df2012s1$edon), FUN=sum, na.rm=TRUE)
df2012s1$priw <- ave(df2012s1$priw, as.factor(df2012s1$edon), FUN=sum, na.rm=TRUE)
df2012s1$pricw <- ave(df2012s1$pricw, as.factor(df2012s1$edon), FUN=sum, na.rm=TRUE)
df2012s1$prdcw <- ave(df2012s1$prdcw, as.factor(df2012s1$edon), FUN=sum, na.rm=TRUE)
df2012s1$pvemw <- ave(df2012s1$pvemw, as.factor(df2012s1$edon), FUN=sum, na.rm=TRUE)
df2012s1$panalw <- ave(df2012s1$panalw, as.factor(df2012s1$edon), FUN=sum, na.rm=TRUE)
df2012s1 <- df2012s1[duplicated(df2012s1$edon)==FALSE,]
tmp <- df2012s1; tmp[, grep("disn|win|shSec", colnames(tmp))] <- NULL; df2012s1 <- tmp # drop useless columns
#
# shares
df2012s1$pansh   <- df2012s1$pan/df2012s1$efec
df2012s1$prish   <- df2012s1$pri/df2012s1$efec
df2012s1$pricsh  <- df2012s1$pric/df2012s1$efec
df2012s1$prdcsh  <- df2012s1$prdc/df2012s1$efec
df2012s1$pvemsh  <- df2012s1$pvem/df2012s1$efec
df2012s1$panalsh <- df2012s1$panal/df2012s1$efec
#
df2012s1$panw   <- df2012s1$panw/df2012s1$ndis
df2012s1$priw   <- df2012s1$priw/df2012s1$ndis
df2012s1$pricw  <- df2012s1$pricw/df2012s1$ndis
df2012s1$prdcw  <- df2012s1$prdcw/df2012s1$ndis
df2012s1$pvemw  <- df2012s1$pvemw/df2012s1$ndis
df2012s1$panalw <- df2012s1$panalw/df2012s1$ndis
#df2012s1$shDisCoalPri <-  df2012s1$shDisCoalPri/df2012s1$ndis
head(df2012s1)
#
df2012s3 <- df2012d3
df2012s3 <- df2012s3[order(df2012s3$edon),]
df2012s3$pan <- ave(df2012s3$pan, as.factor(df2012s3$edon), FUN=sum, na.rm=TRUE)
df2012s3$pri <- ave(df2012s3$pri, as.factor(df2012s3$edon), FUN=sum, na.rm=TRUE)
df2012s3$pric <- ave(df2012s3$pric, as.factor(df2012s3$edon), FUN=sum, na.rm=TRUE)
df2012s3$prdc <- ave(df2012s3$prdc, as.factor(df2012s3$edon), FUN=sum, na.rm=TRUE)
df2012s3$pvem <- ave(df2012s3$pvem, as.factor(df2012s3$edon), FUN=sum, na.rm=TRUE)
df2012s3$panal <- ave(df2012s3$panal, as.factor(df2012s3$edon), FUN=sum, na.rm=TRUE)
df2012s3$efec <- ave(df2012s3$efec, as.factor(df2012s3$edon), FUN=sum, na.rm=TRUE)
df2012s3$ndis <- rep(1, 300)
df2012s3$ndis <- ave(df2012s3$ndis, as.factor(df2012s3$edon), FUN=sum, na.rm=TRUE)
df2012s3$panw <- ave(df2012s3$panw, as.factor(df2012s3$edon), FUN=sum, na.rm=TRUE)
df2012s3$priw <- ave(df2012s3$priw, as.factor(df2012s3$edon), FUN=sum, na.rm=TRUE)
df2012s3$pricw <- ave(df2012s3$pricw, as.factor(df2012s3$edon), FUN=sum, na.rm=TRUE)
df2012s3$prdcw <- ave(df2012s3$prdcw, as.factor(df2012s3$edon), FUN=sum, na.rm=TRUE)
df2012s3$pvemw <- ave(df2012s3$pvemw, as.factor(df2012s3$edon), FUN=sum, na.rm=TRUE)
df2012s3$panalw <- ave(df2012s3$panalw, as.factor(df2012s3$edon), FUN=sum, na.rm=TRUE)
df2012s3 <- df2012s3[duplicated(df2012s3$edon)==FALSE,]
tmp <- df2012s3; tmp[, grep("disn|win|shSec", colnames(tmp))] <- NULL; df2012s3 <- tmp # drop useless columns
#
# shares
df2012s3$pansh   <- df2012s3$pan/df2012s3$efec
df2012s3$prish   <- df2012s3$pri/df2012s3$efec
df2012s3$pricsh  <- df2012s3$pric/df2012s3$efec
df2012s3$prdcsh  <- df2012s3$prdc/df2012s3$efec
df2012s3$pvemsh  <- df2012s3$pvem/df2012s3$efec
df2012s3$panalsh <- df2012s3$panal/df2012s3$efec
#
df2012s3$panw   <- df2012s3$panw/df2012s3$ndis
df2012s3$priw   <- df2012s3$priw/df2012s3$ndis
df2012s3$pricw  <- df2012s3$pricw/df2012s3$ndis
df2012s3$prdcw  <- df2012s3$prdcw/df2012s3$ndis
df2012s3$pvemw  <- df2012s3$pvemw/df2012s3$ndis
df2012s3$panalw <- df2012s3$panalw/df2012s3$ndis
#df2012s3$shDisCoalPri <-  df2012s3$shDisCoalPri/df2012s3$ndis
head(df2012s3)
#
##############
# 2009 votes #
##############
# READ ELEC DATA
tmp <- read.csv( paste(dd, "dfSeccion2009.csv", sep=""), header=TRUE)
tmp <- tmp[order(tmp$edon, tmp$disn, tmp$seccion),]
colnames(tmp)[which(colnames(tmp)=="primero_mexico")] <- "pripvem"
colnames(tmp)[which(colnames(tmp)=="salvemos_mexico")] <- "ptconve"
#
# agrega votos de coalicion donde las hubo
tmp$ptc <- tmp$pt+tmp$conve+tmp$ptconve
tmp$pt  <- tmp$conve <- tmp$ptconve <- NULL # NO SON NECESARIOS COMO LOS DE PRI Y PVEM CON SUS COALICIONES PARCIALES
#
tmp$dcoalpri <- rep(0, times=nrow(tmp))
tmp$dcoalpri[tmp$pripvem>0] <- 1 # misses sections with coalition but no joint pri-pvem vote... next lines fix this
table(tmp$dcoalpri) # debug
tmp1 <- ave(tmp$dcoalpri, as.factor(tmp$edon*100+tmp$disn), FUN=sum, na.rm=TRUE)
tmp2 <- ave(rep(1,length(tmp$dcoalpri)), as.factor(tmp$edon*100+tmp$disn), FUN=sum, na.rm=TRUE)
tmp3 <- tmp1/tmp2
tmp$dcoalpri[tmp3>.5] <- 1
rm(tmp1,tmp2,tmp3)
tmp$pric <- tmp$pri+tmp$pvem+tmp$pripvem
#
## ANEXA LOS DATOS DE LOS DISTRITOS 2013
tmp$edon.secn <- tmp$edon*10000 + tmp$seccion
tmp1 <- dis2013[,c("edon.secn","dis1er13","dis3er13")]
tmp <- merge(x = tmp, y = tmp1, by = "edon.secn", all.x = TRUE)
colnames(tmp)[which(colnames(tmp)=="dis1er13")] <- "dis13.1"
colnames(tmp)[which(colnames(tmp)=="dis3er13")] <- "dis13.3"
rm(tmp1)
#
## Agrega resultados 2009 por distrito
df2009d0 <- tmp[order(tmp$edon, tmp$disn),] # resultados con distritos reales (adoptados en 2005)
df2009d0$pan <- ave(df2009d0$pan, as.factor(df2009d0$edon*100+df2009d0$disn), FUN=sum, na.rm=TRUE)
df2009d0$pri <- ave(df2009d0$pri, as.factor(df2009d0$edon*100+df2009d0$disn), FUN=sum, na.rm=TRUE)
df2009d0$pric <- ave(df2009d0$pric, as.factor(df2009d0$edon*100+df2009d0$disn), FUN=sum, na.rm=TRUE)
df2009d0$prd <- ave(df2009d0$prd, as.factor(df2009d0$edon*100+df2009d0$disn), FUN=sum, na.rm=TRUE)
df2009d0$pvem <- ave(df2009d0$pvem, as.factor(df2009d0$edon*100+df2009d0$disn), FUN=sum, na.rm=TRUE)
df2009d0$ptc <- ave(df2009d0$ptc, as.factor(df2009d0$edon*100+df2009d0$disn), FUN=sum, na.rm=TRUE)
df2009d0$panal <- ave(df2009d0$panal, as.factor(df2009d0$edon*100+df2009d0$disn), FUN=sum, na.rm=TRUE)
df2009d0$psd <- ave(df2009d0$psd, as.factor(df2009d0$edon*100+df2009d0$disn), FUN=sum, na.rm=TRUE)
df2009d0$pripvem <- ave(df2009d0$pripvem, as.factor(df2009d0$edon*100+df2009d0$disn), FUN=sum, na.rm=TRUE)
df2009d0$nr <- ave(df2009d0$nr, as.factor(df2009d0$edon*100+df2009d0$disn), FUN=sum, na.rm=TRUE)
df2009d0$nul <- ave(df2009d0$nul, as.factor(df2009d0$edon*100+df2009d0$disn), FUN=sum, na.rm=TRUE)
df2009d0$tot <- ave(df2009d0$tot, as.factor(df2009d0$edon*100+df2009d0$disn), FUN=sum, na.rm=TRUE)
df2009d0$lisnom <- ave(df2009d0$lisnom, as.factor(df2009d0$edon*100+df2009d0$disn), FUN=sum, na.rm=TRUE)
df2009d0$tmp <- rep(1, times = nrow(df2009d0))
df2009d0$tmp <- ave(df2009d0$tmp, as.factor(df2009d0$edon*100+df2009d0$disn), FUN=sum, na.rm=TRUE)
df2009d0$dcoalpri <- ave(df2009d0$dcoalpri, as.factor(df2009d0$edon*100+df2009d0$disn), FUN=sum, na.rm=TRUE)
df2009d0$dcoalpri <- df2009d0$dcoalpri/df2009d0$tmp # share of secciones with pri coalition in district
df2009d0$tmp <- NULL
df2009d0 <- df2009d0[duplicated(df2009d0$edon*100+df2009d0$disn)==FALSE,]
dim(df2009d0)
df2009d0[df2009d0$edon==20,]
#
# RESULTADOS CON LOS DISTRITOS PROPUESTOS EN 2013 (escen. 1)
df2009d1 <- tmp[is.na(tmp$dis13.1)==FALSE,] # elimina las secciones no asignadas a distrito (posiblemente por reseccionamiento, habr? que recuperarlas)
df2009d1 <- df2009d1[order(df2009d1$edon, df2009d1$dis13.1),]
#
df2009d1$pan <- ave(df2009d1$pan, as.factor(df2009d1$edon*100+df2009d1$dis13.1), FUN=sum, na.rm=TRUE)
df2009d1$pri <- ave(df2009d1$pri, as.factor(df2009d1$edon*100+df2009d1$dis13.1), FUN=sum, na.rm=TRUE)
df2009d1$pric <- ave(df2009d1$pric, as.factor(df2009d1$edon*100+df2009d1$dis13.1), FUN=sum, na.rm=TRUE)
df2009d1$prd <- ave(df2009d1$prd, as.factor(df2009d1$edon*100+df2009d1$dis13.1), FUN=sum, na.rm=TRUE)
df2009d1$pvem <- ave(df2009d1$pvem, as.factor(df2009d1$edon*100+df2009d1$dis13.1), FUN=sum, na.rm=TRUE)
df2009d1$ptc <- ave(df2009d1$ptc, as.factor(df2009d1$edon*100+df2009d1$dis13.1), FUN=sum, na.rm=TRUE)
df2009d1$panal <- ave(df2009d1$panal, as.factor(df2009d1$edon*100+df2009d1$dis13.1), FUN=sum, na.rm=TRUE)
df2009d1$psd <- ave(df2009d1$psd, as.factor(df2009d1$edon*100+df2009d1$dis13.1), FUN=sum, na.rm=TRUE)
df2009d1$pripvem <- ave(df2009d1$pripvem, as.factor(df2009d1$edon*100+df2009d1$dis13.1), FUN=sum, na.rm=TRUE)
df2009d1$nr <- ave(df2009d1$nr, as.factor(df2009d1$edon*100+df2009d1$dis13.1), FUN=sum, na.rm=TRUE)
df2009d1$nul <- ave(df2009d1$nul, as.factor(df2009d1$edon*100+df2009d1$dis13.1), FUN=sum, na.rm=TRUE)
df2009d1$tot <- ave(df2009d1$tot, as.factor(df2009d1$edon*100+df2009d1$dis13.1), FUN=sum, na.rm=TRUE)
df2009d1$lisnom <- ave(df2009d1$lisnom, as.factor(df2009d1$edon*100+df2009d1$dis13.1), FUN=sum, na.rm=TRUE)
df2009d1$tmp <- rep(1, times = nrow(df2009d1))
df2009d1$tmp <- ave(df2009d1$tmp, as.factor(df2009d1$edon*100+df2009d1$dis13.1), FUN=sum, na.rm=TRUE)
df2009d1$dcoalpri <- ave(df2009d1$dcoalpri, as.factor(df2009d1$edon*100+df2009d1$dis13.1), FUN=sum, na.rm=TRUE)
df2009d1$dcoalpri <- df2009d1$dcoalpri/df2009d1$tmp # share of secciones with pri coalition in district
df2009d1$tmp <- NULL
df2009d1 <- df2009d1[duplicated(df2009d1$edon*100+df2009d1$dis13.1)==FALSE,]
dim(df2009d1)
#
# RESULTADOS CON LOS DISTRITOS PROPUESTOS EN 2013 (escen. 3)
df2009d3 <- tmp[is.na(tmp$dis13.3)==FALSE,] # elimina las secciones no asignadas a distrito (posiblemente por reseccionamiento, habr? que recuperarlas)
df2009d3 <- df2009d3[order(df2009d3$edon, df2009d3$dis13.3),]
#
df2009d3$pan <- ave(df2009d3$pan, as.factor(df2009d3$edon*100+df2009d3$dis13.3), FUN=sum, na.rm=TRUE)
df2009d3$pri <- ave(df2009d3$pri, as.factor(df2009d3$edon*100+df2009d3$dis13.3), FUN=sum, na.rm=TRUE)
df2009d3$pric <- ave(df2009d3$pric, as.factor(df2009d3$edon*100+df2009d3$dis13.3), FUN=sum, na.rm=TRUE)
df2009d3$prd <- ave(df2009d3$prd, as.factor(df2009d3$edon*100+df2009d3$dis13.3), FUN=sum, na.rm=TRUE)
df2009d3$pvem <- ave(df2009d3$pvem, as.factor(df2009d3$edon*100+df2009d3$dis13.3), FUN=sum, na.rm=TRUE)
df2009d3$ptc <- ave(df2009d3$ptc, as.factor(df2009d3$edon*100+df2009d3$dis13.3), FUN=sum, na.rm=TRUE)
df2009d3$panal <- ave(df2009d3$panal, as.factor(df2009d3$edon*100+df2009d3$dis13.3), FUN=sum, na.rm=TRUE)
df2009d3$psd <- ave(df2009d3$psd, as.factor(df2009d3$edon*100+df2009d3$dis13.3), FUN=sum, na.rm=TRUE)
df2009d3$pripvem <- ave(df2009d3$pripvem, as.factor(df2009d3$edon*100+df2009d3$dis13.3), FUN=sum, na.rm=TRUE)
df2009d3$nr <- ave(df2009d3$nr, as.factor(df2009d3$edon*100+df2009d3$dis13.3), FUN=sum, na.rm=TRUE)
df2009d3$nul <- ave(df2009d3$nul, as.factor(df2009d3$edon*100+df2009d3$dis13.3), FUN=sum, na.rm=TRUE)
df2009d3$tot <- ave(df2009d3$tot, as.factor(df2009d3$edon*100+df2009d3$dis13.3), FUN=sum, na.rm=TRUE)
df2009d3$lisnom <- ave(df2009d3$lisnom, as.factor(df2009d3$edon*100+df2009d3$dis13.3), FUN=sum, na.rm=TRUE)
df2009d3$tmp <- rep(1, times = nrow(df2009d3))
df2009d3$tmp <- ave(df2009d3$tmp, as.factor(df2009d3$edon*100+df2009d3$dis13.3), FUN=sum, na.rm=TRUE)
df2009d3$dcoalpri <- ave(df2009d3$dcoalpri, as.factor(df2009d3$edon*100+df2009d3$dis13.3), FUN=sum, na.rm=TRUE)
df2009d3$dcoalpri <- df2009d3$dcoalpri/df2009d3$tmp # share of secciones with pri coalition in district
df2009d3$tmp <- NULL
df2009d3 <- df2009d3[duplicated(df2009d3$edon*100+df2009d3$dis13.3)==FALSE,]
dim(df2009d3)
## df2009d3 <- df2009d3[-which(df2009d3$dis13.3==0),]  # drops unassigned sections in 3rd plan (thisinfo should arrive later)
#
# removes redundant columns
df2009d0 <- df2009d0[,c("edon","disn"   ,"pan","pri","pric","prd","pvem","panal","pripvem","ptc","nr","nul","tot","lisnom","dcoalpri")]
df2009d1 <- df2009d1[,c("edon","dis13.1","pan","pri","pric","prd","pvem","panal","pripvem","ptc","nr","nul","tot","lisnom","dcoalpri")]
df2009d3 <- df2009d3[,c("edon","dis13.3","pan","pri","pric","prd","pvem","panal","pripvem","ptc","nr","nul","tot","lisnom","dcoalpri")]
#
colnames(df2009d0) <- colnames(df2009d1) <- colnames(df2009d3) <- c("edon","disn","pan","pri","pric","prd","pvem","panal","pripvem","ptc","nr","nul","tot","lisnom","shSecCoalPri")
#
dim(df2009d0)
dim(df2009d1)
dim(df2009d3)
#
# suma votacion efectiva
df2009d0$efec <- df2009d0$pan+df2009d0$pri+df2009d0$prd+df2009d0$pvem+df2009d0$panal+df2009d0$pripvem+df2009d0$ptc
df2009d1$efec <- df2009d1$pan+df2009d1$pri+df2009d1$prd+df2009d1$pvem+df2009d1$panal+df2009d1$pripvem+df2009d1$ptc
df2009d3$efec <- df2009d3$pan+df2009d3$pri+df2009d3$prd+df2009d3$pvem+df2009d3$panal+df2009d3$pripvem+df2009d3$ptc
#
# agrega votos de coalicion pri donde las hubo con base en la proporcion de secciones con coalicion en el nuevo distrito
df2009d0$pric[df2009d0$shSecCoalPri<.5] <- 0
df2009d0$pripvem <- NULL
df2009d0$pri[df2009d0$shSecCoalPri>=.5] <- 0
df2009d0$pvem[df2009d0$shSecCoalPri>=.5] <- 0
#
#df2009d1$pripvem[df2009d1$shSecCoalPri<.5] # debug: need to assign >0 to pri and pvem in new districts coded as having no coalition
df2009d1$pric[df2009d1$shSecCoalPri<.5] <- 0
shrPri <- df2009d1$pri[df2009d1$shSecCoalPri<.5] / ( df2009d1$pri[df2009d1$shSecCoalPri<.5] + df2009d1$pvem[df2009d1$shSecCoalPri<.5] )
df2009d1$pri[df2009d1$shSecCoalPri<.5] <- df2009d1$pri[df2009d1$shSecCoalPri<.5] + shrPri * df2009d1$pripvem[df2009d1$shSecCoalPri<.5]
df2009d1$pvem[df2009d1$shSecCoalPri<.5] <- df2009d1$pvem[df2009d1$shSecCoalPri<.5] + (1-shrPri) * df2009d1$pripvem[df2009d1$shSecCoalPri<.5]
df2009d1$pripvem <- NULL
df2009d1$pri[df2009d1$shSecCoalPri>=.5] <- 0
df2009d1$pvem[df2009d1$shSecCoalPri>=.5] <- 0
#
df2009d3$pric[df2009d3$shSecCoalPri<.5] <- 0
shrPri <- df2009d3$pri[df2009d3$shSecCoalPri<.5] / ( df2009d3$pri[df2009d3$shSecCoalPri<.5] + df2009d3$pvem[df2009d3$shSecCoalPri<.5] )
df2009d3$pri[df2009d3$shSecCoalPri<.5] <- df2009d3$pri[df2009d3$shSecCoalPri<.5] + shrPri * df2009d3$pripvem[df2009d3$shSecCoalPri<.5]
df2009d3$pvem[df2009d3$shSecCoalPri<.5] <- df2009d3$pvem[df2009d3$shSecCoalPri<.5] + (1-shrPri) * df2009d3$pripvem[df2009d3$shSecCoalPri<.5]
df2009d3$pripvem <- NULL
df2009d3$pri[df2009d3$shSecCoalPri>=.5] <- 0
df2009d3$pvem[df2009d3$shSecCoalPri>=.5] <- 0
rm(shrPri)
df2009d1[df2009d1$edon==24,]
#
## winner
tmp <- rep(0, times=300)
df2009d0$panw <- df2009d0$priw <- df2009d0$pricw <- df2009d0$prdw <- df2009d0$pvemw <- df2009d0$panalw <- df2009d0$ptcw <- tmp
tmp <- apply( df2009d0[,c("pan", "pri", "pric", "prd", "pvem", "panal", "ptc")], MARGIN = 1, max)
df2009d0$panw[df2009d0$pan==tmp] <- 1
df2009d0$priw[df2009d0$pri==tmp] <- 1
df2009d0$pricw[df2009d0$pric==tmp] <- 1
df2009d0$prdw[df2009d0$prd==tmp] <- 1
df2009d0$pvemw[df2009d0$pvem==tmp] <- 1
df2009d0$panalw[df2009d0$panal==tmp] <- 1
df2009d0$ptcw[df2009d0$ptc==tmp] <- 1
## winner's margin
tmp <- apply( df2009d0[,c("pan", "pri", "pric", "prd", "pvem", "panal", "ptc")], MARGIN = 1, max) - apply( df2009d0[,c("pan", "pri", "pric", "prd", "pvem", "panal", "ptc")], MARGIN = 1, function(x) sort(x,partial=length(x)-1)[length(x)-1])
df2009d0$winmg <- tmp/df2009d0$efec
#
tmp <- rep(0, times=300)
df2009d1$panw <- df2009d1$priw <- df2009d1$pricw <- df2009d1$prdw <- df2009d1$pvemw <- df2009d1$panalw <- df2009d1$ptcw <- tmp
tmp <- apply( df2009d1[,c("pan", "pri", "pric", "prd", "pvem", "panal", "ptc")], MARGIN = 1, max)
df2009d1$panw[df2009d1$pan==tmp] <- 1
df2009d1$priw[df2009d1$pri==tmp] <- 1
df2009d1$pricw[df2009d1$pric==tmp] <- 1
df2009d1$prdw[df2009d1$prd==tmp] <- 1
df2009d1$pvemw[df2009d1$pvem==tmp] <- 1
df2009d1$panalw[df2009d1$panal==tmp] <- 1
df2009d1$ptcw[df2009d1$ptc==tmp] <- 1
## winner's margin
tmp <- apply( df2009d1[,c("pan", "pri", "pric", "prd", "pvem", "panal", "ptc")], MARGIN = 1, max) - apply( df2009d1[,c("pan", "pri", "pric", "prd", "pvem", "panal", "ptc")], MARGIN = 1, function(x) sort(x,partial=length(x)-1)[length(x)-1])
df2009d1$winmg <- tmp/df2009d1$efec
#
tmp <- rep(0, times=300)
df2009d3$panw <- df2009d3$priw <- df2009d3$pricw <- df2009d3$prdw <- df2009d3$pvemw <- df2009d3$panalw <- df2009d3$ptcw <- tmp
tmp <- apply( df2009d3[,c("pan", "pri", "pric", "prd", "pvem", "panal", "ptc")], MARGIN = 1, max)
df2009d3$panw[df2009d3$pan==tmp] <- 1
df2009d3$priw[df2009d3$pri==tmp] <- 1
df2009d3$pricw[df2009d3$pric==tmp] <- 1
df2009d3$prdw[df2009d3$prd==tmp] <- 1
df2009d3$pvemw[df2009d3$pvem==tmp] <- 1
df2009d3$panalw[df2009d3$panal==tmp] <- 1
df2009d3$ptcw[df2009d3$ptc==tmp] <- 1
df2009d0[df2009d0$edon==20,]
## winner's margin
tmp <- apply( df2009d3[,c("pan", "pri", "pric", "prd", "pvem", "panal", "ptc")], MARGIN = 1, max) - apply( df2009d3[,c("pan", "pri", "pric", "prd", "pvem", "panal", "ptc")], MARGIN = 1, function(x) sort(x,partial=length(x)-1)[length(x)-1])
df2009d3$winmg <- tmp/df2009d3$efec
#
# state aggregates statistics
df2009s0 <- df2009d0
df2009s0 <- df2009s0[order(df2009s0$edon),]
df2009s0$pan <- ave(df2009s0$pan, as.factor(df2009s0$edon), FUN=sum, na.rm=TRUE)
df2009s0$pri <- ave(df2009s0$pri, as.factor(df2009s0$edon), FUN=sum, na.rm=TRUE)
df2009s0$pric <- ave(df2009s0$pric, as.factor(df2009s0$edon), FUN=sum, na.rm=TRUE)
df2009s0$prd <- ave(df2009s0$prd, as.factor(df2009s0$edon), FUN=sum, na.rm=TRUE)
df2009s0$pvem <- ave(df2009s0$pvem, as.factor(df2009s0$edon), FUN=sum, na.rm=TRUE)
df2009s0$panal <- ave(df2009s0$panal, as.factor(df2009s0$edon), FUN=sum, na.rm=TRUE)
df2009s0$ptc <- ave(df2009s0$ptc, as.factor(df2009s0$edon), FUN=sum, na.rm=TRUE)
df2009s0$efec <- ave(df2009s0$efec, as.factor(df2009s0$edon), FUN=sum, na.rm=TRUE)
df2009s0$ndis <- rep(1, 300)
df2009s0$ndis <- ave(df2009s0$ndis, as.factor(df2009s0$edon), FUN=sum, na.rm=TRUE)
df2009s0$panw <- ave(df2009s0$panw, as.factor(df2009s0$edon), FUN=sum, na.rm=TRUE)
df2009s0$priw <- ave(df2009s0$priw, as.factor(df2009s0$edon), FUN=sum, na.rm=TRUE)
df2009s0$pricw <- ave(df2009s0$pricw, as.factor(df2009s0$edon), FUN=sum, na.rm=TRUE)
df2009s0$prdw <- ave(df2009s0$prdw, as.factor(df2009s0$edon), FUN=sum, na.rm=TRUE)
df2009s0$pvemw <- ave(df2009s0$pvemw, as.factor(df2009s0$edon), FUN=sum, na.rm=TRUE)
df2009s0$panalw <- ave(df2009s0$panalw, as.factor(df2009s0$edon), FUN=sum, na.rm=TRUE)
df2009s0$ptcw <- ave(df2009s0$ptcw, as.factor(df2009s0$edon), FUN=sum, na.rm=TRUE)
df2009s0 <- df2009s0[duplicated(df2009s0$edon)==FALSE,]
tmp <- df2009s0; tmp[, grep("disn|win|shSec", colnames(tmp))] <- NULL; df2009s0 <- tmp # drop useless columns
#
# shares
df2009s0$pansh   <- df2009s0$pan/df2009s0$efec
df2009s0$prish   <- df2009s0$pri/df2009s0$efec
df2009s0$pricsh  <- df2009s0$pric/df2009s0$efec
df2009s0$prdsh  <- df2009s0$prd/df2009s0$efec
df2009s0$pvemsh  <- df2009s0$pvem/df2009s0$efec
df2009s0$panalsh <- df2009s0$panal/df2009s0$efec
df2009s0$ptcsh  <- df2009s0$ptc/df2009s0$efec
#
df2009s0$panw   <- df2009s0$panw/df2009s0$ndis
df2009s0$priw   <- df2009s0$priw/df2009s0$ndis
df2009s0$pricw  <- df2009s0$pricw/df2009s0$ndis
df2009s0$prdw  <- df2009s0$prdw/df2009s0$ndis
df2009s0$pvemw  <- df2009s0$pvemw/df2009s0$ndis
df2009s0$panalw <- df2009s0$panalw/df2009s0$ndis
df2009s0$ptcw  <- df2009s0$ptcw/df2009s0$ndis
#df2009s0$shDisCoalPri <-  df2009s0$shDisCoalPri/df2009s0$ndis
head(df2009s0)
#
df2009s1 <- df2009d1
df2009s1 <- df2009s1[order(df2009s1$edon),]
## # WILL DROP THIS STEP THAT FORCES STATES WITH PARTIAL COALITION TO REPORT ALL VOTES IN PRI OR IN PRIC... MAYBE NEEDED TO COMPUTE PRI BIAS?
## colnames(df2009s1)[which(colnames(df2009s1)=="shSecCoalPri")] <- "shDisCoalPri"
## df2009s1$shDisCoalPri <- rep(0, 300)
## df2009s1$shDisCoalPri[df2009s1$pric>0] <- 1
## df2009s1$shDisCoalPri <- ave(df2009s1$shDisCoalPri, as.factor(df2009s1$edon), FUN=sum, na.rm=TRUE)
df2009s1$pan <- ave(df2009s1$pan, as.factor(df2009s1$edon), FUN=sum, na.rm=TRUE)
df2009s1$pri <- ave(df2009s1$pri, as.factor(df2009s1$edon), FUN=sum, na.rm=TRUE)
df2009s1$pric <- ave(df2009s1$pric, as.factor(df2009s1$edon), FUN=sum, na.rm=TRUE)
df2009s1$prd <- ave(df2009s1$prd, as.factor(df2009s1$edon), FUN=sum, na.rm=TRUE)
df2009s1$pvem <- ave(df2009s1$pvem, as.factor(df2009s1$edon), FUN=sum, na.rm=TRUE)
df2009s1$panal <- ave(df2009s1$panal, as.factor(df2009s1$edon), FUN=sum, na.rm=TRUE)
df2009s1$ptc <- ave(df2009s1$ptc, as.factor(df2009s1$edon), FUN=sum, na.rm=TRUE)
df2009s1$efec <- ave(df2009s1$efec, as.factor(df2009s1$edon), FUN=sum, na.rm=TRUE)
df2009s1$ndis <- rep(1, 300)
df2009s1$ndis <- ave(df2009s1$ndis, as.factor(df2009s1$edon), FUN=sum, na.rm=TRUE)
df2009s1$panw <- ave(df2009s1$panw, as.factor(df2009s1$edon), FUN=sum, na.rm=TRUE)
df2009s1$priw <- ave(df2009s1$priw, as.factor(df2009s1$edon), FUN=sum, na.rm=TRUE)
df2009s1$pricw <- ave(df2009s1$pricw, as.factor(df2009s1$edon), FUN=sum, na.rm=TRUE)
df2009s1$prdw <- ave(df2009s1$prdw, as.factor(df2009s1$edon), FUN=sum, na.rm=TRUE)
df2009s1$pvemw <- ave(df2009s1$pvemw, as.factor(df2009s1$edon), FUN=sum, na.rm=TRUE)
df2009s1$panalw <- ave(df2009s1$panalw, as.factor(df2009s1$edon), FUN=sum, na.rm=TRUE)
df2009s1$ptcw <- ave(df2009s1$ptcw, as.factor(df2009s1$edon), FUN=sum, na.rm=TRUE)
df2009s1 <- df2009s1[duplicated(df2009s1$edon)==FALSE,]
df2009s1[df2009s1$edon==9,]
tmp <- df2009s1; tmp[, grep("disn|win|shSec", colnames(tmp))] <- NULL; df2009s1 <- tmp # drop useless columns
#
# shares
df2009s1$pansh   <- df2009s1$pan/df2009s1$efec
df2009s1$prish   <- df2009s1$pri/df2009s1$efec
df2009s1$pricsh  <- df2009s1$pric/df2009s1$efec
df2009s1$prdsh  <- df2009s1$prd/df2009s1$efec
df2009s1$pvemsh  <- df2009s1$pvem/df2009s1$efec
df2009s1$panalsh <- df2009s1$panal/df2009s1$efec
df2009s1$ptcsh  <- df2009s1$ptc/df2009s1$efec
#
df2009s1$panw   <- df2009s1$panw/df2009s1$ndis
df2009s1$priw   <- df2009s1$priw/df2009s1$ndis
df2009s1$pricw  <- df2009s1$pricw/df2009s1$ndis
df2009s1$prdw  <- df2009s1$prdw/df2009s1$ndis
df2009s1$pvemw  <- df2009s1$pvemw/df2009s1$ndis
df2009s1$panalw <- df2009s1$panalw/df2009s1$ndis
df2009s1$ptcw  <- df2009s1$ptcw/df2009s1$ndis
#CoalPri <-  df2009s1$shDisCoalPri/df2009s1$ndis
head(df2009s1)
#
df2009s3 <- df2009d3
df2009s3 <- df2009s3[order(df2009s3$edon),]
df2009s3$pan <- ave(df2009s3$pan, as.factor(df2009s3$edon), FUN=sum, na.rm=TRUE)
df2009s3$pri <- ave(df2009s3$pri, as.factor(df2009s3$edon), FUN=sum, na.rm=TRUE)
df2009s3$pric <- ave(df2009s3$pric, as.factor(df2009s3$edon), FUN=sum, na.rm=TRUE)
df2009s3$prd <- ave(df2009s3$prd, as.factor(df2009s3$edon), FUN=sum, na.rm=TRUE)
df2009s3$pvem <- ave(df2009s3$pvem, as.factor(df2009s3$edon), FUN=sum, na.rm=TRUE)
df2009s3$panal <- ave(df2009s3$panal, as.factor(df2009s3$edon), FUN=sum, na.rm=TRUE)
df2009s3$ptc <- ave(df2009s3$ptc, as.factor(df2009s3$edon), FUN=sum, na.rm=TRUE)
df2009s3$efec <- ave(df2009s3$efec, as.factor(df2009s3$edon), FUN=sum, na.rm=TRUE)
df2009s3$ndis <- rep(1, 300)
df2009s3$ndis <- ave(df2009s3$ndis, as.factor(df2009s3$edon), FUN=sum, na.rm=TRUE)
df2009s3$panw <- ave(df2009s3$panw, as.factor(df2009s3$edon), FUN=sum, na.rm=TRUE)
df2009s3$priw <- ave(df2009s3$priw, as.factor(df2009s3$edon), FUN=sum, na.rm=TRUE)
df2009s3$pricw <- ave(df2009s3$pricw, as.factor(df2009s3$edon), FUN=sum, na.rm=TRUE)
df2009s3$prdw <- ave(df2009s3$prdw, as.factor(df2009s3$edon), FUN=sum, na.rm=TRUE)
df2009s3$pvemw <- ave(df2009s3$pvemw, as.factor(df2009s3$edon), FUN=sum, na.rm=TRUE)
df2009s3$panalw <- ave(df2009s3$panalw, as.factor(df2009s3$edon), FUN=sum, na.rm=TRUE)
df2009s3$ptcw <- ave(df2009s3$ptcw, as.factor(df2009s3$edon), FUN=sum, na.rm=TRUE)
df2009s3 <- df2009s3[duplicated(df2009s3$edon)==FALSE,]
tmp <- df2009s3; tmp[, grep("disn|win|shSec", colnames(tmp))] <- NULL; df2009s3 <- tmp # drop useless columns
#
# shares
df2009s3$pansh   <- df2009s3$pan/df2009s3$efec
df2009s3$prish   <- df2009s3$pri/df2009s3$efec
df2009s3$pricsh  <- df2009s3$pric/df2009s3$efec
df2009s3$prdsh  <- df2009s3$prd/df2009s3$efec
df2009s3$pvemsh  <- df2009s3$pvem/df2009s3$efec
df2009s3$panalsh <- df2009s3$panal/df2009s3$efec
df2009s3$ptcsh  <- df2009s3$ptc/df2009s3$efec
#
df2009s3$panw   <- df2009s3$panw/df2009s3$ndis
df2009s3$priw   <- df2009s3$priw/df2009s3$ndis
df2009s3$pricw  <- df2009s3$pricw/df2009s3$ndis
df2009s3$prdw  <- df2009s3$prdw/df2009s3$ndis
df2009s3$pvemw  <- df2009s3$pvemw/df2009s3$ndis
df2009s3$panalw <- df2009s3$panalw/df2009s3$ndis
df2009s3$ptcw  <- df2009s3$ptcw/df2009s3$ndis
#CoalPri <-  df2009s3$shDisCoalPri/df2009s3$ndis
head(df2009s3)
#
##############
# 2006 votes #
##############
# READ ELEC DATA
tmp <- read.csv( paste(dd, "dfSeccion2006.csv", sep=""), header=TRUE)
tmp <- tmp[order(tmp$edon, tmp$disn, tmp$seccion),]
colnames(tmp) <- c("edon","disn","munn","seccion","idelec","pan","pric","prdc","panal","asdc","efec","nr","nul","tot") 
#
## ANEXA LOS DATOS DE LOS DISTRITOS 2013
tmp$edon.secn <- tmp$edon*10000 + tmp$seccion
tmp1 <- dis2013[,c("edon.secn","dis1er13","dis3er13")]
tmp <- merge(x = tmp, y = tmp1, by = "edon.secn", all.x = TRUE)
colnames(tmp)[which(colnames(tmp)=="dis1er13")] <- "dis13.1"
colnames(tmp)[which(colnames(tmp)=="dis3er13")] <- "dis13.3"
rm(tmp1)
#
## Agrega resultados 2006 por distrito
df2006d0 <- tmp[order(tmp$edon, tmp$disn),] # resultados con distritos reales (adoptados en 2005)
df2006d0$pan <- ave(df2006d0$pan, as.factor(df2006d0$edon*100+df2006d0$disn), FUN=sum, na.rm=TRUE)
df2006d0$pric <- ave(df2006d0$pric, as.factor(df2006d0$edon*100+df2006d0$disn), FUN=sum, na.rm=TRUE)
df2006d0$prdc <- ave(df2006d0$prdc, as.factor(df2006d0$edon*100+df2006d0$disn), FUN=sum, na.rm=TRUE)
df2006d0$panal <- ave(df2006d0$panal, as.factor(df2006d0$edon*100+df2006d0$disn), FUN=sum, na.rm=TRUE)
df2006d0$asdc <- ave(df2006d0$asdc, as.factor(df2006d0$edon*100+df2006d0$disn), FUN=sum, na.rm=TRUE)
df2006d0$nr <- ave(df2006d0$nr, as.factor(df2006d0$edon*100+df2006d0$disn), FUN=sum, na.rm=TRUE)
df2006d0$nul <- ave(df2006d0$nul, as.factor(df2006d0$edon*100+df2006d0$disn), FUN=sum, na.rm=TRUE)
df2006d0$efec <- ave(df2006d0$efec, as.factor(df2006d0$edon*100+df2006d0$disn), FUN=sum, na.rm=TRUE)
df2006d0 <- df2006d0[duplicated(df2006d0$edon*100+df2006d0$disn)==FALSE,]
dim(df2006d0)
#
# RESULTADOS CON LOS DISTRITOS PROPUESTOS EN 2013 (escen. 1)
df2006d1 <- tmp[is.na(tmp$dis13.1)==FALSE,] # elimina las secciones no asignadas a distrito (posiblemente por reseccionamiento, habr? que recuperarlas)
df2006d1 <- df2006d1[order(df2006d1$edon, df2006d1$dis13.1),]
#
df2006d1$pan <- ave(df2006d1$pan, as.factor(df2006d1$edon*100+df2006d1$dis13.1), FUN=sum, na.rm=TRUE)
df2006d1$pric <- ave(df2006d1$pric, as.factor(df2006d1$edon*100+df2006d1$dis13.1), FUN=sum, na.rm=TRUE)
df2006d1$prdc <- ave(df2006d1$prdc, as.factor(df2006d1$edon*100+df2006d1$dis13.1), FUN=sum, na.rm=TRUE)
df2006d1$panal <- ave(df2006d1$panal, as.factor(df2006d1$edon*100+df2006d1$dis13.1), FUN=sum, na.rm=TRUE)
df2006d1$asdc <- ave(df2006d1$asdc, as.factor(df2006d1$edon*100+df2006d1$dis13.1), FUN=sum, na.rm=TRUE)
df2006d1$nr <- ave(df2006d1$nr, as.factor(df2006d1$edon*100+df2006d1$dis13.1), FUN=sum, na.rm=TRUE)
df2006d1$nul <- ave(df2006d1$nul, as.factor(df2006d1$edon*100+df2006d1$dis13.1), FUN=sum, na.rm=TRUE)
df2006d1$efec <- ave(df2006d1$efec, as.factor(df2006d1$edon*100+df2006d1$dis13.1), FUN=sum, na.rm=TRUE)
df2006d1 <- df2006d1[duplicated(df2006d1$edon*100+df2006d1$dis13.1)==FALSE,]
dim(df2006d1)
#
# RESULTADOS CON LOS DISTRITOS PROPUESTOS EN 2013 (escen. 3)
df2006d3 <- tmp[is.na(tmp$dis13.3)==FALSE,] # elimina las secciones no asignadas a distrito (posiblemente por reseccionamiento, habr? que recuperarlas)
df2006d3 <- df2006d3[order(df2006d3$edon, df2006d3$dis13.3),]
#
df2006d3$pan <- ave(df2006d3$pan, as.factor(df2006d3$edon*100+df2006d3$dis13.3), FUN=sum, na.rm=TRUE)
df2006d3$pric <- ave(df2006d3$pric, as.factor(df2006d3$edon*100+df2006d3$dis13.3), FUN=sum, na.rm=TRUE)
df2006d3$prdc <- ave(df2006d3$prdc, as.factor(df2006d3$edon*100+df2006d3$dis13.3), FUN=sum, na.rm=TRUE)
df2006d3$panal <- ave(df2006d3$panal, as.factor(df2006d3$edon*100+df2006d3$dis13.3), FUN=sum, na.rm=TRUE)
df2006d3$asdc <- ave(df2006d3$asdc, as.factor(df2006d3$edon*100+df2006d3$dis13.3), FUN=sum, na.rm=TRUE)
df2006d3$nr <- ave(df2006d3$nr, as.factor(df2006d3$edon*100+df2006d3$dis13.3), FUN=sum, na.rm=TRUE)
df2006d3$nul <- ave(df2006d3$nul, as.factor(df2006d3$edon*100+df2006d3$dis13.3), FUN=sum, na.rm=TRUE)
df2006d3$efec <- ave(df2006d3$efec, as.factor(df2006d3$edon*100+df2006d3$dis13.3), FUN=sum, na.rm=TRUE)
df2006d3 <- df2006d3[duplicated(df2006d3$edon*100+df2006d3$dis13.3)==FALSE,]
head(df2006d3)
## df2006d3 <- df2006d3[-which(df2006d3$dis13.3==0),]  # drops unassigned sections in 3rd plan (thisinfo should arrive later)
dim(df2006d3)
#
# removes redundant columns
df2006d0 <- df2006d0[,c("edon","disn"   ,"pan","pric","prdc","panal","asdc","efec","nr","nul")]
df2006d1 <- df2006d1[,c("edon","dis13.1","pan","pric","prdc","panal","asdc","efec","nr","nul")]
df2006d3 <- df2006d3[,c("edon","dis13.3","pan","pric","prdc","panal","asdc","efec","nr","nul")]
#
colnames(df2006d0) <- colnames(df2006d1) <- colnames(df2006d3) <- c("edon","disn","pan","pric","prdc","panal","asdc","efec","nr","nul")
#
## winner
tmp <- rep(0, times=300)
df2006d0$panw <- df2006d0$pricw <- df2006d0$prdcw <- df2006d0$asdcw <- df2006d0$panalw <- tmp
tmp <- apply( df2006d0[,c("pan", "pric", "prdc", "panal", "asdc")], MARGIN = 1, max)
df2006d0$panw[df2006d0$pan==tmp] <- 1
df2006d0$pricw[df2006d0$pric==tmp] <- 1
df2006d0$prdcw[df2006d0$prdc==tmp] <- 1
df2006d0$panalw[df2006d0$panal==tmp] <- 1
df2006d0$asdcw[df2006d0$asdc==tmp] <- 1
## winner's margin
tmp <- apply( df2006d0[,c("pan", "pric", "prdc", "panal", "asdc")], MARGIN = 1, max) - apply( df2006d0[,c("pan", "pric", "prdc", "panal", "asdc")], MARGIN = 1, function(x) sort(x,partial=length(x)-1)[length(x)-1])
df2006d0$winmg <- tmp/df2006d0$efec
#
tmp <- rep(0, times=300)
df2006d1$panw <- df2006d1$pricw <- df2006d1$prdcw <- df2006d1$asdcw <- df2006d1$panalw <- tmp
tmp <- apply( df2006d1[,c("pan", "pric", "prdc", "panal", "asdc")], MARGIN = 1, max)
df2006d1$panw[df2006d1$pan==tmp] <- 1
df2006d1$pricw[df2006d1$pric==tmp] <- 1
df2006d1$prdcw[df2006d1$prdc==tmp] <- 1
df2006d1$panalw[df2006d1$panal==tmp] <- 1
df2006d1$asdcw[df2006d1$asdc==tmp] <- 1
## winner's margin
tmp <- apply( df2006d1[,c("pan", "pric", "prdc", "panal", "asdc")], MARGIN = 1, max) - apply( df2006d1[,c("pan", "pric", "prdc", "panal", "asdc")], MARGIN = 1, function(x) sort(x,partial=length(x)-1)[length(x)-1])
df2006d1$winmg <- tmp/df2006d1$efec
#
tmp <- rep(0, times=300)
df2006d3$panw <- df2006d3$pricw <- df2006d3$prdcw <- df2006d3$asdcw <- df2006d3$panalw <- tmp
tmp <- apply( df2006d3[,c("pan", "pric", "prdc", "panal", "asdc")], MARGIN = 1, max)
df2006d3$panw[df2006d3$pan==tmp] <- 1
df2006d3$pricw[df2006d3$pric==tmp] <- 1
df2006d3$prdcw[df2006d3$prdc==tmp] <- 1
df2006d3$panalw[df2006d3$panal==tmp] <- 1
df2006d3$asdcw[df2006d3$asdc==tmp] <- 1
## winner's margin
tmp <- apply( df2006d3[,c("pan", "pric", "prdc", "panal", "asdc")], MARGIN = 1, max) - apply( df2006d3[,c("pan", "pric", "prdc", "panal", "asdc")], MARGIN = 1, function(x) sort(x,partial=length(x)-1)[length(x)-1])
df2006d3$winmg <- tmp/df2006d3$efec
#
# state aggregates statistics
df2006s0 <- df2006d0
df2006s0 <- df2006s0[order(df2006s0$edon),]
df2006s0$pan <- ave(df2006s0$pan, as.factor(df2006s0$edon), FUN=sum, na.rm=TRUE)
df2006s0$pric <- ave(df2006s0$pric, as.factor(df2006s0$edon), FUN=sum, na.rm=TRUE)
df2006s0$prdc <- ave(df2006s0$prdc, as.factor(df2006s0$edon), FUN=sum, na.rm=TRUE)
df2006s0$panal <- ave(df2006s0$panal, as.factor(df2006s0$edon), FUN=sum, na.rm=TRUE)
df2006s0$asdc <- ave(df2006s0$asdc, as.factor(df2006s0$edon), FUN=sum, na.rm=TRUE)
df2006s0$efec <- ave(df2006s0$efec, as.factor(df2006s0$edon), FUN=sum, na.rm=TRUE)
df2006s0$ndis <- rep(1, 300)
df2006s0$ndis <- ave(df2006s0$ndis, as.factor(df2006s0$edon), FUN=sum, na.rm=TRUE)
df2006s0$panw <- ave(df2006s0$panw, as.factor(df2006s0$edon), FUN=sum, na.rm=TRUE)
df2006s0$pricw <- ave(df2006s0$pricw, as.factor(df2006s0$edon), FUN=sum, na.rm=TRUE)
df2006s0$prdcw <- ave(df2006s0$prdcw, as.factor(df2006s0$edon), FUN=sum, na.rm=TRUE)
df2006s0$panalw <- ave(df2006s0$panalw, as.factor(df2006s0$edon), FUN=sum, na.rm=TRUE)
df2006s0$asdcw <- ave(df2006s0$asdcw, as.factor(df2006s0$edon), FUN=sum, na.rm=TRUE)
df2006s0 <- df2006s0[duplicated(df2006s0$edon)==FALSE,]
tmp <- df2006s0; tmp[, grep("disn|win|shSec", colnames(tmp))] <- NULL; df2006s0 <- tmp # drop useless columns
#
# shares
df2006s0$pansh   <- df2006s0$pan/df2006s0$efec
df2006s0$pricsh  <- df2006s0$pric/df2006s0$efec
df2006s0$prdcsh  <- df2006s0$prdc/df2006s0$efec
df2006s0$panalsh <- df2006s0$panal/df2006s0$efec
df2006s0$asdcsh  <- df2006s0$asdc/df2006s0$efec
#
df2006s0$panw   <- df2006s0$panw/df2006s0$ndis
df2006s0$pricw  <- df2006s0$pricw/df2006s0$ndis
df2006s0$prdcw  <- df2006s0$prdcw/df2006s0$ndis
df2006s0$panalw <- df2006s0$panalw/df2006s0$ndis
df2006s0$asdcw  <- df2006s0$asdcw/df2006s0$ndis
head(df2006s0)
#
df2006s1 <- df2006d1
df2006s1 <- df2006s1[order(df2006s1$edon),]
df2006s1$pan <- ave(df2006s1$pan, as.factor(df2006s1$edon), FUN=sum, na.rm=TRUE)
df2006s1$pric <- ave(df2006s1$pric, as.factor(df2006s1$edon), FUN=sum, na.rm=TRUE)
df2006s1$prdc <- ave(df2006s1$prdc, as.factor(df2006s1$edon), FUN=sum, na.rm=TRUE)
df2006s1$panal <- ave(df2006s1$panal, as.factor(df2006s1$edon), FUN=sum, na.rm=TRUE)
df2006s1$asdc <- ave(df2006s1$asdc, as.factor(df2006s1$edon), FUN=sum, na.rm=TRUE)
df2006s1$efec <- ave(df2006s1$efec, as.factor(df2006s1$edon), FUN=sum, na.rm=TRUE)
df2006s1$ndis <- rep(1, 300)
df2006s1$ndis <- ave(df2006s1$ndis, as.factor(df2006s1$edon), FUN=sum, na.rm=TRUE)
df2006s1$panw <- ave(df2006s1$panw, as.factor(df2006s1$edon), FUN=sum, na.rm=TRUE)
df2006s1$pricw <- ave(df2006s1$pricw, as.factor(df2006s1$edon), FUN=sum, na.rm=TRUE)
df2006s1$prdcw <- ave(df2006s1$prdcw, as.factor(df2006s1$edon), FUN=sum, na.rm=TRUE)
df2006s1$panalw <- ave(df2006s1$panalw, as.factor(df2006s1$edon), FUN=sum, na.rm=TRUE)
df2006s1$asdcw <- ave(df2006s1$asdcw, as.factor(df2006s1$edon), FUN=sum, na.rm=TRUE)
df2006s1 <- df2006s1[duplicated(df2006s1$edon)==FALSE,]
tmp <- df2006s1; tmp[, grep("disn|win|shSec", colnames(tmp))] <- NULL; df2006s1 <- tmp # drop useless columns
#
# shares
df2006s1$pansh   <- df2006s1$pan/df2006s1$efec
df2006s1$pricsh  <- df2006s1$pric/df2006s1$efec
df2006s1$prdcsh  <- df2006s1$prdc/df2006s1$efec
df2006s1$panalsh <- df2006s1$panal/df2006s1$efec
df2006s1$asdcsh  <- df2006s1$asdc/df2006s1$efec
#
df2006s1$panw   <- df2006s1$panw/df2006s1$ndis
df2006s1$pricw  <- df2006s1$pricw/df2006s1$ndis
df2006s1$prdcw  <- df2006s1$prdcw/df2006s1$ndis
df2006s1$panalw <- df2006s1$panalw/df2006s1$ndis
df2006s1$asdcw  <- df2006s1$asdcw/df2006s1$ndis
head(df2006s1)
#
df2006s3 <- df2006d3
df2006s3 <- df2006s3[order(df2006s3$edon),]
df2006s3$pan <- ave(df2006s3$pan, as.factor(df2006s3$edon), FUN=sum, na.rm=TRUE)
df2006s3$pric <- ave(df2006s3$pric, as.factor(df2006s3$edon), FUN=sum, na.rm=TRUE)
df2006s3$prdc <- ave(df2006s3$prdc, as.factor(df2006s3$edon), FUN=sum, na.rm=TRUE)
df2006s3$panal <- ave(df2006s3$panal, as.factor(df2006s3$edon), FUN=sum, na.rm=TRUE)
df2006s3$asdc <- ave(df2006s3$asdc, as.factor(df2006s3$edon), FUN=sum, na.rm=TRUE)
df2006s3$efec <- ave(df2006s3$efec, as.factor(df2006s3$edon), FUN=sum, na.rm=TRUE)
df2006s3$ndis <- rep(1, 300)
df2006s3$ndis <- ave(df2006s3$ndis, as.factor(df2006s3$edon), FUN=sum, na.rm=TRUE)
df2006s3$panw <- ave(df2006s3$panw, as.factor(df2006s3$edon), FUN=sum, na.rm=TRUE)
df2006s3$pricw <- ave(df2006s3$pricw, as.factor(df2006s3$edon), FUN=sum, na.rm=TRUE)
df2006s3$prdcw <- ave(df2006s3$prdcw, as.factor(df2006s3$edon), FUN=sum, na.rm=TRUE)
df2006s3$panalw <- ave(df2006s3$panalw, as.factor(df2006s3$edon), FUN=sum, na.rm=TRUE)
df2006s3$asdcw <- ave(df2006s3$asdcw, as.factor(df2006s3$edon), FUN=sum, na.rm=TRUE)
df2006s3 <- df2006s3[duplicated(df2006s3$edon)==FALSE,]
tmp <- df2006s3; tmp[, grep("disn|win|shSec", colnames(tmp))] <- NULL; df2006s3 <- tmp # drop useless columns
#
# shares
df2006s3$pansh   <- df2006s3$pan/df2006s3$efec
df2006s3$pricsh  <- df2006s3$pric/df2006s3$efec
df2006s3$prdcsh  <- df2006s3$prdc/df2006s3$efec
df2006s3$panalsh <- df2006s3$panal/df2006s3$efec
df2006s3$asdcsh  <- df2006s3$asdc/df2006s3$efec
#
df2006s3$panw   <- df2006s3$panw/df2006s3$ndis
df2006s3$pricw  <- df2006s3$pricw/df2006s3$ndis
df2006s3$prdcw  <- df2006s3$prdcw/df2006s3$ndis
df2006s3$panalw <- df2006s3$panalw/df2006s3$ndis
df2006s3$asdcw  <- df2006s3$asdcw/df2006s3$ndis
#
## compute effective number of parties by state
hh <- df2006s0$pansh^2 + df2006s0$pricsh^2 + df2006s0$prdcsh^2 + df2006s0$panalsh^2 + df2006s0$asdcsh^2
df2006s0$N <- df2006s1$N <- df2006s3$N <- 1/hh ## laakso y taagepera
#
hh <- df2009s0$pansh^2 + df2009s0$prish^2 + df2009s0$pricsh^2 + df2009s0$prdsh^2 + df2009s0$pvemsh^2 + df2009s0$panalsh^2 + df2009s0$ptcsh^2
df2009s0$N <- df2009s1$N <- df2009s3$N <- 1/hh ## laakso y taagepera
#
hh <- df2012s0$pansh^2 + df2012s0$prish^2 + df2012s0$pricsh^2 + df2012s0$prdcsh^2 + df2012s0$pvemsh^2 + df2012s0$panalsh^2
df2012s0$N <- df2012s1$N <- df2012s3$N <- 1/hh ## laakso y taagepera
#
hh <- df2015s0$pansh^2 + df2015s0$prish^2 + df2015s0$pricsh^2 + df2015s0$prdsh^2 + df2015s0$prdcsh^2 + df2015s0$ptsh^2 + df2015s0$pvemsh^2 + df2015s0$mcsh^2 + df2015s0$panalsh^2 + df2015s0$morenash^2 + df2015s0$phsh^2 + df2015s0$pssh^2 + df2015s0$indep1sh^2 + df2015s0$indep2sh^2
df2015s0$N <- df2015s1$N <- df2015s3$N <- 1/hh ## laakso y taagepera
#

## # work 1997d97, 2000d97, and 2003d97 from district data (section aggregatios do not work, see below)
## tmp <- read.csv( paste(dd, "dfdf2003prep.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)
## # compute effective vote (without void ballots)
## tmp$efec <- tmp$pan + tmp$pri + tmp$pric + tmp$prd + tmp$pt + tmp$pvem + tmp$c + tmp$psn + tmp$pas + tmp$mp + tmp$plm + tmp$fc
## # gives pric to pri
## tmp$pri <- tmp$pri + tmp$pric; tmp$pric <- NULL
## tmp$ord <- tmp$yr <- NULL
## df2003d97 <- tmp
## #
## tmp <- read.csv( paste(dd, "dfdf2000.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)
## tmp$efec <- tmp$panc + tmp$pri + tmp$prdc + tmp$parm + tmp$pcd + tmp$pds
## tmp$pancoal <- tmp$pricoal <- tmp$prdcoal <- tmp$yr <- NULL
## df2000d97 <- tmp
## #
## tmp <- read.csv( paste(dd, "dfdf1997.csv", sep=""), header=TRUE, stringsAsFactors = FALSE)
## tmp$efec <- tmp$pan + tmp$pri + tmp$prd + tmp$pt + tmp$pvem + tmp$pps + tmp$pc + tmp$uno
## tmp$yr <- NULL
## df1997d97 <- tmp
## rm(tmp)
## x

####################################################################################################
## READ CASILLA-LEVEL ELEC DATA 2003                                                              ##
## IFE SECCION-LEVEL DATA HAS 18K SECCIONES MISSING... USE PRECINCT-LEVEL (CASILLAS) DATA INSTEAD ##
####################################################################################################
tmp <- read.csv( paste(dd, "dfCasilla2003.csv", sep=""), header=TRUE)
tmp <- tmp[order(tmp$edon, tmp$seccion),] # sort
tmp$ID_ELEC <- NULL
tmp <- tmp[tmp$status=="",] # drop null casillas
tmp[is.na(tmp)] <- 0        # fill NAs with 0
# aggregate secciones (cols 6:20 contain substantive data)
for (i in 6:20){
    tmp[,i] <- ave(tmp[,i]     , as.factor(tmp$edon*10000+tmp$seccion), FUN=sum, na.rm=TRUE)
}
tmp <- tmp[duplicated(tmp$edon*10000+tmp$seccion)==FALSE,] # drop redundant obs
tmp$casilla <- tmp$status <- NULL
#
# aggregate d97 districts (1997 map, in use)
# OJO: 2 districts missing: 506 and 1605
tmp <- tmp[order(tmp$edon, tmp$disn, tmp$seccion),]
# compute effective vote
colnames(tmp)[which(colnames(tmp)=="apt")] <- "pric"
tmp$efec <- tmp$pan + tmp$pri + tmp$prd + tmp$pt + tmp$pvem + tmp$conve + tmp$psn + tmp$pas + tmp$mp + tmp$plm + tmp$fc + tmp$pric
#head(tmp[tmp$edon==11,])
# aggregates coalition votes where needed
sel <- which(tmp$pric>0 & tmp$pri>0) # secciones reporting pri+pvem>0 and pri>0
tmp$pric[sel] <- tmp$pri[sel] + tmp$pric[sel] + tmp$pvem[sel]; tmp$pri[sel] <- tmp$pvem[sel] <- 0
tmp$dcoalpri <- rep(0, times=nrow(tmp))
tmp$dcoalpri[tmp$pric>0] <- 1
#table(tmp$dcoalpri) # debug
# share of secciones with coalition
tmp1 <- ave(tmp$dcoalpri, as.factor(tmp$edon*100+tmp$disn), FUN=sum, na.rm=TRUE)
tmp2 <- ave(rep(1,length(tmp$dcoalpri)), as.factor(tmp$edon*100+tmp$disn), FUN=sum, na.rm=TRUE)
tmp3 <- tmp1/tmp2
tmp$dcoalpri[tmp3>.5] <- 1
rm(tmp1,tmp2,tmp3)
tmp$pric <- tmp$pri + tmp$pvem + tmp$pric
#
# extracts 2006 districts info
dis2003 <-       eq[,c("edon","dis2003","munn","seccion","dis2006")]
colnames(dis2003) <- c("edon","disn","munn","seccion","dis2006")
#
dis2003$ife <- dis2003$edon*1000 + dis2003$munn
dis2003 <- dis2003[,c("edon","disn","munn","seccion","ife","dis2006")]
#
tmp$edon.secn <- tmp$edon*10000 + tmp$seccion
dis2003$edon.secn <- dis2003$edon*10000 + dis2003$seccion
tmp1 <- dis2003[,c("edon.secn","dis2006")]
tmp <- merge(x = tmp, y = tmp1, by = "edon.secn", all.x = TRUE)
#
colnames(tmp)
sel <- 6:21  # it is these cols that have substantive data
## Aggregates 2003 results by district
df2003d97 <- tmp[order(tmp$edon, tmp$disn),] # resultados con distritos reales (adoptados en 1996)
for (i in sel){
    df2003d97[,i] <- ave(df2003d97[,i]     , as.factor(df2003d97$edon*100+df2003d97$disn), FUN=sum, na.rm=TRUE)
}
#
df2003d97$tmp <- rep(1, times = nrow(df2003d97))
df2003d97$tmp      <- ave(df2003d97$tmp     , as.factor(df2003d97$edon*100+df2003d97$disn), FUN=sum, na.rm=TRUE)
df2003d97$dcoalpri <- ave(df2003d97$dcoalpri, as.factor(df2003d97$edon*100+df2003d97$disn), FUN=sum, na.rm=TRUE)
df2003d97$dcoalpri <- df2003d97$dcoalpri/df2003d97$tmp # share of secciones with pri coalition in district
colnames(df2003d97)[which(colnames(df2003d97)=="dcoalpri")] <- "shSecCoalPri"
df2003d97$tmp <- NULL
df2003d97 <- df2003d97[duplicated(df2003d97$edon*100+df2003d97$disn)==FALSE,]
# df2003d97$shSecCoalPri ## debug: should now be 0 or 1 after fix used above
df2003d97$pric[df2003d97$shSecCoalPri==0] <- 0
df2003d97$pri[df2003d97$shSecCoalPri==1] <- 0
df2003d97$pvem[df2003d97$shSecCoalPri==1] <- 0
df2003d97$dis2006 <- df2003d97$edon.secn <- df2003d97$seccion <- NULL
#
# RESULTADOS CON MAPA 2006
df2003d0 <- tmp[order(tmp$edon, tmp$dis2006),]
df2003d0$disn <- df2003d0$dis2006; df2003d0$dis2006 <- NULL # change district info
head(df2003d0)
for (i in sel){
    df2003d0[,i] <- ave(df2003d0[,i]     , as.factor(df2003d0$edon*100+df2003d0$disn), FUN=sum, na.rm=TRUE)
}
#
df2003d0$tmp <- rep(1, times = nrow(df2003d0))
df2003d0$tmp <- ave(df2003d0$tmp, as.factor(df2003d0$edon*100+df2003d0$disn), FUN=sum, na.rm=TRUE)
df2003d0$dcoalpri <- ave(df2003d0$dcoalpri, as.factor(df2003d0$edon*100+df2003d0$disn), FUN=sum, na.rm=TRUE)
df2003d0$dcoalpri <- df2003d0$dcoalpri/df2003d0$tmp # share of secciones with pri coalition in district
## table(df2003d0$dcoalpri[df2003d0$edon==24]) # debug
## data.frame(coal=df2003d0$dcoalpri[df2003d0$edon==24], dis97=df2003d0$disn[df2003d0$edon==24], dis0=df2003d0$dis03.1[df2003d0$edon==24]) # debug
colnames(df2003d0)[which(colnames(df2003d0)=="dcoalpri")] <- "shSecCoalPri"
df2003d0$tmp <- NULL
df2003d0 <- df2003d0[duplicated(df2003d0$edon*100+df2003d0$disn)==FALSE,]
#df2003d0$shSecCoalPri ## debug: should now be 0 or 1 after fix used above
df2003d0$pric[df2003d0$shSecCoalPri==0] <- 0
df2003d0$pri[df2003d0$shSecCoalPri==1] <- 0
df2003d0$pvem[df2003d0$shSecCoalPri==1] <- 0
#
df2003d0$edon.secn <- df2003d0$seccion <- NULL
#
# removes redundant columns
df2003d97 <- df2003d97[,c("edon", "disn", "pan", "pri", "prd", "pt", "pvem", "conve", "psn", "pas", "mp", "plm", "fc", "pric", "nr", "nul", "efec", "shSecCoalPri")]
df2003d0 <-  df2003d0 [,c("edon", "disn", "pan", "pri", "prd", "pt", "pvem", "conve", "psn", "pas", "mp", "plm", "fc", "pric", "nr", "nul", "efec", "shSecCoalPri")]
#
dim(df2003d97) # <- OJO: two full districts missing: 506 and 1605
dim(df2003d0)

# adds winner, somehow not added for 2003 here
#colnames(df2003d97)
tmp <- c("pan", "pri", "prd", "pt", "pvem", "conve", "psn", "pas", "mp", "plm","fc","pric")
tmpv <- df2003d97[,tmp]
tmpl <- matrix(rep(tmp,298), byrow=TRUE, nrow=298) # OJO two districts missing from vote data
tmpl <- sortBy(target=tmpl, By=tmpv)
#tmpv <- t(apply(tmpv, 1, function(x) sort(x, decreasing = TRUE)))
df2003d97$win <- tmpl[,1]


############################################################################################################
## *Bloc 3*:                                                                                              ##
## READ DISTRICT-LEVEL POPULATION DATA (LINEAR PROJECTIONS OF CENSUS FIGURES) AND MERGE WITH VOTE OBJECTS ##
############################################################################################################
load(paste(dd, "votPobDis0018.RData", sep = ""))
summary(votPobDis0018) # this object wrongly calls 2013 map "2015 map"
colnames(votPobDis0018$pob.distMap1997)
colnames(votPobDis0018$pob.distMap2006)
# simplify objects with relevant year only
tmp <- votPobDis0018$pob.distMap1997
tmp <- tmp[,c("edon","disn","ptot2003","rris2003","rrin2003")]
colnames(tmp) <- c("edon","disn","ptot","rris","rrin")
votPobDis0018$pob.df2003d97 <- tmp
#
tmp <- votPobDis0018$pob.distMap2006
tmp <- tmp[,c("edon","disn","ptot2003","rris2003","rrin2003")]
colnames(tmp) <- c("edon","disn","ptot","rris","rrin")
votPobDis0018$pob.df2003d0 <- tmp
#
tmp <- votPobDis0018$pob.distMap2006
tmp <- tmp[,c("edon","disn","ptot2006","rris2006","rrin2006")]
colnames(tmp) <- c("edon","disn","ptot","rris","rrin")
votPobDis0018$pob.df2006d0 <- tmp
#
tmp <- votPobDis0018$pob.distMap2015p3
tmp <- tmp[,c("edon","disn","ptot2006","rris2006","rrin2006")]
colnames(tmp) <- c("edon","disn","ptot","rris","rrin")
votPobDis0018$pob.df2006d3 <- tmp
#
tmp <- votPobDis0018$pob.distMap2006
tmp <- tmp[,c("edon","disn","ptot2009","rris2009","rrin2009")]
colnames(tmp) <- c("edon","disn","ptot","rris","rrin")
votPobDis0018$pob.df2009d0 <- tmp
#
tmp <- votPobDis0018$pob.distMap2015p3
tmp <- tmp[,c("edon","disn","ptot2009","rris2009","rrin2009")]
colnames(tmp) <- c("edon","disn","ptot","rris","rrin")
votPobDis0018$pob.df2009d3 <- tmp
#
tmp <- votPobDis0018$pob.distMap2006
tmp <- tmp[,c("edon","disn","ptot2012","rris2012","rrin2012")]
colnames(tmp) <- c("edon","disn","ptot","rris","rrin")
votPobDis0018$pob.df2012d0 <- tmp
#
tmp <- votPobDis0018$pob.distMap2015p3
tmp <- tmp[,c("edon","disn","ptot2012","rris2012","rrin2012")]
colnames(tmp) <- c("edon","disn","ptot","rris","rrin")
votPobDis0018$pob.df2012d3 <- tmp
#
tmp <- votPobDis0018$pob.distMap2006
tmp <- tmp[,c("edon","disn","ptot2015","rris2015","rrin2015")]
colnames(tmp) <- c("edon","disn","ptot","rris","rrin")
votPobDis0018$pob.df2015d0 <- tmp
#
tmp <- votPobDis0018$pob.distMap2015p3
tmp <- tmp[,c("edon","disn","ptot2015","rris2015","rrin2015")]
colnames(tmp) <- c("edon","disn","ptot","rris","rrin")
votPobDis0018$pob.df2015d3 <- tmp
#
# add poulation stats
tmpFrom <- votPobDis0018$pob.df2003d97; tmpTo <- df2003d97                                  # select one year and map
tmpTo <- rbind(tmpTo, c(5,6,rep(NA,ncol(tmpTo))), c(16,5,rep(NA,ncol(tmpTo))))              # adds missing districts for square matrix
tmpFrom <- tmpFrom[order(tmpFrom$edon, tmpFrom$disn),]                                      # sort
tmpTo <- tmpTo[order(tmpTo$edon, tmpTo$disn),]                                              # sort
table(paste(tmpFrom$edon, tmpFrom$disn, sep=".")==paste(tmpTo$edon, tmpTo$disn, sep = ".")) # verify districts match in both objects (300 TRUEs)
tmpFrom <- tmpFrom[, c("ptot","rris","rrin")]                                               # keep population data only
tmpTo <- cbind(tmpTo, tmpFrom)                                                              # merge
tmpTo <- tmpTo[-which((tmpTo$edon==5 & tmpTo$disn==6) | tmpTo$edon==16 & tmpTo$disn==5),]   # drop missing districts
df2003d97 <- tmpTo
#
tmpFrom <- votPobDis0018$pob.df2003d0; tmpTo <- df2003d0                                    # select one year and map
tmpFrom <- tmpFrom[order(tmpFrom$edon, tmpFrom$disn),]                                      # sort
table(paste(tmpFrom$edon, tmpFrom$disn, sep=".")==paste(tmpTo$edon, tmpTo$disn, sep = ".")) # verify districts match in both objects (300 TRUEs)
tmpFrom <- tmpFrom[, c("ptot","rris","rrin")]                                               # keep population data only
tmpTo <- cbind(tmpTo, tmpFrom)                                                              # merge
df2003d0 <- tmpTo
#
tmpFrom <- votPobDis0018$pob.df2006d0; tmpTo <- df2006d0                                    # select one year and map
tmpFrom <- tmpFrom[order(tmpFrom$edon, tmpFrom$disn),]                                      # sort
table(paste(tmpFrom$edon, tmpFrom$disn, sep=".")==paste(tmpTo$edon, tmpTo$disn, sep = ".")) # verify districts match in both objects (300 TRUEs)
tmpFrom <- tmpFrom[, c("ptot","rris","rrin")]                                               # keep population data only
tmpTo <- cbind(tmpTo, tmpFrom)                                                              # merge
df2006d0 <- tmpTo
#
tmpFrom <- votPobDis0018$pob.df2006d3; tmpTo <- df2006d3                                    # select one year and map
tmpFrom <- tmpFrom[order(tmpFrom$edon, tmpFrom$disn),]                                      # sort
table(paste(tmpFrom$edon, tmpFrom$disn, sep=".")==paste(tmpTo$edon, tmpTo$disn, sep = ".")) # verify districts match in both objects (300 TRUEs)
tmpFrom <- tmpFrom[, c("ptot","rris","rrin")]                                               # keep population data only
tmpTo <- cbind(tmpTo, tmpFrom)                                                              # merge
df2006d3 <- tmpTo
#
tmpFrom <- votPobDis0018$pob.df2009d0; tmpTo <- df2009d0                                    # select one year and map
tmpFrom <- tmpFrom[order(tmpFrom$edon, tmpFrom$disn),]                                      # sort
table(paste(tmpFrom$edon, tmpFrom$disn, sep=".")==paste(tmpTo$edon, tmpTo$disn, sep = ".")) # verify districts match in both objects (300 TRUEs)
tmpFrom <- tmpFrom[, c("ptot","rris","rrin")]                                               # keep population data only
tmpTo <- cbind(tmpTo, tmpFrom)                                                              # merge
df2009d0 <- tmpTo
#
tmpFrom <- votPobDis0018$pob.df2009d3; tmpTo <- df2009d3                                    # select one year and map
tmpFrom <- tmpFrom[order(tmpFrom$edon, tmpFrom$disn),]                                      # sort
table(paste(tmpFrom$edon, tmpFrom$disn, sep=".")==paste(tmpTo$edon, tmpTo$disn, sep = ".")) # verify districts match in both objects (300 TRUEs)
tmpFrom <- tmpFrom[, c("ptot","rris","rrin")]                                               # keep population data only
tmpTo <- cbind(tmpTo, tmpFrom)                                                              # merge
df2009d3 <- tmpTo
#
tmpFrom <- votPobDis0018$pob.df2012d0; tmpTo <- df2012d0                                        # select one year and map
tmpFrom <- tmpFrom[order(tmpFrom$edon, tmpFrom$disn),]                                      # sort
table(paste(tmpFrom$edon, tmpFrom$disn, sep=".")==paste(tmpTo$edon, tmpTo$disn, sep = ".")) # verify districts match in both objects (300 TRUEs)
tmpFrom <- tmpFrom[, c("ptot","rris","rrin")]                                               # keep population data only
tmpTo <- cbind(tmpTo, tmpFrom)                                                              # merge
df2012d0 <- tmpTo
#
tmpFrom <- votPobDis0018$pob.df2012d3; tmpTo <- df2012d3                                        # select one year and map
tmpFrom <- tmpFrom[order(tmpFrom$edon, tmpFrom$disn),]                                      # sort
table(paste(tmpFrom$edon, tmpFrom$disn, sep=".")==paste(tmpTo$edon, tmpTo$disn, sep = ".")) # verify districts match in both objects (300 TRUEs)
tmpFrom <- tmpFrom[, c("ptot","rris","rrin")]                                               # keep population data only
tmpTo <- cbind(tmpTo, tmpFrom)                                                              # merge
df2012d3 <- tmpTo
#
tmpFrom <- votPobDis0018$pob.df2015d0; tmpTo <- df2015d0                                    # select one year and map
tmpFrom <- tmpFrom[order(tmpFrom$edon, tmpFrom$disn),]                                      # sort
table(paste(tmpFrom$edon, tmpFrom$disn, sep=".")==paste(tmpTo$edon, tmpTo$disn, sep = ".")) # verify districts match in both objects (300 TRUEs)
tmpFrom <- tmpFrom[, c("ptot","rris","rrin")]                                               # keep population data only
tmpTo <- cbind(tmpTo, tmpFrom)                                                              # merge
df2015d0 <- tmpTo
#
tmpFrom <- votPobDis0018$pob.df2015d3; tmpTo <- df2015d3                                    # select one year and map
tmpFrom <- tmpFrom[order(tmpFrom$edon, tmpFrom$disn),]                                      # sort
table(paste(tmpFrom$edon, tmpFrom$disn, sep=".")==paste(tmpTo$edon, tmpTo$disn, sep = ".")) # verify districts match in both objects (300 TRUEs)
tmpFrom <- tmpFrom[, c("ptot","rris","rrin")]                                               # keep population data only
tmpTo <- cbind(tmpTo, tmpFrom)                                                              # merge
df2015d3 <- tmpTo
#
rm(tmpFrom, tmpTo, votPobDis0018)
#
tmpFrom <- df2006d0; tmpTo <- df2006s0
tmpFrom$ptot <- ave(tmpFrom$ptot, as.factor(tmpFrom$edon), FUN=sum, na.rm=TRUE)
tmpFrom <- tmpFrom[duplicated(tmpFrom$edon)==FALSE,]
table(tmpFrom$edon==tmpTo$edon) # verify districts match in both objects (32 TRUEs)
tmpTo$ptot <- tmpFrom$ptot
df2006s0 <- tmpTo
#
## tmpFrom <- df2006d1; tmpTo <- df2006s1
## tmpFrom$ptot <- ave(tmpFrom$ptot, as.factor(tmpFrom$edon), FUN=sum, na.rm=TRUE)
## tmpFrom <- tmpFrom[duplicated(tmpFrom$edon)==FALSE,]
## table(tmpFrom$edon==tmpTo$edon) # verify districts match in both objects (32 TRUEs)
## tmpTo$ptot <- tmpFrom$ptot
## df2006s1 <- tmpTo
## #
tmpFrom <- df2006d3; tmpTo <- df2006s3
tmpFrom$ptot <- ave(tmpFrom$ptot, as.factor(tmpFrom$edon), FUN=sum, na.rm=TRUE)
tmpFrom <- tmpFrom[duplicated(tmpFrom$edon)==FALSE,]
table(tmpFrom$edon==tmpTo$edon) # verify districts match in both objects (32 TRUEs)
tmpTo$ptot <- tmpFrom$ptot
df2006s3 <- tmpTo
#
tmpFrom <- df2009d0; tmpTo <- df2009s0
tmpFrom$ptot <- ave(tmpFrom$ptot, as.factor(tmpFrom$edon), FUN=sum, na.rm=TRUE)
tmpFrom <- tmpFrom[duplicated(tmpFrom$edon)==FALSE,]
table(tmpFrom$edon==tmpTo$edon) # verify districts match in both objects (32 TRUEs)
tmpTo$ptot <- tmpFrom$ptot
df2009s0 <- tmpTo
#
## tmpFrom <- df2009d1; tmpTo <- df2009s1
## tmpFrom$ptot <- ave(tmpFrom$ptot, as.factor(tmpFrom$edon), FUN=sum, na.rm=TRUE)
## tmpFrom <- tmpFrom[duplicated(tmpFrom$edon)==FALSE,]
## table(tmpFrom$edon==tmpTo$edon) # verify districts match in both objects (32 TRUEs)
## tmpTo$ptot <- tmpFrom$ptot
## df2009s1 <- tmpTo
## #
tmpFrom <- df2009d3; tmpTo <- df2009s3
tmpFrom$ptot <- ave(tmpFrom$ptot, as.factor(tmpFrom$edon), FUN=sum, na.rm=TRUE)
tmpFrom <- tmpFrom[duplicated(tmpFrom$edon)==FALSE,]
table(tmpFrom$edon==tmpTo$edon) # verify districts match in both objects (32 TRUEs)
tmpTo$ptot <- tmpFrom$ptot
df2009s3 <- tmpTo
#
tmpFrom <- df2012d0; tmpTo <- df2012s0
tmpFrom$ptot <- ave(tmpFrom$ptot, as.factor(tmpFrom$edon), FUN=sum, na.rm=TRUE)
tmpFrom <- tmpFrom[duplicated(tmpFrom$edon)==FALSE,]
table(tmpFrom$edon==tmpTo$edon) # verify districts match in both objects (32 TRUEs)
tmpTo$ptot <- tmpFrom$ptot
df2012s0 <- tmpTo
#
## tmpFrom <- df2012d1; tmpTo <- df2012s1
## tmpFrom$ptot <- ave(tmpFrom$ptot, as.factor(tmpFrom$edon), FUN=sum, na.rm=TRUE)
## tmpFrom <- tmpFrom[duplicated(tmpFrom$edon)==FALSE,]
## table(tmpFrom$edon==tmpTo$edon) # verify districts match in both objects (32 TRUEs)
## tmpTo$ptot <- tmpFrom$ptot
## df2012s1 <- tmpTo
## #
tmpFrom <- df2012d3; tmpTo <- df2012s3
tmpFrom$ptot <- ave(tmpFrom$ptot, as.factor(tmpFrom$edon), FUN=sum, na.rm=TRUE)
tmpFrom <- tmpFrom[duplicated(tmpFrom$edon)==FALSE,]
table(tmpFrom$edon==tmpTo$edon) # verify districts match in both objects (32 TRUEs)
tmpTo$ptot <- tmpFrom$ptot
df2012s3 <- tmpTo
#
tmpFrom <- df2015d0; tmpTo <- df2015s0
tmpFrom$ptot <- ave(tmpFrom$ptot, as.factor(tmpFrom$edon), FUN=sum, na.rm=TRUE)
tmpFrom <- tmpFrom[duplicated(tmpFrom$edon)==FALSE,]
table(tmpFrom$edon==tmpTo$edon) # verify districts match in both objects (32 TRUEs)
tmpTo$ptot <- tmpFrom$ptot
df2015s0 <- tmpTo
#
## tmpFrom <- df2015d1; tmpTo <- df2015s1
## tmpFrom$ptot <- ave(tmpFrom$ptot, as.factor(tmpFrom$edon), FUN=sum, na.rm=TRUE)
## tmpFrom <- tmpFrom[duplicated(tmpFrom$edon)==FALSE,]
## table(tmpFrom$edon==tmpTo$edon) # verify districts match in both objects (32 TRUEs)
## tmpTo$ptot <- tmpFrom$ptot
## df2015s1 <- tmpTo
## #
tmpFrom <- df2015d3; tmpTo <- df2015s3
tmpFrom$ptot <- ave(tmpFrom$ptot, as.factor(tmpFrom$edon), FUN=sum, na.rm=TRUE)
tmpFrom <- tmpFrom[duplicated(tmpFrom$edon)==FALSE,]
table(tmpFrom$edon==tmpTo$edon) # verify districts match in both objects (32 TRUEs)
tmpTo$ptot <- tmpFrom$ptot
df2015s3 <- tmpTo
#
rm(tmp, tmpFrom, tmpTo, select)

########################################################################################
## *Bloc 4*:                                                                          ##
## REGRESS VOTE SHARE ON RAW TOTAL VOTE                                               ##
## These statistics underlie comments in the text (eg., in paragraph before Table 2). ##
########################################################################################
tmp <- rep(NA,5)
tmp1 <- data.frame(pancf=tmp, panp=tmp, pricf=tmp, prip=tmp, prdcf=tmp, prdp=tmp, morenacf=tmp, morenap=tmp); rownames(tmp1) <- seq(2003, 2015, 3)
# 2003
tmp <- df2003d0
#head(tmp)
tmp2 <- tmp$efec/10000
tmp1[1,1:2] <- summary(lm(tmp$pan/tmp$efec ~ tmp2))$coefficients[2,c(1,4)]
tmp1[1,3:4] <- summary(lm((tmp$pri+tmp$pric)/tmp$efec ~ tmp2))$coefficients[2,c(1,4)]
tmp1[1,5:6] <- summary(lm(tmp$prd/tmp$efec ~ tmp2))$coefficients[2,c(1,4)]
#
# 2006
tmp <- df2006d0
#head(tmp)
tmp2 <- tmp$efec/10000
tmp1[2,1:2] <- summary(lm(tmp$pan/tmp$efec ~ tmp2))$coefficients[2,c(1,4)]
tmp1[2,3:4] <- summary(lm(tmp$pric/tmp$efec ~ tmp2))$coefficients[2,c(1,4)]
tmp1[2,5:6] <- summary(lm(tmp$prdc/tmp$efec ~ tmp2))$coefficients[2,c(1,4)]
#
# 2009
tmp <- df2009d0
#head(tmp)
tmp2 <- tmp$efec/10000
tmp1[3,1:2] <- summary(lm(tmp$pan/tmp$efec ~ tmp2))$coefficients[2,c(1,4)]
tmp1[3,3:4] <- summary(lm((tmp$pri+tmp$pric)/tmp$efec ~ tmp2))$coefficients[2,c(1,4)]
tmp1[3,5:6] <- summary(lm(tmp$prd/tmp$efec ~ tmp2))$coefficients[2,c(1,4)]
#
# 2012
tmp <- df2012d0
#head(tmp)
tmp2 <- tmp$efec/10000
tmp1[4,1:2] <- summary(lm(tmp$pan/tmp$efec ~ tmp2))$coefficients[2,c(1,4)]
tmp1[4,3:4] <- summary(lm((tmp$pri+tmp$pric)/tmp$efec ~ tmp2))$coefficients[2,c(1,4)]
tmp1[4,5:6] <- summary(lm(tmp$prdc/tmp$efec ~ tmp2))$coefficients[2,c(1,4)]
#
# 2015
tmp <- df2015d0
#head(tmp)
tmp2 <- tmp$efec/10000
tmp1[5,1:2] <- summary(lm(tmp$pan/tmp$efec ~ tmp2))$coefficients[2,c(1,4)]
tmp1[5,3:4] <- summary(lm((tmp$pri+tmp$pric)/tmp$efec ~ tmp2))$coefficients[2,c(1,4)]
tmp1[5,5:6] <- summary(lm((tmp$prd+tmp$prdc)/tmp$efec ~ tmp2))$coefficients[2,c(1,4)]
tmp1[5,7:8] <- summary(lm(tmp$morena/tmp$efec ~ tmp2))$coefficients[2,c(1,4)]
#
#tmp1 <- tmp1/10000
round(tmp1,3)

## mean winning margin
tmp <- rep(NA,4)
tmp1 <- data.frame(panmg=tmp, primg=tmp, prdmg=tmp, morenamg=tmp); rownames(tmp1) <- seq(2006,2015,3)
tmp <- df2006d0
tmp1[1,1] <- mean(tmp$winmg[tmp$panw==1])
tmp1[1,2] <- mean(tmp$winmg[tmp$pricw==1])
tmp1[1,3] <- mean(tmp$winmg[tmp$prdcw==1])
tmp <- df2009d0
tmp1[2,1] <- mean(tmp$winmg[tmp$panw==1])
tmp1[2,2] <- mean(tmp$winmg[tmp$pricw==1 | tmp$priw==1])
tmp1[2,3] <- mean(tmp$winmg[tmp$prdw==1])
tmp <- df2012d0
tmp1[3,1] <- mean(tmp$winmg[tmp$panw==1])
tmp1[3,2] <- mean(tmp$winmg[tmp$pricw==1 | tmp$priw==1])
tmp1[3,3] <- mean(tmp$winmg[tmp$prdcw==1])
tmp <- df2015d0
tmp1[4,1] <- mean(tmp$winmg[tmp$panw==1])
tmp1[4,2] <- mean(tmp$winmg[tmp$pricw==1 | tmp$priw==1])
tmp1[4,3] <- mean(tmp$winmg[tmp$prdcw==1 | tmp$prdw==1])
tmp1[4,4] <- mean(tmp$winmg[tmp$morenaw==1])
round(tmp1,2)

######################################################################################################################
## *Bloc 5*:                                                                                                        ##
## IMPORT SIMULATED NATIONAL SEATS-VOTES DATA PRODUCED WITH LINZER METHOD                                           ##
## Important:                                                                                                       ##
## If you wish to re-produce Linzer draws and have not done so, you must do so by running script linzerElas.r now.  ##
## Otherwise the simulated data in the distribution will be used. You will lose all unsaved data from the current   ##
## session. You can choose to save the image now, or re-run all the above commands once linzerElas.r has finished.  ##
######################################################################################################################
#
## plot natl true and Linzer-simulated votes and seats
load(paste(dd, "swingRatios9715.RData", sep = ""))
#pdf(file = paste(wd, "graphs/vs2003.pdf", sep = ""), width = 6, height = 6)
#png(file = paste(wd, "graphs/vs2003.png", sep = ""))
dat <- swRats$df2003d0
plot(dat$vmat, dat$seatmat, type="n", xlim=0:1, ylim=0:1, ylab = "seat share", xlab = "vote share", main = 2003)
abline(a=0, b=1, lty=2)
text(t(dat$vmat), t(dat$seatmat), labels=colnames(dat$vmat), col="gray")
text(dat$truevote, dat$trueseat, labels=colnames(dat$vmat))
#dev.off()
#pdf(file = paste(wd, "graphs/vs2006.pdf", sep = ""), width = 6, height = 6)
#png(file = paste(wd, "graphs/vs2006.png", sep = ""))
dat <- swRats$df2006d0
plot(dat$vmat, dat$seatmat, type="n", xlim=0:1, ylim=0:1, ylab = "seat share", xlab = "vote share", main = 2006)
abline(a=0, b=1, lty=2)
text(t(dat$vmat), t(dat$seatmat), labels=colnames(dat$vmat), col="gray")
text(dat$truevote, dat$trueseat, labels=colnames(dat$vmat))
#dev.off()
#pdf(file = paste(wd, "graphs/vs2009.pdf", sep = ""), width = 6, height = 6)
#png(file = paste(wd, "graphs/vs2009.png", sep = ""))
dat <- swRats$df2009d0
plot(dat$vmat, dat$seatmat, type="n", xlim=0:1, ylim=0:1, ylab = "seat share", xlab = "vote share", main = 2009)
abline(a=0, b=1, lty=2)
text(t(dat$vmat), t(dat$seatmat), labels=colnames(dat$vmat), col="gray")
text(dat$truevote, dat$trueseat, labels=colnames(dat$vmat))
#dev.off()
#pdf(file = paste(wd, "graphs/vs2012.pdf", sep = ""), width = 6, height = 6)
#png(file = paste(wd, "graphs/vs2012.png", sep = ""))
dat <- swRats$df2012d0
plot(dat$vmat, dat$seatmat, type="n", xlim=0:1, ylim=0:1, ylab = "seat share", xlab = "vote share", main = 2012)
abline(a=0, b=1, lty=2)
text(t(dat$vmat), t(dat$seatmat), labels=colnames(dat$vmat), col="gray")
text(dat$truevote, dat$trueseat, labels=colnames(dat$vmat))
#dev.off()
#pdf(file = paste(wd, "graphs/vs2015.pdf", sep = ""), width = 6, height = 6)
#png(file = paste(wd, "graphs/vs2015.png", sep = ""))
dat <- swRats$df2015d0
plot(dat$vmat, dat$seatmat, type="n", xlim=0:1, ylim=0:1, ylab = "seat share", xlab = "vote share", main = 2015)
abline(a=0, b=1, lty=2)
text(t(dat$vmat), t(dat$seatmat), labels=colnames(dat$vmat), col="gray")
text(dat$truevote, dat$trueseat, labels=colnames(dat$vmat))
#dev.off()

##########################################################################################################
## *Bloc 6*:                                                                                            ##
## BAYESIAN ESTIMATION OF KING MODEL USING JAGS                                                         ##
## See http://mcmc-jags.sourceforge.net for details on how to downolad and install JAGS in your machine ##
##########################################################################################################
### Packages for JAGS
library(R2jags)
### JAGS/BUGS models
lambda.rho.A <- function() {
    ### Calvo-Micozzi likelihood
    for (i in 1:I){
        S[i] ~ dbin(pi[i], D[i])  # D is the number of SMD seats in observation i's state
        logit(pi[i]) <- c * log(N[i]-1) + lambda[pty[i]] + rho * logit(v[i])
    }
    ### priors
    for (p in 1:J){ # there are 7 party labels in the 3-election data
        lambda[p] ~ dnorm( 0, tau.lambda )
    }
    tau.lambda <- pow(.25, -2)
    rho ~ dexp(.75) # this has positive range, median close to 1, mean 1.25, max 4.5
    c ~ dnorm( 0, tau.c )
    tau.c <- pow(.25, -2)
}
##
lambda.rho.B <- function() {
    ### Calvo-Micozzi likelihood dropping N
    for (i in 1:I){
        S[i] ~ dbin(pi[i], D[i])  # D is the number of SMD seats in observation i's state
        logit(pi[i]) <- lambda[pty[i]] + rho * logit(v[i])
    }
    ### priors
    for (p in 1:J){ 
        lambda[p] ~ dnorm( 0, tau.lambda )
    }
    tau.lambda <- pow(.25, -2)
    rho ~ dexp(.75) # this has positive range, median close to 1, mean 1.25, max 4.5
}
lambda.rho.C <- function() {
    ### King likelihood with no reference party
    for (i in 1:I){ # loop over state-years
        for (j in 1:J){ # loop over parties (dummy will select those voted)
            S[i,j] ~ dbin(pi[i,j], D[i])  # D is the number of SMD seats in observation i's state
            numerator[i,j] <- dummy[i,j] * exp( lambda[j] + rho * log(v[i,j]) )
            d1[i,j] <- dummy[i,1] * exp( lambda[1] + rho * log(v[i,1]) )
            d2[i,j] <- dummy[i,2] * exp( lambda[2] + rho * log(v[i,2]) )
            d3[i,j] <- dummy[i,3] * exp( lambda[3] + rho * log(v[i,3]) )
            d4[i,j] <- dummy[i,4] * exp( lambda[4] + rho * log(v[i,4]) )
            d5[i,j] <- dummy[i,5] * exp( lambda[5] + rho * log(v[i,5]) )
            d6[i,j] <- dummy[i,6] * exp( lambda[6] + rho * log(v[i,6]) )
            d7[i,j] <- dummy[i,7] * exp( lambda[7] + rho * log(v[i,7]) )
            denominator[i,j] <- d1[i,j]+d2[i,j]+d3[i,j]+d4[i,j]+d5[i,j]+d6[i,j]+d7[i,j]
            pi[i,j] <- numerator[i,j] / denominator[i,j]
## ##             PROBAR ESTO
##             d1[i,j] <- dummy[i,1] * exp( lambda[1] ) * v[i,1])^rho
##             d2[i,j] <- dummy[i,2] * exp( lambda[2] ) * v[i,2])^rho
##             d3[i,j] <- dummy[i,3] * exp( lambda[3] ) * v[i,3])^rho
##             d4[i,j] <- dummy[i,4] * exp( lambda[4] ) * v[i,4])^rho
##             d5[i,j] <- dummy[i,5] * exp( lambda[5] ) * v[i,5])^rho
##             d6[i,j] <- dummy[i,6] * exp( lambda[6] ) * v[i,6])^rho
##             d7[i,j] <- dummy[i,7] * exp( lambda[7] ) * v[i,7])^rho
##             denominator[i,j] <- d1[i,j]+d2[i,j]+d3[i,j]+d4[i,j]+d5[i,j]+d6[i,j]+d7[i,j]
##             pi[i,j] <- numerator[i,j] / denominator[i,j]
##             S[i,j] ~ dbin(pi[i,j], D[i])  # D is the number of SMD seats in observation i's state
##             for (p in 1:P){
##                 denominator[i,j,p] <- exp( lambda[p] ) * v[i,p]^rho;
## #                numerator[i,j,p] <- dummy[i,p] * exp( lambda[p] ) * v[i,p]^rho
##                 numerator[i,j,p] <- dummy[i,p] * denominator[i,j,p]
##             }
##             pi[i,j] <- apply(X = numerator[i,j,p], MARGIN = 3, FUN = sum) / apply(X = denominator[i,j,p], MARGIN = 3, FUN = sum)
        }
    }
    ### priors
    for (p in 1:J){ # there are 7 party labels in the 3-election data
        lambda[p] ~ dnorm( 0, tau.lambda )
    }
    tau.lambda <- pow(.25, -2)
    rho ~ dexp(.75) # this has positive range, median close to 1, mean 1.25, max 4.5
}
lambda.rho.6 <- function() {
    ### King likelihood using PRI as reference party
    for (i in 1:I){     # loop over state-years
        for (j in 1:J){ # loop over parties (dummy selects those who ran that year) 
            S[i,j] ~ dbin(pi[i,j], D[i])  # D is the number of SMD seats... WHY NOT USE S[i,1:J] ~ dmulti(pi[i,1:J], D[i])
        }
        numerator[i,1] <- dummy[i,1] * exp( lambda[1] + rho * log(v[i,1]) )
        numerator[i,2] <- dummy[i,2] * exp(             rho * log(v[i,2]) )
        for (j in 3:J){
#            numerator[i,j] <- dummy[i,j] * exp( lambda[j-1] + rho * log(v[i,j]) )
            numerator[i,j] <- dummy[i,j] * exp( lambda[j-1] ) * v[i,j]^rho
        }
        for (j in 1:J){ # loop over parties (dummy selects those who ran that year) 
            d1[i,j] <- dummy[i,1] * exp( lambda[1] ) * v[i,1]^rho 
            d2[i,j] <- dummy[i,2]                    * v[i,2]^rho # pri is reference
            d3[i,j] <- dummy[i,3] * exp( lambda[2] ) * v[i,3]^rho 
            d4[i,j] <- dummy[i,4] * exp( lambda[3] ) * v[i,4]^rho 
            d5[i,j] <- dummy[i,5] * exp( lambda[4] ) * v[i,5]^rho 
            d6[i,j] <- dummy[i,6] * exp( lambda[5] ) * v[i,6]^rho 
            denominator[i,j] <- d1[i,j]+d2[i,j]+d3[i,j]+d4[i,j]+d5[i,j]+d6[i,j]
            pi[i,j] <- numerator[i,j] / denominator[i,j]
        }
    }
    ### priors
    for (p in 1:5){ # there are 6 party labels in the 3-election data, PRI is reference
        lambda[p] ~ dnorm( 0, tau.lambda )
    }
    tau.lambda <- pow(.25, -2)
    rho ~ dexp(.75) # this has positive range, median close to 1, mean 1.25, max 4.5
}
lambda.rho.5 <- function() {
    ### King likelihood using PRI as reference party
    for (i in 1:I){     # loop over state-years
        for (j in 1:J){ # loop over parties (dummy selects those who ran that year) 
            S[i,j] ~ dbin(pi[i,j], D[i])  # D is the number of SMD seats... WHY NOT USE S[i,1:J] ~ dmulti(pi[i,1:J], D[i])
        }
        numerator[i,1] <- dummy[i,1] * exp( lambda[1] + rho * log(v[i,1]) )
        numerator[i,2] <- dummy[i,2] * exp(             rho * log(v[i,2]) )
        for (j in 3:J){
#            numerator[i,j] <- dummy[i,j] * exp( lambda[j-1] + rho * log(v[i,j]) )
            numerator[i,j] <- dummy[i,j] * exp( lambda[j-1] ) * v[i,j]^rho
        }
        for (j in 1:J){ # loop over parties (dummy selects those who ran that year) 
            d1[i,j] <- dummy[i,1] * exp( lambda[1] ) * v[i,1]^rho 
            d2[i,j] <- dummy[i,2]                    * v[i,2]^rho # pri is reference
            d3[i,j] <- dummy[i,3] * exp( lambda[2] ) * v[i,3]^rho 
            d4[i,j] <- dummy[i,4] * exp( lambda[3] ) * v[i,4]^rho 
            d5[i,j] <- dummy[i,5] * exp( lambda[4] ) * v[i,5]^rho 
            denominator[i,j] <- d1[i,j]+d2[i,j]+d3[i,j]+d4[i,j]+d5[i,j]
            pi[i,j] <- numerator[i,j] / denominator[i,j]
        }
    }
    ### priors
    for (p in 1:4){ # there are 6 party labels in the 3-election data, PRI is reference
        lambda[p] ~ dnorm( 0, tau.lambda )
    }
    tau.lambda <- pow(.25, -2)
    rho ~ dexp(.75) # this has positive range, median close to 1, mean 1.25, max 4.5
}
lambda.rho.7 <- function() {
    ### King likelihood using PRI as reference party
    for (i in 1:I){     # loop over state-years
        for (j in 1:J){ # loop over parties (dummy selects those who ran that year) 
            S[i,j] ~ dbin(pi[i,j], D[i])  # D is the number of SMD seats... WHY NOT USE S[i,1:J] ~ dmulti(pi[i,1:J], D[i])
        }
        numerator[i,1] <- dummy[i,1] * exp( lambda[1] + rho * log(v[i,1]) )
        numerator[i,2] <- dummy[i,2] * exp(             rho * log(v[i,2]) )
        for (j in 3:J){
#            numerator[i,j] <- dummy[i,j] * exp( lambda[j-1] + rho * log(v[i,j]) )
            numerator[i,j] <- dummy[i,j] * exp( lambda[j-1] ) * v[i,j]^rho
        }
        for (j in 1:J){ # loop over parties (dummy selects those who ran that year) 
            d1[i,j] <- dummy[i,1] * exp( lambda[1] ) * v[i,1]^rho 
            d2[i,j] <- dummy[i,2]                    * v[i,2]^rho # pri is reference
            d3[i,j] <- dummy[i,3] * exp( lambda[2] ) * v[i,3]^rho 
            d4[i,j] <- dummy[i,4] * exp( lambda[3] ) * v[i,4]^rho 
            d5[i,j] <- dummy[i,5] * exp( lambda[4] ) * v[i,5]^rho 
            d6[i,j] <- dummy[i,6] * exp( lambda[5] ) * v[i,6]^rho 
            d7[i,j] <- dummy[i,7] * exp( lambda[6] ) * v[i,7]^rho 
            denominator[i,j] <- d1[i,j]+d2[i,j]+d3[i,j]+d4[i,j]+d5[i,j]+d6[i,j]+d7[i,j]
            pi[i,j] <- numerator[i,j] / denominator[i,j]
        }
    }
    ### priors
    for (p in 1:6){ # there are 6 party labels in the 3-election data, PRI is reference
        lambda[p] ~ dnorm( 0, tau.lambda )
    }
    tau.lambda <- pow(.25, -2)
    rho ~ dexp(.75) # this has positive range, median close to 1, mean 1.25, max 4.5
}

summary(swRats)
summary(swRats$df2015d0gree$seat)

######################################################
# wrap data prep and jags estimation in one function #
######################################################
load(paste(dd, "swingRatios9715.RData", sep = ""))
#
my.jags <- function(which.elec=2006,          # options are: 2003, 2006, 2009, 2012, 2015
                    which.map="d0",           #              "d0", "d1", "d3"
                    which.measure="v",        #              "v" for R, "v.bar" for P, "w.bar" for M
                    model.file=lambda.rho.6,
                    test.ride=TRUE,           # if TRUE, overrides n.chains, n.iter, n.thin 
                    n.chains=3,               # jags parameters
                    n.iter=50000,
                    n.thin=50
                    ){
    ####################################################################################################
    ### Data prep for national-agg King with Linzer-simulated data (data matrix with 6 party columns ###
    ####################################################################################################
    ## read swing ratio simulated data (Linzer) to estimate sigma and lamda on 1000 simulated elections each year
    if (which.measure=="v") which.measure <- "vmat";                            # v is called vmat in linzer sims
    data <- eval(parse(text=paste("swRats$df", which.elec, which.map, sep=""))) # <-- select year/map object to manipulate
    #data <- swRats$df2006d0                                                     # <-- select year/map object to manipulate
    colnames(data$seatmat) <- colnames(data$vmat)                               # linzer code does not add names here
    v <- as.data.frame(eval(parse(text=paste("data", "$", which.measure, sep=""))))          # extracts vote aggregation for estimation
    S <- as.data.frame(data$seatmat)                                          # extracts seat allocations
    #
    # orders party columns, adds zeroes for those not fielding candidates that year
    colnames(v)[which(colnames(v)=="left")] <- "prd"  # rename
    colnames(S)[which(colnames(S)=="left")] <- "prd"  # rename
    #ordered <- c("pan","pri","prd","green","mc","morena")        # 6 party reported in paper
    #ordered <- c("pan","pri","pric","prd","green","mc","morena") # 7 party alternative coalition specification
    ordered <- c("pan","pri","prd","mc","morena")                # 5 party alternative coalition specification
    tmp <- setdiff(ordered, colnames(v))
    if (length(tmp)>0) {
        tmp1 <- data.frame(matrix(0, nrow=nrow(v), ncol=length(tmp)))
        colnames(tmp1) <- setdiff(ordered, colnames(v))
        v <- cbind(v, tmp1)
        S <- cbind(S, tmp1)
    }
    v <- v[, ordered]
    S <- S[, ordered]
    #
    D <- 300 # OJO: 298 for df2003d97!
    S <- S*D # turn share into number of seats won 
    #
    I <- nrow(S)
    J <- ncol(S)
    D <- rep(D, I)
    dummy <- v; dummy[,] <- 0; dummy[v>0] <- 1 # indicates parties with v>0
    #head(v)
    # labels to interpret parameters
    party.labels <- list(party.labels.model = colnames(v),
                         lambda.labels      = paste(colnames(v)[-2], colnames(v)[2], sep = "."))
                                        #
                                        # reduce from 1000 to 100 sims to accelerate jags estimation
    S <- S[1:100,]; v <- v[1:100,]; I <- 100; D <- D[1:100]; dummy <- dummy[1:100,]
    ### Data, initial values, and parameter vector for bugs
    l.r.data <- list("S", "v", "I", "J", "D", "dummy")
    l.r.inits <- function(){ list (lambda=rnorm(J-1), rho=rexp(1)) }
    l.r.parameters <- c("lambda", "rho")
    ###########################################################
    ### End data prep for nat-agg king with Linzer sim data ###
    ###########################################################
    ## test ride
    if (test.ride==TRUE){
        n.chains=2;
        n.iter=100;
        n.thin=10;
    }
    message(sprintf("Will run jags with %s chains and %s iterations", n.chains, n.iter))
    ## estimate
    tmpRes <- jags (data=l.r.data, inits=l.r.inits, l.r.parameters,
                    model.file=model.file,
                    n.chains=n.chains,
                    n.iter=n.iter,
                    n.thin=n.thin
                    )
    tmpRes$party.labels <- party.labels # add object to interpret bias parameters: relative to pri=2
    summary(tmpRes)
    return(tmpRes)
}

tmpRes <- my.jags(which.elec = 2015,        # options are: 2003, 2006, 2009, 2012, 2015            
                  which.map  = "d0",        #              "d0", "d1", "d3"                        
                  which.measure = "w.bar",  #              "v" for R, "v.bar" for P, "w.bar" for M (RPM are Grofman et al's notation)
                  model.file=lambda.rho.5,  #                                                       
                  test.ride=TRUE            # if TRUE, overrides n.chains, n.iter, n.thin          
)

res2015d0w.bar <- tmpRes; #rm(tmpRes)
res2015d0v<-tmp$res2015d0v # ERIC is this right?
res2015d0v.bar<-tmp$res2015d0v.bar

# inspect results
quantile(tmpRes$BUGSoutput$sims.list$lambda[,1])
#
quantile(res2015d0v$BUGSoutput$sims.list$lambda[,1])                                                                # raw
quantile(res2015d0v.bar$BUGSoutput$sims.list$lambda[,1])                                                            # dist
quantile(res2015d0w.bar$BUGSoutput$sims.list$lambda[,1]) - quantile(res2015d0v.bar$BUGSoutput$sims.list$lambda[,1]) # malapp
quantile(res2015d0v$BUGSoutput$sims.list$lambda[,1])     - quantile(res2015d0w.bar$BUGSoutput$sims.list$lambda[,1]) # turnout

# create a list with all results in, save it
biasRespOnLinzerSimsRPM <- lapply(ls(pattern = "res[0-9]"), get);
names(biasRespOnLinzerSimsRPM) <- ls(pattern = "res[0-9]")
summary(biasRespOnLinzerSimsRPM)
## # uncomment and change file name to avoid overwriting distributed results
## save(biasRespOnLinzerSimsRPM,
##      file=paste(dd, "biasRespOnLinzerSims3components0315.RData", sep =""))

tmp$win <- NULL
tmp <- df2015d0; round(colSums(tmp) / sum(tmp$efec), 2)

################################
################################
###   LOAD SAVED ESTIMATES   ###
################################
################################
## run all above except jags estimations to proceed with saved data
load(file=paste(dd, "biasRespOnLinzerSims3components0315.RData", sep ="")) # results reported in publication
summary(biasRespOnLinzerSimsRPM)

## ## useful to extract all objects from a list
## laLista <- biasRespOnLinzerSimsRPM
## summary(laLista)
## for(i in 1:length(laLista)){
##   ##first extract the object value
##   tempobj=laLista[[i]]
##   ##now create a new variable with the original name of the list item
##   eval(parse(text=paste(names(laLista)[[i]],"= tempobj")))
## }
## rm(laLista)

# export pdf traceplots
gd2 <- paste(wd, "graphs/traceplots/", sep = "")
which.elec <- 2015
which.map <- "d3"
which.v <- "w.bar"
tmp <- eval(parse(text=paste("biasRespOnLinzerSimsRPM$res", which.elec, which.map, which.v, sep="")))
#summary(tmp)
pdf(paste(gd2, "traceplot", which.elec, which.map, which.v, ".pdf", sep = ""))
traceplot(tmp, ask = FALSE)


dev.off()
rm(gd2, which.elec, which.map, which.v, tmp)

tmp <- df2003d97
head(tmp)
table(tmp$ps>0)
sum(tmp$mc)/sum(tmp$efec)
sum(tmp$morenaw)/300
colnames(tmp)

# summarize central tendency of party bias ? la Grofman et al
tmp <- biasRespOnLinzerSimsRPM
#tmp <- biasResp0612oldNewDistrictsRPM
tmp1 <- data.frame(panpri=rep(NA,4), prdpri=rep(NA,4), minorpri=rep(NA,4)); rownames(tmp1) <- c("raw","dist","turn","malap")
tmp15d3 <- tmp15d0 <- tmp12d3 <- tmp12d0 <- tmp09d0 <- tmp06d0 <- tmp03d97 <- tmp03d0 <- tmp1
#
tmp03d97[1,] <- round(tmp$res2003d97v$BUGSoutput$median$lambda[1:3], 2) # 1=panpri 2=prdpri 3=pvempri RAW
tmp03d97[2,] <- round(tmp$res2003d97v.bar$BUGSoutput$median$lambda[1:3], 2) # DIST
tmp03d97[3,] <- round(tmp$res2003d97v$BUGSoutput$median$lambda[1:3] - tmp$res2003d97w.bar$BUGSoutput$median$lambda[1:3], 2)    #  TURN
tmp03d97[4,] <- round(tmp$res2003d97w.bar$BUGSoutput$median$lambda[1:3] - tmp$res2003d97v.bar$BUGSoutput$median$lambda[1:3], 2) #  MAL
#tmp03d97[,3] <- tmp03d97[,1] - tmp03d97[,2]
#
tmp03d0[1,] <- round(tmp$res2003d0v$BUGSoutput$median$lambda[1:3], 2)     # 1=panpri 2=prdpri 3=pvempri RAW
tmp03d0[2,] <- round(tmp$res2003d0v.bar$BUGSoutput$median$lambda[1:3], 2) #  DIST
tmp03d0[3,] <- round(tmp$res2003d0v$BUGSoutput$median$lambda[1:3]    - tmp$res2003d0w.bar$BUGSoutput$median$lambda[1:3], 2) #  TURN
tmp03d0[4,] <- round(tmp$res2003d0w.bar$BUGSoutput$median$lambda[1:3] - tmp$res2003d0v.bar$BUGSoutput$median$lambda[1:3], 2) #  MAL
#tmp03d0[,3] <- tmp03d0[,1] - tmp03d0[,2]
#                                        #
tmp06d0[1,] <- round(tmp$res2006d0v$BUGSoutput$median$lambda[1:3], 2) # 1=panpri 2=prdpri 3=panalpri RAW
tmp06d0[2,] <- round(tmp$res2006d0v.bar$BUGSoutput$median$lambda[1:3], 2) # DIST
tmp06d0[3,] <- round(tmp$res2006d0v$BUGSoutput$median$lambda[1:3] - tmp$res2006d0w.bar$BUGSoutput$median$lambda[1:3], 2)    # TURN
tmp06d0[4,] <- round(tmp$res2006d0w.bar$BUGSoutput$median$lambda[1:3] - tmp$res2006d0v.bar$BUGSoutput$median$lambda[1:3], 2) # MAL
#tmp06d0[,3] <- tmp06d0[,1] - tmp06d0[,2]
                                        #
tmp09d0[1,] <- round(tmp$res2009d0v$BUGSoutput$median$lambda[1:3], 2) # 1=panpri 2=prdpri 3=pvempri RAW
tmp09d0[2,] <- round(tmp$res2009d0v.bar$BUGSoutput$median$lambda[1:3], 2) #  DIST
tmp09d0[3,] <- round(tmp$res2009d0v$BUGSoutput$median$lambda[1:3] - tmp$res2009d0w.bar$BUGSoutput$median$lambda[1:3], 2)    #  TURN
tmp09d0[4,] <- round(tmp$res2009d0w.bar$BUGSoutput$median$lambda[1:3] - tmp$res2009d0v.bar$BUGSoutput$median$lambda[1:3], 2) #  MAL
#tmp09d0[,3] <- tmp09d0[,1] - tmp09d0[,2]
                                        #
tmp12d0[1,] <- round(tmp$res2012d0v$BUGSoutput$median$lambda[1:3], 2) # 1=panpri 2=prdpri 3=pvempri RAW
tmp12d0[2,] <- round(tmp$res2012d0v.bar$BUGSoutput$median$lambda[1:3], 2) #  DIST
tmp12d0[3,] <- round(tmp$res2012d0v$BUGSoutput$median$lambda[1:3] - tmp$res2012d0w.bar$BUGSoutput$median$lambda[1:3], 2)    #  TURN
tmp12d0[4,] <- round(tmp$res2012d0w.bar$BUGSoutput$median$lambda[1:3] - tmp$res2012d0v.bar$BUGSoutput$median$lambda[1:3], 2) #  MAL
#tmp12d0[,3] <- tmp12d0[,1] - tmp12d0[,2]
#
tmp12d3[1,] <- round(tmp$res2012d3v$BUGSoutput$median$lambda[1:3], 2) # 1=panpri 2=prdpri 3=pvempri RAW
tmp12d3[2,] <- round(tmp$res2012d3v.bar$BUGSoutput$median$lambda[1:3], 2) #  DIST
tmp12d3[3,] <- round(tmp$res2012d3v$BUGSoutput$median$lambda[1:3] - tmp$res2012d3w.bar$BUGSoutput$median$lambda[1:3], 2)    #  TURN
tmp12d3[4,] <- round(tmp$res2012d3w.bar$BUGSoutput$median$lambda[1:3] - tmp$res2012d3v.bar$BUGSoutput$median$lambda[1:3], 2) #  MAL
#tmp12d3[,3] <- tmp12d3[,1] - tmp12d3[,2]
#
tmp15d0[1,] <- round(tmp$res2015d0v$BUGSoutput$median$lambda[1:3], 2) # 1=panpri 2=prdpri 3=pvempri RAW
tmp15d0[2,] <- round(tmp$res2015d0v.bar$BUGSoutput$median$lambda[1:3], 2) #  DIST
tmp15d0[3,] <- round(tmp$res2015d0v$BUGSoutput$median$lambda[1:3] - tmp$res2015d0w.bar$BUGSoutput$median$lambda[1:3], 2)    #  TURN
tmp15d0[4,] <- round(tmp$res2015d0w.bar$BUGSoutput$median$lambda[1:3] - tmp$res2015d0v.bar$BUGSoutput$median$lambda[1:3], 2) #  MAL
#tmp15d0[,3] <- tmp15d0[,1] - tmp15d0[,2]
#
tmp15d3[1,] <- round(tmp$res2015d3v$BUGSoutput$median$lambda[1:3], 2) # 1=panpri 2=prdpri 3=pvempri RAW
tmp15d3[2,] <- round(tmp$res2015d3v.bar$BUGSoutput$median$lambda[1:3], 2) #  DIST
tmp15d3[3,] <- round(tmp$res2015d3v$BUGSoutput$median$lambda[1:3] - tmp$res2015d3w.bar$BUGSoutput$median$lambda[1:3], 2)    #  TURN
tmp15d3[4,] <- round(tmp$res2015d3w.bar$BUGSoutput$median$lambda[1:3] - tmp$res2015d3v.bar$BUGSoutput$median$lambda[1:3], 2) #  MAL
#tmp12d3[,3] <- tmp12d3[,1] - tmp12d3[,2]
#
#
tmp03d97
tmp03d0
tmp06d0
tmp09d0
tmp12d0
tmp12d3
tmp15d0
tmp15d3

# summarize errors of party bias a la Grofman
nOppoSign <- function(x){
    sh <- round( sum(x>0) / length(x), 2)
    if (median(x)<=0) {
        ret <- sh
    } else {
        ret <- 1 - sh
      }
    return(ret)
}
tmp <- biasRespOnLinzerSimsRPM
tmp1 <- data.frame(panpri=rep(NA,4), prdpri=rep(NA,4), minorpri=rep(NA,4)); rownames(tmp1) <- c("raw","dist","turn","malap")
tmp15d3 <- tmp15d0 <- tmp12d3 <- tmp12d0 <- tmp09d0 <- tmp06d0 <- tmp03d97 <- tmp03d0 <- tmp1
#
tmp1 <- tmp$res2003d97v$BUGSoutput$sims.list$lambda[,1:3] #  RAW
tmp03d97[1,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
#tmp03d97[1,3]   <- nOppoSign(tmp1[,1] - tmp1[,2]) # 3=panprd         
tmp1 <- tmp$res2003d97v.bar$BUGSoutput$sims.list$lambda[,1:3] #  DIST
tmp03d97[2,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
#tmp03d97[2,3]   <- nOppoSign(tmp1[,1] - tmp1[,2]) # 3=panprd        
tmp1 <- tmp$res2003d97v$BUGSoutput$sims.list$lambda[,1:3] - tmp$res2003d97w.bar$BUGSoutput$sims.list$lambda[1:3] #  TURN
tmp03d97[3,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
#tmp03d97[3,3]   <- nOppoSign(tmp1[,1] - tmp1[,2]) # 3=panprd        
tmp1 <- tmp$res2003d97w.bar$BUGSoutput$sims.list$lambda[,1:3] - tmp$res2003d97v.bar$BUGSoutput$sims.list$lambda[1:3] #  MALAP
tmp03d97[4,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
#tmp03d97[4,3]   <- nOppoSign(tmp1[,1] - tmp1[,2]) # 3=panprd        
#                                        #
tmp1 <- tmp$res2003d0v$BUGSoutput$sims.list$lambda[,1:3] #  RAW
tmp03d0[1,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
#tmp03d0[1,3]   <- nOppoSign(tmp1[,1] - tmp1[,2]) # 3=panprd        
tmp1 <- tmp$res2003d0v.bar$BUGSoutput$sims.list$lambda[,1:3] #  DIST
tmp03d0[2,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
#tmp03d0[2,3]   <- nOppoSign(tmp1[,1] - tmp1[,2]) # 3=panprd        
tmp1 <- tmp$res2003d0v$BUGSoutput$sims.list$lambda[,1:3] - tmp$res2003d0w.bar$BUGSoutput$sims.list$lambda[1:3] #  TURN
tmp03d0[3,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
#tmp03d0[3,3]   <- nOppoSign(tmp1[,1] - tmp1[,2]) # 3=panprd        
tmp1 <- tmp$res2003d0w.bar$BUGSoutput$sims.list$lambda[,1:3] - tmp$res2003d0v.bar$BUGSoutput$sims.list$lambda[1:3] # MALAP
tmp03d0[4,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
#tmp03d0[4,3]   <- nOppoSign(tmp1[,1] - tmp1[,2]) # 3=panprd        
#                                        #
tmp1 <- tmp$res2006d0v$BUGSoutput$sims.list$lambda[,1:3] #  RAW
tmp06d0[1,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=panalpri
#tmp06d0[1,3]   <- nOppoSign(tmp1[,1] - tmp1[,2]) # 3=panprd        
tmp1 <- tmp$res2006d0v.bar$BUGSoutput$sims.list$lambda[,1:3] #  DIST
tmp06d0[2,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=panalpri
#tmp06d0[2,3]   <- nOppoSign(tmp1[,1] - tmp1[,2]) # 3=panprd        
tmp1 <- tmp$res2006d0v$BUGSoutput$sims.list$lambda[,1:3] - tmp$res2006d0w.bar$BUGSoutput$sims.list$lambda[1:3] #  TURN
tmp06d0[3,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=panalpri
#tmp06d0[3,3]   <- nOppoSign(tmp1[,1] - tmp1[,2]) # 3=panprd        
tmp1 <- tmp$res2006d0w.bar$BUGSoutput$sims.list$lambda[,1:3] - tmp$res2006d0v.bar$BUGSoutput$sims.list$lambda[1:3] # MALAP
tmp06d0[4,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=panalpri
#tmp06d0[4,3]   <- nOppoSign(tmp1[,1] - tmp1[,2]) # 3=panprd        
                                        #
tmp1 <- tmp$res2009d0v$BUGSoutput$sims.list$lambda[,1:3] #  RAW
tmp09d0[1,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
#tmp09d0[1,3]   <- nOppoSign(tmp1[,1] - tmp1[,2]) # 3=panprd        
tmp1 <- tmp$res2009d0v.bar$BUGSoutput$sims.list$lambda[,1:3] #  DIST
tmp09d0[2,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
#tmp09d0[2,3]   <- nOppoSign(tmp1[,1] - tmp1[,2]) # 3=panprd        
tmp1 <- tmp$res2009d0v$BUGSoutput$sims.list$lambda[,1:3] - tmp$res2009d0w.bar$BUGSoutput$sims.list$lambda[1:3] #  TURN
tmp09d0[3,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
#tmp09d0[3,3]   <- nOppoSign(tmp1[,1] - tmp1[,2]) # 3=panprd        
tmp1 <- tmp$res2009d0w.bar$BUGSoutput$sims.list$lambda[,1:3] - tmp$res2009d0v.bar$BUGSoutput$sims.list$lambda[1:3] # MALAP
tmp09d0[4,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
#tmp09d0[4,3]   <- nOppoSign(tmp1[,1] - tmp1[,2]) # 3=panprd        
                                        #
tmp1 <- tmp$res2012d0v$BUGSoutput$sims.list$lambda[,1:3] #  RAW
tmp12d0[1,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
#tmp12d0[1,3]   <- nOppoSign(tmp1[,1] - tmp1[,2]) # 3=panprd        
tmp1 <- tmp$res2012d0v.bar$BUGSoutput$sims.list$lambda[,1:3] #  DIST
tmp12d0[2,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
#tmp12d0[2,3]   <- nOppoSign(tmp1[,1] - tmp1[,2]) # 3=panprd        
tmp1 <- tmp$res2012d0v$BUGSoutput$sims.list$lambda[,1:3] - tmp$res2012d0w.bar$BUGSoutput$sims.list$lambda[1:3] #  TURN
tmp12d0[3,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
#tmp12d0[3,3]   <- nOppoSign(tmp1[,1] - tmp1[,2]) # 3=panprd        
tmp1 <- tmp$res2012d0w.bar$BUGSoutput$sims.list$lambda[,1:3] - tmp$res2012d0v.bar$BUGSoutput$sims.list$lambda[1:3] # MALAP
tmp12d0[4,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
#tmp12d0[4,3]   <- nOppoSign(tmp1[,1] - tmp1[,2]) # 3=panprd        
#
tmp1 <- tmp$res2012d3v$BUGSoutput$sims.list$lambda[,1:3] #  RAW
tmp12d3[1,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
#tmp12d3[1,3]   <- nOppoSign(tmp1[,1] - tmp1[,2]) # 3=panprd        
tmp1 <- tmp$res2012d3v.bar$BUGSoutput$sims.list$lambda[,1:3] #  DIST
tmp12d3[2,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
#tmp12d3[2,3]   <- nOppoSign(tmp1[,1] - tmp1[,2]) # 3=panprd        
tmp1 <- tmp$res2012d3v$BUGSoutput$sims.list$lambda[,1:3] - tmp$res2012d3w.bar$BUGSoutput$sims.list$lambda[1:3] #  TURN
tmp12d3[3,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
#tmp12d3[3,3]   <- nOppoSign(tmp1[,1] - tmp1[,2]) # 3=panprd        
tmp1 <- tmp$res2012d3w.bar$BUGSoutput$sims.list$lambda[,1:3] - tmp$res2012d3v.bar$BUGSoutput$sims.list$lambda[1:3] # MALAP
tmp12d3[4,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
#tmp12d3[4,3]   <- nOppoSign(tmp1[,1] - tmp1[,2]) # 3=panprd
#
tmp1 <- tmp$res2015d0v$BUGSoutput$sims.list$lambda[,1:3] #  RAW
tmp15d0[1,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
#tmp15d0[1,3]   <- nOppoSign(tmp1[,1] - tmp1[,2]) # 3=panprd        
tmp1 <- tmp$res2015d0v.bar$BUGSoutput$sims.list$lambda[,1:3] #  DIST
tmp15d0[2,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
#tmp15d0[2,3]   <- nOppoSign(tmp1[,1] - tmp1[,2]) # 3=panprd        
tmp1 <- tmp$res2015d0v$BUGSoutput$sims.list$lambda[,1:3] - tmp$res2015d0w.bar$BUGSoutput$sims.list$lambda[1:3] #  TURN
tmp15d0[3,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
#tmp15d0[3,3]   <- nOppoSign(tmp1[,1] - tmp1[,2]) # 3=panprd        
tmp1 <- tmp$res2015d0w.bar$BUGSoutput$sims.list$lambda[,1:3] - tmp$res2015d0v.bar$BUGSoutput$sims.list$lambda[1:3] # MALAP
tmp15d0[4,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
#tmp15d0[4,3]   <- nOppoSign(tmp1[,1] - tmp1[,2]) # 3=panprd        
#
tmp1 <- tmp$res2015d3v$BUGSoutput$sims.list$lambda[,1:3] #  RAW
tmp15d3[1,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
#tmp15d3[1,3]   <- nOppoSign(tmp1[,1] - tmp1[,2]) # 3=panprd        
tmp1 <- tmp$res2015d3v.bar$BUGSoutput$sims.list$lambda[,1:3] #  DIST
tmp15d3[2,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
#tmp15d3[2,3]   <- nOppoSign(tmp1[,1] - tmp1[,2]) # 3=panprd        
tmp1 <- tmp$res2015d3v$BUGSoutput$sims.list$lambda[,1:3] - tmp$res2015d3w.bar$BUGSoutput$sims.list$lambda[1:3] #  TURN
tmp15d3[3,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
#tmp15d3[3,3]   <- nOppoSign(tmp1[,1] - tmp1[,2]) # 3=panprd        
tmp1 <- tmp$res2015d3w.bar$BUGSoutput$sims.list$lambda[,1:3] - tmp$res2015d3v.bar$BUGSoutput$sims.list$lambda[1:3] # MALAP
tmp15d3[4,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
#tmp15d3[4,3]   <- nOppoSign(tmp1[,1] - tmp1[,2]) # 3=panprd        
#
tmp03d97
tmp03d0
tmp06d0
tmp09d0
tmp12d0
tmp12d3
tmp15d0
tmp15d3


# summarize responsiveness parameters (90% and median estimated with raw vote --- R in GKB's terminology)
tmp <- biasRespOnLinzerSimsRPM
round( quantile(tmp$res2003d97v$BUGSoutput$sims.list$rho, probs = c(.05, .5, .95)), 2)
round( quantile(tmp$res2006d0v$BUGSoutput$sims.list$rho, probs = c(.05, .5, .95)), 2)
round( quantile(tmp$res2009d0v$BUGSoutput$sims.list$rho, probs = c(.05, .5, .95)), 2)
round( quantile(tmp$res2012d0v$BUGSoutput$sims.list$rho, probs = c(.05, .5, .95)), 2)
round( quantile(tmp$res2015d0v$BUGSoutput$sims.list$rho, probs = c(.05, .5, .95)), 2)
#round( quantile(tmp$res0612d0v$BUGSoutput$sims.list$rho, probs = c(.05, .5, .95)), 2)


###########################################################################
## REPORT EFFECTS OF SPECIFYING PRI-GREEN PARTIAL COALITIONS DIFFERENTLY ##
###########################################################################
#wd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/git-repo/mex-open-map/")  # where to save and retrieve objects
#dd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/git-repo/mex-open-map/data/") # raw data directory
load(file=paste(dd, "biasRespOnLinzerSims3components2015altCoalSpecs.RData", sep ="")) # 2015 results w alternative partial coalition handling
#
biasRespOnLinzerSimsRPMaltCoalSpecs$res2015d0TWSv$party.labels
summary(biasRespOnLinzerSimsRPMaltCoalSpecs$res2015d0TWSv$BUGSoutput$sims.list$lambda)
summary(biasRespOnLinzerSimsRPMaltCoalSpecs$res2015d0TWSv.bar$BUGSoutput$sims.list$lambda)
tmp <- biasRespOnLinzerSimsRPMaltCoalSpecs$res2015d0TWSv$BUGSoutput$sims.list$lambda -
       biasRespOnLinzerSimsRPMaltCoalSpecs$res2015d0TWSw.bar$BUGSoutput$sims.list$lambda
summary(tmp)
tmp <- biasRespOnLinzerSimsRPMaltCoalSpecs$res2015d0TWSw.bar$BUGSoutput$sims.list$lambda -
       biasRespOnLinzerSimsRPMaltCoalSpecs$res2015d0TWSv.bar$BUGSoutput$sims.list$lambda
summary(tmp)
#
table(biasRespOnLinzerSimsRPMaltCoalSpecs$res2015d0TWSv$BUGSoutput$sims.list$lambda[,1]>0)
table(biasRespOnLinzerSimsRPMaltCoalSpecs$res2015d0TWSv$BUGSoutput$sims.list$lambda[,3]>0)
table(biasRespOnLinzerSimsRPMaltCoalSpecs$res2015d0TWSv$BUGSoutput$sims.list$lambda[,4]>0)
table(biasRespOnLinzerSimsRPMaltCoalSpecs$res2015d0TWSv$BUGSoutput$sims.list$lambda[,2]>0)
#
table(biasRespOnLinzerSimsRPMaltCoalSpecs$res2015d0TWSv.bar$BUGSoutput$sims.list$lambda[,1]>0)
table(biasRespOnLinzerSimsRPMaltCoalSpecs$res2015d0TWSv.bar$BUGSoutput$sims.list$lambda[,3]>0)
table(biasRespOnLinzerSimsRPMaltCoalSpecs$res2015d0TWSv.bar$BUGSoutput$sims.list$lambda[,4]>0)
table(biasRespOnLinzerSimsRPMaltCoalSpecs$res2015d0TWSv.bar$BUGSoutput$sims.list$lambda[,2]>0)
#
tmp <- biasRespOnLinzerSimsRPMaltCoalSpecs$res2015d0TWSv$BUGSoutput$sims.list$lambda -
       biasRespOnLinzerSimsRPMaltCoalSpecs$res2015d0TWSw.bar$BUGSoutput$sims.list$lambda
table(tmp[,1]>0)/1500
table(tmp[,3]>0)/1500
table(tmp[,4]>0)/1500
table(tmp[,2]>0)/1500
#
tmp <- biasRespOnLinzerSimsRPMaltCoalSpecs$res2015d0TWSw.bar$BUGSoutput$sims.list$lambda -
       biasRespOnLinzerSimsRPMaltCoalSpecs$res2015d0TWSv.bar$BUGSoutput$sims.list$lambda
table(tmp[,1]>0)/1500
table(tmp[,3]>0)/1500
table(tmp[,4]>0)/1500
table(tmp[,2]>0)/1500
#
#
biasRespOnLinzerSimsRPMaltCoalSpecs$res2015d0GAPFv$party.labels
summary(biasRespOnLinzerSimsRPMaltCoalSpecs$res2015d0GAPFv$BUGSoutput$sims.list$lambda)
summary(biasRespOnLinzerSimsRPMaltCoalSpecs$res2015d0GAPFv.bar$BUGSoutput$sims.list$lambda)
tmp <- biasRespOnLinzerSimsRPMaltCoalSpecs$res2015d0GAPFv$BUGSoutput$sims.list$lambda -
       biasRespOnLinzerSimsRPMaltCoalSpecs$res2015d0GAPFw.bar$BUGSoutput$sims.list$lambda
summary(tmp)
tmp <- biasRespOnLinzerSimsRPMaltCoalSpecs$res2015d0GAPFw.bar$BUGSoutput$sims.list$lambda -
       biasRespOnLinzerSimsRPMaltCoalSpecs$res2015d0GAPFv.bar$BUGSoutput$sims.list$lambda
summary(tmp)
#
table(biasRespOnLinzerSimsRPMaltCoalSpecs$res2015d0GAPFv$BUGSoutput$sims.list$lambda[,1]>0)/1500
table(biasRespOnLinzerSimsRPMaltCoalSpecs$res2015d0GAPFv$BUGSoutput$sims.list$lambda[,2]>0)/1500
#
table(biasRespOnLinzerSimsRPMaltCoalSpecs$res2015d0GAPFv.bar$BUGSoutput$sims.list$lambda[,1]>0)/1500
table(biasRespOnLinzerSimsRPMaltCoalSpecs$res2015d0GAPFv.bar$BUGSoutput$sims.list$lambda[,2]>0)/1500
#
tmp <- biasRespOnLinzerSimsRPMaltCoalSpecs$res2015d0GAPFv$BUGSoutput$sims.list$lambda -
       biasRespOnLinzerSimsRPMaltCoalSpecs$res2015d0GAPFw.bar$BUGSoutput$sims.list$lambda
table(tmp[,1]>0)/1500
table(tmp[,2]>0)/1500
#
tmp <- biasRespOnLinzerSimsRPMaltCoalSpecs$res2015d0GAPFw.bar$BUGSoutput$sims.list$lambda -
       biasRespOnLinzerSimsRPMaltCoalSpecs$res2015d0GAPFv.bar$BUGSoutput$sims.list$lambda
table(tmp[,1]>0)/1500
table(tmp[,2]>0)/1500
rm(tmp)

##########################################################
# compute swing ratios with regressions from Linzer sims #
##########################################################
#wd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/git-repo/mex-open-map/")  # where to save and retrieve objects
#dd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/git-repo/mex-open-map/data/") # raw data directory
load(paste(dd, "swingRatios9715.RData", sep = ""))

# single-map version
tmp <- swRats$df2012d0
s <- tmp$seatmat
v <- tmp$vmat
v.bar <- tmp$v.barmat
w.bar <- tmp$w.barmat
colnames(s) <- colnames(v)
res <- head(v, n=6); res[] <- NA; rownames(res) <- c("swR","se","p","lo95","hi95","r2") # generate empty matrix with proper col/row names
    #
    for (i in 1:ncol(v)){
#    i <- 1 # debug
        vreg <- lm(s[,i] ~ v[,i])
#    v.barreg <- lm(s[,i] ~ v.bar[,i])
#    w.barreg <- lm(s[,i] ~ w.bar[,i])
        res["swR",i] <- vreg$coefficients[2]
        res["se",i]  <- summary(vreg)$sigma
        res["r2",i]  <- summary(vreg)$r.squared
        res["p",i] <- round(summary(vreg)$coefficients[2,"Pr(>|t|)"], 3)
        res["lo95",i] <- res["swR",i] - 2 * res["se",i]
        res["hi95",i] <- res["swR",i] + 2 * res["se",i]
    }
res <- round(res,2)
res[1:2,]

# two maps pooled version
tmp <- swRats$df2006d0
tmp1 <- tmp$seatmat
tmp2 <- tmp$vmat
tmp3 <- tmp$v.barmat
tmp4 <- tmp$w.barmat
colnames(tmp1) <- colnames(tmp2)
tmp5 <- rep(0, nrow(tmp1))
s <- tmp1
v <- tmp2
v.bar <- tmp3
w.bar <- tmp4
dmap <- tmp5
#
tmp <- swRats$df2006d3
tmp1 <- tmp$seatmat
tmp2 <- tmp$vmat
tmp3 <- tmp$v.barmat
tmp4 <- tmp$w.barmat
colnames(tmp1) <- colnames(tmp2)
tmp5 <- rep(1, nrow(tmp1))
s <- rbind(s, tmp1)
v <- rbind(v, tmp2)
v.bar <- rbind(v.bar, tmp3)
w.bar <- rbind(w.bar, tmp4)
dmap <- c(dmap, tmp5)
#
dmapxv <- v*dmap
    #
for (i in 1:3){#ncol(v)){
                                        #    i <- 2 # debug
    vreg  <- lm(s[,i] ~ v[,i])
    vreg2 <- lm(s[,i] ~ v[,i] + dmap + dmapxv[,i])
    print( colnames(v)[i])
    print(round(summary(vreg2)$coefficients[c(2,4),1:2], 2))
                                        #    v.barreg <- lm(s[,i] ~ v.bar[,i])
                                        #    w.barreg <- lm(s[,i] ~ w.bar[,i])
    ## res["swR",i] <- vreg$coefficients[2]
    ## res["se",i]  <- summary(vreg)$sigma
    ## res["r2",i]  <- summary(vreg)$r.squared
    ## res["p",i] <- round(summary(vreg)$coefficients[2,"Pr(>|t|)"], 3)
    ## res["lo95",i] <- res["swR",i] - 2 * res["se",i]
    ## res["hi95",i] <- res["swR",i] + 2 * res["se",i]
}

# predict seats from pan=.33, pri=.33, prd=.33, rest=.01 (Marquez's unlikely but illustrative scenario).
res <- biasRespOnLinzerSimsRPM$res2012d0v;
seats <- function(pan.sh=.3, pri.sh=.35, prd.sh=.25){
    rest <- 1-pan.sh-pri.sh-prd.sh;
    denom <- exp(res$BUGSoutput$sims.list$lambda[,1]) * pan.sh^res$BUGSoutput$sims.list$rho +
                                                        pri.sh^res$BUGSoutput$sims.list$rho +
             exp(res$BUGSoutput$sims.list$lambda[,2]) * prd.sh^res$BUGSoutput$sims.list$rho +
             exp(res$BUGSoutput$sims.list$lambda[,3]) * (rest/2)^res$BUGSoutput$sims.list$rho +
             exp(res$BUGSoutput$sims.list$lambda[,4]) * (rest/2)^res$BUGSoutput$sims.list$rho # drops ptc & asdc
    pan.seats <- (exp(res$BUGSoutput$sims.list$lambda[,1]) * pan.sh^res$BUGSoutput$sims.list$rho) / denom;
    pri.seats <-                                             pri.sh^res$BUGSoutput$sims.list$rho  / denom;
    prd.seats <- (exp(res$BUGSoutput$sims.list$lambda[,2]) * prd.sh^res$BUGSoutput$sims.list$rho) / denom;
    return(list(pan.seats=pan.seats, pri.seats=pri.seats, prd.seats=prd.seats))
}

#ERIC Fails here... 
seat.sh <- seats(pan.sh = .33, pri.sh = .33, prd.sh = .33)
quantile(x = seat.sh$pan.seats * 300, probs = c(.025,.975))
quantile(x = seat.sh$pri.seats * 300, probs = c(.025,.975))
quantile(x = seat.sh$prd.seats * 300, probs = c(.025,.975))
# would be preferable to model S[i,j] with multinomial in jags model above and then report the actual posterior simulations

#c.hat <- res$BUGSoutput$median$c
lambda.hat <- res$BUGSoutput$median$lambda
rho.hat <- res$BUGSoutput$median$rho
#
### Uncomment if using reference party (model lambda.rho.6)
## PRI is the reference party with lambda=0
lambda.hat <- c(lambda.hat[1], 0, lambda.hat[2:6])
#

## PERFORMS A VISUAL INSPECTION OF MODEL PARAMETERS
#save.dir <- "~/Dropbox/mydocs/op-edsEtc/blog/graphs/" # donde guardar
save.dir <- paste(wd, "graphs/", sep = "")
logit <- function(X){ log( X / (1-X) ) }
antilogit <- function(X){ exp(X) / (exp(X)+1) }
v.tmp <- (1:999)/1000
#
library(tikzDevice)
#title <- expression(paste("La responsividad ", rho, " y sesgo ", lambda, " de los distritos")) 
#title <- expression(paste("District responsiveness ", rho, " (and partisan bias ", lambda, ">0 in grey)"))
title <- ""
file <- "rhoExample"
#tikz(file = paste(save.dir, file, ".tex", sep=""), width = 4, height = 4)
#pdf (file = paste(save.dir, file, ".pdf", sep=""), width = 6,   height = 6)
#png (file = paste(save.dir, file, ".png", sep=""), width = 6,   height = 6)
##library(Cairo)
## type <-  "pdf" 
## Cairo(file=file,
##       type = type,
##       width = 6,
##       height = 6,
##       units = "in",
##       dpi = 72,
##       bg = "white")
##       #bg = "transparent")
#
plot(c(0,1),c(0,1), type = "n",
#     xlab = "% votos", ylab = "% esca?os", axes = FALSE,
     xlab = "vote share", ylab = "seat share", axes = FALSE,
     main = title)
#axis(1, at = seq(0,1,.2), labels = seq(0,100,20))
#axis(2, at = seq(0,1,.2), labels = seq(0,100,20))
axis(1, at = seq(0,1,.2), labels = seq(0,1,.2))
axis(2, at = seq(0,1,.2), labels = seq(0,1,.2))
#
# versiones con sesgo en grises
s.tmp <- antilogit( 1.5 + 1 * logit( v.tmp ) )
lines(v.tmp, s.tmp, lty = 2, col="grey")
s.tmp <- antilogit ( 1.5 + 3 * logit( v.tmp ) ) 
#lines(v.tmp, s.tmp, col = "grey") 
lines(v.tmp, s.tmp, col = "grey", lty = 3) 
s.tmp <- antilogit ( 1.5 + 15 * logit( v.tmp ) ) # ESTO HACE LO DEBIDO: SESGO DE .25 EN FAVOR
lines(v.tmp, s.tmp, col = "grey")
#
s.tmp <- antilogit( 1 * logit( v.tmp ) )
#lines(v.tmp, s.tmp, col = "darkgreen", lty = 2)
lines(v.tmp, s.tmp, col = "black", lty = 2)
text(v.tmp[660], s.tmp[660], labels = expression(paste(rho, "=1")), pos=4)
#text(v.tmp[660], s.tmp[660], labels = "$\\rho=1$", pos=4)
s.tmp <- antilogit ( 3 * logit( v.tmp ) ) 
#lines(v.tmp, s.tmp, col = "red") 
lines(v.tmp, s.tmp, col = "black", lty = 3) 
text(v.tmp[600], s.tmp[600], labels = expression(paste(rho, "=3")), pos=4)
#text(v.tmp[600], s.tmp[600], labels = "$\\rho=3$", pos=4)
s.tmp <- antilogit ( 15 * logit( v.tmp ) ) # ESTO HACE LO DEBIDO: SESGO DE .25 EN FAVOR
#lines(v.tmp, s.tmp, col = "blue")
lines(v.tmp, s.tmp, col = "black")
text(v.tmp[440], s.tmp[440], labels = expression(rho %->% infinity), pos=4)
#text(v.tmp[420], s.tmp[420], labels = "$\\rho=6$", pos=4)
#mtext(text = "Preparado por Eric Magar con resultados oficiales del IFE", side = 1, line = 4, col = "grey", cex = .75)
#mtext(text = "Prepared by Eric Magar with official IFE returns", side = 1, line = 4, col = "grey", cex = .75)
#
#dev.off()


#### Plot Posterior lambda Samples For d0 And d3
#tmp <- biasResp0612oldNewDistrictsRPM # compact name
tmp <- biasRespOnLinzerSimsRPM
myQ <- function(Q,i){ # function to extract quantiles from posterior sample for plot: Q is desired quantile, i desired index number
    return(quantile(x= res$BUGSoutput$sims.list$lambda[,i], probs = Q, names=FALSE))
}
#save.dir <- "~/Dropbox/mydocs/op-edsEtc/blog/graphs/" # donde guardar
save.dir <- paste(wd, "graphs", sep = "")
logit <- function(X){ log( X / (1-X) ) }
antilogit <- function(X){ exp(X) / (exp(X)+1) }
v.tmp <- (1:999)/1000
#
library(Cairo)
type <-  "pdf" 
## #file <- paste("bias200612d0R.", type, sep="")
## file <- paste("bias200615d0v.", type, sep="")
## setwd(save.dir)
## Cairo(file=file,
##       type = type,
##       width = 7,
##       height = 7,
##       units = "in",
##       dpi = 72,
##       bg = "transparent")
#
par(mar=c(5,2,2,2)+0.1) # drop space for title and left labels
#jitter <- runif(n = 300, min=-.1, max=.1)
jitter <- rnorm(n = 300, sd = .03)
color1.minus.pri <- c( "blue", "gold", "green", "cyan", "orange", "violet" )
#
# 2003
res <- tmp$res2003d0v; shift.v <- .35
plot( c( -2.5, 2.5), -c(.5,5-.35),
     type="n", axes = FALSE, ylab = "", xlab = "bias relative to PRI")#, main = "Party bias")#"Bias: 2015 map (hypothetical)")
#     type="n", axes = FALSE, ylab = "", xlab = "sesgo en relaci?n al PRI", main = "Distritos propuestos")#"Distritos en vigor")
axis( side = 1, at = seq(from = -2.25, to = 2.25, by = .25), labels = FALSE)
axis( side = 1, at = seq(from = -2, to = 2, by = 1), labels = c("-2","-1","0","+1","+2"))
abline(v=seq(-2,2,.5), col= "gray70")
abline(v=0, lty=2)
abline(h=seq(-4.5,-1.5,1), lty=3, col= "gray70")
#for (i in 1:6){
for (i in c(1:4)){ # some parties absent or dropped
    # if party color desired, this does the trick: col = color1.minus.pri[i]
    points(sample(res$BUGSoutput$sims.list$lambda[,i], 300), -i+shift.v+jitter, cex=.1, col = "gray70");
    lines(x = c(myQ(.05,i),myQ(.95,i)), y = c(-i+shift.v,-i+shift.v), lwd = 2)
    lines(x = c(myQ(.25,i),myQ(.75,i)), y = c(-i+shift.v,-i+shift.v), lwd = 6)
    points(myQ(.5,i), -i+shift.v, pch = 19, col="white")
    #points(myQ(.5,i), -i+shift.v, pch = 19, cex = .5, col=color1.minus.pri[i])
    points(myQ(.5,i), -i+shift.v, pch = 19, cex = .5)
}
#
# 2006
res <- tmp$res2006d0v; shift.v <- .175
#res$BUGSoutput$sims.list$lambda[,4] <- res$BUGSoutput$sims.list$lambda[,3] # move panal to 4th (3rd will be ignored)
#for (i in 1:6){
for (i in c(1:2)){ # some parties absent or dropped
    # if party color desired, this does the trick: col = color1.minus.pri[i]
    points(sample(res$BUGSoutput$sims.list$lambda[,i], 300), -i+shift.v+jitter, cex=.1, col = "gray70");
    lines(x = c(myQ(.05,i),myQ(.95,i)), y = c(-i+shift.v,-i+shift.v), lwd = 2)
    lines(x = c(myQ(.25,i),myQ(.75,i)), y = c(-i+shift.v,-i+shift.v), lwd = 6)
    points(myQ(.5,i), -i+shift.v, pch = 19, col="white")
    #points(myQ(.5,i), -i+shift.v, pch = 19, cex = .5, col=color1.minus.pri[i])
    points(myQ(.5,i), -i+shift.v, pch = 19, cex = .5)
}
#
# 2009
res <- tmp$res2009d0v; shift.v <- 0
for (i in 1:4){ # some parties absent or dropped
    # if party color desired, this does the trick: col = color1.minus.pri[i]
    points(sample(res$BUGSoutput$sims.list$lambda[,i], 300), -i+shift.v+jitter, cex=.1, col = "gray70");
    lines(x = c(myQ(.05,i),myQ(.95,i)), y = c(-i+shift.v,-i+shift.v), lwd = 2)
    lines(x = c(myQ(.25,i),myQ(.75,i)), y = c(-i+shift.v,-i+shift.v), lwd = 6)
    points(myQ(.5,i), -i+shift.v, pch = 19, col="white")
    #points(myQ(.5,i), -i+shift.v, pch = 19, cex = .5, col=color1.minus.pri[i])
    points(myQ(.5,i), -i+shift.v, pch = 19, cex = .5)
}
#
# 2012
#res <- tmp$res0612s0R; shift.v <- -.3
res <- tmp$res2012d0v; shift.v <- -.175 # use nation estimates
for (i in 1:3){ # some parties absent
    # if party color desired, this does the trick: col = color1.minus.pri[i]
    #points(res$BUGSoutput$sims.list$lambda[,i], -i+shift.v+jitter, cex=.1, col = "gray70");
    points(sample(res$BUGSoutput$sims.list$lambda[,i], 300), -i+shift.v+jitter, cex=.1, col = "gray70"); # sample to get 300 points
    lines(x = c(myQ(.05,i),myQ(.95,i)), y = c(-i+shift.v,-i+shift.v), lwd = 2)
    lines(x = c(myQ(.25,i),myQ(.75,i)), y = c(-i+shift.v,-i+shift.v), lwd = 6)
    points(myQ(.5,i), -i+shift.v, pch = 19, col="white")
    #points(myQ(.5,i), -i+shift.v, pch = 19, cex = .5, col=color1.minus.pri[i])
    points(myQ(.5,i), -i+shift.v, pch = 19, cex = .5)
}
#
# 2015
res <- tmp$res2015d0v; shift.v <- -.35 # use nation estimates

for (i in 1:4){ # some parties absent (Morena treated separately bec y shift
    # if party color desired, this does the trick: col = color1.minus.pri[i]
    #points(res$BUGSoutput$sims.list$lambda[,i], -i+shift.v+jitter, cex=.1, col = "gray70");
    points(sample(res$BUGSoutput$sims.list$lambda[,i], 300), -i+shift.v+jitter, cex=.1, col = "gray70"); # sample to get 300 points
    lines(x = c(myQ(.05,i),myQ(.95,i)), y = c(-i+shift.v,-i+shift.v), lwd = 2)
    lines(x = c(myQ(.25,i),myQ(.75,i)), y = c(-i+shift.v,-i+shift.v), lwd = 6)
    points(myQ(.5,i), -i+shift.v, pch = 19, col="white")
    #points(myQ(.5,i), -i+shift.v, pch = 19, cex = .5, col=color1.minus.pri[i])
    points(myQ(.5,i), -i+shift.v, pch = 19, cex = .5)
}
# morena 2015 separate bec y shift dift
i <- 5
shift.v <- .35
points(sample(res$BUGSoutput$sims.list$lambda[,i], 300), -i+shift.v+jitter, cex=.1, col = "gray70"); # sample to get 300 points
lines(x = c(myQ(.05,i),myQ(.95,i)), y = c(-i+shift.v,-i+shift.v), lwd = 2)
lines(x = c(myQ(.25,i),myQ(.75,i)), y = c(-i+shift.v,-i+shift.v), lwd = 6)
points(myQ(.5,i), -i+shift.v, pch = 19, col="white")
#points(myQ(.5,i), -i+shift.v, pch = 19, cex = .5, col=color1.minus.pri[i])
points(myQ(.5,i), -i+shift.v, pch = 19, cex = .5)
#
text(x =     2.1   , y =  -1   +.55,   labels =     "Election", cex = .65)
text(x = rep(2.1,4), y = -c(1:4)+.35,  labels = rep("2003", 4), cex = .65)
text(x = rep(2.1,2), y = -c(1:2)+.175, labels = rep("2006", 2), cex = .65)
text(x = rep(2.1,4), y = -(1:4),       labels = rep("2009", 4), cex = .65)
text(x = rep(2.1,3), y = -(1:3)-.175,  labels = rep("2012", 3), cex = .65)
text(x = rep(2.1,4), y = -(1:4)-.35,   labels = rep("2015", 5), cex = .65)
text(x =     2.1   , y = -5     +.35,  labels =     "2015"    , cex = .65) # morena separate bec y shift dift
#text(x = rep(2.1,4), y = -(1:4)-.3,    labels = rep("todos", 5), cex = .65)
#
text(x = rep(-2.1,5), y = -c(1:4,5-.35), labels = c("PAN", "PRD", "Green", "MC", "Morena"))
## dev.off()
setwd(wd)

### plot rho for s0 and s3
tmp <- biasResp0612oldNewDistrictsRPM # compact name
myQ <- function(Q){ # function to extract quantiles from posterior sample for plot: Q is desired quantile
    return(quantile(x= res$BUGSoutput$sims.list$rho, probs = Q, names=FALSE))
}
#save.dir <- "~/Dropbox/mydocs/op-edsEtc/blog/graphs/" # donde guardar
save.dir <- paste(wd, "graphs", sep = "")
logit <- function(X){ log( X / (1-X) ) }
antilogit <- function(X){ exp(X) / (exp(X)+1) }
v.tmp <- (1:999)/1000
#
library(Cairo)
type <-  "pdf" 
file <- paste("resp200612s0s3R.", type, sep="")
setwd(save.dir)
## Cairo(file=file,
##       type = type,
##       width = 6,
##       height = 6,
##       units = "in",
##       dpi = 72,
##       bg = "transparent")
#
jitter <- rnorm(n = 300, sd = .02)
plot(c(1.75,7.5), c(0.5,2.75), type = "n", axes = FALSE, xlab = expression(hat(rho)), ylab = "",
#     main = "Responsividad de los distritos en tres elecciones")
     main = "Responsiveness")
axis(1, at = seq(1.75,7.5,.25), labels = FALSE)
axis(1, at = seq(2,7,1))
#
res <- tmp$res06s0R; shift.v <- .3
points( res$BUGSoutput$sims.list$rho, 2.25+shift.v+jitter, cex = .1, col = "gray70" )
lines(x = c(myQ(.05),myQ(.95)), y = c(2.25+shift.v,2.25+shift.v), lwd = 2)
lines(x = c(myQ(.25),myQ(.75)), y = c(2.25+shift.v,2.25+shift.v), lwd = 6)
points( myQ(.5), 2.25+shift.v, pch = 19, col="white")
points( myQ(.5), 2.25+shift.v, pch = 19, cex = .5)
res <- tmp$res09s0R; shift.v <- .1
points( res$BUGSoutput$sims.list$rho, 2.25+shift.v+jitter, cex = .1, col = "gray70" )
lines(x = c(myQ(.05),myQ(.95)), y = c(2.25+shift.v,2.25+shift.v), lwd = 2)
lines(x = c(myQ(.25),myQ(.75)), y = c(2.25+shift.v,2.25+shift.v), lwd = 6)
points( myQ(.5), 2.25+shift.v, pch = 19, col="white")
points( myQ(.5), 2.25+shift.v, pch = 19, cex = .5)
res <- tmp$res12s0R; shift.v <- -.1
points( res$BUGSoutput$sims.list$rho, 2.25+shift.v+jitter, cex = .1, col = "gray70" )
lines(x = c(myQ(.05),myQ(.95)), y = c(2.25+shift.v,2.25+shift.v), lwd = 2)
lines(x = c(myQ(.25),myQ(.75)), y = c(2.25+shift.v,2.25+shift.v), lwd = 6)
points( myQ(.5), 2.25+shift.v, pch = 19, col="white")
points( myQ(.5), 2.25+shift.v, pch = 19, cex = .5)
#res <- tmp$res0612s0R; shift.v <- -.3
res <- tmp$res0612n0R; shift.v <- -.3 # use nation estimates
#points( res$BUGSoutput$sims.list$rho, 2.25+shift.v+jitter, cex = .1, col = "gray70" )
points( sample(res$BUGSoutput$sims.list$rho, size=300), 2.25+shift.v+jitter, cex = .1, col = "gray70" ) # sample to get 300 points
lines(x = c(myQ(.05),myQ(.95)), y = c(2.25+shift.v,2.25+shift.v), lwd = 2)
lines(x = c(myQ(.25),myQ(.75)), y = c(2.25+shift.v,2.25+shift.v), lwd = 6)
points( myQ(.5), 2.25+shift.v, pch = 19, col="white")
points( myQ(.5), 2.25+shift.v, pch = 19, cex = .5)
#
res <- tmp$res06s3R; shift.v <- .3
#points( res$BUGSoutput$sims.list$rho, 1+shift.v+jitter, cex = .1, col = "darkgrey" )
#points( res$BUGSoutput$median$rho, 1+shift.v )
points( res$BUGSoutput$sims.list$rho, 1+shift.v+jitter, cex = .1, col = "gray70" )
lines(x = c(myQ(.05),myQ(.95)), y = c(1+shift.v,1+shift.v), lwd = 2)
lines(x = c(myQ(.25),myQ(.75)), y = c(1+shift.v,1+shift.v), lwd = 6)
points( myQ(.5), 1+shift.v, pch = 19, col="white")
points( myQ(.5), 1+shift.v, pch = 19, cex = .5)
res <- tmp$res09s3R; shift.v <- .1
points( res$BUGSoutput$sims.list$rho, 1+shift.v+jitter, cex = .1, col = "gray70" )
lines(x = c(myQ(.05),myQ(.95)), y = c(1+shift.v,1+shift.v), lwd = 2)
lines(x = c(myQ(.25),myQ(.75)), y = c(1+shift.v,1+shift.v), lwd = 6)
points( myQ(.5), 1+shift.v, pch = 19, col="white")
points( myQ(.5), 1+shift.v, pch = 19, cex = .5)
res <- tmp$res12s3R; shift.v <- -.1
points( res$BUGSoutput$sims.list$rho, 1+shift.v+jitter, cex = .1, col = "gray70" )
lines(x = c(myQ(.05),myQ(.95)), y = c(1+shift.v,1+shift.v), lwd = 2)
lines(x = c(myQ(.25),myQ(.75)), y = c(1+shift.v,1+shift.v), lwd = 6)
points( myQ(.5), 1+shift.v, pch = 19, col="white")
points( myQ(.5), 1+shift.v, pch = 19, cex = .5)
#res <- tmp$res0612s3R; shift.v <- -.3
res <- tmp$res0612n3R; shift.v <- -.3 # use nation estimates
#points( res$BUGSoutput$sims.list$rho, 1+shift.v+jitter, cex = .1, col = "gray70" )
points( sample(res$BUGSoutput$sims.list$rho, size=300), 1+shift.v+jitter, cex = .1, col = "gray70" ) # sample to get 300 points
lines(x = c(myQ(.05),myQ(.95)), y = c(1+shift.v,1+shift.v), lwd = 2)
lines(x = c(myQ(.25),myQ(.75)), y = c(1+shift.v,1+shift.v), lwd = 6)
points( myQ(.5), 1+shift.v, pch = 19, col="white")
points( myQ(.5), 1+shift.v, pch = 19, cex = .5)
## text(x= 5.5, y = 2.75, labels = "Distritos en vigor")
## text(x= 5.5, y = 1.5, labels = "Distritos propuestos")
text(x= 4.75, y = 2.75, labels = "2006 map")
text(x= 4.75, y = 1.5, labels = "2015 map (hypothetical)")
text(x = rep(7.25,2), y = c(2.25,1)+.3, labels = rep("2006 (states)", 2), cex = .65)
text(x = rep(7.25,2), y = c(2.25,1)+.1, labels = rep("2009 (states)", 2), cex = .65)
text(x = rep(7.25,2), y = c(2.25,1)-.1, labels = rep("2012 (states)", 2), cex = .65)
text(x = rep(7.25,2), y = c(2.25,1)-.3, labels = rep("all (nation)", 2), cex = .65)
## dev.off()
## setwd(wd)

## over- vs under-represented by party
table(S$pan/D - v$pan > 0)
table(S$pri/D - v$pri > 0)
table(S$prd/D - v$prd > 0)

## ### FOR CALVO-MICOZZI VERSION
logit <- function(X){ log( X / (1-X) ) }
antilogit <- function(X){ exp(X) / (exp(X)+1) }
## #mean.N <- mean(tmp$N[tmp$pty==1]) ## pan ran in all elections
## equis <- seq(0,1,by=.01)
## p <- 1; s.hat.pan <- antilogit(lambda.hat[p] + rho.hat * logit(equis))
## p <- 2; s.hat.pri <- antilogit(lambda.hat[p] + rho.hat * logit(equis))
## p <- 3; s.hat.prd <- antilogit(lambda.hat[p] + rho.hat * logit(equis))

### FOR KING VERSION
#
party.vote <- 2 # for debug
seat.hat <- function(party.vote=NA, lambdas=lambda.hat){ # analizes vote increments for party.vote using lambda.hat bias
    mean.v <- apply(v, 2, mean) / sum(apply(v, 2, mean)); # compute parties' mean vote across elections and normalize to sum 1
#    mean.v <- c(.3, .3, .3, .04, .04, .01, .01) # or fix vote distribution
    share.mean <- data.frame(matrix(0, nrow=7, ncol=7)); # when analizing party i, produces share of vote without i won by rest of parties
    for (i in 1:7){ 
        for (j in setdiff(1:7,i)){
            share.mean[i,j] <- mean.v[j] / (1-mean.v[i])
        }
    }
    equis <- matrix(NA, nrow=101, ncol=7); # 0-1 vote increments for party, rest split by other parties according to mean vote
    equis[,party.vote] <- seq(from = 0, to = 1, by = .01);
    for (j in setdiff(1:7,party.vote)){
        equis[,j] <- (1-equis[,party.vote])*share.mean[party.vote,j]
    }
    d1 <- exp( lambdas[1] ) * equis[,1]^rho.hat;
    d2 <- exp( lambdas[2] ) * equis[,2]^rho.hat;
    d3 <- exp( lambdas[3] ) * equis[,3]^rho.hat;
    d4 <- exp( lambdas[4] ) * equis[,4]^rho.hat;
    d5 <- exp( lambdas[5] ) * equis[,5]^rho.hat;
    d6 <- exp( lambdas[6] ) * equis[,6]^rho.hat;
    d7 <- exp( lambdas[7] ) * equis[,7]^rho.hat;
    return(exp( lambdas[party.vote] ) *  equis[,party.vote]^rho.hat  / (d1+d2+d3+d4+d5+d6+d7)) # voto party crece linealmente, dem?s seg?n peso relativo
}

tmp.l <- c(.06,0,-.02,0,0,0,0)
#
s.hat.pan   <- seat.hat(party.vote = 1)#, lambdas = tmp.l) 
s.hat.pri   <- seat.hat(party.vote = 2)#, lambdas = tmp.l) 
s.hat.prd   <- seat.hat(party.vote = 3)#, lambdas = tmp.l)
s.hat.pvem  <- seat.hat(party.vote = 4)#, lambdas = tmp.l)
s.hat.panal <- seat.hat(party.vote = 5)#, lambdas = tmp.l)
s.hat.ptc   <- seat.hat(party.vote = 6)#, lambdas = tmp.l)
s.hat.pasdc <- seat.hat(party.vote = 7)#, lambdas = tmp.l)
#
equis <- seq(0,1,.01)


## Plot party-by-state-votes-to-seats 2006--2012 with regression line
#save.dir <- "~/Dropbox/mydocs/op-edsEtc/blog/graphs/" # donde guardar
#
## tmp is the object to graph (adds pri+pric and prd+prdc) <---SEARCH FOR THIS IN THE CODE ABOVE TO PREPARE TMP FOR STATE GRAPH
#
## # nat aggregates for plot
## tmpv <- apply(df2006d0[,c("pan","pric","prdc","panal","asdc")], 2, sum)
## tmpv <- tmpv/sum(tmpv)
## tmps <- apply(df2006d0[,c("panw","pricw","prdcw","panalw","asdcw")], 2, sum) / 300
## tmp <- data.frame(votes=tmpv, seats=tmps)
## tmpv <- df2009d0[,c("pan","pri","pric","prd","pvem","panal","ptc")]
## tmpv$pri <- tmpv$pri + tmpv$pric; tmpv$pric <- NULL
## tmpv <- apply(tmpv, 2, sum)
## tmpv <- tmpv/sum(tmpv)
## tmps <- df2009d0[,c("panw","priw","pricw","prdw","pvemw","panalw","ptcw")]
## tmps$priw <- tmps$priw + tmps$pricw; tmps$pricw <- NULL
## tmps <- apply(tmps, 2, sum) / 300
## tmp <- rbind(tmp, data.frame(votes=tmpv, seats=tmps))
## tmpv <- df2012d0[,c("pan","pri","pric","prdc","pvem","panal")]
## tmpv$pri <- tmpv$pri + tmpv$pric; tmpv$pric <- NULL
## tmpv <- apply(tmpv, 2, sum)
## tmpv <- tmpv/sum(tmpv)
## tmps <- df2012d0[,c("panw","priw","pricw","prdcw","pvemw","panalw")]
## tmps$priw <- tmps$priw + tmps$pricw; tmps$pricw <- NULL
## tmps <- apply(tmps, 2, sum) / 300
## tmp <- rbind(tmp, data.frame(votes=tmpv, seats=tmps))
## rm(tmpv, tmps)
#
save.dir <- paste(wd, "graphs", sep = "")
setwd(save.dir)
#setwd(paste(wd, "graphs", sep=""))
library(Cairo)
title <- "State aggregates"
#title <- "Federal deputies by state 2006-2012"
#title <- "DipFed por estado 2006-2012, distritos en vigor" #propuestos"
#title <- "Diputados Federales de mayor?a 2006-2012"
type <- "pdf"
#file <- paste("biasResp2006s3.", type, sep="")
#file <- paste("resXedo20062012.", type, sep="")
file <- paste("resXedo20062012sh.", type, sep="")
## Cairo(file=file,
##       type = type,
##       width = 6,
##       height = 6,
##       units = "in",
##       dpi = 72,
##       bg = "transparent")
#
plot(c(0,1),c(0,1), type="n",
     main=title,
     xlab = "vote share", ylab = "seat share", axes = FALSE)
#     xlab = "% votes won in state", ylab = "% plurality seats won in state", axes = FALSE)
#     xlab = "% votos en el estado", ylab = "% esca?os en el estado", axes = FALSE)
axis(1, at=seq(0,1,.2), lab=seq(0,1,.2))
axis(2, at=seq(0,1,.2), lab=seq(0,1,.2))
abline(a=0, b=1, lty=2)
#reg <- lm(tmp$seats ~ tmp$votes)
#reg <- lm(tmp$seats ~ poly(tmp$votes, 2, raw=TRUE))
points(tmp$votes, tmp$seats/tmp$ndis, pch=19, cex=.5)#, col=color)
## lines(equis, s.hat.pan, col = color[tmp$pty==1][1])
## lines(equis, s.hat.pri, col = color[tmp$pty==2][1])
## lines(equis, s.hat.prd, col = color[tmp$pty==3][1])
#abline(reg)
#text(.85,.3,  paste("Responsividad=", round(rho.hat, digits=2)))
#text(.85,.25, "Sesgo relativo al PRI:")
## text(.85,.2,  paste("PAN=", round( exp(lambda.hat[1]) / (exp(lambda.hat[1]) + 1) - .5 , digits=2)))
## text(.85,.15, paste("PRI=", round( exp(lambda.hat[2]) / (exp(lambda.hat[2]) + 1) - .5 , digits=2)))
## text(.85,.1,  paste("PRD=", round( exp(lambda.hat[3]) / (exp(lambda.hat[3]) + 1) - .5 , digits=2)))
#text(.85,.2,  paste("PAN=", round( lambda.hat[1]-lambda.hat[2], digits=2)))
## text(.85,.15, paste("PRI=", round( lambda.hat[2]-lambda.hat[2], digits=2)))
#text(.85,.15,  paste("PRD=", round( lambda.hat[3]-lambda.hat[2], digits=2)))
#text(.85,.1,  paste("PANAL=", round( lambda.hat[6]-lambda.hat[2], digits=2)))
#
## dev.off()
setwd(wd)

## ## graph different bias resp combos in 2-pty setting
## equis <- seq(0,.99,.01)
## #logit(ye) <-  0 + 1 * logit(equis)
## plot(c(0,1), c(0,1), type = "n", xlab = "%voto", ylab = "%esca?os de mayor?a", axes = FALSE)
## abline(0,1,lty=2)
## axis(1, at = seq(0,1,.25), labels = seq(0,100,25))
## axis(2, at = seq(0,1,.25), labels = seq(0,100,25))
## ye <- antilogit ( 0 + 6 * logit(equis) )
## lines(equis, ye, col = "red")
## ye <- antilogit ( 1 + 6 * logit(equis) )
## lines(equis, ye, col = "blue")

dim(tmp)


cmn-2006-12-s0 # calvo-micozzi with N
cm-2006-12-s0 # calvo-micozzi without N
k7-2006-12-s0 # king with all parties
k6-2006-12-s0 # king with pri omitted as reference





edos <- c("ags", "bcn", "bcs", "cam", "coa", "col", "cps", "cua", "df", "dgo", "gua", "gue", "hgo", "jal", "mex", "mic", "mor", "nay", "nl", "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac")


library(Cairo)

setwd(paste(wd, "/graphs/", sep=""))
types <- c("pdf", "png", "svg"); type <- types[2] # select type here
Cairo(file=paste("lisnom.dis.2012.", type, sep=""),
      type=type,
      units="in",
      width=10,
      height=6,
      dpi = 96)
plot(ln2012$rel, ylab = "sobre-/sub-objetivo (%)", xlab = "distritos",
     ylim = c(min(ln2012$rel), max(ln2012$rel)+25),
     type="n", main = "Lista nominal 2012 vs. tama?o ideal", axes = FALSE)
axis(1, at = c(1, seq(from = 50, to = 300, by = 50)))
axis(2, at = seq(from = -40, to = 80, by = 20))
polygon(x = c(-10,310,310,-10), y = c(-15,-15,15,15), lty = 0, col = "grey80")
abline(h=0, lty=2)
abline(h=c(-2,2,4,6,8)*10, col="grey")
points(ln2012$rel,
     col = clr.300, pch=20)
text(ln2012$rel, labels=ln2012$disn, cex = .25, col="white")
text(nom.pos, rep( c(max(ln2012$rel)+24, max(ln2012$rel)+20, max(ln2012$rel)+16, max(ln2012$rel)+11), times=8), labels = edos, cex = .75, col=clr.32)
dev.off()
setwd(wd)

