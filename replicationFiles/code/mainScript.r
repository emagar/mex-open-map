#####################################################################################
## This is an abridged version of analizaEscenarios2013.r for replication purposes ##
#####################################################################################

# NOTE:
# Maps are named after the first congressional election they were used: the 1979, 1997, and 2006 maps; the 2013 proposal that was never adopted for 2015 election is the 2013 map.
# Short names for objects in the analysis of the 2013 proposals are d0 (the 2006 map or status quo), d1 (initial IFE proposal), d2 (second proposal, with party feedback), and d3 (third and final proposal, with more party feedback). 

options(width = 130) # emacs screen size
rm(list=ls())
#
if(length(grep('mainScript.r',dir(),fixed=TRUE))==0) stop ("Please set R's working directory to the base directory on the location of the unzipped directories from replicationFiles.zip")

#wd <- "~/Desktop/mex-open-map/replicationFiles/code/"
#setwd(wd)
wd <- getwd()
dd <- c("../data/")   # data directory
gd <- c("../graphs/") # graph directory


library(R2jags)


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
sel <- which(dis2013$disn==0 | dis2013$dis3er13==0); print("These obs have unassigned secciones in one or both maps:"); sel
## dis2013[sel,] # inspect them
dis2013 <- dis2013[-sel,] # drop them
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
df2015d0$pric[df2015d0$shSecCoalPri==0] <- 0
df2015d0$pri[df2015d0$shSecCoalPri==1] <- 0
df2015d0$pvem[df2015d0$shSecCoalPri==1] <- 0
#df2015d0$pri_pvem <- NULL
#
df2015d0$prdc[df2015d0$shSecCoalPrd==0] <- 0
df2015d0$prd[df2015d0$shSecCoalPrd==1] <- 0
df2015d0$pt[df2015d0$shSecCoalPrd==1] <- 0
#df2015d0$prd_pt <- NULL
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
#
# removes redundant columns
df2015d0 <- df2015d0[,c("edon","disn"   ,"pan","pri","pric","prd","prdc","pt","pvem","mc","panal","morena","ph","ps","pri_pvem","prd_pt","indep1","indep2","efec","nr","nul","lisnom","shSecCoalPri","shSecCoalPrd")]
df2015d1 <- df2015d1[,c("edon","dis13.1","pan","pri","pric","prd","prdc","pt","pvem","mc","panal","morena","ph","ps","pri_pvem","prd_pt","indep1","indep2","efec","nr","nul","lisnom","shSecCoalPri","shSecCoalPrd")]
df2015d3 <- df2015d3[,c("edon","dis13.3","pan","pri","pric","prd","prdc","pt","pvem","mc","panal","morena","ph","ps","pri_pvem","prd_pt","indep1","indep2","efec","nr","nul","lisnom","shSecCoalPri","shSecCoalPrd")]
#
colnames(df2015d0)[2] <- colnames(df2015d1)[2] <- colnames(df2015d3)[2] <- "disn"
#
#
# agregagates pri coalition votes where needed, based on the relative proportion of secciones with such coalition in the new district
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
tmp <- c("pan", "pri", "pric", "prd", "prdc", "pt", "pvem", "mc", "panal", "morena", "ph", "ps","indep1","indep2")
tmpv <- df2015d1[,tmp]
tmpl <- matrix(rep(tmp,300), byrow=TRUE, nrow=300)
tmpl <- sortBy(target=tmpl, By=tmpv)
tmpv <- t(apply(tmpv, 1, function(x) sort(x, decreasing = TRUE)))
df2015d1$win <- tmpl[,1]
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
#
##############
# 2012 votes #
##############
tmp <- read.csv( paste(dd, "dfSeccion2012.csv", sep=""), header=TRUE)
tmp <- tmp[order(tmp$edon, tmp$disn, tmp$seccion),]
# compute effective vote (without void ballots)
tmp$efec <- tmp$pan + tmp$pri + tmp$prd + tmp$pvem + tmp$pt + tmp$mc + tmp$panal + tmp$pripvem + tmp$prdptmc + tmp$prdpt + tmp$prdmc + tmp$ptmc
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
df2012d0$pric[df2012d0$shSecCoalPri==0] <- 0
df2012d0$pri[df2012d0$shSecCoalPri==1] <- 0
df2012d0$pvem[df2012d0$shSecCoalPri==1] <- 0
#df2012d0$pripvem <- NULL
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
colnames(df2012d3)[which(colnames(df2012d3)=="dcoalpri")] <- "shSecCoalPri"
df2012d3$tmp <- NULL
df2012d3 <- df2012d3[duplicated(df2012d3$edon*100+df2012d3$dis13.3)==FALSE,]
#
# removes redundant columns
df2012d0 <- df2012d0[,c("edon","disn"   ,"pan","pri","pric","prdc","pvem","panal","pripvem","efec","nr","nul","lisnom","shSecCoalPri")]
df2012d1 <- df2012d1[,c("edon","dis13.1","pan","pri","pric","prdc","pvem","panal","pripvem","efec","nr","nul","lisnom","shSecCoalPri")]
df2012d3 <- df2012d3[,c("edon","dis13.3","pan","pri","pric","prdc","pvem","panal","pripvem","efec","nr","nul","lisnom","shSecCoalPri")]
#
colnames(df2012d0)[2] <- colnames(df2012d1)[2] <- colnames(df2012d3)[2] <- "disn"
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
## df2009d3 <- df2009d3[-which(df2009d3$dis13.3==0),]  # drops unassigned sections in 3rd plan (thisinfo should arrive later)
#
# removes redundant columns
df2009d0 <- df2009d0[,c("edon","disn"   ,"pan","pri","pric","prd","pvem","panal","pripvem","ptc","nr","nul","tot","lisnom","dcoalpri")]
df2009d1 <- df2009d1[,c("edon","dis13.1","pan","pri","pric","prd","pvem","panal","pripvem","ptc","nr","nul","tot","lisnom","dcoalpri")]
df2009d3 <- df2009d3[,c("edon","dis13.3","pan","pri","pric","prd","pvem","panal","pripvem","ptc","nr","nul","tot","lisnom","dcoalpri")]
#
colnames(df2009d0) <- colnames(df2009d1) <- colnames(df2009d3) <- c("edon","disn","pan","pri","pric","prd","pvem","panal","pripvem","ptc","nr","nul","tot","lisnom","shSecCoalPri")
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
## df2006d3 <- df2006d3[-which(df2006d3$dis13.3==0),]  # drops unassigned sections in 3rd plan (thisinfo should arrive later)
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
print("Two districts missing in 2003") # <- NOTE: two full districts missing: 506 and 1605
dim(df2003d97) 
dim(df2003d0)

# adds winner
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
print("This object calls 2013 map '2015 map', as does the published article")
summary(votPobDis0018) 
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
#table(paste(tmpFrom$edon, tmpFrom$disn, sep=".")==paste(tmpTo$edon, tmpTo$disn, sep = ".")) # verify districts match in both objects (300 TRUEs)
tmpFrom <- tmpFrom[, c("ptot","rris","rrin")]                                               # keep population data only
tmpTo <- cbind(tmpTo, tmpFrom)                                                              # merge
tmpTo <- tmpTo[-which((tmpTo$edon==5 & tmpTo$disn==6) | tmpTo$edon==16 & tmpTo$disn==5),]   # drop missing districts
df2003d97 <- tmpTo
#
tmpFrom <- votPobDis0018$pob.df2003d0; tmpTo <- df2003d0                                    # select one year and map
tmpFrom <- tmpFrom[order(tmpFrom$edon, tmpFrom$disn),]                                      # sort
#table(paste(tmpFrom$edon, tmpFrom$disn, sep=".")==paste(tmpTo$edon, tmpTo$disn, sep = ".")) # verify districts match in both objects (300 TRUEs)
tmpFrom <- tmpFrom[, c("ptot","rris","rrin")]                                               # keep population data only
tmpTo <- cbind(tmpTo, tmpFrom)                                                              # merge
df2003d0 <- tmpTo
#
tmpFrom <- votPobDis0018$pob.df2006d0; tmpTo <- df2006d0                                    # select one year and map
tmpFrom <- tmpFrom[order(tmpFrom$edon, tmpFrom$disn),]                                      # sort
#table(paste(tmpFrom$edon, tmpFrom$disn, sep=".")==paste(tmpTo$edon, tmpTo$disn, sep = ".")) # verify districts match in both objects (300 TRUEs)
tmpFrom <- tmpFrom[, c("ptot","rris","rrin")]                                               # keep population data only
tmpTo <- cbind(tmpTo, tmpFrom)                                                              # merge
df2006d0 <- tmpTo
#
tmpFrom <- votPobDis0018$pob.df2006d3; tmpTo <- df2006d3                                    # select one year and map
tmpFrom <- tmpFrom[order(tmpFrom$edon, tmpFrom$disn),]                                      # sort
#table(paste(tmpFrom$edon, tmpFrom$disn, sep=".")==paste(tmpTo$edon, tmpTo$disn, sep = ".")) # verify districts match in both objects (300 TRUEs)
tmpFrom <- tmpFrom[, c("ptot","rris","rrin")]                                               # keep population data only
tmpTo <- cbind(tmpTo, tmpFrom)                                                              # merge
df2006d3 <- tmpTo
#
tmpFrom <- votPobDis0018$pob.df2009d0; tmpTo <- df2009d0                                    # select one year and map
tmpFrom <- tmpFrom[order(tmpFrom$edon, tmpFrom$disn),]                                      # sort
#table(paste(tmpFrom$edon, tmpFrom$disn, sep=".")==paste(tmpTo$edon, tmpTo$disn, sep = ".")) # verify districts match in both objects (300 TRUEs)
tmpFrom <- tmpFrom[, c("ptot","rris","rrin")]                                               # keep population data only
tmpTo <- cbind(tmpTo, tmpFrom)                                                              # merge
df2009d0 <- tmpTo
#
tmpFrom <- votPobDis0018$pob.df2009d3; tmpTo <- df2009d3                                    # select one year and map
tmpFrom <- tmpFrom[order(tmpFrom$edon, tmpFrom$disn),]                                      # sort
#table(paste(tmpFrom$edon, tmpFrom$disn, sep=".")==paste(tmpTo$edon, tmpTo$disn, sep = ".")) # verify districts match in both objects (300 TRUEs)
tmpFrom <- tmpFrom[, c("ptot","rris","rrin")]                                               # keep population data only
tmpTo <- cbind(tmpTo, tmpFrom)                                                              # merge
df2009d3 <- tmpTo
#
tmpFrom <- votPobDis0018$pob.df2012d0; tmpTo <- df2012d0                                        # select one year and map
tmpFrom <- tmpFrom[order(tmpFrom$edon, tmpFrom$disn),]                                      # sort
#table(paste(tmpFrom$edon, tmpFrom$disn, sep=".")==paste(tmpTo$edon, tmpTo$disn, sep = ".")) # verify districts match in both objects (300 TRUEs)
tmpFrom <- tmpFrom[, c("ptot","rris","rrin")]                                               # keep population data only
tmpTo <- cbind(tmpTo, tmpFrom)                                                              # merge
df2012d0 <- tmpTo
#
tmpFrom <- votPobDis0018$pob.df2012d3; tmpTo <- df2012d3                                        # select one year and map
tmpFrom <- tmpFrom[order(tmpFrom$edon, tmpFrom$disn),]                                      # sort
#table(paste(tmpFrom$edon, tmpFrom$disn, sep=".")==paste(tmpTo$edon, tmpTo$disn, sep = ".")) # verify districts match in both objects (300 TRUEs)
tmpFrom <- tmpFrom[, c("ptot","rris","rrin")]                                               # keep population data only
tmpTo <- cbind(tmpTo, tmpFrom)                                                              # merge
df2012d3 <- tmpTo
#
tmpFrom <- votPobDis0018$pob.df2015d0; tmpTo <- df2015d0                                    # select one year and map
tmpFrom <- tmpFrom[order(tmpFrom$edon, tmpFrom$disn),]                                      # sort
#table(paste(tmpFrom$edon, tmpFrom$disn, sep=".")==paste(tmpTo$edon, tmpTo$disn, sep = ".")) # verify districts match in both objects (300 TRUEs)
tmpFrom <- tmpFrom[, c("ptot","rris","rrin")]                                               # keep population data only
tmpTo <- cbind(tmpTo, tmpFrom)                                                              # merge
df2015d0 <- tmpTo
#
tmpFrom <- votPobDis0018$pob.df2015d3; tmpTo <- df2015d3                                    # select one year and map
tmpFrom <- tmpFrom[order(tmpFrom$edon, tmpFrom$disn),]                                      # sort
#table(paste(tmpFrom$edon, tmpFrom$disn, sep=".")==paste(tmpTo$edon, tmpTo$disn, sep = ".")) # verify districts match in both objects (300 TRUEs)
tmpFrom <- tmpFrom[, c("ptot","rris","rrin")]                                               # keep population data only
tmpTo <- cbind(tmpTo, tmpFrom)                                                              # merge
df2015d3 <- tmpTo
#
rm(tmpFrom, tmpTo, votPobDis0018)
#
tmpFrom <- df2006d0; tmpTo <- df2006s0
tmpFrom$ptot <- ave(tmpFrom$ptot, as.factor(tmpFrom$edon), FUN=sum, na.rm=TRUE)
tmpFrom <- tmpFrom[duplicated(tmpFrom$edon)==FALSE,]
#table(tmpFrom$edon==tmpTo$edon) # verify districts match in both objects (32 TRUEs)
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
#table(tmpFrom$edon==tmpTo$edon) # verify districts match in both objects (32 TRUEs)
tmpTo$ptot <- tmpFrom$ptot
df2006s3 <- tmpTo
#
tmpFrom <- df2009d0; tmpTo <- df2009s0
tmpFrom$ptot <- ave(tmpFrom$ptot, as.factor(tmpFrom$edon), FUN=sum, na.rm=TRUE)
tmpFrom <- tmpFrom[duplicated(tmpFrom$edon)==FALSE,]
#table(tmpFrom$edon==tmpTo$edon) # verify districts match in both objects (32 TRUEs)
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
#table(tmpFrom$edon==tmpTo$edon) # verify districts match in both objects (32 TRUEs)
tmpTo$ptot <- tmpFrom$ptot
df2009s3 <- tmpTo
#
tmpFrom <- df2012d0; tmpTo <- df2012s0
tmpFrom$ptot <- ave(tmpFrom$ptot, as.factor(tmpFrom$edon), FUN=sum, na.rm=TRUE)
tmpFrom <- tmpFrom[duplicated(tmpFrom$edon)==FALSE,]
#table(tmpFrom$edon==tmpTo$edon) # verify districts match in both objects (32 TRUEs)
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
#table(tmpFrom$edon==tmpTo$edon) # verify districts match in both objects (32 TRUEs)
tmpTo$ptot <- tmpFrom$ptot
df2012s3 <- tmpTo
#
tmpFrom <- df2015d0; tmpTo <- df2015s0
tmpFrom$ptot <- ave(tmpFrom$ptot, as.factor(tmpFrom$edon), FUN=sum, na.rm=TRUE)
tmpFrom <- tmpFrom[duplicated(tmpFrom$edon)==FALSE,]
#table(tmpFrom$edon==tmpTo$edon) # verify districts match in both objects (32 TRUEs)
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
#table(tmpFrom$edon==tmpTo$edon) # verify districts match in both objects (32 TRUEs)
tmpTo$ptot <- tmpFrom$ptot
df2015s3 <- tmpTo
#
rm(tmp, tmpFrom, tmpTo)

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
round(tmp1,3) # reports regression coefficients (cf) and p values (p)

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
## If you wish to re-produce Linzer draws and have yt not done so, you must run script linzerElas.r now.            ##
## Otherwise the simulated data in the distribution will be used. You will lose all unsaved data from the current   ##
## session. You can choose to save the image now, or re-run all the above commands once linzerElas.r has finished.  ##
######################################################################################################################
#
## plot natl true and Linzer-simulated votes and seats <-- Fig. 2 in the published article
load(paste(dd, "swingRatios9715.RData", sep = ""))
#pdf(file = paste(gd, "vs2003.pdf", sep = ""), width = 6, height = 6)
dat <- swRats$df2003d0
plot(dat$vmat, dat$seatmat, type="n", xlim=0:1, ylim=0:1, ylab = "seat share", xlab = "vote share", main = 2003)
abline(a=0, b=1, lty=2)
text(t(dat$vmat), t(dat$seatmat), labels=colnames(dat$vmat), col="gray")
text(dat$truevote, dat$trueseat, labels=colnames(dat$vmat))
#dev.off()
#pdf(file = paste(gd, "vs2006.pdf", sep = ""), width = 6, height = 6)
dat <- swRats$df2006d0
plot(dat$vmat, dat$seatmat, type="n", xlim=0:1, ylim=0:1, ylab = "seat share", xlab = "vote share", main = 2006)
abline(a=0, b=1, lty=2)
text(t(dat$vmat), t(dat$seatmat), labels=colnames(dat$vmat), col="gray")
text(dat$truevote, dat$trueseat, labels=colnames(dat$vmat))
#dev.off()
#pdf(file = paste(gd, "vs2009.pdf", sep = ""), width = 6, height = 6)
dat <- swRats$df2009d0
plot(dat$vmat, dat$seatmat, type="n", xlim=0:1, ylim=0:1, ylab = "seat share", xlab = "vote share", main = 2009)
abline(a=0, b=1, lty=2)
text(t(dat$vmat), t(dat$seatmat), labels=colnames(dat$vmat), col="gray")
text(dat$truevote, dat$trueseat, labels=colnames(dat$vmat))
#dev.off()
#pdf(file = paste(gd, "vs2012.pdf", sep = ""), width = 6, height = 6)
dat <- swRats$df2012d0
plot(dat$vmat, dat$seatmat, type="n", xlim=0:1, ylim=0:1, ylab = "seat share", xlab = "vote share", main = 2012)
abline(a=0, b=1, lty=2)
text(t(dat$vmat), t(dat$seatmat), labels=colnames(dat$vmat), col="gray")
text(dat$truevote, dat$trueseat, labels=colnames(dat$vmat))
#dev.off()
#pdf(file = paste(gd, "vs2015.pdf", sep = ""), width = 6, height = 6)
dat <- swRats$df2015d0
plot(dat$vmat, dat$seatmat, type="n", xlim=0:1, ylim=0:1, ylab = "seat share", xlab = "vote share", main = 2015)
abline(a=0, b=1, lty=2)
text(t(dat$vmat), t(dat$seatmat), labels=colnames(dat$vmat), col="gray")
text(dat$truevote, dat$trueseat, labels=colnames(dat$vmat))
#dev.off()

##########################################################################################################
## *Bloc 6*:                                                                                            ##
## BAYESIAN ESTIMATION OF KING MODEL USING JAGS                                                         ##
## See http://mcmc-jags.sourceforge.net for details on how to download and install JAGS in your machine ##
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
                    n.thin=50,
                    D=300                     # needs to be 298 for 1997 estimation, 300 for other years
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
    D <- D; # OJO: 298 for df2003d97!
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

# This will test that jags routine works. Estimation should end fast with no error message
tmpRes <- my.jags(which.elec = 2015,        # options are: 2003, 2006, 2009, 2012, 2015            
                  which.map  = "d0",        #              "d0", "d1", "d3"                        
                  which.measure = "w.bar",  #              "v" for R, "v.bar" for P, "w.bar" for M (RPM are Grofman et al's notation)
                  model.file=lambda.rho.5,  #                                                       
                  test.ride=TRUE            # if TRUE, overrides n.chains, n.iter, n.thin          
)


###########################################################################
###########################################################################
## CHOOSE WHETHER TO RE-ESTIMATE PARTISAN BIAS AND ITS COMPONENTS (SLOW) ##
## OR PROCEED WITH DISTRIBUTED RESULTS (DEFAULT)                         ##
###########################################################################
###########################################################################
urChoice <- menu(c("Re-estimate---SLOW!", "Load distributed estimates"), title="Do you wish to re-estimate partisan bias or load distributed estimates?")

if (urChoice==1){
    cat("Re-estimating");
        source(file="mcmcEstimation.r")
    } else {
        load(file=paste(dd, "biasRespOnLinzerSims3components0315.RData", sep =""));
        cat("Bias estimates reported in publication loaded\n");
    }
rm(urChoice)

print("Inspect partisan bias estimates thus (choose other maps/years)")
tmp <- biasRespOnLinzerSimsRPM # shorten name
summary(tmp)
tmp$res2015d0v$party.labels    # 1 in lambda is for pan relative to pri
print("This is raw partisan bias")
quantile(tmp$res2015d0v$BUGSoutput$sims.list$lambda[,1])                                                                    
print("This is the distributive component")
quantile(tmp$res2015d0v.bar$BUGSoutput$sims.list$lambda[,1])                                                                
print("This is the malapportionment component")
quantile(tmp$res2015d0w.bar$BUGSoutput$sims.list$lambda[,1]) - quantile(tmp$res2015d0v.bar$BUGSoutput$sims.list$lambda[,1]) 
print("This is the turnout component")
quantile(tmp$res2015d0v$BUGSoutput$sims.list$lambda[,1])     - quantile(tmp$res2015d0w.bar$BUGSoutput$sims.list$lambda[,1]) 
#
## ## extracts all objects from the estimates list, if so wished
## theList <- biasRespOnLinzerSimsRPM
## summary(theList)
## for(i in 1:length(theList)){
##   ##first extract the object value
##   tempobj=theList[[i]]
##   ##now create a new variable with the original name of the list item
##   eval(parse(text=paste(names(theList)[[i]],"= tempobj")))
## }
## rm(theList)

# DIAGNOSIS
# commands to export pdf traceplots to verify parameter convergence (will produce a pdf file with a large number of plots)
gd2 <- paste(gd, "traceplots/", sep = "")
which.elec <- 2015
which.map <- "d3"
which.v <- "w.bar"
tmp <- eval(parse(text=paste("biasRespOnLinzerSimsRPM$res", which.elec, which.map, which.v, sep="")))
#summary(tmp)
pdf(paste(gd2, "traceplot", which.elec, which.map, which.v, ".pdf", sep = ""))
traceplot(tmp, ask = FALSE)
dev.off()
rm(gd2, which.elec, which.map, which.v, tmp) # clean

# summarize responsiveness parameter (90% range and median estimated with raw vote --- R in GKB's terminology)
tmp <- biasRespOnLinzerSimsRPM # shorten name
tmp1 <- data.frame(q05=rep(NA,5), q50=rep(NA,5), q95=rep(NA,5)); rownames(tmp1) <- paste("y", seq(2003, 2015, by=3), sep="")
tmp1[1,] <- round(quantile(tmp$res2003d97v$BUGSoutput$sims.list$rho, probs=c(.05,.5,.95)),1)
tmp1[2,] <- round(quantile(tmp$res2006d0v$BUGSoutput$sims.list$rho, probs=c(.05,.5,.95)),1)
tmp1[3,] <- round(quantile(tmp$res2009d0v$BUGSoutput$sims.list$rho, probs=c(.05,.5,.95)),1)
tmp1[4,] <- round(quantile(tmp$res2012d0v$BUGSoutput$sims.list$rho, probs=c(.05,.5,.95)),1)
tmp1[5,] <- round(quantile(tmp$res2015d0v$BUGSoutput$sims.list$rho, probs=c(.05,.5,.95)),1)
     #################################################
tmp1 # <-- reported in p. 8 of the published article # 
     # NOTE: the ranges reported are slightly different from those published -- possibly 
     # due to changes in the underlying R libraries. Rho , however is a parameter of 
     # secondary importance in our argument. 
     #################################################

####################################################
# Rhat reported in fn. 13 of the published article #
####################################################
tmp$res2003d97v$BUGSoutput$summary
tmp$res2003d97v.bar$BUGSoutput$summary
tmp$res2003d97w.bar$BUGSoutput$summary
tmp$res2006d0v$BUGSoutput$summary
tmp$res2006d0v.bar$BUGSoutput$summary
tmp$res2006d0w.bar$BUGSoutput$summary
tmp$res2009d0v$BUGSoutput$summary
tmp$res2009d0v.bar$BUGSoutput$summary
tmp$res2009d0w.bar$BUGSoutput$summary
tmp$res2012d0v$BUGSoutput$summary
tmp$res2012d0v.bar$BUGSoutput$summary
tmp$res2012d0w.bar$BUGSoutput$summary
tmp$res2015d0v$BUGSoutput$summary
tmp$res2015d0v.bar$BUGSoutput$summary
tmp$res2015d0w.bar$BUGSoutput$summary

# PRD congressional vote reported in p. 9 in published article
print("Mean PRD diputado vote, presidential election years")
round(mean(c(
    sum(df2006d0$prdc)*100/sum(df2006d0$efec),
    sum(df2012d0$prdc)*100/sum(df2012d0$efec)
    )
))
print("Mean PRD diputado vote, midterms")
round(mean(c(
    sum(df2003d97$prd)*100/sum(df2003d97$efec),
    sum(df2009d0$prd)*100/sum(df2009d0$efec),
    sum(df2015d0$prd+df2015d0$prdc)*100/sum(df2015d0$efec)
    )
))

# summarize central tendency of party bias a la Grofman et al -- Estimates reported in Table 2, p. 9 in published article
tmp <- biasRespOnLinzerSimsRPM
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
#tmp15d3[,3] <- tmp15d3[,1] - tmp15d3[,2]
#
# Estimates reported in Table 2, p. 9 in published article
tmp03d97 # actual
tmp03d0  # hypothetical
tmp06d0  # actual
tmp09d0  # actual
tmp12d0  # actual
tmp12d3  # hypothetical
tmp15d0  # actual
tmp15d3  # hypothetical

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
tmp1 <- tmp$res2003d97v.bar$BUGSoutput$sims.list$lambda[,1:3] #  DIST
tmp03d97[2,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
tmp1 <- tmp$res2003d97v$BUGSoutput$sims.list$lambda[,1:3] - tmp$res2003d97w.bar$BUGSoutput$sims.list$lambda[1:3] #  TURN
tmp03d97[3,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
tmp1 <- tmp$res2003d97w.bar$BUGSoutput$sims.list$lambda[,1:3] - tmp$res2003d97v.bar$BUGSoutput$sims.list$lambda[1:3] #  MALAP
tmp03d97[4,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
#                                        #
tmp1 <- tmp$res2003d0v$BUGSoutput$sims.list$lambda[,1:3] #  RAW
tmp03d0[1,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
tmp1 <- tmp$res2003d0v.bar$BUGSoutput$sims.list$lambda[,1:3] #  DIST
tmp03d0[2,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
tmp1 <- tmp$res2003d0v$BUGSoutput$sims.list$lambda[,1:3] - tmp$res2003d0w.bar$BUGSoutput$sims.list$lambda[1:3] #  TURN
tmp03d0[3,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
tmp1 <- tmp$res2003d0w.bar$BUGSoutput$sims.list$lambda[,1:3] - tmp$res2003d0v.bar$BUGSoutput$sims.list$lambda[1:3] # MALAP
tmp03d0[4,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
#                                        #
tmp1 <- tmp$res2006d0v$BUGSoutput$sims.list$lambda[,1:3] #  RAW
tmp06d0[1,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=panalpri
tmp1 <- tmp$res2006d0v.bar$BUGSoutput$sims.list$lambda[,1:3] #  DIST
tmp06d0[2,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=panalpri
tmp1 <- tmp$res2006d0v$BUGSoutput$sims.list$lambda[,1:3] - tmp$res2006d0w.bar$BUGSoutput$sims.list$lambda[1:3] #  TURN
tmp06d0[3,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=panalpri
tmp1 <- tmp$res2006d0w.bar$BUGSoutput$sims.list$lambda[,1:3] - tmp$res2006d0v.bar$BUGSoutput$sims.list$lambda[1:3] # MALAP
tmp06d0[4,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=panalpri
                                        #
tmp1 <- tmp$res2009d0v$BUGSoutput$sims.list$lambda[,1:3] #  RAW
tmp09d0[1,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
tmp1 <- tmp$res2009d0v.bar$BUGSoutput$sims.list$lambda[,1:3] #  DIST
tmp09d0[2,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
tmp1 <- tmp$res2009d0v$BUGSoutput$sims.list$lambda[,1:3] - tmp$res2009d0w.bar$BUGSoutput$sims.list$lambda[1:3] #  TURN
tmp09d0[3,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
tmp1 <- tmp$res2009d0w.bar$BUGSoutput$sims.list$lambda[,1:3] - tmp$res2009d0v.bar$BUGSoutput$sims.list$lambda[1:3] # MALAP
tmp09d0[4,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
                                        #
tmp1 <- tmp$res2012d0v$BUGSoutput$sims.list$lambda[,1:3] #  RAW
tmp12d0[1,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
tmp1 <- tmp$res2012d0v.bar$BUGSoutput$sims.list$lambda[,1:3] #  DIST
tmp12d0[2,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
tmp1 <- tmp$res2012d0v$BUGSoutput$sims.list$lambda[,1:3] - tmp$res2012d0w.bar$BUGSoutput$sims.list$lambda[1:3] #  TURN
tmp12d0[3,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
tmp1 <- tmp$res2012d0w.bar$BUGSoutput$sims.list$lambda[,1:3] - tmp$res2012d0v.bar$BUGSoutput$sims.list$lambda[1:3] # MALAP
tmp12d0[4,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
#
tmp1 <- tmp$res2012d3v$BUGSoutput$sims.list$lambda[,1:3] #  RAW
tmp12d3[1,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
tmp1 <- tmp$res2012d3v.bar$BUGSoutput$sims.list$lambda[,1:3] #  DIST
tmp12d3[2,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
tmp1 <- tmp$res2012d3v$BUGSoutput$sims.list$lambda[,1:3] - tmp$res2012d3w.bar$BUGSoutput$sims.list$lambda[1:3] #  TURN
tmp12d3[3,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
tmp1 <- tmp$res2012d3w.bar$BUGSoutput$sims.list$lambda[,1:3] - tmp$res2012d3v.bar$BUGSoutput$sims.list$lambda[1:3] # MALAP
tmp12d3[4,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
#
tmp1 <- tmp$res2015d0v$BUGSoutput$sims.list$lambda[,1:3] #  RAW
tmp15d0[1,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
tmp1 <- tmp$res2015d0v.bar$BUGSoutput$sims.list$lambda[,1:3] #  DIST
tmp15d0[2,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
tmp1 <- tmp$res2015d0v$BUGSoutput$sims.list$lambda[,1:3] - tmp$res2015d0w.bar$BUGSoutput$sims.list$lambda[1:3] #  TURN
tmp15d0[3,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
tmp1 <- tmp$res2015d0w.bar$BUGSoutput$sims.list$lambda[,1:3] - tmp$res2015d0v.bar$BUGSoutput$sims.list$lambda[1:3] # MALAP
tmp15d0[4,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
#
tmp1 <- tmp$res2015d3v$BUGSoutput$sims.list$lambda[,1:3] #  RAW
tmp15d3[1,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
tmp1 <- tmp$res2015d3v.bar$BUGSoutput$sims.list$lambda[,1:3] #  DIST
tmp15d3[2,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
tmp1 <- tmp$res2015d3v$BUGSoutput$sims.list$lambda[,1:3] - tmp$res2015d3w.bar$BUGSoutput$sims.list$lambda[1:3] #  TURN
tmp15d3[3,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
tmp1 <- tmp$res2015d3w.bar$BUGSoutput$sims.list$lambda[,1:3] - tmp$res2015d3v.bar$BUGSoutput$sims.list$lambda[1:3] # MALAP
tmp15d3[4,] <- apply(tmp1, 2, FUN=nOppoSign) # 1=panpri 2=prdpri 3=pvempri
#
# Share of posterior sample with sign opposite to point estimate's, reported in Table 2, p. 9 in published article
print("2003, actual map")
tmp03d97
print("2003, hypothetical map")
tmp03d0
print("2006, actual map")
tmp06d0
print("2009, actual map")
tmp09d0
print("2012, actual map")
tmp12d0
print("2012, hypothetical map")
tmp12d3
print("2015, actual map")
tmp15d0
print("2015, hypothetical map")
tmp15d3

##########################################################
# compute swing ratios with regressions from Linzer sims #
# reported in Table 3, p. 10 or published article         #
##########################################################
load(paste(dd, "swingRatios9715.RData", sep = ""))
# two maps pooled version
# 2006
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
print("year 2006")
for (i in c(2,1,3)){
    vreg  <- lm(s[,i] ~ v[,i])
    vreg2 <- lm(s[,i] ~ v[,i] + dmap + dmapxv[,i])
    print( colnames(v)[i])
    print(round(summary(vreg2)$coefficients[c(2,4),1:2], 2))
}
#
# 2009
tmp <- swRats$df2009d0
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
tmp <- swRats$df2009d3
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
print("year 2009")
for (i in c(2,1,3)){
    vreg  <- lm(s[,i] ~ v[,i])
    vreg2 <- lm(s[,i] ~ v[,i] + dmap + dmapxv[,i])
    print( colnames(v)[i])
    print(round(summary(vreg2)$coefficients[c(2,4),1:2], 2))
}
#
# 2012
tmp <- swRats$df2012d0
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
tmp <- swRats$df2012d3
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
print("year 2012")
for (i in c(2,1,3)){
    vreg  <- lm(s[,i] ~ v[,i])
    vreg2 <- lm(s[,i] ~ v[,i] + dmap + dmapxv[,i])
    print( colnames(v)[i])
    print(round(summary(vreg2)$coefficients[c(2,4),1:2], 2))
}
#
# 2015
tmp <- swRats$df2015d0
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
tmp <- swRats$df2015d3
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
print("year 2015")
for (i in c(2,1,3)){
    vreg  <- lm(s[,i] ~ v[,i])
    vreg2 <- lm(s[,i] ~ v[,i] + dmap + dmapxv[,i])
    print( colnames(v)[i])
    print(round(summary(vreg2)$coefficients[c(2,4),1:2], 2))
}
#

# Figure 1 in published paper
logit <- function(X){ log( X / (1-X) ) }
antilogit <- function(X){ exp(X) / (exp(X)+1) }
v.tmp <- (1:999)/1000
title <- ""
file <- "rhoExample"
#pdf (file = paste(gd, file, ".pdf", sep=""), width = 6,   height = 6)
plot(c(0,1),c(0,1), type = "n",
     xlab = "vote share", ylab = "seat share", axes = FALSE,
     main = title)
axis(1, at = seq(0,1,.2), labels = seq(0,1,.2))
axis(2, at = seq(0,1,.2), labels = seq(0,1,.2))
#
# versiones con sesgo en grises
s.tmp <- antilogit( 1.5 + 1 * logit( v.tmp ) )
lines(v.tmp, s.tmp, lty = 2, col="grey")
s.tmp <- antilogit ( 1.5 + 3 * logit( v.tmp ) ) 
lines(v.tmp, s.tmp, col = "grey", lty = 3) 
s.tmp <- antilogit ( 1.5 + 15 * logit( v.tmp ) ) # ESTO HACE LO DEBIDO: SESGO DE .25 EN FAVOR
lines(v.tmp, s.tmp, col = "grey")
#
s.tmp <- antilogit( 1 * logit( v.tmp ) )
lines(v.tmp, s.tmp, col = "black", lty = 2)
text(v.tmp[660], s.tmp[660], labels = expression(paste(rho, "=1")), pos=4)
s.tmp <- antilogit ( 3 * logit( v.tmp ) ) 
lines(v.tmp, s.tmp, col = "black", lty = 3) 
text(v.tmp[600], s.tmp[600], labels = expression(paste(rho, "=3")), pos=4)
s.tmp <- antilogit ( 15 * logit( v.tmp ) ) # ESTO HACE LO DEBIDO: SESGO DE .25 EN FAVOR
lines(v.tmp, s.tmp, col = "black")

text(v.tmp[440], s.tmp[440], labels = expression(rho %->% infinity), pos=4)
#dev.off()

# Figure 4, p. 8 in published article
#### Plot Posterior lambda Samples For d0 And d3
tmp <- biasRespOnLinzerSimsRPM
myQ <- function(Q,i){ # function to extract quantiles from posterior sample for plot: Q is desired quantile, i desired index number
    return(quantile(x= res$BUGSoutput$sims.list$lambda[,i], probs = Q, names=FALSE))
}
logit <- function(X){ log( X / (1-X) ) }
antilogit <- function(X){ exp(X) / (exp(X)+1) }
v.tmp <- (1:999)/1000
#
## pdf(file=paste(gd, "bias200615d0v.pdf", sep="")
par(mar=c(5,2,2,2)+0.1) # drop space for title and left labels
jitter <- rnorm(n = 300, sd = .03)
color1.minus.pri <- c( "blue", "gold", "green", "cyan", "orange", "violet" )
#
# 2003
res <- tmp$res2003d0v; shift.v <- .35
plot( c( -2.5, 2.5), -c(.5,5-.35),
     type="n", axes = FALSE, ylab = "", xlab = "bias relative to PRI")#, main = "Party bias")#"Bias: 2015 map (hypothetical)")
axis( side = 1, at = seq(from = -2.25, to = 2.25, by = .25), labels = FALSE)
axis( side = 1, at = seq(from = -2, to = 2, by = 1), labels = c("-2","-1","0","+1","+2"))
abline(v=seq(-2,2,.5), col= "gray70")
abline(v=0, lty=2)
abline(h=seq(-4.5,-1.5,1), lty=3, col= "gray70")
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
res <- tmp$res2012d0v; shift.v <- -.175 # use nation estimates
for (i in 1:3){ # some parties absent
    # if party color desired, this does the trick: col = color1.minus.pri[i]
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
#
text(x = rep(-2.1,5), y = -c(1:4,5-.35), labels = c("PAN", "PRD", "Green", "MC", "Morena"))
## dev.off()

## This bloc, taken from red.r, generates Fig. 3 in published article
### with d0
m <- min(c(df2006d0$rrin, df2009d0$rrin, df2012d0$rrin, df2015d0$rrin, df2006d3$rrin, df2009d3$rrin, df2012d3$rrin, df2015d3$rrin)); M <- max(c(df2006d0$rrin, df2009d0$rrin, df2012d0$rrin, df2015d0$rrin, df2006d3$rrin, df2009d3$rrin, df2012d3$rrin, df2015d3$rrin)); # min and max for two graphs
tmp <- c(df2006d0$rrin, df2009d0$rrin, df2012d0$rrin, df2015d0$rrin)
rrin <- data.frame(yr = c(rep(2006,300), rep(2009,300), rep(2012,300), rep(2015,300)), rri=tmp)
jitter <- rnorm(n = 300, sd = .05)
shift = .5 # useful if rris were also included in same plot
## pdf(file = paste(gd, "rrin0615d0.pdf", sep=""),
##       width = 7,
##       height = 3.5)
plot(x = c(m-.25,m,M), y = c(1,1,5), type = "n", ylab = "", xlab = "district relative representation index (RRI)", main = "2006 map (drawn with 2000 census)", axes = FALSE)
axis(1, at = seq(.3,2.1,.1), labels = FALSE, lwd.ticks = .5)
axis(1, at = seq(.5,2,.5))
abline(v = c(0.5,1.5,2), col = "gray70")
abline(v = 1, lty = 2)
abline(v = c(.85,1.15), col = "gray70", lty = 2)
text(x = rep(m-.2, 4), y = seq(1.5,4.5,1), labels = paste("in", seq(2015, 2006, -3)))
#text(x = rep(M+.35, 4),  y = (1:4)+shift, labels = rep("nation", 4), cex = .8)
#text(x = rep(M+.75, 8),  y = c(1.3,1.7, 2.3,2.7, 3.3,3.7, 4.3,4.7), labels = rep(c("state","nation"), 4), cex = .65)
for (i in seq(4,1,-1)){
    obj <- rrin$rri[rrin$yr==seq(2015,2006,-3)[i]] # subset one year
    points(x = obj, y = rep(i+shift, 300)+jitter, cex=.5, col = "gray70")
    select <- which(obj < quantile(obj, probs = .05) | obj > quantile(obj, probs = .95))
    points(x = obj[select], y = rep(i+shift, 300)[select]+jitter[select], cex=.5, col = "gray30")
    lines(x = c(quantile(obj, probs = .05), quantile(obj, probs = .95)), y = c(i+shift,i+shift), lwd = 2)
    lines(x = c(quantile(obj, probs = .25), quantile(obj, probs = .75)), y = c(i+shift,i+shift), lwd = 6)
    points(quantile(obj, probs = .5), i+shift, pch = 19, col="white")
    points(quantile(obj, probs = .5), i+shift, pch = 19, cex = .5)
}
## dev.off()
#
### for d3
jitter <- rnorm(n = 300, sd = .05)
shift = .5 # useful if rris were also included in same plot
tmp <- c(df2006d3$rrin, df2009d3$rrin, df2012d3$rrin, df2015d3$rrin)
rrin <- data.frame(yr = c(rep(2006,300), rep(2009,300), rep(2012,300), rep(2015,300)), rri=tmp)
## pdf(file = paste(gd, "rrin0615d3.pdf", sep=""),
##       width = 7,
##       height = 3.5)
plot(x = c(m-.25,m,M), y = c(1,1,5), type = "n", ylab = "", xlab = "district relative representation index (RRI)", main = "2015 map (drawn with 2010 census)", axes = FALSE)
axis(1, at = seq(.3,2.1,.1), labels = FALSE, lwd.ticks = .5)
axis(1, at = seq(.5,2,.5))
abline(v = c(0.5,1.5,2), col = "gray70")
abline(v = 1, lty = 2)
abline(v = c(.85,1.15), col = "gray70", lty = 2)
text(x = rep(m-.2, 4), y = seq(1.5,4.5,1), labels = paste("in", seq(2015, 2006, -3)))
#text(x = rep(M+.35, 4),  y = (1:4)+shift, labels = rep("nation", 4), cex = .8)
#text(x = rep(M+.75, 8),  y = c(1.3,1.7, 2.3,2.7, 3.3,3.7, 4.3,4.7), labels = rep(c("state","nation"), 4), cex = .65)
for (i in seq(4,1,-1)){
    obj <- rrin$rri[rrin$yr==seq(2015,2006,-3)[i]] # subset one year
    points(x = obj, y = rep(i+shift, 300)+jitter, cex=.5, col = "gray70")
    select <- which(obj < quantile(obj, probs = .05) | obj > quantile(obj, probs = .95))
    points(x = obj[select], y = rep(i+shift, 300)[select]+jitter[select], cex=.5, col = "gray30")
    lines(x = c(quantile(obj, probs = .05), quantile(obj, probs = .95)), y = c(i+shift,i+shift), lwd = 2)
    lines(x = c(quantile(obj, probs = .25), quantile(obj, probs = .75)), y = c(i+shift,i+shift), lwd = 6)
    points(quantile(obj, probs = .5), i+shift, pch = 19, col="white")
    points(quantile(obj, probs = .5), i+shift, pch = 19, cex = .5)
}
## dev.off()
#
# what percentile does rris<.85 and rris>1.15 correspond to? reported in p. 7 of published article
length(df2006d0$rris[df2006d0$rris<.85])*100/300
length(df2009d0$rris[df2009d0$rris<.85])*100/300
length(df2012d0$rris[df2012d0$rris<.85])*100/300
length(df2015d0$rris[df2015d0$rris<.85])*100/300
#
length(df2006d0$rris[df2006d0$rris>1.15])*100/300
length(df2009d0$rris[df2009d0$rris>1.15])*100/300
length(df2012d0$rris[df2012d0$rris>1.15])*100/300
length(df2015d0$rris[df2015d0$rris>1.15])*100/300
#
# which are the districts at the edges in 2015d0, reported in p. 8 of published article
obj <- df2015d0$rrin
select <- which(obj < quantile(obj, probs = .05))
tmp <- df2015d0[select, c("edon","disn","cab","rrin","rris")]
tmp[order(tmp$rrin),]
select <- which(obj > quantile(obj, probs = .95))
tmp <- df2015d0[select, c("edon","disn","cab","rrin","rris")]
tmp[order(-tmp$rrin),]
#
# summarize rris --- reported in p. 8 of published article
round(quantile(df2006d0$rrin, probs = c(0, .05, .25, .5, .75, .95, 1)), 2)
round(quantile(df2009d0$rrin, probs = c(0, .05, .25, .5, .75, .95, 1)), 2)
round(quantile(df2012d0$rrin, probs = c(0, .05, .25, .5, .75, .95, 1)), 2)
round(quantile(df2015d0$rrin, probs = c(0, .05, .25, .5, .75, .95, 1)), 2)
#
round(quantile(df2006d0$rris, probs = c(0, .05, .25, .5, .75, .95, 1)), 2)
round(quantile(df2009d0$rris, probs = c(0, .05, .25, .5, .75, .95, 1)), 2)
round(quantile(df2012d0$rris, probs = c(0, .05, .25, .5, .75, .95, 1)), 2)
round(quantile(df2015d0$rris, probs = c(0, .05, .25, .5, .75, .95, 1)), 2)


###########################################################################
## REPORT EFFECTS OF SPECIFYING PRI-GREEN PARTIAL COALITIONS DIFFERENTLY ##
###########################################################################
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



