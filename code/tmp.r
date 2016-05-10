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
# extracts 2013 districts only to map 2015 ones
dis2013 <-       eq[,c("edon","dis2015","munn","seccion","dis2013.3","dis2013.1")]
colnames(dis2013) <- c("edon","disn","munn","seccion","dis3er13","dis1er13")
#
# unassigned secciones need to be removed
dis2013 <- dis2013[-which(dis2013$disn==0),]
dis2013 <- dis2013[-which(dis2013$dis1er13==0),]
## THESE WERE DROPPED IN EXCEL: dis2013 <- dis2013[-which(dis2013$seccion>8000),] # two secciones in puebla have disconnected seccion nums and blank municipality... ife anticipates but awaits court ruling?
#
# maybe unnecessary (IFE's municipal codes slightly differ from census)
dis2013$ife <- dis2013$edon*1000 + dis2013$munn
dis2013 <- dis2013[,c("edon","disn","munn","seccion","ife","dis3er13","dis1er13")]
#
## NO LOGER NEEDED, OBJECT eq IMPORTED ABOVE HAS ALL THIS INFO
## ## IMPORTS 2013 DISTRICTS PROPOSAL DATA
## #source("codeFor2013districts.r") # RUN TO CODE 2013 ESCENARIO 1 and 3  DISTRICTS
## load(file=paste(dd, "dis2013.RData", sep=""))
#
tmp$edon.secn <- tmp$edon*10000 + tmp$seccion
dis2013$edon.secn <- dis2013$edon*10000 + dis2013$seccion
tmp1 <- dis2013[,c("edon.secn","dis1er13","dis3er13")]
tmp <- merge(x = tmp, y = tmp1, by = "edon.secn", all.x = TRUE)
colnames(tmp)[which(colnames(tmp)=="dis1er13")] <- "dis13.1"
colnames(tmp)[which(colnames(tmp)=="dis3er13")] <- "dis13.3"
rm(tmp1)
#
# OJO: some secciones used in 2015 may not appear listed in IFE's 2013 redistricting scenarios. List of such secciones follows.
## select <- which(dis2013$dis1er13>0 & dis2013$dis3er13==0)
## data.frame(edon=dis2013$edon[select], seccion=dis2013$seccion[select])
## #data.frame(edon=dis2013$edon[is.na(dis2013$dis3er13)==TRUE], seccion=dis2013$seccion[is.na(dis2013$dis3er13)==TRUE])
## dim(tmp); dim(dis2013)
## dis2013[dis2013$dis3er13==0,] # debug
#
colnames(tmp)
## Aggregates 2015 results by district
df2015d0 <- tmp[order(tmp$edon, tmp$disn),] # resultados con distritos reales (adoptados en 2005)
# así se hace en R un by yr mo: egen tmp=sum(invested) de stata
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
#
df2015d0[df2015d0$edon==24,]
#

# RESULTADOS CON LOS DISTRITOS PROPUESTOS EN 2013 (escen. 1)
df2015d1 <- tmp[order(tmp$edon, tmp$disn),] # resultados con distritos reales (adoptados en 2005)
# así se hace en R un by yr mo: egen tmp=sum(invested) de stata
df2015d1$pan <- ave(df2015d1$pan, as.factor(df2015d1$edon*100+df2015d1$disn), FUN=sum, na.rm=TRUE)
df2015d1$pri <- ave(df2015d1$pri, as.factor(df2015d1$edon*100+df2015d1$disn), FUN=sum, na.rm=TRUE)
df2015d1$pric <- ave(df2015d1$pric, as.factor(df2015d1$edon*100+df2015d1$disn), FUN=sum, na.rm=TRUE)
df2015d1$prd <- ave(df2015d1$prd, as.factor(df2015d1$edon*100+df2015d1$disn), FUN=sum, na.rm=TRUE)
df2015d1$prdc <- ave(df2015d1$prdc, as.factor(df2015d1$edon*100+df2015d1$disn), FUN=sum, na.rm=TRUE)
df2015d1$pt <- ave(df2015d1$pt, as.factor(df2015d1$edon*100+df2015d1$disn), FUN=sum, na.rm=TRUE)
df2015d1$pvem <- ave(df2015d1$pvem, as.factor(df2015d1$edon*100+df2015d1$disn), FUN=sum, na.rm=TRUE)
df2015d1$mc <- ave(df2015d1$mc, as.factor(df2015d1$edon*100+df2015d1$disn), FUN=sum, na.rm=TRUE)
df2015d1$panal <- ave(df2015d1$panal, as.factor(df2015d1$edon*100+df2015d1$disn), FUN=sum, na.rm=TRUE)
df2015d1$morena <- ave(df2015d1$morena, as.factor(df2015d1$edon*100+df2015d1$disn), FUN=sum, na.rm=TRUE)
df2015d1$ph <- ave(df2015d1$ph, as.factor(df2015d1$edon*100+df2015d1$disn), FUN=sum, na.rm=TRUE)
df2015d1$ps <- ave(df2015d1$ps, as.factor(df2015d1$edon*100+df2015d1$disn), FUN=sum, na.rm=TRUE)
df2015d1$indep1 <- ave(df2015d1$indep1, as.factor(df2015d1$edon*100+df2015d1$disn), FUN=sum, na.rm=TRUE)
df2015d1$indep2 <- ave(df2015d1$indep2, as.factor(df2015d1$edon*100+df2015d1$disn), FUN=sum, na.rm=TRUE)
df2015d1$pri_pvem <- ave(df2015d1$pri_pvem, as.factor(df2015d1$edon*100+df2015d1$disn), FUN=sum, na.rm=TRUE)
df2015d1$prd_pt <- ave(df2015d1$prd_pt, as.factor(df2015d1$edon*100+df2015d1$disn), FUN=sum, na.rm=TRUE)
df2015d1$nr <- ave(df2015d1$nr, as.factor(df2015d1$edon*100+df2015d1$disn), FUN=sum, na.rm=TRUE)
df2015d1$nul <- ave(df2015d1$nul, as.factor(df2015d1$edon*100+df2015d1$disn), FUN=sum, na.rm=TRUE)
df2015d1$efec <- ave(df2015d1$efec, as.factor(df2015d1$edon*100+df2015d1$disn), FUN=sum, na.rm=TRUE)
df2015d1$lisnom <- ave(df2015d1$lisnom, as.factor(df2015d1$edon*100+df2015d1$disn), FUN=sum, na.rm=TRUE)
df2015d1$tmp <- rep(1, times = nrow(df2015d1))
df2015d1$tmp <- ave(df2015d1$tmp, as.factor(df2015d1$edon*100+df2015d1$disn), FUN=sum, na.rm=TRUE)
df2015d1$dcoalpri <- ave(df2015d1$dcoalpri, as.factor(df2015d1$edon*100+df2015d1$disn), FUN=sum, na.rm=TRUE)
df2015d1$dcoalpri <- df2015d1$dcoalpri/df2015d1$tmp # share of secciones with pri coalition in district
colnames(df2015d1)[which(colnames(df2015d1)=="dcoalpri")] <- "shSecCoalPri"
df2015d1$dcoalprd <- ave(df2015d1$dcoalprd, as.factor(df2015d1$edon*100+df2015d1$disn), FUN=sum, na.rm=TRUE)
df2015d1$dcoalprd <- df2015d1$dcoalprd/df2015d1$tmp # share of secciones with prd coalition in district
colnames(df2015d1)[which(colnames(df2015d1)=="dcoalprd")] <- "shSecCoalPrd"
df2015d1$tmp <- NULL
df2015d1 <- df2015d1[duplicated(df2015d1$edon*100+df2015d1$disn)==FALSE,]

# RESULTADOS CON LOS DISTRITOS PROPUESTOS EN 2013 (escen. 3)
df2015d3 <- tmp[order(tmp$edon, tmp$disn),] # resultados con distritos reales (adoptados en 2005)
# así se hace en R un by yr mo: egen tmp=sum(invested) de stata
df2015d3$pan <- ave(df2015d3$pan, as.factor(df2015d3$edon*100+df2015d3$disn), FUN=sum, na.rm=TRUE)
df2015d3$pri <- ave(df2015d3$pri, as.factor(df2015d3$edon*100+df2015d3$disn), FUN=sum, na.rm=TRUE)
df2015d3$pric <- ave(df2015d3$pric, as.factor(df2015d3$edon*100+df2015d3$disn), FUN=sum, na.rm=TRUE)
df2015d3$prd <- ave(df2015d3$prd, as.factor(df2015d3$edon*100+df2015d3$disn), FUN=sum, na.rm=TRUE)
df2015d3$prdc <- ave(df2015d3$prdc, as.factor(df2015d3$edon*100+df2015d3$disn), FUN=sum, na.rm=TRUE)
df2015d3$pt <- ave(df2015d3$pt, as.factor(df2015d3$edon*100+df2015d3$disn), FUN=sum, na.rm=TRUE)
df2015d3$pvem <- ave(df2015d3$pvem, as.factor(df2015d3$edon*100+df2015d3$disn), FUN=sum, na.rm=TRUE)
df2015d3$mc <- ave(df2015d3$mc, as.factor(df2015d3$edon*100+df2015d3$disn), FUN=sum, na.rm=TRUE)
df2015d3$panal <- ave(df2015d3$panal, as.factor(df2015d3$edon*100+df2015d3$disn), FUN=sum, na.rm=TRUE)
df2015d3$morena <- ave(df2015d3$morena, as.factor(df2015d3$edon*100+df2015d3$disn), FUN=sum, na.rm=TRUE)
df2015d3$ph <- ave(df2015d3$ph, as.factor(df2015d3$edon*100+df2015d3$disn), FUN=sum, na.rm=TRUE)
df2015d3$ps <- ave(df2015d3$ps, as.factor(df2015d3$edon*100+df2015d3$disn), FUN=sum, na.rm=TRUE)
df2015d3$indep1 <- ave(df2015d3$indep1, as.factor(df2015d3$edon*100+df2015d3$disn), FUN=sum, na.rm=TRUE)
df2015d3$indep2 <- ave(df2015d3$indep2, as.factor(df2015d3$edon*100+df2015d3$disn), FUN=sum, na.rm=TRUE)
df2015d3$pri_pvem <- ave(df2015d3$pri_pvem, as.factor(df2015d3$edon*100+df2015d3$disn), FUN=sum, na.rm=TRUE)
df2015d3$prd_pt <- ave(df2015d3$prd_pt, as.factor(df2015d3$edon*100+df2015d3$disn), FUN=sum, na.rm=TRUE)
df2015d3$nr <- ave(df2015d3$nr, as.factor(df2015d3$edon*100+df2015d3$disn), FUN=sum, na.rm=TRUE)
df2015d3$nul <- ave(df2015d3$nul, as.factor(df2015d3$edon*100+df2015d3$disn), FUN=sum, na.rm=TRUE)
df2015d3$efec <- ave(df2015d3$efec, as.factor(df2015d3$edon*100+df2015d3$disn), FUN=sum, na.rm=TRUE)
df2015d3$lisnom <- ave(df2015d3$lisnom, as.factor(df2015d3$edon*100+df2015d3$disn), FUN=sum, na.rm=TRUE)
df2015d3$tmp <- rep(1, times = nrow(df2015d3))
df2015d3$tmp <- ave(df2015d3$tmp, as.factor(df2015d3$edon*100+df2015d3$disn), FUN=sum, na.rm=TRUE)
df2015d3$dcoalpri <- ave(df2015d3$dcoalpri, as.factor(df2015d3$edon*100+df2015d3$disn), FUN=sum, na.rm=TRUE)
df2015d3$dcoalpri <- df2015d3$dcoalpri/df2015d3$tmp # share of secciones with pri coalition in district
colnames(df2015d3)[which(colnames(df2015d3)=="dcoalpri")] <- "shSecCoalPri"
df2015d3$dcoalprd <- ave(df2015d3$dcoalprd, as.factor(df2015d3$edon*100+df2015d3$disn), FUN=sum, na.rm=TRUE)
df2015d3$dcoalprd <- df2015d3$dcoalprd/df2015d3$tmp # share of secciones with prd coalition in district
colnames(df2015d3)[which(colnames(df2015d3)=="dcoalprd")] <- "shSecCoalPrd"
df2015d3$tmp <- NULL
df2015d3 <- df2015d3[duplicated(df2015d3$edon*100+df2015d3$disn)==FALSE,]
df2015d3$shSecCoalPrd ## debug: should have some districts with incomplete coalition...



## table(df2015d1$shSecCoalPri) ## debug: should now be 0 or 1 after fix used above
## table(df2015d1$shSecCoalPrd) ## debug: should now be 0 or 1 after fix used above
df2015d1$pri_pvem[df2015d1$shSecCoalPri==0] # debug: no need to assign, all == 0
df2015d1$pric[df2015d1$shSecCoalPri==0] <- 0
df2015d1$pri[df2015d1$shSecCoalPri==1] <- 0
df2015d1$pvem[df2015d1$shSecCoalPri==1] <- 0
#df2015d1$pri_pvem <- NULL
#
df2015d1$prd_pt[df2015d1$shSecCoalPrd==0] # debug: no need to assign, all == 0
df2015d1$prdc[df2015d1$shSecCoalPrd==0] <- 0
df2015d1$prd[df2015d1$shSecCoalPrd==1] <- 0
df2015d1$pt[df2015d1$shSecCoalPrd==1] <- 0
#df2015d1$prd_pt <- NULL
#
df2015d1[df2015d1$edon==24,]

#






## table(df2015d3$shSecCoalPri) ## debug: should now be 0 or 1 after fix used above
## table(df2015d3$shSecCoalPrd) ## debug: should now be 0 or 1 after fix used above
df2015d3$pri_pvem[df2015d3$shSecCoalPri==0] # debug: no need to assign, all == 0
df2015d3$pric[df2015d3$shSecCoalPri==0] <- 0
df2015d3$pri[df2015d3$shSecCoalPri==1] <- 0
df2015d3$pvem[df2015d3$shSecCoalPri==1] <- 0
#df2015d3$pri_pvem <- NULL
#
df2015d3$prd_pt[df2015d3$shSecCoalPrd==0] # debug: no need to assign, all == 0
df2015d3$prdc[df2015d3$shSecCoalPrd==0] <- 0
df2015d3$prd[df2015d3$shSecCoalPrd==1] <- 0
df2015d3$pt[df2015d3$shSecCoalPrd==1] <- 0
#df2015d3$prd_pt <- NULL
#
df2015d3[df2015d3$edon==24,]

#
# removes redundant columns
df2015d0 <- df2015d0[,c("edon","disn"   ,"pan","pri","pric","prdc","pvem","panal","pripvem","efec","nr","nul","lisnom","shSecCoalPri")]
df2015d1 <- df2015d1[,c("edon","dis13.1","pan","pri","pric","prdc","pvem","panal","pripvem","efec","nr","nul","lisnom","shSecCoalPri")]
df2015d3 <- df2015d3[,c("edon","dis13.3","pan","pri","pric","prdc","pvem","panal","pripvem","efec","nr","nul","lisnom","shSecCoalPri")]
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
df2015d0$pripvem <- NULL
## df2015d0$pri[df2015d0$shSecCoalPri>=.5] <- 0
## df2015d0$pvem[df2015d0$shSecCoalPri>=.5] <- 0
#
#df2015d1$pripvem[df2015d1$shSecCoalPri<.5] # debug: need to assign >0 to pri and pvem in new districts coded as having no coalition
df2015d1$pric[df2015d1$shSecCoalPri<.5] <- 0
shrPri <- df2015d1$pri[df2015d1$shSecCoalPri<.5] / ( df2015d1$pri[df2015d1$shSecCoalPri<.5] + df2015d1$pvem[df2015d1$shSecCoalPri<.5] )
df2015d1$pri[df2015d1$shSecCoalPri<.5] <- df2015d1$pri[df2015d1$shSecCoalPri<.5] + shrPri * df2015d1$pripvem[df2015d1$shSecCoalPri<.5]
df2015d1$pvem[df2015d1$shSecCoalPri<.5] <- df2015d1$pvem[df2015d1$shSecCoalPri<.5] + (1-shrPri) * df2015d1$pripvem[df2015d1$shSecCoalPri<.5]
df2015d1$pripvem <- NULL
df2015d1$pri[df2015d1$shSecCoalPri>=.5] <- 0
df2015d1$pvem[df2015d1$shSecCoalPri>=.5] <- 0
#
df2015d3$pric[df2015d3$shSecCoalPri<.5] <- 0
shrPri <- df2015d3$pri[df2015d3$shSecCoalPri<.5] / ( df2015d3$pri[df2015d3$shSecCoalPri<.5] + df2015d3$pvem[df2015d3$shSecCoalPri<.5] )
df2015d3$pri[df2015d3$shSecCoalPri<.5] <- df2015d3$pri[df2015d3$shSecCoalPri<.5] + shrPri * df2015d3$pripvem[df2015d3$shSecCoalPri<.5]
df2015d3$pvem[df2015d3$shSecCoalPri<.5] <- df2015d3$pvem[df2015d3$shSecCoalPri<.5] + (1-shrPri) * df2015d3$pripvem[df2015d3$shSecCoalPri<.5]
df2015d3$pripvem <- NULL
df2015d3$pri[df2015d3$shSecCoalPri>=.5] <- 0
df2015d3$pvem[df2015d3$shSecCoalPri>=.5] <- 0
rm(shrPri)
df2015d1[df2015d1$edon==24,]
#
## winner
tmp <- rep(0, times=300)
df2015d0$panw <- df2015d0$priw <- df2015d0$pricw <- df2015d0$prdcw <- df2015d0$pvemw <- df2015d0$panalw <- tmp
tmp <- apply( df2015d0[,c("pan", "pri", "pric", "prdc", "pvem", "panal")], MARGIN = 1, max)
df2015d0$panw[df2015d0$pan==tmp] <- 1
df2015d0$priw[df2015d0$pri==tmp] <- 1
df2015d0$pricw[df2015d0$pric==tmp] <- 1
df2015d0$prdcw[df2015d0$prdc==tmp] <- 1
df2015d0$pvemw[df2015d0$pvem==tmp] <- 1
df2015d0$panalw[df2015d0$panal==tmp] <- 1
## winner's margin
tmp <- apply( df2015d0[,c("pan", "pri", "pric", "prdc", "pvem", "panal")], MARGIN = 1, max) - apply( df2015d0[,c("pan", "pri", "pric", "prdc", "pvem", "panal")], MARGIN = 1, function(x) sort(x,partial=length(x)-1)[length(x)-1])
df2015d0$winmg <- tmp/df2015d0$efec
#
tmp <- rep(0, times=300)
df2015d1$panw <- df2015d1$priw <- df2015d1$pricw <- df2015d1$prdcw <- df2015d1$pvemw <- df2015d1$panalw <- tmp
tmp <- apply( df2015d1[,c("pan", "pri", "pric", "prdc", "pvem", "panal")], MARGIN = 1, max)
df2015d1$panw[df2015d1$pan==tmp] <- 1
df2015d1$priw[df2015d1$pri==tmp] <- 1
df2015d1$pricw[df2015d1$pric==tmp] <- 1
df2015d1$prdcw[df2015d1$prdc==tmp] <- 1
df2015d1$pvemw[df2015d1$pvem==tmp] <- 1
df2015d1$panalw[df2015d1$panal==tmp] <- 1
## winner's margin
tmp <- apply( df2015d1[,c("pan", "pri", "pric", "prdc", "pvem", "panal")], MARGIN = 1, max) - apply( df2015d1[,c("pan", "pri", "pric", "prdc", "pvem", "panal")], MARGIN = 1, function(x) sort(x,partial=length(x)-1)[length(x)-1])
df2015d1$winmg <- tmp/df2015d1$efec
#
table(df2015d3$disn) # there are disn==0... i'll drop them but they need fixing
df2015d3 <- df2015d3[-which(df2015d3$disn==0),]
tmp <- rep(0, times=300)
df2015d3$panw <- df2015d3$priw <- df2015d3$pricw <- df2015d3$prdcw <- df2015d3$pvemw <- df2015d3$panalw <- tmp
tmp <- apply( df2015d3[,c("pan", "pri", "pric", "prdc", "pvem", "panal")], MARGIN = 1, max)
df2015d3$panw[df2015d3$pan==tmp] <- 1
df2015d3$priw[df2015d3$pri==tmp] <- 1
df2015d3$pricw[df2015d3$pric==tmp] <- 1
df2015d3$prdcw[df2015d3$prdc==tmp] <- 1
df2015d3$pvemw[df2015d3$pvem==tmp] <- 1
df2015d3$panalw[df2015d3$panal==tmp] <- 1
## winner's margin
tmp <- apply( df2015d3[,c("pan", "pri", "pric", "prdc", "pvem", "panal")], MARGIN = 1, max) - apply( df2015d3[,c("pan", "pri", "pric", "prdc", "pvem", "panal")], MARGIN = 1, function(x) sort(x,partial=length(x)-1)[length(x)-1])
df2015d3$winmg <- tmp/df2015d3$efec
#
# debug
df2015d0[df2015d0$edon==6,]
df2015d1[df2015d1$edon==24,]
df2015d3[df2015d3$edon==24,]
#
# state aggregates statistics
df2015s0 <- df2015d0
df2015s0 <- df2015s0[order(df2015s0$edon),]
## # WILL DROP THIS STEP THAT FORCES STATES WITH PARTIAL COALITION TO REPORT ALL VOTES IN PRI OR IN PRIC... MAYBE NEEDED TO COMPUTE PRI BIAS?
## colnames(df2015s0)[which(colnames(df2015s0)=="shSecCoalPri")] <- "shDisCoalPri"
## df2015s0$shDisCoalPri <- rep(0, 300)
## df2015s0$shDisCoalPri[df2015s0$pric>0] <- 1
## df2015s0$shDisCoalPri <- ave(df2015s0$shDisCoalPri, as.factor(df2015s0$edon), FUN=sum, na.rm=TRUE)
df2015s0$pan <- ave(df2015s0$pan, as.factor(df2015s0$edon), FUN=sum, na.rm=TRUE)
df2015s0$pri <- ave(df2015s0$pri, as.factor(df2015s0$edon), FUN=sum, na.rm=TRUE)
df2015s0$pric <- ave(df2015s0$pric, as.factor(df2015s0$edon), FUN=sum, na.rm=TRUE)
df2015s0$prdc <- ave(df2015s0$prdc, as.factor(df2015s0$edon), FUN=sum, na.rm=TRUE)
df2015s0$pvem <- ave(df2015s0$pvem, as.factor(df2015s0$edon), FUN=sum, na.rm=TRUE)
df2015s0$panal <- ave(df2015s0$panal, as.factor(df2015s0$edon), FUN=sum, na.rm=TRUE)
df2015s0$efec <- ave(df2015s0$efec, as.factor(df2015s0$edon), FUN=sum, na.rm=TRUE)
df2015s0$ndis <- rep(1, 300)
df2015s0$ndis <- ave(df2015s0$ndis, as.factor(df2015s0$edon), FUN=sum, na.rm=TRUE)
df2015s0$panw <- ave(df2015s0$panw, as.factor(df2015s0$edon), FUN=sum, na.rm=TRUE)
df2015s0$priw <- ave(df2015s0$priw, as.factor(df2015s0$edon), FUN=sum, na.rm=TRUE)
df2015s0$pricw <- ave(df2015s0$pricw, as.factor(df2015s0$edon), FUN=sum, na.rm=TRUE)
df2015s0$prdcw <- ave(df2015s0$prdcw, as.factor(df2015s0$edon), FUN=sum, na.rm=TRUE)
df2015s0$pvemw <- ave(df2015s0$pvemw, as.factor(df2015s0$edon), FUN=sum, na.rm=TRUE)
df2015s0$panalw <- ave(df2015s0$panalw, as.factor(df2015s0$edon), FUN=sum, na.rm=TRUE)
df2015s0 <- df2015s0[duplicated(df2015s0$edon)==FALSE,]
#
# shares
df2015s0$pansh   <- df2015s0$pan/df2015s0$efec
df2015s0$prish   <- df2015s0$pri/df2015s0$efec
df2015s0$pricsh  <- df2015s0$pric/df2015s0$efec
df2015s0$prdcsh  <- df2015s0$prdc/df2015s0$efec
df2015s0$pvemsh  <- df2015s0$pvem/df2015s0$efec
df2015s0$panalsh <- df2015s0$panal/df2015s0$efec
#
df2015s0$panw   <- df2015s0$panw/df2015s0$ndis
df2015s0$priw   <- df2015s0$priw/df2015s0$ndis
df2015s0$pricw  <- df2015s0$pricw/df2015s0$ndis
df2015s0$prdcw  <- df2015s0$prdcw/df2015s0$ndis
df2015s0$pvemw  <- df2015s0$pvemw/df2015s0$ndis
df2015s0$panalw <- df2015s0$panalw/df2015s0$ndis
#df2015s0$shDisCoalPri <-  df2015s0$shDisCoalPri/df2015s0$ndis
head(df2015s0)
df2015s1 <- df2015d1
df2015s1 <- df2015s1[order(df2015s1$edon),]
## # WILL DROP THIS STEP THAT FORCES STATES WITH PARTIAL COALITION TO REPORT ALL VOTES IN PRI OR IN PRIC... MAYBE NEEDED TO COMPUTE PRI BIAS?
## colnames(df2015s1)[which(colnames(df2015s1)=="shSecCoalPri")] <- "shDisCoalPri"
## df2015s1$shDisCoalPri <- rep(0, 300)
## df2015s1$shDisCoalPri[df2015s1$pric>0] <- 1
## df2015s1$shDisCoalPri <- ave(df2015s1$shDisCoalPri, as.factor(df2015s1$disn), FUN=sum, na.rm=TRUE)
df2015s1$pan <- ave(df2015s1$pan, as.factor(df2015s1$edon), FUN=sum, na.rm=TRUE)
df2015s1$pri <- ave(df2015s1$pri, as.factor(df2015s1$edon), FUN=sum, na.rm=TRUE)
df2015s1$pric <- ave(df2015s1$pric, as.factor(df2015s1$edon), FUN=sum, na.rm=TRUE)
df2015s1$prdc <- ave(df2015s1$prdc, as.factor(df2015s1$edon), FUN=sum, na.rm=TRUE)
df2015s1$pvem <- ave(df2015s1$pvem, as.factor(df2015s1$edon), FUN=sum, na.rm=TRUE)
df2015s1$panal <- ave(df2015s1$panal, as.factor(df2015s1$edon), FUN=sum, na.rm=TRUE)
df2015s1$efec <- ave(df2015s1$efec, as.factor(df2015s1$edon), FUN=sum, na.rm=TRUE)
df2015s1$ndis <- rep(1, 300)
df2015s1$ndis <- ave(df2015s1$ndis, as.factor(df2015s1$edon), FUN=sum, na.rm=TRUE)
df2015s1$panw <- ave(df2015s1$panw, as.factor(df2015s1$edon), FUN=sum, na.rm=TRUE)
df2015s1$priw <- ave(df2015s1$priw, as.factor(df2015s1$edon), FUN=sum, na.rm=TRUE)
df2015s1$pricw <- ave(df2015s1$pricw, as.factor(df2015s1$edon), FUN=sum, na.rm=TRUE)
df2015s1$prdcw <- ave(df2015s1$prdcw, as.factor(df2015s1$edon), FUN=sum, na.rm=TRUE)
df2015s1$pvemw <- ave(df2015s1$pvemw, as.factor(df2015s1$edon), FUN=sum, na.rm=TRUE)
df2015s1$panalw <- ave(df2015s1$panalw, as.factor(df2015s1$edon), FUN=sum, na.rm=TRUE)
df2015s1 <- df2015s1[duplicated(df2015s1$edon)==FALSE,]
#
# shares
df2015s1$pansh   <- df2015s1$pan/df2015s1$efec
df2015s1$prish   <- df2015s1$pri/df2015s1$efec
df2015s1$pricsh  <- df2015s1$pric/df2015s1$efec
df2015s1$prdcsh  <- df2015s1$prdc/df2015s1$efec
df2015s1$pvemsh  <- df2015s1$pvem/df2015s1$efec
df2015s1$panalsh <- df2015s1$panal/df2015s1$efec
#
df2015s1$panw   <- df2015s1$panw/df2015s1$ndis
df2015s1$priw   <- df2015s1$priw/df2015s1$ndis
df2015s1$pricw  <- df2015s1$pricw/df2015s1$ndis
df2015s1$prdcw  <- df2015s1$prdcw/df2015s1$ndis
df2015s1$pvemw  <- df2015s1$pvemw/df2015s1$ndis
df2015s1$panalw <- df2015s1$panalw/df2015s1$ndis
#df2015s1$shDisCoalPri <-  df2015s1$shDisCoalPri/df2015s1$ndis
head(df2015s1)
#
df2015s3 <- df2015d3
df2015s3 <- df2015s3[order(df2015s3$edon),]
## # WILL DROP THIS STEP THAT FORCES STATES WITH PARTIAL COALITION TO REPORT ALL VOTES IN PRI OR IN PRIC... MAYBE NEEDED TO COMPUTE PRI BIAS?
## colnames(df2015s3)[which(colnames(df2015s3)=="shSecCoalPri")] <- "shDisCoalPri"
## df2015s3$shDisCoalPri <- rep(0, 300)
## df2015s3$shDisCoalPri[df2015s3$pric>0] <- 1
## df2015s3$shDisCoalPri <- ave(df2015s3$shDisCoalPri, as.factor(df2015s3$edon), FUN=sum, na.rm=TRUE)
df2015s3$pan <- ave(df2015s3$pan, as.factor(df2015s3$edon), FUN=sum, na.rm=TRUE)
df2015s3$pri <- ave(df2015s3$pri, as.factor(df2015s3$edon), FUN=sum, na.rm=TRUE)
df2015s3$pric <- ave(df2015s3$pric, as.factor(df2015s3$edon), FUN=sum, na.rm=TRUE)
df2015s3$prdc <- ave(df2015s3$prdc, as.factor(df2015s3$edon), FUN=sum, na.rm=TRUE)
df2015s3$pvem <- ave(df2015s3$pvem, as.factor(df2015s3$edon), FUN=sum, na.rm=TRUE)
df2015s3$panal <- ave(df2015s3$panal, as.factor(df2015s3$edon), FUN=sum, na.rm=TRUE)
df2015s3$efec <- ave(df2015s3$efec, as.factor(df2015s3$edon), FUN=sum, na.rm=TRUE)
df2015s3$ndis <- rep(1, 300)
df2015s3$ndis <- ave(df2015s3$ndis, as.factor(df2015s3$edon), FUN=sum, na.rm=TRUE)
df2015s3$panw <- ave(df2015s3$panw, as.factor(df2015s3$edon), FUN=sum, na.rm=TRUE)
df2015s3$priw <- ave(df2015s3$priw, as.factor(df2015s3$edon), FUN=sum, na.rm=TRUE)
df2015s3$pricw <- ave(df2015s3$pricw, as.factor(df2015s3$edon), FUN=sum, na.rm=TRUE)
df2015s3$prdcw <- ave(df2015s3$prdcw, as.factor(df2015s3$edon), FUN=sum, na.rm=TRUE)
df2015s3$pvemw <- ave(df2015s3$pvemw, as.factor(df2015s3$edon), FUN=sum, na.rm=TRUE)
df2015s3$panalw <- ave(df2015s3$panalw, as.factor(df2015s3$edon), FUN=sum, na.rm=TRUE)
df2015s3 <- df2015s3[duplicated(df2015s3$edon)==FALSE,]
#
# shares
df2015s3$pansh   <- df2015s3$pan/df2015s3$efec
df2015s3$prish   <- df2015s3$pri/df2015s3$efec
df2015s3$pricsh  <- df2015s3$pric/df2015s3$efec
df2015s3$prdcsh  <- df2015s3$prdc/df2015s3$efec
df2015s3$pvemsh  <- df2015s3$pvem/df2015s3$efec
df2015s3$panalsh <- df2015s3$panal/df2015s3$efec
#
df2015s3$panw   <- df2015s3$panw/df2015s3$ndis
df2015s3$priw   <- df2015s3$priw/df2015s3$ndis
df2015s3$pricw  <- df2015s3$pricw/df2015s3$ndis
df2015s3$prdcw  <- df2015s3$prdcw/df2015s3$ndis
df2015s3$pvemw  <- df2015s3$pvemw/df2015s3$ndis
df2015s3$panalw <- df2015s3$panalw/df2015s3$ndis
#df2015s3$shDisCoalPri <-  df2015s3$shDisCoalPri/df2015s3$ndis
head(df2015s3)
#
# 2009 data
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
