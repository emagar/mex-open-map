############################################################################
############################################################################
## OJO: biasStats tiene molde para estimar bias y responsiveness nacional ##
############################################################################
############################################################################

# NOTE:
# Maps are named after the first congressional election they were used: the 1979, 1997, 2006 maps and the 2015 proposal (never adopted).
# Short names for objects in the analysis of the 2015 proposals are d0 (the 2006 map or status quo), d1 (first IFE proposal) and d3 (third and final proposal, with party feedback). 

rm(list=ls())
#
wd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/git-repo/mex-open-map/")  # where to save and retrieve objects
dd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/git-repo/mex-open-map/data/") # raw data directory
setwd(wd)
#
#
######################################################################################################################
## IMPORTS OBJECT eq TO MAP SECCIONES TO 1979, 1997, 2006 DISTRICTS AND TO TWO 2015 REDISTRICTING PROPOSALS.        ##
## SCRIPT eqPrep.r MANIPULATES equivalencias FILE, WHICH HAS MISSING SECCIONES AND CAN BE UPDATED WHEN INFO ARRIVES ##
## ** This takes a minute or so to complete **                                                                      ##
######################################################################################################################
cd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/git-repo/mex-open-map/code/")
source(file = paste(cd, "eqPrep.r", sep = ""), echo = TRUE)
rm(cd)
#
#
#
##################################
## READ SECCION-LEVEL ELEC DATA ##
##################################
tmp <- read.csv( paste(dd, "dfSeccion2012.csv", sep=""), header=TRUE)
tmp <- tmp[order(tmp$edon, tmp$disn, tmp$seccion),]
# compute effective vote (without void ballots)
tmp$efec <- tmp$pan + tmp$pri + tmp$prd + tmp$pvem + tmp$pt + tmp$mc + tmp$panal + tmp$pripvem + tmp$prdptmc + tmp$prdpt + tmp$prdmc + tmp$ptmc
head(tmp[tmp$edon==1,])
# aggregates coalition votes where needed
tmp$prdc <- tmp$prd+tmp$pt+tmp$mc+tmp$prdptmc+tmp$prdpt+tmp$prdmc+tmp$ptmc
tmp$prd <- tmp$pt <- tmp$mc <- tmp$prdptmc <- tmp$prdpt <- tmp$prdmc <- tmp$ptmc <- NULL # UNLIKE PRI AND PVEM PARTIAL COALITIONS, THESE ARE REDUNDANT
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
# extracts 2013 districts only to map 2012 ones
dis2013 <-       eq[,c("edon","dis2012","munn","seccion","dis2013.3","dis2013.1")]
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
# OJO: some secciones used in 2012 do not appear listed in IFE's 2013 redistricting scenarios. List of such secciones follows.
select <- which(dis2013$dis1er13>0 & dis2013$dis3er13==0)
data.frame(edon=dis2013$edon[select], seccion=dis2013$seccion[select])
#data.frame(edon=dis2013$edon[is.na(dis2013$dis3er13)==TRUE], seccion=dis2013$seccion[is.na(dis2013$dis3er13)==TRUE])
dim(tmp); dim(dis2013)
dis2013[dis2013$dis3er13==0,] # debug
#
colnames(tmp)
## Aggregates 2012 results by district
df2012d0 <- tmp[order(tmp$edon, tmp$disn),] # resultados con distritos reales (adoptados en 2005)
# así se hace en R un by yr mo: egen tmp=sum(invested) de stata
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
df2012d1 <- tmp[is.na(tmp$dis13.1)==FALSE,] # elimina las secciones no asignadas a distrito (posiblemente por reseccionamiento, habrá que recuperarlas)
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
df2012d3 <- tmp[is.na(tmp$dis13.3)==FALSE,] # elimina las secciones no asignadas a distrito (posiblemente por reseccionamiento, habrá que recuperarlas)
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
df2012d1[df2012d1$edon==24,]
#
## winner  OJO: PREPARE CODE FOR MARGIN AS WELL
tmp <- rep(0, times=300)
df2012d0$panw <- df2012d0$priw <- df2012d0$pricw <- df2012d0$prdcw <- df2012d0$pvemw <- df2012d0$panalw <- tmp
tmp <- apply( df2012d0[,c("pan", "pri", "pric", "prdc", "pvem", "panal")], MARGIN = 1, max)
df2012d0$panw[df2012d0$pan==tmp] <- 1
df2012d0$priw[df2012d0$pri==tmp] <- 1
df2012d0$pricw[df2012d0$pric==tmp] <- 1
df2012d0$prdcw[df2012d0$prdc==tmp] <- 1
df2012d0$pvemw[df2012d0$pvem==tmp] <- 1
df2012d0$panalw[df2012d0$panal==tmp] <- 1
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
#
table(df2012d3$disn) # there are disn==0... i'll drop them but they need fixing
df2012d3 <- df2012d3[-which(df2012d3$disn==0),]
tmp <- rep(0, times=300)
df2012d3$panw <- df2012d3$priw <- df2012d3$pricw <- df2012d3$prdcw <- df2012d3$pvemw <- df2012d3$panalw <- tmp
tmp <- apply( df2012d3[,c("pan", "pri", "pric", "prdc", "pvem", "panal")], MARGIN = 1, max)
df2012d3$panw[df2012d3$pan==tmp] <- 1
df2012d3$priw[df2012d3$pri==tmp] <- 1
df2012d3$pricw[df2012d3$pric==tmp] <- 1
df2012d3$prdcw[df2012d3$prdc==tmp] <- 1
df2012d3$pvemw[df2012d3$pvem==tmp] <- 1
df2012d3$panalw[df2012d3$panal==tmp] <- 1
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
## # WILL DROP THIS STEP THAT FORCES STATES WITH PARTIAL COALITION TO REPORT ALL VOTES IN PRI OR IN PRIC... MAYBE NEEDED TO COMPUTE PRI BIAS?
## colnames(df2012s1)[which(colnames(df2012s1)=="shSecCoalPri")] <- "shDisCoalPri"
## df2012s1$shDisCoalPri <- rep(0, 300)
## df2012s1$shDisCoalPri[df2012s1$pric>0] <- 1
## df2012s1$shDisCoalPri <- ave(df2012s1$shDisCoalPri, as.factor(df2012s1$disn), FUN=sum, na.rm=TRUE)
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
## # WILL DROP THIS STEP THAT FORCES STATES WITH PARTIAL COALITION TO REPORT ALL VOTES IN PRI OR IN PRIC... MAYBE NEEDED TO COMPUTE PRI BIAS?
## colnames(df2012s3)[which(colnames(df2012s3)=="shSecCoalPri")] <- "shDisCoalPri"
## df2012s3$shDisCoalPri <- rep(0, 300)
## df2012s3$shDisCoalPri[df2012s3$pric>0] <- 1
## df2012s3$shDisCoalPri <- ave(df2012s3$shDisCoalPri, as.factor(df2012s3$edon), FUN=sum, na.rm=TRUE)
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
#
## Agrega resultados 2009 por distrito
df2009d0 <- tmp[order(tmp$edon, tmp$disn),] # resultados con distritos reales (adoptados en 2005)
# así se hace en R un by yr mo: egen tmp=sum(invested) de stata
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
df2009d1 <- tmp[is.na(tmp$dis13.1)==FALSE,] # elimina las secciones no asignadas a distrito (posiblemente por reseccionamiento, habrá que recuperarlas)
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
df2009d3 <- tmp[is.na(tmp$dis13.3)==FALSE,] # elimina las secciones no asignadas a distrito (posiblemente por reseccionamiento, habrá que recuperarlas)
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
df2009d3 <- df2009d3[-which(df2009d3$dis13.3==0),]  # drops unassigned sections in 3rd plan (thisinfo should arrive later)
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
#
# state aggregates statistics
df2009s0 <- df2009d0
df2009s0 <- df2009s0[order(df2009s0$edon),]
## # WILL DROP THIS STEP THAT FORCES STATES WITH PARTIAL COALITION TO REPORT ALL VOTES IN PRI OR IN PRIC... MAYBE NEEDED TO COMPUTE PRI BIAS?
## colnames(df2009s0)[which(colnames(df2009s0)=="shSecCoalPri")] <- "shDisCoalPri"
## df2009s0$shDisCoalPri <- rep(0, 300)
## df2009s0$shDisCoalPri[df2009s0$pric>0] <- 1
## df2009s0$shDisCoalPri <- ave(df2009s0$shDisCoalPri, as.factor(df2009s0$edon), FUN=sum, na.rm=TRUE)
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
## # WILL DROP THIS STEP THAT FORCES STATES WITH PARTIAL COALITION TO REPORT ALL VOTES IN PRI OR IN PRIC... MAYBE NEEDED TO COMPUTE PRI BIAS?
## colnames(df2009s3)[which(colnames(df2009s3)=="shSecCoalPri")] <- "shDisCoalPri"
## df2009s3$shDisCoalPri <- rep(0, 300)
## df2009s3$shDisCoalPri[df2009s3$pric>0] <- 1
## df2009s3$shDisCoalPri <- ave(df2009s3$shDisCoalPri, as.factor(df2009s3$edon), FUN=sum, na.rm=TRUE)
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
# 2006 data
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
# así se hace en R un by yr mo: egen tmp=sum(invested) de stata
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
df2006d1 <- tmp[is.na(tmp$dis13.1)==FALSE,] # elimina las secciones no asignadas a distrito (posiblemente por reseccionamiento, habrá que recuperarlas)
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
df2006d3 <- tmp[is.na(tmp$dis13.3)==FALSE,] # elimina las secciones no asignadas a distrito (posiblemente por reseccionamiento, habrá que recuperarlas)
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
df2006d3 <- df2006d3[-which(df2006d3$dis13.3==0),]  # drops unassigned sections in 3rd plan (thisinfo should arrive later)
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
#
tmp <- rep(0, times=300)
df2006d1$panw <- df2006d1$pricw <- df2006d1$prdcw <- df2006d1$asdcw <- df2006d1$panalw <- tmp
tmp <- apply( df2006d1[,c("pan", "pric", "prdc", "panal", "asdc")], MARGIN = 1, max)
df2006d1$panw[df2006d1$pan==tmp] <- 1
df2006d1$pricw[df2006d1$pric==tmp] <- 1
df2006d1$prdcw[df2006d1$prdc==tmp] <- 1
df2006d1$panalw[df2006d1$panal==tmp] <- 1
df2006d1$asdcw[df2006d1$asdc==tmp] <- 1
#
tmp <- rep(0, times=300)
df2006d3$panw <- df2006d3$pricw <- df2006d3$prdcw <- df2006d3$asdcw <- df2006d3$panalw <- tmp
tmp <- apply( df2006d3[,c("pan", "pric", "prdc", "panal", "asdc")], MARGIN = 1, max)
df2006d3$panw[df2006d3$pan==tmp] <- 1
df2006d3$pricw[df2006d3$pric==tmp] <- 1
df2006d3$prdcw[df2006d3$prdc==tmp] <- 1
df2006d3$panalw[df2006d3$panal==tmp] <- 1
df2006d3$asdcw[df2006d3$asdc==tmp] <- 1
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
head(df2006s3)
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
########################################################################################
## Export prepared 2006-2009-2012 electoral data for use in other scripts (eg. red.r) ##
########################################################################################
elec060912 <-
    list(df2006d0=df2006d0,
         df2006d1=df2006d1,
         df2006d3=df2006d3,
         df2006s0=df2006s0,
         df2006s1=df2006s1,
         df2006s3=df2006s3,
         df2009d0=df2009d0,
         df2009d1=df2009d1,
         df2009d3=df2009d3,
         df2009s0=df2009s0,
         df2009s1=df2009s1,
         df2009s3=df2009s3,
         df2012d0=df2012d0,
         df2012d1=df2012d1,
         df2012d3=df2012d3,
         df2012s0=df2012s0,
         df2012s1=df2012s1,
         df2012s3=df2012s3)
save(elec060912, file = paste(dd, "elec060912.RData"))
rm(elec060912)

###############################
###############################
##   describe seat changes   ##
###############################
###############################
edo <- c("ags", "bc", "bcs", "cam", "coa", "col", "cps", "cua", "df", "dgo", "gua", "gue", "hgo", "jal", "mex", "mic", "mor", "nay", "nl", "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac")
estados <- c("aguascalientes", "baja california", "baja california sur", "campeche", "coahuila", "colima", "chiapas", "chihuahua", "distrito federal", "durango", "guanajuato", "guerrero", "hidalgo", "jalisco", "mexico", "michoacan", "morelos", "nayarit", "nuevo leon", "oaxaca", "puebla", "queretaro", "quintana roo", "san luis potosi", "sinaloa", "sonora", "tabasco", "tamaulipas", "tlaxcala", "veracruz", "yucatan", "zacatecas")
##
## 2012
tmp10 <- df2012s0$panw * df2012s0$ndis
tmp11 <- (df2012s1$panw * df2012s1$ndis) - (df2012s0$panw * df2012s0$ndis)
tmp13 <- (df2012s3$panw * df2012s3$ndis) - (df2012s0$panw * df2012s0$ndis)
#
tmp20 <- df2012s0$priw * df2012s0$ndis
tmp21 <- (df2012s1$priw * df2012s1$ndis) - (df2012s0$priw * df2012s0$ndis)
tmp23 <- (df2012s3$priw * df2012s3$ndis) - (df2012s0$priw * df2012s0$ndis)
#
tmp30 <- df2012s0$pricw * df2012s0$ndis
tmp31 <- (df2012s1$pricw * df2012s1$ndis) - (df2012s0$pricw * df2012s0$ndis)
tmp33 <- (df2012s3$pricw * df2012s3$ndis) - (df2012s0$pricw * df2012s0$ndis)
#
tmp40 <- df2012s0$prdcw * df2012s0$ndis
tmp41 <- (df2012s1$prdcw * df2012s1$ndis) - (df2012s0$prdcw * df2012s0$ndis)
tmp43 <- (df2012s3$prdcw * df2012s3$ndis) - (df2012s0$prdcw * df2012s0$ndis)
#
# other
tmp50 <- df2012s0$ndis - tmp10 - tmp20 - tmp30 - tmp40
tmp51 <- round( (1 - df2012s1$panw - df2012s1$priw - df2012s1$pricw - df2012s1$prdcw) * df2012s1$ndis - tmp50, digits = 0 )
tmp53 <- round( (1 - df2012s3$panw - df2012s3$priw - df2012s3$pricw - df2012s3$prdcw) * df2012s3$ndis - tmp50, digits = 0 )
#
tmp10 <- c(tmp10, sum(tmp10))
tmp11 <- c(tmp11, sum(tmp11))
tmp13 <- c(tmp13, sum(tmp13))
tmp20 <- c(tmp20, sum(tmp20))
tmp21 <- c(tmp21, sum(tmp21))
tmp23 <- c(tmp23, sum(tmp23))
tmp30 <- c(tmp30, sum(tmp30))
tmp31 <- c(tmp31, sum(tmp31))
tmp33 <- c(tmp33, sum(tmp33))
tmp40 <- c(tmp40, sum(tmp40))
tmp41 <- c(tmp41, sum(tmp41))
tmp43 <- c(tmp43, sum(tmp43))
tmp50 <- c(tmp50, sum(tmp50))
tmp51 <- c(tmp51, sum(tmp51))
tmp53 <- c(tmp53, sum(tmp53))
#
print("** 2012 seats won by state and difference with redistricting scenarios 1 and 3 **")
data.frame(edo=c(edo, "tot"),
#data.frame(edo=c(estados, "total"),
           pan0=tmp10, pan1=tmp11, pan3=tmp13,
           pri0=tmp20, pri1=tmp21, pri3=tmp23,
           pric0=tmp30, pric1=tmp31, pric3=tmp33,
           prdc0=tmp40, prdc1=tmp41, prdc3=tmp43,
           oth0=tmp50, oth1=tmp51, oth3=tmp53)
#
# sum of squares 2012
sqrt(sum(tmp13^2, tmp23^2, tmp33^2, tmp43^2, tmp53^2)/2)
##
## 2009
tmp10 <- df2009s0$panw * df2009s0$ndis
tmp11 <- (df2009s1$panw * df2009s1$ndis) - (df2009s0$panw * df2009s0$ndis)
tmp13 <- (df2009s3$panw * df2009s3$ndis) - (df2009s0$panw * df2009s0$ndis)
#
tmp20 <- df2009s0$priw * df2009s0$ndis
tmp21 <- (df2009s1$priw * df2009s1$ndis) - (df2009s0$priw * df2009s0$ndis)
tmp23 <- (df2009s3$priw * df2009s3$ndis) - (df2009s0$priw * df2009s0$ndis)
#
tmp30 <- df2009s0$pricw * df2009s0$ndis
tmp31 <- (df2009s1$pricw * df2009s1$ndis) - (df2009s0$pricw * df2009s0$ndis)
tmp33 <- (df2009s3$pricw * df2009s3$ndis) - (df2009s0$pricw * df2009s0$ndis)
#
tmp40 <- df2009s0$prdw * df2009s0$ndis
tmp41 <- (df2009s1$prdw * df2009s1$ndis) - (df2009s0$prdw * df2009s0$ndis)
tmp43 <- (df2009s3$prdw * df2009s3$ndis) - (df2009s0$prdw * df2009s0$ndis)
#
# other
tmp50 <- df2009s0$ndis - tmp10 - tmp20 - tmp30 - tmp40
tmp51 <- round( (1 - df2009s1$panw - df2009s1$priw - df2009s1$pricw - df2009s1$prdw) * df2009s1$ndis - tmp50, digits = 0 )
tmp53 <- round( (1 - df2009s3$panw - df2009s3$priw - df2009s3$pricw - df2009s3$prdw) * df2009s3$ndis - tmp50, digits = 0 )
#
tmp10 <- c(tmp10, sum(tmp10))
tmp11 <- c(tmp11, sum(tmp11))
tmp13 <- c(tmp13, sum(tmp13))
tmp20 <- c(tmp20, sum(tmp20))
tmp21 <- c(tmp21, sum(tmp21))
tmp23 <- c(tmp23, sum(tmp23))
tmp30 <- c(tmp30, sum(tmp30))
tmp31 <- c(tmp31, sum(tmp31))
tmp33 <- c(tmp33, sum(tmp33))
tmp40 <- c(tmp40, sum(tmp40))
tmp41 <- c(tmp41, sum(tmp41))
tmp43 <- c(tmp43, sum(tmp43))
tmp50 <- c(tmp50, sum(tmp50))
tmp51 <- c(tmp51, sum(tmp51))
tmp53 <- c(tmp53, sum(tmp53))
#
print("** 2009 seats won by state and difference with redistricting scenarios 1 and 3 **")
data.frame(edo=c(edo, "tot"),
#data.frame(edo=c(estados, "total"),
           pan0=tmp10, pan1=tmp11, pan3=tmp13,
           pri0=tmp20, pri1=tmp21, pri3=tmp23,
           pric0=tmp30, pric1=tmp31, pric3=tmp33,
           prdc0=tmp40, prdc1=tmp41, prdc3=tmp43,
           oth0=tmp50, oth1=tmp51, oth3=tmp53)
##
## 2006
tmp10 <- df2006s0$panw * df2006s0$ndis
tmp11 <- (df2006s1$panw * df2006s1$ndis) - (df2006s0$panw * df2006s0$ndis)
tmp13 <- (df2006s3$panw * df2006s3$ndis) - (df2006s0$panw * df2006s0$ndis)
#
tmp30 <- df2006s0$pricw * df2006s0$ndis
tmp31 <- (df2006s1$pricw * df2006s1$ndis) - (df2006s0$pricw * df2006s0$ndis)
tmp33 <- (df2006s3$pricw * df2006s3$ndis) - (df2006s0$pricw * df2006s0$ndis)
#
tmp40 <- df2006s0$prdcw * df2006s0$ndis
tmp41 <- (df2006s1$prdcw * df2006s1$ndis) - (df2006s0$prdcw * df2006s0$ndis); tmp41 <- round(tmp41, digits=0)
tmp43 <- (df2006s3$prdcw * df2006s3$ndis) - (df2006s0$prdcw * df2006s0$ndis)
#
# other
tmp50 <- df2006s0$ndis - tmp10 - tmp30 - tmp40
tmp51 <- round( (1 - df2006s1$panw - df2006s1$pricw - df2006s1$prdcw) * df2006s1$ndis - tmp50, digits = 0 )
tmp53 <- round( (1 - df2006s3$panw - df2006s3$pricw - df2006s3$prdcw) * df2006s3$ndis - tmp50, digits = 0 )
#
tmp10 <- c(tmp10, sum(tmp10))
tmp11 <- c(tmp11, sum(tmp11))
tmp13 <- c(tmp13, sum(tmp13))
tmp20 <- c(tmp20, sum(tmp20))
tmp21 <- c(tmp21, sum(tmp21))
tmp23 <- c(tmp23, sum(tmp23))
tmp30 <- c(tmp30, sum(tmp30))
tmp31 <- c(tmp31, sum(tmp31))
tmp33 <- c(tmp33, sum(tmp33))
tmp40 <- c(tmp40, sum(tmp40))
tmp41 <- c(tmp41, sum(tmp41))
tmp43 <- c(tmp43, sum(tmp43))
tmp50 <- c(tmp50, sum(tmp50))
tmp51 <- c(tmp51, sum(tmp51))
tmp53 <- c(tmp53, sum(tmp53))
#
print("** 2006 seats won by state and difference with redistricting scenarios 1 and 3 **")
data.frame(edo=c(edo, "tot"),
#data.frame(edo=c(estados, "total"),
           pan0=tmp10, pan1=tmp11, pan3=tmp13,
           pric0=tmp30, pric1=tmp31, pric3=tmp33,
           prdc0=tmp40, prdc1=tmp41, prdc3=tmp43,
           oth0=tmp50, oth1=tmp51, oth3=tmp53)


rm(tmp10, tmp11, tmp13, tmp20, tmp21, tmp23, tmp30, tmp31, tmp33, tmp40, tmp41, tmp43, tmp50, tmp51, tmp53)


## change s0/s1/s3 and years from here on to graph/estimate different data subsets
## 3-year symmetric vote share matrix
obj1 <- df2012s0
obj2 <- df2009s0
obj3 <- df2006s0
v <- data.frame(
    pan =   obj1$pansh,
    pri =   obj1$prish,
    pric =  obj1$pricsh, 
    prd =   rep(0,32),
    prdc =  obj1$prdcsh, 
    pvem =  obj1$pvemsh, 
    panal = obj1$panalsh,
    ptc =   rep(0,32),
    asdc =  rep(0,32))
v <- rbind(v, data.frame(
    pan =   obj2$pansh,
    pri =   obj2$prish,
    pric =  obj2$pricsh, 
    prd =   obj2$prdsh,  
    prdc =  rep(0,32),
    pvem =  obj2$pvemsh, 
    panal = obj2$panalsh,
    ptc =   obj2$ptcsh,  
    asdc =  rep(0,32)))
v <- rbind(v, data.frame(
    pan =   obj3$pansh,
    pri =   rep(0,32),
    pric =  obj3$pricsh,
    prd =   rep(0,32),
    prdc =  obj3$prdcsh,
    pvem =  rep(0,32),
    panal = obj3$panalsh,
    ptc =   rep(0,32),
    asdc =  obj3$asdcsh))
v <- round(v, digits  =  3)

## tmp is the object to graph (adds pri+pric and prd+prdc)
tmp <- data.frame(votes=c(obj1$pansh, obj1$prish+obj1$pricsh, obj1$prdcsh, obj1$pvemsh, obj1$panalsh),
                  seats=c(obj1$panw*obj1$ndis, (obj1$priw+obj1$pricw)*obj1$ndis, obj1$prdcw*obj1$ndis, obj1$pvemw*obj1$ndis, obj1$panalw*obj1$ndis),
                  ndis=rep(obj1$ndis, times=5), N=rep(obj1$N, times=5), P=rep(5, times=(5*32)),
                  pty=c(rep(1, 32),     rep(2, 32),      rep(3, 32),      rep(4, 32),      rep(5, 32)),
                  lab=c(rep("pan", 32), rep("pric", 32), rep("prdc", 32), rep("pvem", 32), rep("panal", 32)),
                  yr=rep(2012,times=(32*5)))
color <- c(rep("blue", 32), rep("red", 32), rep("gold", 32), rep("green", 32), rep("cyan", 32))
tmp <- rbind(tmp, data.frame(votes=c(obj2$pansh, obj2$prish+obj2$pricsh, obj2$prdsh, obj2$pvemsh, obj2$panalsh, obj2$ptcsh),
                  seats=c(obj2$panw*obj2$ndis, (obj2$priw+obj2$pricw)*obj2$ndis, obj2$prdw*obj2$ndis, obj2$pvemw*obj2$ndis, obj2$panalw*obj2$ndis, obj2$ptcw*obj2$ndis),
                  ndis=rep(obj2$ndis, times=6), N=rep(obj2$N, times=6), P=rep(6, times=(6*32)),
                  pty=c(rep(1, 32),     rep(2, 32),      rep(3, 32),     rep(4, 32),      rep(5, 32),       rep(6, 32)),
                  lab=c(rep("pan", 32), rep("pric", 32), rep("prd", 32), rep("pvem", 32), rep("panal", 32), rep("ptc", 32)),
                  yr=rep(2009,times=(32*6))))
color <- c(color, c(rep("blue", 32), rep("red", 32), rep("gold", 32), rep("green", 32), rep("cyan", 32), rep("orange", 32)))
tmp <- rbind(tmp, data.frame(votes=c(obj3$pansh, obj3$pricsh, obj3$prdcsh, obj3$panalsh, obj3$asdcsh),
                  seats=c(obj3$panw*obj3$ndis, obj3$pricw*obj3$ndis, obj3$prdcw*obj3$ndis, obj3$panalw*obj3$ndis, obj3$asdcw*obj3$ndis),
                  ndis=rep(obj3$ndis, times=5), N=rep(obj3$N, times=5), P=rep(5, times=(5*32)),
                  pty=c(rep(1, 32),     rep(2, 32),      rep(3, 32),      rep(5, 32),       rep(7, 32)),
                  lab=c(rep("pan", 32), rep("pric", 32), rep("prdc", 32), rep("panal", 32), rep("asdc", 32)),
                  yr=rep(2006,times=(32*5))))
color <- c(color, c(rep("blue", 32), rep("red", 32), rep("gold", 32), rep("cyan", 32), rep("violet", 32)))
## paste v to compute denominator of King's bias/responsiveness algorythm
tmp2 <- rbind( v[1:32,], v[1:32,], v[1:32,], v[1:32,], v[1:32,],
               v[33:64,], v[33:64,], v[33:64,], v[33:64,], v[33:64,], v[33:64,],
               v[65:96,], v[65:96,], v[65:96,], v[65:96,], v[65:96,])
tmp <- cbind(tmp, tmp2)
colnames(tmp)
## dummies to know which v-columns are relevant (needs revision given I aggregated pri+pric and prd+prdc)
tmp2[is.na(tmp2)==TRUE] <- 0; tmp3 <- tmp2; tmp3[,] <- 0; tmp3[tmp2>0] <- 1; colnames(tmp3) <- paste("d",1:7,sep="")
tmp <- cbind(tmp, tmp3); rm(tmp2,tmp3)
tmp$d1[tmp$pty==1] <- 0 ## dummy should put pan at zero when that party is in the numerator. COMMENT 2-5-14: this idea seems wrong, where do I apply it in code?
tmp$d2[tmp$pty==2] <- 0
tmp$d3[tmp$pty==3] <- 0
tmp$d4[tmp$pty==4] <- 0
tmp$d5[tmp$pty==5] <- 0
tmp$d6[tmp$pty==6] <- 0
tmp$d7[tmp$pty==7] <- 0
#
# drop cases with no votes
color <- color[tmp$votes>0]
tmp <- tmp[tmp$votes>0,] 
#
## # Data preparations
## S <- tmp$seats; v <- tmp$votes; N <- tmp$N; rest <- tmp[,c("pan", "pri", "pric", "prd", "prdc", "pvem", "panal", "ptc", "asdc")]; d <- tmp[,paste("d", 1:9, sep="")]; pty <- tmp$pty; D <- tmp$ndis; I <- length(S); 
## # estimate party bias and system responsiveness as King does (would need matrx-structured data S and v, with appropriate indices)
## Define E[i] subset of parties that ran in that election (with setdiff and [v>0])
## s[i] ~ dmultinom( pi[i], 
## for (i in 1:I){
##     numerator[i] <- exp( c * log(N[i]-1) + lambda[pty[i]] + rho * log(v[i]) )
##     denom1[i] <- d1[i] * exp( c * log(N[i]-1) + lambda[1] + rho * log(rest[i,1]) )
##     denom2[i] <- d2[i] * exp( c * log(N[i]-1) + lambda[2] + rho * log(rest[i,2]) )
##     denom3[i] <- d3[i] * exp( c * log(N[i]-1) + lambda[3] + rho * log(rest[i,3]) )
##     denom4[i] <- d4[i] * exp( c * log(N[i]-1) + lambda[4] + rho * log(rest[i,4]) )
##     denom5[i] <- d5[i] * exp( c * log(N[i]-1) + lambda[5] + rho * log(rest[i,5]) )
##     denom6[i] <- d6[i] * exp( c * log(N[i]-1) + lambda[6] + rho * log(rest[i,6]) )
##     denom7[i] <- d7[i] * exp( c * log(N[i]-1) + lambda[7] + rho * log(rest[i,7]) )
##     denom8[i] <- d8[i] * exp( c * log(N[i]-1) + lambda[8] + rho * log(rest[i,8]) )
##     denom9[i] <- d9[i] * exp( c * log(N[i]-1) + lambda[9] + rho * log(rest[i,9]) )
##     denominator[i] <- denom1[i] + denom2[i] + denom3[i] + denom4[i] + denom5[i] + denom6[i] + denom7[i] + denom8[i] + denom9[i]
##     pi[pty[i]] <- numerator[i] / denominator[i]
## ESTO SE DEBE DE PODER ESTIMAR...
##
## ## modelo calvo-micozzi
## model
## {
## #First Level Likelihood Model
## for (i in 1:N) {
## s[i] ~ dbin(g[i], K[i])
## logit(g[i]) <- c[prov[i]] * log(encp[i]-1) + b[regimenum[i]] * incumb[i] + rho[regimenum[i]]*lnvdp[i]
## }
## #Second-Level estimation of _ and b for each of the 204 elections
## for (k in 1:K) {
## rho[k]~dexp(mu.rho[k])
## log(mu.rho[k]) <- mu.rho.b
## b[k]~dnorm(mu.b[k],tau.b)
## mu.b[k] <- r.b*refelec[k]
## }
## # Second-Level Model for the Competitiveness Parameter c for each of the 24 provinces
## for (j in 1:24) {
## c[j] ~ dnorm(0,tau.c)
## }
## #Definitions
## r.b ~ dnorm(0,.001)
## mu.rho.b ~ dnorm(0,.001)
## tau.b ~ dgamma(.0001,.0001)
## tau.c ~ dgamma(.0001,.0001)
## }


# READ DISTRICT-LEVEL DATA PREPARED WITH red.r THAT CONTAINS POPULATION STATISTICS; MERGE POP STATS TO VOTE OBJECTS
# SOMEWHAT REDUNDANT, BUT POPULATION DATA CAN BE MERGED TO OBJECTS CREATED HERE (AND AVOIDS HAVING TO VERIFY THAT COALITION VOTES HANDLED AS ABOVE)
load(paste(dd, "votPobDis0018.RData", sep = ""))
#
tmpFrom <- votPobDis0018$df2006d0; tmpTo <- df2006d0                                        # select one year and map
tmpFrom <- tmpFrom[order(tmpFrom$edon, tmpFrom$disn),]                                      # sort
table(paste(tmpFrom$edon, tmpFrom$disn, sep=".")==paste(tmpTo$edon, tmpTo$disn, sep = ".")) # verify districts match in both objects (300 TRUEs)
tmpFrom <- tmpFrom[, c("ptot","rels","reln","rris","rrin")]                                 # keep population data only
tmpTo <- cbind(tmpTo, tmpFrom)                                                              # merge
df2006d0 <- tmpTo
#
tmpFrom <- votPobDis0018$df2006d1; tmpTo <- df2006d1                                        # select one year and map
tmpFrom <- tmpFrom[order(tmpFrom$edon, tmpFrom$disn),]                                      # sort
table(paste(tmpFrom$edon, tmpFrom$disn, sep=".")==paste(tmpTo$edon, tmpTo$disn, sep = ".")) # verify districts match in both objects (300 TRUEs)
tmpFrom <- tmpFrom[, c("ptot","rels","reln","rris","rrin")]                                 # keep population data only
tmpTo <- cbind(tmpTo, tmpFrom)                                                              # merge
df2006d1 <- tmpTo
#
tmpFrom <- votPobDis0018$df2006d3; tmpTo <- df2006d3                                        # select one year and map
tmpFrom <- tmpFrom[order(tmpFrom$edon, tmpFrom$disn),]                                      # sort
table(paste(tmpFrom$edon, tmpFrom$disn, sep=".")==paste(tmpTo$edon, tmpTo$disn, sep = ".")) # verify districts match in both objects (300 TRUEs)
tmpFrom <- tmpFrom[, c("ptot","rels","reln","rris","rrin")]                                 # keep population data only
tmpTo <- cbind(tmpTo, tmpFrom)                                                              # merge
df2006d3 <- tmpTo
#
tmpFrom <- votPobDis0018$df2009d0; tmpTo <- df2009d0                                        # select one year and map
tmpFrom <- tmpFrom[order(tmpFrom$edon, tmpFrom$disn),]                                      # sort
table(paste(tmpFrom$edon, tmpFrom$disn, sep=".")==paste(tmpTo$edon, tmpTo$disn, sep = ".")) # verify districts match in both objects (300 TRUEs)
tmpFrom <- tmpFrom[, c("ptot","rels","reln","rris","rrin")]                                 # keep population data only
tmpTo <- cbind(tmpTo, tmpFrom)                                                              # merge
df2009d0 <- tmpTo
#
tmpFrom <- votPobDis0018$df2009d1; tmpTo <- df2009d1                                        # select one year and map
tmpFrom <- tmpFrom[order(tmpFrom$edon, tmpFrom$disn),]                                      # sort
table(paste(tmpFrom$edon, tmpFrom$disn, sep=".")==paste(tmpTo$edon, tmpTo$disn, sep = ".")) # verify districts match in both objects (300 TRUEs)
tmpFrom <- tmpFrom[, c("ptot","rels","reln","rris","rrin")]                                 # keep population data only
tmpTo <- cbind(tmpTo, tmpFrom)                                                              # merge
df2009d1 <- tmpTo
#
tmpFrom <- votPobDis0018$df2009d3; tmpTo <- df2009d3                                        # select one year and map
tmpFrom <- tmpFrom[order(tmpFrom$edon, tmpFrom$disn),]                                      # sort
table(paste(tmpFrom$edon, tmpFrom$disn, sep=".")==paste(tmpTo$edon, tmpTo$disn, sep = ".")) # verify districts match in both objects (300 TRUEs)
tmpFrom <- tmpFrom[, c("ptot","rels","reln","rris","rrin")]                                 # keep population data only
tmpTo <- cbind(tmpTo, tmpFrom)                                                              # merge
df2009d3 <- tmpTo
#
tmpFrom <- votPobDis0018$df2012d0; tmpTo <- df2012d0                                        # select one year and map
tmpFrom <- tmpFrom[order(tmpFrom$edon, tmpFrom$disn),]                                      # sort
table(paste(tmpFrom$edon, tmpFrom$disn, sep=".")==paste(tmpTo$edon, tmpTo$disn, sep = ".")) # verify districts match in both objects (300 TRUEs)
tmpFrom <- tmpFrom[, c("ptot","rels","reln","rris","rrin")]                                 # keep population data only
tmpTo <- cbind(tmpTo, tmpFrom)                                                              # merge
df2012d0 <- tmpTo
#
tmpFrom <- votPobDis0018$df2012d1; tmpTo <- df2012d1                                        # select one year and map
tmpFrom <- tmpFrom[order(tmpFrom$edon, tmpFrom$disn),]                                      # sort
table(paste(tmpFrom$edon, tmpFrom$disn, sep=".")==paste(tmpTo$edon, tmpTo$disn, sep = ".")) # verify districts match in both objects (300 TRUEs)
tmpFrom <- tmpFrom[, c("ptot","rels","reln","rris","rrin")]                                 # keep population data only
tmpTo <- cbind(tmpTo, tmpFrom)                                                              # merge
df2012d1 <- tmpTo
#
tmpFrom <- votPobDis0018$df2012d3; tmpTo <- df2012d3                                        # select one year and map
tmpFrom <- tmpFrom[order(tmpFrom$edon, tmpFrom$disn),]                                      # sort
table(paste(tmpFrom$edon, tmpFrom$disn, sep=".")==paste(tmpTo$edon, tmpTo$disn, sep = ".")) # verify districts match in both objects (300 TRUEs)
tmpFrom <- tmpFrom[, c("ptot","rels","reln","rris","rrin")]                                 # keep population data only
tmpTo <- cbind(tmpTo, tmpFrom)                                                              # merge
df2012d3 <- tmpTo
#
rm(tmpFrom, tmpTo, votPobDis0018)

# add state population aggregates to state vote objects 
tmpFrom <- df2006d0; tmpTo <- df2006s0
tmpFrom$ptot <- ave(tmpFrom$ptot, as.factor(tmpFrom$edon), FUN=sum, na.rm=TRUE)
tmpFrom <- tmpFrom[duplicated(tmpFrom$edon)==FALSE,]
table(tmpFrom$edon==tmpTo$edon) # verify districts match in both objects (32 TRUEs)
tmpTo$ptot <- tmpFrom$ptot
df2006s0 <- tmpTo
#
tmpFrom <- df2006d1; tmpTo <- df2006s1
tmpFrom$ptot <- ave(tmpFrom$ptot, as.factor(tmpFrom$edon), FUN=sum, na.rm=TRUE)
tmpFrom <- tmpFrom[duplicated(tmpFrom$edon)==FALSE,]
table(tmpFrom$edon==tmpTo$edon) # verify districts match in both objects (32 TRUEs)
tmpTo$ptot <- tmpFrom$ptot
df2006s1 <- tmpTo
#
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
tmpFrom <- df2009d1; tmpTo <- df2009s1
tmpFrom$ptot <- ave(tmpFrom$ptot, as.factor(tmpFrom$edon), FUN=sum, na.rm=TRUE)
tmpFrom <- tmpFrom[duplicated(tmpFrom$edon)==FALSE,]
table(tmpFrom$edon==tmpTo$edon) # verify districts match in both objects (32 TRUEs)
tmpTo$ptot <- tmpFrom$ptot
df2009s1 <- tmpTo
#
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
tmpFrom <- df2012d1; tmpTo <- df2012s1
tmpFrom$ptot <- ave(tmpFrom$ptot, as.factor(tmpFrom$edon), FUN=sum, na.rm=TRUE)
tmpFrom <- tmpFrom[duplicated(tmpFrom$edon)==FALSE,]
table(tmpFrom$edon==tmpTo$edon) # verify districts match in both objects (32 TRUEs)
tmpTo$ptot <- tmpFrom$ptot
df2012s1 <- tmpTo
#
tmpFrom <- df2012d3; tmpTo <- df2012s3
tmpFrom$ptot <- ave(tmpFrom$ptot, as.factor(tmpFrom$edon), FUN=sum, na.rm=TRUE)
tmpFrom <- tmpFrom[duplicated(tmpFrom$edon)==FALSE,]
table(tmpFrom$edon==tmpTo$edon) # verify districts match in both objects (32 TRUEs)
tmpTo$ptot <- tmpFrom$ptot
df2012s3 <- tmpTo
#
rm(tmp, tmpFrom, tmpTo, select)

### Packages for JAGS
library(R2jags)
### JAGS/BUGS models
lambda.rho.1 <- function() {
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
lambda.rho.2 <- function() {
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
lambda.rho.3 <- function() {
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
lambda.rho.4 <- function() {
    ### King likelihood using PRI as reference party
    ### Note 30-4-2014: this code was prepared to estimate 2006-09-12 jointly, hence 7 parties. But I also use it to
    ### estimate each election alone, with fewer parties. Data below is structured to include zero seats and zero votes
    ### for parties absent, leading to estimate lambdas for those absent parties---absurd, though maybe inconsequential
    ### in Bayesian estimation. To verify, should make code more flexible to handle a variable number of parties...
    ### NOTE 2-5-14: dummy[i,1:7] already took care of the problem above. 
    for (i in 1:I){     # loop over state-years
        for (j in 1:J){ # loop over parties (dummy selects those who ran that year) 
            S[i,j] ~ dbin(pi[i,j], D[i])  # D is the number of SMD seats in observation i's state ... WHY DON'T I USE S[i,1:J] ~ dmulti(pi[i,1:J], D[i])
        }
        numerator[i,1] <- dummy[i,1] * exp( lambda[1] + rho * log(v[i,1]) )
        numerator[i,2] <- dummy[i,2] * exp(             rho * log(v[i,2]) )
        for (j in 3:J){
#            numerator[i,j] <- dummy[i,j] * exp( lambda[j-1] + rho * log(v[i,j]) )
            numerator[i,j] <- dummy[i,j] * exp( lambda[j-1] ) * v[i,j]^rho
        }
        for (j in 1:J){ # loop over parties (dummy selects those who ran that year) 
            d1[i,j] <- dummy[i,1] * exp( lambda[1] ) * v[i,1]^rho 
            d2[i,j] <- dummy[i,2]                    * v[i,2]^rho 
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
    for (p in 1:6){ # there are 7 party labels in the 3-election data, PRI is reference
        lambda[p] ~ dnorm( 0, tau.lambda )
    }
    tau.lambda <- pow(.25, -2)
    rho ~ dexp(.75) # this has positive range, median close to 1, mean 1.25, max 4.5
}
lambda.rho.5 <- function() {
    ### King likelihood using PRI as reference party and separating Grofman et al's (1997) 3-sources of party bias (haven't tried it)
    for (i in 1:I){     # loop over state-years
        for (j in 1:J){ # loop over parties (dummy selects those who ran that year) 
            S[i,j] ~ dbin(pi[i,j], D[i])  # D is the number of SMD seats in observation i's state ... WHY DON'T I USE S[i,1:J] ~ dmulti(pi[i,1:J], D[i])
        }
        numeratorR[i,1] <- dummy[i,1] * exp( lambdaR[1] + rho * log(R[i,1]) )
        numeratorR[i,2] <- dummy[i,2] * exp(              rho * log(R[i,2]) )
        numeratorP[i,1] <- dummy[i,1] * exp( lambdaP[1] + rho * log(P[i,1]) )
        numeratorP[i,2] <- dummy[i,2] * exp(              rho * log(P[i,2]) )
        numeratorM[i,1] <- dummy[i,1] * exp( lambdaM[1] + rho * log(M[i,1]) )
        numeratorM[i,2] <- dummy[i,2] * exp(              rho * log(M[i,2]) )
        for (j in 3:J){
#            numerator[i,j] <- dummy[i,j] * exp( lambda[j-1] + rho * log(v[i,j]) )
            numeratorR[i,j] <- dummy[i,j] * exp( lambdaR[j-1] ) * R[i,j]^rho
            numeratorP[i,j] <- dummy[i,j] * exp( lambdaP[j-1] ) * P[i,j]^rho
            numeratorM[i,j] <- dummy[i,j] * exp( lambdaM[j-1] ) * M[i,j]^rho
        }
        for (j in 1:J){ # loop over parties (dummy selects those who ran that year) 
            d1R[i,j] <- dummy[i,1] * exp( lambdaR[1] ) * R[i,1]^rho 
            d2R[i,j] <- dummy[i,2]                     * R[i,2]^rho 
            d3R[i,j] <- dummy[i,3] * exp( lambdaR[2] ) * R[i,3]^rho 
            d4R[i,j] <- dummy[i,4] * exp( lambdaR[3] ) * R[i,4]^rho 
            d5R[i,j] <- dummy[i,5] * exp( lambdaR[4] ) * R[i,5]^rho 
            d6R[i,j] <- dummy[i,6] * exp( lambdaR[5] ) * R[i,6]^rho 
            d7R[i,j] <- dummy[i,7] * exp( lambdaR[6] ) * R[i,7]^rho 
            d1P[i,j] <- dummy[i,1] * exp( lambdaP[1] ) * P[i,1]^rho 
            d2P[i,j] <- dummy[i,2]                     * P[i,2]^rho 
            d3P[i,j] <- dummy[i,3] * exp( lambdaP[2] ) * P[i,3]^rho 
            d4P[i,j] <- dummy[i,4] * exp( lambdaP[3] ) * P[i,4]^rho 
            d5P[i,j] <- dummy[i,5] * exp( lambdaP[4] ) * P[i,5]^rho 
            d6P[i,j] <- dummy[i,6] * exp( lambdaP[5] ) * P[i,6]^rho 
            d7P[i,j] <- dummy[i,7] * exp( lambdaP[6] ) * P[i,7]^rho 
            d1M[i,j] <- dummy[i,1] * exp( lambdaM[1] ) * M[i,1]^rho 
            d2M[i,j] <- dummy[i,2]                     * M[i,2]^rho 
            d3M[i,j] <- dummy[i,3] * exp( lambdaM[2] ) * M[i,3]^rho 
            d4M[i,j] <- dummy[i,4] * exp( lambdaM[3] ) * M[i,4]^rho 
            d5M[i,j] <- dummy[i,5] * exp( lambdaM[4] ) * M[i,5]^rho 
            d6M[i,j] <- dummy[i,6] * exp( lambdaM[5] ) * M[i,6]^rho 
            d7M[i,j] <- dummy[i,7] * exp( lambdaM[6] ) * M[i,7]^rho 
            denominatorR[i,j] <- d1R[i,j]+d2R[i,j]+d3R[i,j]+d4R[i,j]+d5R[i,j]+d6R[i,j]+d7R[i,j]
            denominatorP[i,j] <- d1P[i,j]+d2P[i,j]+d3P[i,j]+d4P[i,j]+d5P[i,j]+d6P[i,j]+d7P[i,j]
            denominatorM[i,j] <- d1M[i,j]+d2M[i,j]+d3M[i,j]+d4M[i,j]+d5M[i,j]+d6M[i,j]+d7M[i,j]
            pi[i,j] <- numeratorR[i,j] / denominatorR[i,j]
        }
    }
    ### priors
    for (p in 1:6){ # there are 7 party labels in the 3-election data, PRI is reference
        lambdaR[p] ~ dnorm( 0, tau.lambdaR )
        lambdaP[p] ~ dnorm( 0, tau.lambdaP )
        lambdaM[p] ~ dnorm( 0, tau.lambdaM )
    }
    tau.lambdaR <- pow(.25, -2)
    tau.lambdaP <- pow(.25, -2)
    tau.lambdaM <- pow(.25, -2)
    rho ~ dexp(.75) # this has positive range, median close to 1, mean 1.25, max 4.5
}


##############################################################################
### Data preparations for state-agg King (data matrix with 7 party columns ###
##############################################################################
obj1 <- df2012s3
obj2 <- df2009s3
obj3 <- df2006s3
S <-          data.frame(pan=obj1$panw, pri=obj1$priw+obj1$pricw, prd=obj1$prdcw, pvem=obj1$pvemw, panal=obj1$panalw, ptc=rep(0,32), asdc=rep(0,32))
S <- rbind(S, data.frame(pan=obj2$panw, pri=obj2$priw+obj2$pricw, prd=obj2$prdw,  pvem=obj2$pvemw, panal=obj2$panalw, ptc=obj2$ptcw, asdc=rep(0,32)))
S <- rbind(S, data.frame(pan=obj3$panw, pri=obj3$pricw,           prd=obj3$prdcw, pvem=rep(0,32),  panal=obj3$panalw, ptc=rep(0,32), asdc=obj3$asdcw))
#
# v=R is national vote shares --- Grofman et al 1997 show that its use conflates 3 sources of party bias, so below are options to use other vote shares to separate them
R <-          data.frame(pan=obj1$pansh, pri=obj1$prish+obj1$pricsh, prd=obj1$prdcsh, pvem=obj1$pvemsh, panal=obj1$panalsh, ptc=rep(0,32),  asdc=rep(0,32))
R <- rbind(R, data.frame(pan=obj2$pansh, pri=obj2$prish+obj2$pricsh, prd=obj2$prdsh,  pvem=obj2$pvemsh, panal=obj2$panalsh, ptc=obj2$ptcsh, asdc=rep(0,32)))
R <- rbind(R, data.frame(pan=obj3$pansh, pri=obj3$pricsh,            prd=obj3$prdcsh, pvem=rep(0,32),   panal=obj3$panalsh, ptc=rep(0,32),  asdc=obj3$asdcsh))
#
# v=P is mean district vote share (in state) --- for party bias due to distributive party strength
## change d0/d1/d3 to graph/estimate different data subsets
## 3-year symmetric vote share matrix
obj1b <- df2012d3
obj2b <- df2009d3
obj3b <- df2006d3
#
P <-             data.frame(pan=obj1b$pan, pri=obj1b$pri+obj1b$pric, prd=obj1b$prdc, pvem=obj1b$pvem, panal=obj1b$panal, ptc=rep(0,300), asdc=rep(0,300))/obj1b$efec
P$pan   <- ave(P$pan,   as.factor(obj1b$edon), FUN=mean, na.rm=TRUE)
P$pri   <- ave(P$pri,   as.factor(obj1b$edon), FUN=mean, na.rm=TRUE)
P$prd   <- ave(P$prd,   as.factor(obj1b$edon), FUN=mean, na.rm=TRUE)
P$pvem  <- ave(P$pvem,  as.factor(obj1b$edon), FUN=mean, na.rm=TRUE)
P$panal <- ave(P$panal, as.factor(obj1b$edon), FUN=mean, na.rm=TRUE)
P <- P[duplicated(obj1b$edon)==FALSE,]
#
tmp <-             data.frame(pan=obj2b$pan, pri=obj2b$pri+obj2b$pric, prd=obj2b$prd, pvem=obj2b$pvem, panal=obj2b$panal, ptc=obj2b$ptc, asdc=rep(0,300))/obj2b$efec
tmp$pan   <- ave(tmp$pan,   as.factor(obj2b$edon), FUN=mean, na.rm=TRUE)
tmp$pri   <- ave(tmp$pri,   as.factor(obj2b$edon), FUN=mean, na.rm=TRUE)
tmp$prd   <- ave(tmp$prd,   as.factor(obj2b$edon), FUN=mean, na.rm=TRUE)
tmp$pvem  <- ave(tmp$pvem,  as.factor(obj2b$edon), FUN=mean, na.rm=TRUE)
tmp$panal <- ave(tmp$panal, as.factor(obj2b$edon), FUN=mean, na.rm=TRUE)
tmp$ptc   <- ave(tmp$ptc,   as.factor(obj2b$edon), FUN=mean, na.rm=TRUE)
tmp <- tmp[duplicated(obj2b$edon)==FALSE,]
P <- rbind(P, tmp)
#
tmp <-             data.frame(pan=obj3b$pan, pri=obj3b$pric, prd=obj3b$prdc, pvem=rep(0,300), panal=obj3b$panal, ptc=rep(0,300), asdc=obj3b$asdc)/obj3b$efec
tmp$pan   <- ave(tmp$pan,   as.factor(obj3b$edon), FUN=mean, na.rm=TRUE)
tmp$pri   <- ave(tmp$pri,   as.factor(obj3b$edon), FUN=mean, na.rm=TRUE)
tmp$prd   <- ave(tmp$prd,   as.factor(obj3b$edon), FUN=mean, na.rm=TRUE)
tmp$panal <- ave(tmp$panal, as.factor(obj3b$edon), FUN=mean, na.rm=TRUE)
tmp$asdc  <- ave(tmp$asdc,  as.factor(obj3b$edon), FUN=mean, na.rm=TRUE)
tmp <- tmp[duplicated(obj3b$edon)==FALSE,]
P <- rbind(P, tmp)
#
# v=M is malapportionment-corrected (statewide) vote share --- for party bias due to malapportioned districts
## change d0/d1/d3 to graph/estimate different data subsets
## 3-year symmetric vote share matrix
M <-             data.frame(pan=obj1b$pan, pri=obj1b$pri+obj1b$pric, prd=obj1b$prdc, pvem=obj1b$pvem, panal=obj1b$panal, ptc=rep(0,300), asdc=rep(0,300))/obj1b$efec
totPop <- ave(obj1b$ptot,   as.factor(obj1b$edon), FUN=sum, na.rm=TRUE); totPop <- obj1b$ptot / totPop # district:state population ratios vector
M <- M*totPop # weight district vote shares by district:state population ratio
M$pan   <- ave(M$pan,   as.factor(obj1b$edon), FUN=sum, na.rm=TRUE)
M$pri   <- ave(M$pri,   as.factor(obj1b$edon), FUN=sum, na.rm=TRUE)
M$prd   <- ave(M$prd,   as.factor(obj1b$edon), FUN=sum, na.rm=TRUE)
M$pvem  <- ave(M$pvem,  as.factor(obj1b$edon), FUN=sum, na.rm=TRUE)
M$panal <- ave(M$panal, as.factor(obj1b$edon), FUN=sum, na.rm=TRUE)
M <- M[duplicated(obj1b$edon)==FALSE,]
#
tmp <-             data.frame(pan=obj2b$pan, pri=obj2b$pri+obj2b$pric, prd=obj2b$prd, pvem=obj2b$pvem, panal=obj2b$panal, ptc=obj2b$ptc, asdc=rep(0,300))/obj2b$efec
totPop <- ave(obj2b$ptot,   as.factor(obj2b$edon), FUN=sum, na.rm=TRUE); totPop <- obj2b$ptot / totPop # district:state population ratios vector
tmp <- tmp*totPop # weight district vote shares by district:state population ratio
tmp$pan   <- ave(tmp$pan,   as.factor(obj2b$edon), FUN=sum, na.rm=TRUE)
tmp$pri   <- ave(tmp$pri,   as.factor(obj2b$edon), FUN=sum, na.rm=TRUE)
tmp$prd   <- ave(tmp$prd,   as.factor(obj2b$edon), FUN=sum, na.rm=TRUE)
tmp$pvem  <- ave(tmp$pvem,  as.factor(obj2b$edon), FUN=sum, na.rm=TRUE)
tmp$panal <- ave(tmp$panal, as.factor(obj2b$edon), FUN=sum, na.rm=TRUE)
tmp$ptc   <- ave(tmp$ptc,   as.factor(obj2b$edon), FUN=sum, na.rm=TRUE)
tmp <- tmp[duplicated(obj2b$edon)==FALSE,]
M <- rbind(M, tmp)
#
tmp <-             data.frame(pan=obj3b$pan, pri=obj3b$pric, prd=obj3b$prdc, pvem=rep(0,300), panal=obj3b$panal, ptc=rep(0,300), asdc=obj3b$asdc)/obj3b$efec
totPop <- ave(obj3b$ptot,   as.factor(obj3b$edon), FUN=sum, na.rm=TRUE); totPop <- obj3b$ptot / totPop # district:state population ratios vector
tmp <- tmp*totPop # weight district vote shares by district:state population ratio
tmp$pan   <- ave(tmp$pan,   as.factor(obj3b$edon), FUN=sum, na.rm=TRUE)
tmp$pri   <- ave(tmp$pri,   as.factor(obj3b$edon), FUN=sum, na.rm=TRUE)
tmp$prd   <- ave(tmp$prd,   as.factor(obj3b$edon), FUN=sum, na.rm=TRUE)
tmp$panal <- ave(tmp$panal, as.factor(obj3b$edon), FUN=sum, na.rm=TRUE)
tmp$asdc  <- ave(tmp$asdc,  as.factor(obj3b$edon), FUN=sum, na.rm=TRUE)
tmp <- tmp[duplicated(obj3b$edon)==FALSE,]
M <- rbind(M, tmp)

##-----------------------------------------------------------------
## SELECT HERE WHAT MEASURE OF VOTE SHARE TO USE: CLASSIC USES v=R
##-----------------------------------------------------------------
v <- R   # classic: conflates three sources of party bias (subtract bias(R) - bias(M) to separate turnout-based party bias)
#v <- P   # measures party bias due to *distributional* factors (vote geography)
#v <- M # measures party bias due to *malapportionment* (subtract bias(M) - bias(P) to separate malapp.-based party bias)
#
dummy <- R; dummy[,] <- 0; dummy[R>0] <- 1 # indicates parties with v>0
#
D <- c(obj1$ndis, obj2$ndis, obj3$ndis); 
N <- c(obj1$N, obj2$N, obj3$N);
I <- length(D);
J <- 7;
#
# changes S from share to number of seats
for (j in 1:J){
    S[,j] <- S[,j]*D
}

## subset year for analysis
dim(S); dim(v); dim(dummy);
dim(tmp); length(color);
length(D); length(N); length(I); length(J);
#
# 2012
S <- S[1:32,]; v <- v[1:32,]; dummy <- dummy[1:32,]; 
D <- D[1:32]; N <- N[1:32]; I <- length(D);
color <- color[tmp$yr==2012]; tmp <- tmp[tmp$yr==2012,]
## # 2009
## S <- S[33:64,]; v <- v[33:64,]; dummy <- dummy[33:64,]; 
## D <- D[33:64]; N <- N[33:64]; I <- length(D);
## color <- color[tmp$yr==2009]; tmp <- tmp[tmp$yr==2009,]; 
## # 2006
## S <- S[65:96,]; v <- v[65:96,]; dummy <- dummy[65:96,]; 
## D <- D[65:96]; N <- N[65:96]; I <- length(D);
## color <- color[tmp$yr==2006]; tmp <- tmp[tmp$yr==2006,]; 

#
### Data, initial values, and parameter vector for bugs
l.r.data <- list("S", "v", "I", "J", "D", "dummy")
l.r.inits <- function(){ list (lambda=rnorm(J-1), rho=rexp(1)) }
l.r.parameters <- c("lambda", "rho")
##########################################################################
### End data prep for state-agg king (data matrix with 7 party columns ###
##########################################################################

#################################################################################
### Data preparations for national-agg King (data matrix with 7 party columns ###
#################################################################################
obj1 <- df2012d3
obj2 <- df2009d3
obj3 <- df2006d3
# national aggregates
S <-          data.frame(pan=obj1$panw, pri=obj1$priw+obj1$pricw, prd=obj1$prdcw, pvem=obj1$pvemw, panal=obj1$panalw, ptc=rep(0,300), asdc=rep(0,300))
S <- apply(S, 2, sum) / 300
tmp <-        data.frame(pan=obj2$panw, pri=obj2$priw+obj2$pricw, prd=obj2$prdw,  pvem=obj2$pvemw, panal=obj2$panalw, ptc=obj2$ptcw, asdc=rep(0,300))
tmp <- apply(tmp, 2, sum) / 300
S <- rbind(S, tmp)
tmp <-        data.frame(pan=obj3$panw, pri=obj3$pricw,           prd=obj3$prdcw, pvem=rep(0,300), panal=obj3$panalw, ptc=rep(0,300), asdc=obj3$asdcw)
tmp <- apply(tmp, 2, sum) / 300
S <- rbind(S, tmp)
## # re-does data.frame to check that aggregation went well
## S <- data.frame(S=as.vector(S), yr=rep(c(2012,2009,2006),7), pty=c(rep("pan",3),rep("pri",3),rep("prd",3),rep("pvem",3),rep("panal",3),rep("ptc",3),rep("asdc",3)))
## S <- S$S
#
# v=R is national vote shares --- Grofman et al 1997 show that its use conflates 3 sources of party bias, so below are options to use other vote shares to separate them
R <-          data.frame(pan=obj1$pan, pri=obj1$pri+obj1$pric, prd=obj1$prdc, pvem=obj1$pvem, panal=obj1$panal, ptc=rep(0,300),  asdc=rep(0,300), efec=obj1$efec)
R <- apply(R, 2, sum)
R <- R[-length(R)] / R[length(R)]
tmp <-        data.frame(pan=obj2$pan, pri=obj2$pri+obj2$pric, prd=obj2$prd,  pvem=obj2$pvem,  panal=obj2$panal, ptc=obj2$ptc, asdc=rep(0,300), efec=obj2$efec)
tmp <- apply(tmp, 2, sum)
tmp <- tmp[-length(tmp)] / tmp[length(tmp)]
R <- rbind(R, tmp)
tmp <-        data.frame(pan=obj3$pan, pri=obj3$pric,          prd=obj3$prdc, pvem=rep(0,300), panal=obj3$panal, ptc=rep(0,300), asdc=obj3$asdc, efec=obj3$efec)
tmp <- apply(tmp, 2, sum)
tmp <- tmp[-length(tmp)] / tmp[length(tmp)]
R <- rbind(R, tmp)
## # re-does data.frame to check that aggregation went well
## R <- data.frame(R=as.vector(R), yr=rep(c(2012,2009,2006),7), pty=c(rep("pan",3),rep("pri",3),rep("prd",3),rep("pvem",3),rep("panal",3),rep("ptc",3),rep("asdc",3)))
## R <- R$R
#
# v=M is malapportionment-corrected (statewide) vote share --- for party bias due to malapportioned districts
M <-   data.frame(pan=obj1$pan, pri=obj1$pri+obj1$pric, prd=obj1$prdc, pvem=obj1$pvem, panal=obj1$panal, ptc=rep(0,300), asdc=rep(0,300))/obj1$efec
totPop <- obj1$ptot / sum(obj1$ptot) # district:national population ratios vector
M <- M*totPop # weight district vote shares by district:state population ratio
M <- apply(M, 2, sum)
tmp <- data.frame(pan=obj2$pan, pri=obj2$pri+obj2$pric, prd=obj2$prd, pvem=obj2$pvem, panal=obj2$panal, ptc=obj2$ptc, asdc=rep(0,300))/obj2$efec
totPop <- obj2$ptot / sum(obj2$ptot) # district:national population ratios vector
tmp <- tmp*totPop # weight district vote shares by district:state population ratio
tmp <- apply(tmp, 2, sum)
M <- rbind(M, tmp)
tmp <- data.frame(pan=obj3$pan, pri=obj3$pric, prd=obj3$prdc, pvem=rep(0,300), panal=obj3$panal, ptc=rep(0,300), asdc=obj3$asdc)/obj3$efec
totPop <- obj3$ptot / sum(obj3$ptot) # district:national population ratios vector
tmp <- tmp*totPop # weight district vote shares by district:state population ratio
tmp <- apply(tmp, 2, sum)
M <- rbind(M, tmp)
## # re-does data.frame to check that aggregation went well
## M <- data.frame(M=as.vector(M), yr=rep(c(2012,2009,2006),7), pty=c(rep("pan",3),rep("pri",3),rep("prd",3),rep("pvem",3),rep("panal",3),rep("ptc",3),rep("asdc",3)))
## M <- M$M
#
# v=P is mean district vote share (in state) --- for party bias due to distributive party strength
P <-   data.frame(pan=obj1$pan, pri=obj1$pri+obj1$pric, prd=obj1$prdc, pvem=obj1$pvem, panal=obj1$panal, ptc=rep(0,300), asdc=rep(0,300))/obj1$efec
P <- apply(P, 2, mean)
tmp <- data.frame(pan=obj2$pan, pri=obj2$pri+obj2$pric, prd=obj2$prd, pvem=obj2$pvem, panal=obj2$panal, ptc=obj2$ptc, asdc=rep(0,300))/obj2$efec
tmp <- apply(tmp, 2, mean)
P <- rbind(P, tmp)
tmp <- data.frame(pan=obj3$pan, pri=obj3$pric, prd=obj3$prdc, pvem=rep(0,300), panal=obj3$panal, ptc=rep(0,300), asdc=obj3$asdc)/obj3$efec
tmp <- apply(tmp, 2, mean)
P <- rbind(P, tmp)

##-----------------------------------------------------------------
## SELECT HERE WHAT MEASURE OF VOTE SHARE TO USE: CLASSIC USES v=R
##-----------------------------------------------------------------
v <- R   # classic: conflates three sources of party bias (subtract bias(R) - bias(M) to separate turnout-based party bias)
#v <- P   # measures party bias due to *distributional* factors (vote geography)
#v <- M # measures party bias due to *malapportionment* (subtract bias(M) - bias(P) to separate malapp.-based party bias)
#
dummy <- R; dummy[] <- 0; dummy[R>0] <- 1 # indicates parties with v>0
#
I <- nrow(R);
D <- rep(300,I)
J <- 7;
#
# changes S from share to number of seats
for (j in 1:J){
    S[,j] <- S[,j]*D
}
### Data, initial values, and parameter vector for bugs
l.r.data <- list("S", "v", "I", "J", "D", "dummy")
l.r.inits <- function(){ list (lambda=rnorm(J-1), rho=rexp(1)) }
l.r.parameters <- c("lambda", "rho")
#############################################################################
### End data prep for national-agg king (data matrix with 7 party columns ###
#############################################################################


## ###########################################################################
## ### Data preparations for my version of Calvo-Micozzi (data in vectors) ###
## ###########################################################################
## S <- tmp$seats; v <- tmp$votes; N <- tmp$N; d <- tmp[,paste("d", 1:7, sep="")]; pty <- tmp$pty; D <- tmp$ndis; I <- length(S); 
## ### Data, initial values, and parameter vector for bugs
## #l.r.data <- list("S", "v", "I", "pty", "N", "D")
## #l.r.inits <- function(){ list (lambda=rnorm(K), c=rnorm(1), rho=rexp(1)) }
## #l.r.parameters <- c("lambda", "c", "rho")
## l.r.data <- list("S", "v", "I", "pty", "D")
## l.r.inits <- function(){ list (lambda=rnorm(7), rho=rexp(1)) }
## l.r.parameters <- c("lambda", "rho")
## #######################################################################
## ### End data prep for my version of Calvo-Micozzi (data in vectors) ###
## #######################################################################

## test ride
tmpRes <- jags (data=l.r.data, inits=l.r.inits, l.r.parameters,
                 model.file=lambda.rho.4, n.chains=3,
                 n.iter=100, n.thin=10
                 )
## estimate
tmpRes <- jags (data=l.r.data, inits=l.r.inits, l.r.parameters,
                 model.file=lambda.rho.4, n.chains=3,
                 n.iter=100000, n.thin=50,
                 )
res0612n3R <- tmpRes; rm(tmpRes)

# create a list with all results in
biasResp0612oldNewDistrictsRPM <- lapply(ls(pattern = "res[0-9]"), get);
names(biasResp0612oldNewDistrictsRPM) <- ls(pattern = "res[0-9]")
summary(biasResp0612oldNewDistrictsRPM)
rm(list=ls(pattern = "res[0-9]"))
# repeat the above bloc changing data to get all the objects listed 
save(
     biasResp0612oldNewDistrictsRPM,
     file=paste(dd, "biasResp2006-2012oldNewDistrictsRPM.RData", sep =""))

## run all above except jags estimations to proceed with saved data
load(file="biasResp2006-2012oldNewDistricts.RData")
load(file=paste(dd, "biasResp2006-2012oldNewDistrictsRPM.RData", sep =""))

# summarize central tendency of party bias à la Grofman
tmp <- biasResp0612oldNewDistrictsRPM
tmp1 <- data.frame(panpri=rep(NA,4), prdpri=rep(NA,4), panprd=rep(NA,4)); rownames(tmp1) <- c("raw","turn","malap","dist")
tmp0612n3 <- tmp0612n0 <- tmp12s3 <- tmp12s0 <- tmp09s3 <- tmp09s0 <- tmp06s3 <- tmp06s0 <- tmp1
#
tmp06s0[1,1:2] <- round(tmp$res06s0R$BUGSoutput$median$lambda[1:2], 2) # 1=panpri 2=prdpri 3=panprd 
tmp06s0[2,1:2] <- round(tmp$res06s0R$BUGSoutput$median$lambda[1:2] - tmp$res06s0M$BUGSoutput$median$lambda[1:2], 2) # 1=panpri 2=prdpri 3=panprd 
tmp06s0[3,1:2] <- round(tmp$res06s0M$BUGSoutput$median$lambda[1:2] - tmp$res06s0P$BUGSoutput$median$lambda[1:2], 2) # 1=panpri 2=prdpri 3=panprd 
tmp06s0[4,1:2] <- round(tmp$res06s0P$BUGSoutput$median$lambda[1:2], 2) # 1=panpri 2=prdpri 3=panprd 
tmp06s0[,3] <- tmp06s0[,1] - tmp06s0[,2]
                                        #
tmp06s3[1,1:2] <- round(tmp$res06s3R$BUGSoutput$median$lambda[1:2], 2) # 1=panpri 2=prdpri 3=panprd 
tmp06s3[2,1:2] <- round(tmp$res06s3R$BUGSoutput$median$lambda[1:2] - tmp$res06s3M$BUGSoutput$median$lambda[1:2], 2) # 1=panpri 2=prdpri 3=panprd 
tmp06s3[3,1:2] <- round(tmp$res06s3M$BUGSoutput$median$lambda[1:2] - tmp$res06s3P$BUGSoutput$median$lambda[1:2], 2) # 1=panpri 2=prdpri 3=panprd 
tmp06s3[4,1:2] <- round(tmp$res06s3P$BUGSoutput$median$lambda[1:2], 2) # 1=panpri 2=prdpri 3=panprd 
tmp06s3[,3] <- tmp06s3[,1] - tmp06s3[,2]
                                        #
tmp09s0[1,1:2] <- round(tmp$res09s0R$BUGSoutput$median$lambda[1:2], 2) # 1=panpri 2=prdpri 3=panprd 
tmp09s0[2,1:2] <- round(tmp$res09s0R$BUGSoutput$median$lambda[1:2] - tmp$res09s0M$BUGSoutput$median$lambda[1:2], 2) # 1=panpri 2=prdpri 3=panprd 
tmp09s0[3,1:2] <- round(tmp$res09s0M$BUGSoutput$median$lambda[1:2] - tmp$res09s0P$BUGSoutput$median$lambda[1:2], 2) # 1=panpri 2=prdpri 3=panprd 
tmp09s0[4,1:2] <- round(tmp$res09s0P$BUGSoutput$median$lambda[1:2], 2) # 1=panpri 2=prdpri 3=panprd 
tmp09s0[,3] <- tmp09s0[,1] - tmp09s0[,2]
                                        #
tmp09s3[1,1:2] <- round(tmp$res09s3R$BUGSoutput$median$lambda[1:2], 2) # 1=panpri 2=prdpri 3=panprd 
tmp09s3[2,1:2] <- round(tmp$res09s3R$BUGSoutput$median$lambda[1:2] - tmp$res09s3M$BUGSoutput$median$lambda[1:2], 2) # 1=panpri 2=prdpri 3=panprd 
tmp09s3[3,1:2] <- round(tmp$res09s3M$BUGSoutput$median$lambda[1:2] - tmp$res09s3P$BUGSoutput$median$lambda[1:2], 2) # 1=panpri 2=prdpri 3=panprd 
tmp09s3[4,1:2] <- round(tmp$res09s3P$BUGSoutput$median$lambda[1:2], 2) # 1=panpri 2=prdpri 3=panprd 
tmp09s3[,3] <- tmp09s3[,1] - tmp09s3[,2]
                                        #
tmp12s0[1,1:2] <- round(tmp$res12s0R$BUGSoutput$median$lambda[1:2], 2) # 1=panpri 2=prdpri 3=panprd 
tmp12s0[2,1:2] <- round(tmp$res12s0R$BUGSoutput$median$lambda[1:2] - tmp$res12s0M$BUGSoutput$median$lambda[1:2], 2) # 1=panpri 2=prdpri 3=panprd 
tmp12s0[3,1:2] <- round(tmp$res12s0M$BUGSoutput$median$lambda[1:2] - tmp$res12s0P$BUGSoutput$median$lambda[1:2], 2) # 1=panpri 2=prdpri 3=panprd 
tmp12s0[4,1:2] <- round(tmp$res12s0P$BUGSoutput$median$lambda[1:2], 2) # 1=panpri 2=prdpri 3=panprd 
tmp12s0[,3] <- tmp12s0[,1] - tmp12s0[,2]
                                        #
tmp12s3[1,1:2] <- round(tmp$res12s3R$BUGSoutput$median$lambda[1:2], 2) # 1=panpri 2=prdpri 3=panprd 
tmp12s3[2,1:2] <- round(tmp$res12s3R$BUGSoutput$median$lambda[1:2] - tmp$res12s3M$BUGSoutput$median$lambda[1:2], 2) # 1=panpri 2=prdpri 3=panprd 
tmp12s3[3,1:2] <- round(tmp$res12s3M$BUGSoutput$median$lambda[1:2] - tmp$res12s3P$BUGSoutput$median$lambda[1:2], 2) # 1=panpri 2=prdpri 3=panprd 
tmp12s3[4,1:2] <- round(tmp$res12s3P$BUGSoutput$median$lambda[1:2], 2) # 1=panpri 2=prdpri 3=panprd 
tmp12s3[,3] <- tmp12s3[,1] - tmp12s3[,2]
                                        #
tmp0612n0[1,1:2] <- round(tmp$res0612n0R$BUGSoutput$median$lambda[1:2], 2) # 1=panpri 2=prdpri 3=panprd 
tmp0612n0[2,1:2] <- round(tmp$res0612n0R$BUGSoutput$median$lambda[1:2] - tmp$res0612n0M$BUGSoutput$median$lambda[1:2], 2) # 1=panpri 2=prdpri 3=panprd 
tmp0612n0[3,1:2] <- round(tmp$res0612n0M$BUGSoutput$median$lambda[1:2] - tmp$res0612n0P$BUGSoutput$median$lambda[1:2], 2) # 1=panpri 2=prdpri 3=panprd 
tmp0612n0[4,1:2] <- round(tmp$res0612n0P$BUGSoutput$median$lambda[1:2], 2) # 1=panpri 2=prdpri 3=panprd 
tmp0612n0[,3] <- tmp0612n0[,1] - tmp0612n0[,2]
                                        #
tmp0612n3[1,1:2] <- round(tmp$res0612n3R$BUGSoutput$median$lambda[1:2], 2) # 1=panpri 2=prdpri 3=panprd 
tmp0612n3[2,1:2] <- round(tmp$res0612n3R$BUGSoutput$median$lambda[1:2] - tmp$res0612n3M$BUGSoutput$median$lambda[1:2], 2) # 1=panpri 2=prdpri 3=panprd 
tmp0612n3[3,1:2] <- round(tmp$res0612n3M$BUGSoutput$median$lambda[1:2] - tmp$res0612n3P$BUGSoutput$median$lambda[1:2], 2) # 1=panpri 2=prdpri 3=panprd 
tmp0612n3[4,1:2] <- round(tmp$res0612n3P$BUGSoutput$median$lambda[1:2], 2) # 1=panpri 2=prdpri 3=panprd 
tmp0612n3[,3] <- tmp0612n3[,1] - tmp0612n3[,2]

tmp06s0
tmp06s3
tmp09s0
tmp09s3
tmp12s0
tmp12s3
tmp0612n0
tmp0612n3

tmp <- biasResp0612oldNewDistrictsRPM
res <- tmp$res0612s0R
res$BUGSoutput$median
summary(res0612s0R$BUGSoutput$sims.list$lambda - res0612s0M$BUGSoutput$sims.list$lambda)


# predict seats from pan=.33, pri=.33, prd=.33, rest=.01 (Márquez's unlikely but illustrative scenario).
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
seat.sh <- seats(pan.sh = .33, pri.sh = .33, prd.sh = .33)
quantile(x = seat.sh$pan.seats * 300, probs = c(.025,.975))
quantile(x = seat.sh$pri.seats * 300, probs = c(.025,.975))
quantile(x = seat.sh$prd.seats * 300, probs = c(.025,.975))
# would be preferable to model S[i,j] with multinomial in jags model above and then report the actual posterior simulations

#c.hat <- res$BUGSoutput$median$c
lambda.hat <- res$BUGSoutput$median$lambda
rho.hat <- res$BUGSoutput$median$rho
#
### Uncomment if using reference party (model lambda.rho.4)
## PRI is the reference party with lambda=0
lambda.hat <- c(lambda.hat[1], 0, lambda.hat[2:6])
#

## ESTO HACE UNA VERSION VISUAL DE LOS PARAMETROS DEL MODELO
#save.dir <- "~/Dropbox/mydocs/op-edsEtc/blog/graphs/" # donde guardar
save.dir <- paste(wd, "graphs", sep = "")
logit <- function(X){ log( X / (1-X) ) }
antilogit <- function(X){ exp(X) / (exp(X)+1) }
v.tmp <- (1:999)/1000
#
library(Cairo)
#title <- expression(paste("La responsividad ", rho, " y sesgo ", lambda, " de los distritos")) 
title <- expression(paste("District responsiveness ", rho, " (and party bias ", lambda, ">0 in grey)")) 
type <-  "pdf" 
file <- paste("rhoExample.", type, sep="")
setwd(save.dir)
## Cairo(file=file,
##       type = type,
##       width = 6,
##       height = 6,
##       units = "in",
##       dpi = 72,
##       bg = "transparent")
#
plot(c(0,1),c(0,1), type = "n",
#     xlab = "% votos", ylab = "% escaños", axes = FALSE,
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
lines(v.tmp, s.tmp, col = "grey") 
s.tmp <- antilogit ( 1.5 + 6 * logit( v.tmp ) ) # ESTO HACE LO DEBIDO: SESGO DE .25 EN FAVOR
lines(v.tmp, s.tmp, col = "grey")
#
s.tmp <- antilogit( 1 * logit( v.tmp ) )
lines(v.tmp, s.tmp, col = "darkgreen", lty = 2)
text(v.tmp[660], s.tmp[660], labels = expression(paste(rho, "=1")), pos=4)
s.tmp <- antilogit ( 3 * logit( v.tmp ) ) 
lines(v.tmp, s.tmp, col = "red") 
text(v.tmp[600], s.tmp[600], labels = expression(paste(rho, "=3")), pos=4)
s.tmp <- antilogit ( 6 * logit( v.tmp ) ) # ESTO HACE LO DEBIDO: SESGO DE .25 EN FAVOR
lines(v.tmp, s.tmp, col = "blue")
text(v.tmp[420], s.tmp[420], labels = expression(paste(rho, "=6")), pos=4)
#mtext(text = "Preparado por Eric Magar con resultados oficiales del IFE", side = 1, line = 4, col = "grey", cex = .75)
#mtext(text = "Prepared by Eric Magar with official IFE returns", side = 1, line = 4, col = "grey", cex = .75)
#
## dev.off()
## setwd(wd)


#### Plot Posterior lambda Samples For S0 And S3
tmp <- biasResp0612oldNewDistrictsRPM # compact name
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
file <- paste("bias200612s0R.", type, sep="")
setwd(save.dir)
Cairo(file=file,
      type = type,
      width = 6,
      height = 6,
      units = "in",
      dpi = 72,
      bg = "transparent")
#
#jitter <- runif(n = 300, min=-.1, max=.1)
jitter <- rnorm(n = 300, sd = .03)
color1.minus.pri <- c( "blue", "gold", "green", "cyan", "orange", "violet" )
res <- tmp$res06s0R; shift.v <- .3
plot( c( -1, 1), -c(.5,5),
     type="n", axes = FALSE, ylab = "", xlab = "bias relative to PRI", main = "Bias: 2006 map")#"Bias: 2015 map (hypothetical)")
#     type="n", axes = FALSE, ylab = "", xlab = "sesgo en relación al PRI", main = "Distritos propuestos")#"Distritos en vigor")
axis( side = 1, at = seq(from = -1, to = 1, by = .1), labels = FALSE)
axis( side = 1, at = seq(from = -.5, to = .5, by = .5), labels = c("-0.5","0","+0.5"))
abline(v=0, lty=2)
#for (i in 1:6){
for (i in c(1,2,4)){ # some parties absent, asdc dropped from graph
    # if party color desired, this does the trick: col = color1.minus.pri[i]
    points(res$BUGSoutput$sims.list$lambda[,i], -i+shift.v+jitter, cex=.1, col = "gray70");
    lines(x = c(myQ(.05,i),myQ(.95,i)), y = c(-i+shift.v,-i+shift.v), lwd = 2)
    lines(x = c(myQ(.25,i),myQ(.75,i)), y = c(-i+shift.v,-i+shift.v), lwd = 6)
    points(myQ(.5,i), -i+shift.v, pch = 19, col="white")
    #points(myQ(.5,i), -i+shift.v, pch = 19, cex = .5, col=color1.minus.pri[i])
    points(myQ(.5,i), -i+shift.v, pch = 19, cex = .5)
}
#
res <- tmp$res09s0R; shift.v <- .1
#for (i in 1:6){
for (i in 1:5){ # some parties absent
    # if party color desired, this does the trick: col = color1.minus.pri[i]
    points(res$BUGSoutput$sims.list$lambda[,i], -i+shift.v+jitter, cex=.1, col = "gray70");
    lines(x = c(myQ(.05,i),myQ(.95,i)), y = c(-i+shift.v,-i+shift.v), lwd = 2)
    lines(x = c(myQ(.25,i),myQ(.75,i)), y = c(-i+shift.v,-i+shift.v), lwd = 6)
    points(myQ(.5,i), -i+shift.v, pch = 19, col="white")
    #points(myQ(.5,i), -i+shift.v, pch = 19, cex = .5, col=color1.minus.pri[i])
    points(myQ(.5,i), -i+shift.v, pch = 19, cex = .5)
}
#
res <- tmp$res12s0R; shift.v <- -.1
for (i in 1:4){ # some parties absent
    # if party color desired, this does the trick: col = color1.minus.pri[i]
    points(res$BUGSoutput$sims.list$lambda[,i], -i+shift.v+jitter, cex=.1, col = "gray70");
    lines(x = c(myQ(.05,i),myQ(.95,i)), y = c(-i+shift.v,-i+shift.v), lwd = 2)
    lines(x = c(myQ(.25,i),myQ(.75,i)), y = c(-i+shift.v,-i+shift.v), lwd = 6)
    points(myQ(.5,i), -i+shift.v, pch = 19, col="white")
    #points(myQ(.5,i), -i+shift.v, pch = 19, cex = .5, col=color1.minus.pri[i])
    points(myQ(.5,i), -i+shift.v, pch = 19, cex = .5)
}
#
#res <- tmp$res0612s0R; shift.v <- -.3
res <- tmp$res0612n0R; shift.v <- -.3 # use nation estimates
for (i in 1:4){ # some parties absent
    # if party color desired, this does the trick: col = color1.minus.pri[i]
    #points(res$BUGSoutput$sims.list$lambda[,i], -i+shift.v+jitter, cex=.1, col = "gray70");
    points(sample(res$BUGSoutput$sims.list$lambda[,i], 300), -i+shift.v+jitter, cex=.1, col = "gray70"); # sample to get 300 points
    lines(x = c(myQ(.05,i),myQ(.95,i)), y = c(-i+shift.v,-i+shift.v), lwd = 2)
    lines(x = c(myQ(.25,i),myQ(.75,i)), y = c(-i+shift.v,-i+shift.v), lwd = 6)
    points(myQ(.5,i), -i+shift.v, pch = 19, col="white")
    #points(myQ(.5,i), -i+shift.v, pch = 19, cex = .5, col=color1.minus.pri[i])
    points(myQ(.5,i), -i+shift.v, pch = 19, cex = .5)
}
text(x =     .9   , y =  -1   +.55, labels =     "Election (mod.)", cex = .65)
text(x = rep(.9,4), y = -c(1,2,4)+.3, labels = rep("2006 (sta.)", 5), cex = .65)
text(x = rep(.9,5), y = -(1:5)+.1, labels = rep("2009 (sta.)", 5), cex = .65)
text(x = rep(.9,4), y = -(1:4)-.1, labels = rep("2012 (sta.)", 5), cex = .65)
text(x = rep(.9,4), y = -(1:4)-.3, labels = rep("all (nat.)", 5), cex = .65)
#text(x = rep(.9,4), y = -(1:4)-.3, labels = rep("todos", 5), cex = .65)
#
text(x = rep(-.9,5), y = -c(1:4,4.9), labels = c("PAN", "PRD", "PVEM", "PANAL", "PT-C"))
dev.off()
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
    return(exp( lambdas[party.vote] ) *  equis[,party.vote]^rho.hat  / (d1+d2+d3+d4+d5+d6+d7)) # voto party crece linealmente, demás según peso relativo
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
save.dir <- paste(wd, "graphs", sep = "")
setwd(save.dir)
#setwd(paste(wd, "graphs", sep=""))
library(Cairo)
title <- "Federal deputies by state 2006-2012"
#title <- "DipFed por estado 2006-2012, distritos en vigor" #propuestos"
#title <- "Diputados Federales de mayoría 2006-2012"
type <- "pdf"
#file <- paste("biasResp2006s3.", type, sep="")
file <- paste("resXedo20062012.", type, sep="")
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
     xlab = "% votes won in state", ylab = "% plurality seats won in state", axes = FALSE)
#     xlab = "% votos en el estado", ylab = "% escaños en el estado", axes = FALSE)
axis(1, at=seq(0,1,.2), lab=seq(0,100,20))
axis(2, at=seq(0,1,.2), lab=seq(0,100,20))
abline(a=0, b=1, lty=2)
#reg <- lm(tmp$seats ~ tmp$votes)
#reg <- lm(tmp$seats ~ poly(tmp$votes, 2, raw=TRUE))
points(tmp$votes, tmp$seats/tmp$ndis, pch=19, cex=.5, col=color)
lines(equis, s.hat.pan, col = color[tmp$pty==1][1])
lines(equis, s.hat.pri, col = color[tmp$pty==2][1])
lines(equis, s.hat.prd, col = color[tmp$pty==3][1])
#abline(reg)
text(.85,.3,  paste("Responsividad=", round(rho.hat, digits=2)))
text(.85,.25, "Sesgo relativo al PRI:")
## text(.85,.2,  paste("PAN=", round( exp(lambda.hat[1]) / (exp(lambda.hat[1]) + 1) - .5 , digits=2)))
## text(.85,.15, paste("PRI=", round( exp(lambda.hat[2]) / (exp(lambda.hat[2]) + 1) - .5 , digits=2)))
## text(.85,.1,  paste("PRD=", round( exp(lambda.hat[3]) / (exp(lambda.hat[3]) + 1) - .5 , digits=2)))
text(.85,.2,  paste("PAN=", round( lambda.hat[1]-lambda.hat[2], digits=2)))
## text(.85,.15, paste("PRI=", round( lambda.hat[2]-lambda.hat[2], digits=2)))
text(.85,.15,  paste("PRD=", round( lambda.hat[3]-lambda.hat[2], digits=2)))
#text(.85,.1,  paste("PANAL=", round( lambda.hat[6]-lambda.hat[2], digits=2)))
#
## dev.off()
setwd(wd)

## ## graph different bias resp combos in 2-pty setting
## equis <- seq(0,.99,.01)
## #logit(ye) <-  0 + 1 * logit(equis)
## plot(c(0,1), c(0,1), type = "n", xlab = "%voto", ylab = "%escaños de mayoría", axes = FALSE)
## abline(0,1,lty=2)
## axis(1, at = seq(0,1,.25), labels = seq(0,100,25))
## axis(2, at = seq(0,1,.25), labels = seq(0,100,25))
## ye <- antilogit ( 0 + 6 * logit(equis) )
## lines(equis, ye, col = "red")
## ye <- antilogit ( 1 + 6 * logit(equis) )
## lines(equis, ye, col = "blue")

dim(tmp)

PARA DETECTAR EL PROBLEMA DE CURVAS QUE NO LLEGAN A S=1 CON VALORES ALTOS DE V
1. Estimar King sin partido de referencia para ver si también tiene el problema
2. Verificar que la omisión del partido de referencia en el modelo 4 esté hecha conforme a la especificación del paper de King
3. Verificar funciín seat.hat

TO DO:
    1-comparar lambda y rho de modelo Calvo-Micozzi, King con 7 y King con 6+ref
    2-método de graficado con sólo el partido ganador y usando las lambdas de los otros
    3-meter 5 distritos extra para cada una de las circunscripciones pluri
    4-estimate models for 2006, 2009, 2012 separately and for 2006-2012
    5-repaeat with s0 and with s3, graph each and make summary

sum(df2012d0$pvemw)

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
     type="n", main = "Lista nominal 2012 vs. tamaño ideal", axes = FALSE)
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

