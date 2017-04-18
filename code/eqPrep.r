###########################################################################
## Code is invoked from other scripts                                    ##
## It imports and prepares data frame mapping secciones to 1977, 1996,   ##
## and 2005 districts, and also two redistricting proposals made in 2013 ##
## ** revised May 2016 to include 2014 reseccionamiento **               ##
###########################################################################
#
# START EQ PREP
#
# where equivalencias are saved
eqd <- "~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/ifeRedist2013/equivSecc/"
#
# read secciones-to-districts relations 1994--2013
#eq <- read.csv(file = paste(eqd, "tablaEquivalenciasSeccionales1994-2010.2013.csv", sep = ""), header = TRUE)
#eq <- read.csv(file = paste(eqd, "tablaEquivalenciasSeccionales1994-2014.csv", sep = ""), header = TRUE, stringsAsFactors = FALSE)
eq <- read.csv(file = paste(eqd, "tablaEquivalenciasSeccionalesDesde1994.csv", sep = ""), header = TRUE, stringsAsFactors = FALSE)
eq <- eq[, -grep("loc|nac", colnames(eq))] # drop party amendments, useful for another project
eq[, grep(x = colnames(eq), pattern = "dis")][is.na(eq[,grep(x = colnames(eq), pattern = "dis")])==TRUE] <- 0 # replace NAs with zero in district columns
#eq <- eq[-which(eq$dis2013.1==0 & eq$dis2015>0), ] #drop new secciones created after 2013 redistricting --- commented to add 2015 votes
#dim(eq)  #debug
#head(eq) #debug
#
## Fills which district new secciones would have belonged to before their creation 
select <- which(eq$action=="new" & eq$fr.to=="from" & eq$orig.dest!="." & eq$orig.dest!=" ") # new secciones that have info
tmp <- eq[select,]
tmp$orig.dest <- as.numeric(as.character(tmp$orig.dest))
tmp$pick.from <- NA
for (i in 1:nrow(tmp)){
    tmp$pick.from[i] <- which(eq$edon==tmp$edon[i] & eq$seccion==tmp$orig.dest[i])
}
tmp$when <- as.numeric(as.character(tmp$when))
#
#table(tmp$when) # debug
sel2 <- which(tmp$when==2002)
tmp$dis1994[sel2] <- eq$dis1994[tmp$pick.from[sel2]]
tmp$dis1997[sel2] <- eq$dis1997[tmp$pick.from[sel2]]
tmp$dis2000[sel2] <- eq$dis2000[tmp$pick.from[sel2]]
sel2 <- which(tmp$when==2005)
tmp$dis1994[sel2] <- eq$dis1994[tmp$pick.from[sel2]]
tmp$dis1997[sel2] <- eq$dis1997[tmp$pick.from[sel2]]
tmp$dis2000[sel2] <- eq$dis2000[tmp$pick.from[sel2]]
tmp$dis2003[sel2] <- eq$dis2003[tmp$pick.from[sel2]]
sel2 <- which(tmp$when==2007)
tmp$dis1994[sel2] <- eq$dis1994[tmp$pick.from[sel2]]
tmp$dis1997[sel2] <- eq$dis1997[tmp$pick.from[sel2]]
tmp$dis2000[sel2] <- eq$dis2000[tmp$pick.from[sel2]]
tmp$dis2003[sel2] <- eq$dis2003[tmp$pick.from[sel2]]
tmp$dis2006[sel2] <- eq$dis2006[tmp$pick.from[sel2]]
sel2 <- which(tmp$when==2009 | tmp$when==2010)
tmp$dis1994[sel2] <- eq$dis1994[tmp$pick.from[sel2]]
tmp$dis1997[sel2] <- eq$dis1997[tmp$pick.from[sel2]]
tmp$dis2000[sel2] <- eq$dis2000[tmp$pick.from[sel2]]
tmp$dis2003[sel2] <- eq$dis2003[tmp$pick.from[sel2]]
tmp$dis2006[sel2] <- eq$dis2006[tmp$pick.from[sel2]]
tmp$dis2009[sel2] <- eq$dis2009[tmp$pick.from[sel2]]
sel2 <- which(tmp$when==2012)
tmp$dis1994[sel2] <- eq$dis1994[tmp$pick.from[sel2]]
tmp$dis1997[sel2] <- eq$dis1997[tmp$pick.from[sel2]]
tmp$dis2000[sel2] <- eq$dis2000[tmp$pick.from[sel2]]
tmp$dis2003[sel2] <- eq$dis2003[tmp$pick.from[sel2]]
tmp$dis2006[sel2] <- eq$dis2006[tmp$pick.from[sel2]]
tmp$dis2009[sel2] <- eq$dis2009[tmp$pick.from[sel2]]
tmp$dis2012[sel2] <- eq$dis2012[tmp$pick.from[sel2]]
sel2 <- which(tmp$when==2014)
tmp$dis1994[sel2] <- eq$dis1994[tmp$pick.from[sel2]]
tmp$dis1997[sel2] <- eq$dis1997[tmp$pick.from[sel2]]
tmp$dis2000[sel2] <- eq$dis2000[tmp$pick.from[sel2]]
tmp$dis2003[sel2] <- eq$dis2003[tmp$pick.from[sel2]]
tmp$dis2006[sel2] <- eq$dis2006[tmp$pick.from[sel2]]
tmp$dis2009[sel2] <- eq$dis2009[tmp$pick.from[sel2]]
tmp$dis2012[sel2] <- eq$dis2012[tmp$pick.from[sel2]]
tmp$dis2013.1[sel2] <- eq$dis2013.1[tmp$pick.from[sel2]]
tmp$dis2013.2[sel2] <- eq$dis2013.2[tmp$pick.from[sel2]]
tmp$dis2013.3[sel2] <- eq$dis2013.3[tmp$pick.from[sel2]]
#
tmp2 <- tmp
tmp$pick.from <- NULL
eq[select,] <- tmp # paste back to dataset
## tmp <- tmp2 #debug
## head(tmp2)  #debug
## dim(tmp2)   #debug
#
# Fill what districts split secciones would have belonged to afterwards
tmp2$send.to <- tmp2$pick.from; tmp2$pick.from <- NULL
#tmp2 <- tmp2[order(tmp2$send.to),]
tmp2$drop <- 0; tmp3 <- tmp2$send.to; tmp3 <- c(NA, tmp3[1:(nrow(tmp2)-1)]); tmp3 <- tmp3 - tmp2$send.to; tmp3[tmp3!=0] <- 1; tmp3 <- 1 - tmp3; tmp2$drop[2:nrow(tmp2)] <- tmp3[-1]; tmp2 <- tmp2[tmp2$drop==0,] # drop repeated send.tos
rm(tmp3); tmp2$drop <- NULL # clean
tmp3 <- eq[tmp2$send.to,]
#
#table(tmp3$when) # debug
sel2 <- which(tmp3$when==2002)
tmp3$dis2003[sel2] <- tmp2$dis2003[sel2]
tmp3$dis2006[sel2] <- tmp2$dis2006[sel2]
tmp3$dis2009[sel2] <- tmp2$dis2009[sel2]
tmp3$dis2012[sel2] <- tmp2$dis2012[sel2]
tmp3$dis2013.1[sel2] <- tmp2$dis2013.1[sel2]
tmp3$dis2013.2[sel2] <- tmp2$dis2013.2[sel2]
tmp3$dis2013.3[sel2] <- tmp2$dis2013.3[sel2]
tmp3$dis2015[sel2] <- tmp2$dis2015[sel2]
sel2 <- which(tmp3$when==2005)
tmp3$dis2006[sel2] <- tmp2$dis2006[sel2]
tmp3$dis2009[sel2] <- tmp2$dis2009[sel2]
tmp3$dis2012[sel2] <- tmp2$dis2012[sel2]
tmp3$dis2013.1[sel2] <- tmp2$dis2013.1[sel2]
tmp3$dis2013.2[sel2] <- tmp2$dis2013.2[sel2]
tmp3$dis2013.3[sel2] <- tmp2$dis2013.3[sel2]
tmp3$dis2015[sel2] <- tmp2$dis2015[sel2]
sel2 <- which(tmp3$when==2007)
tmp3$dis2009[sel2] <- tmp2$dis2009[sel2]
tmp3$dis2012[sel2] <- tmp2$dis2012[sel2]
tmp3$dis2013.1[sel2] <- tmp2$dis2013.1[sel2]
tmp3$dis2013.2[sel2] <- tmp2$dis2013.2[sel2]
tmp3$dis2013.3[sel2] <- tmp2$dis2013.3[sel2]
tmp3$dis2015[sel2] <- tmp2$dis2015[sel2]
sel2 <- which(tmp3$when==2009 | tmp3$when==2010)
tmp3$dis2012[sel2] <- tmp2$dis2012[sel2]
tmp3$dis2013.1[sel2] <- tmp2$dis2013.1[sel2]
tmp3$dis2013.2[sel2] <- tmp2$dis2013.2[sel2]
tmp3$dis2013.3[sel2] <- tmp2$dis2013.3[sel2]
tmp3$dis2015[sel2] <- tmp2$dis2015[sel2]
sel2 <- which(tmp3$when==2012)
tmp3$dis2013.1[sel2] <- tmp2$dis2013.1[sel2]
tmp3$dis2013.2[sel2] <- tmp2$dis2013.2[sel2]
tmp3$dis2013.3[sel2] <- tmp2$dis2013.3[sel2]
tmp3$dis2015[sel2] <- tmp2$dis2015[sel2]
sel2 <- which(tmp3$when==2014)
tmp3$dis2015[sel2] <- tmp2$dis2015[sel2]
rm(sel2)
#
eq[tmp2$send.to,] <- tmp3 # paste back to dataset
#
## Fills which district merged secciones would have belonged to afterwards
#select <- which(eq$action=="merged" & eq$fr.to=="to" & eq$orig.dest!="." & eq$orig.dest!=" ") # new secciones that have info
select <- which(eq$action=="merged" & eq$fr.to=="to" & eq$orig.dest!="." & eq$orig.dest!=" " & eq$when<2014) # new secciones that have info
tmp <- eq[select,]
tmp$orig.dest <- as.numeric(as.character(tmp$orig.dest))
tmp$pick.from <- NA
for (i in 1:nrow(tmp)){
    tmp$pick.from[i] <- which(eq$edon==tmp$edon[i] & eq$seccion==tmp$orig.dest[i])
}
tmp$when <- as.numeric(as.character(tmp$when))
#
# when new "merged to" info appears, will need to check new "when" years and modify next block accordingly
#table(tmp$when)
sel2 <- which(tmp$when==2008)
tmp$dis2009[sel2] <- eq$dis2009[tmp$pick.from[sel2]]
tmp$dis2012[sel2] <- eq$dis2012[tmp$pick.from[sel2]]
tmp$dis2013.1[sel2] <- eq$dis2013.1[tmp$pick.from[sel2]]
tmp$dis2013.2[sel2] <- eq$dis2013.2[tmp$pick.from[sel2]]
tmp$dis2013.3[sel2] <- eq$dis2013.3[tmp$pick.from[sel2]]
tmp$dis2015[sel2] <- eq$dis2015[tmp$pick.from[sel2]]
sel2 <- which(tmp$when==2009 | tmp$when==2010 | tmp$when==2011 | tmp$when==2012)
tmp$dis2012[sel2] <- eq$dis2012[tmp$pick.from[sel2]]
tmp$dis2013.1[sel2] <- eq$dis2013.1[tmp$pick.from[sel2]]
tmp$dis2013.2[sel2] <- eq$dis2013.2[tmp$pick.from[sel2]]
tmp$dis2013.3[sel2] <- eq$dis2013.3[tmp$pick.from[sel2]]
tmp$dis2015[sel2] <- eq$dis2015[tmp$pick.from[sel2]]
#
tmp$pick.from <- NULL
eq[select,] <- tmp # paste back to dataset
rm(i, select, sel2, tmp, tmp2, tmp3, eqd) # housecleaning
#
# END EQ PREP
