# Note: 2015 map was originally called 2013 (the year 2013 still appears in some graphs)
# When cleaning the code, will have to change object and column names from "13" to "15"

rm(list=ls())

wd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/git-repo/mex-open-map/data/")
dd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/") # data directory
gd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/git-repo/mex-open-map/graphs/")
setwd(wd)

#########################################################################################
# IMPORTS AND PREPARES OBJECT MAPPING SECTIONS TO 1979, 1997, 2006, AND 2015 DISTRICTS ##
#########################################################################################
#
cd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/git-repo/mex-open-map/code/")
source(file = paste(cd, "eqPrep.r", sep = ""), echo = TRUE)
rm(cd)
#
####################################################################################
## IMPORTS 2015 DISTRICT DATA --- no longer needed, info is included in object eq ##
####################################################################################
## #source("codeFor2015districts.r") # RUN TO CODE 1st and 3rd PROPOSALS
## load(file="dis2015.RData")
## head(dis2013)

###########################################
# nomenclatura de estructuras distritales #
# 1977-1995: 1979 map                     #
# 1996-2004: 1997 map                     #
# 2005-2015: 2006 map                     #
# 2013.1: 2015 map proposal 1             #
# 2013.3: 2015 map proposal 3 (final)     #
###########################################

##################################
## READ SECTION-LEVEL ELEC DATA ##
##################################
e12 <- read.csv( paste(dd, "dfSeccion2012.csv", sep=""), header=TRUE)
e12 <- e12[order(e12$edon, e12$seccion),]
head(e12); dim(e12); ls()
#
e09 <- read.csv( paste(dd, "dfSeccion2009.csv", sep=""), header=TRUE)
e09 <- e09[order(e09$edon, e09$seccion),]
head(e09); dim(e09); ls()
#
e06 <- read.csv( paste(dd, "dfSeccion2006.csv", sep=""), header=TRUE)
e06 <- e06[order(e06$edon, e06$seccion),]
head(e06); dim(e06); ls()
#
elecs060912.seccion <- list(e06=e06, e09=e09, e12=e12); rm(e06, e09, e12)

##############################################################################################################
## READ 060912 ELEC DATA AGGREGATED IN 2012, 2013.1, AND 2013.3 DISTRICTS PREPARED WITH analizaEscenarios.r ##
##############################################################################################################
dd2 <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/git-repo/mex-open-map/data/") # prepared data directory
load(file = paste(dd2, "elec060912.RData"))
rm(dd2)
#
## use to extract all objects from list elec060912 just imported
for(i in 1:length(elec060912)){
  ##first extract the object value
  tempobj=elec060912[[i]]
  ##now create a new variable with the original name of the list item
  eval(parse(text=paste(names(elec060912)[[i]],"= tempobj")))
}
rm(elec060912)
dim(df2012d0)

## poblacion del conteo 2005
c05 <- c("~/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/conteo2005")
#c05 <- c("d:/01/Dropbox/data/elecs/MXelsCalendGovt/censos/secciones/conteo2005")
#
edo <- "ags"; fl <- "01_ags_pob.csv"
tmp <- read.csv( paste(c05,edo,fl,sep="/"), header=TRUE)
pob05 <- data.frame(edon=tmp[,c("ENTIDAD.N.4.0")],
                    disn=tmp[,c("DISTRITO.N.4.0")],
                    munn=tmp[,c("MUNICIPIO.N.6.0")],
                    seccion=tmp[,c("SECCION.N.6.0")],
                    ptot=tmp[,c("POB_TOT.N.18.0")])  ## FALTA INCLUIR p18
#
edo <- "bc"; fl <- "02_bc_pob.csv"
tmp <- read.csv( paste(c05,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    munn=tmp[,c("MUNICIPIO")],
                    seccion=tmp[,c("SECCION")],
                    ptot=tmp[,c("POB_TOT")])  ## FALTA INCLUIR p18
pob05 <- rbind(pob05, tmp2)
#
edo <- "bcs"; fl <- "03_bcs_pob.csv"
tmp <- read.csv( paste(c05,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD.N.4.0")],
                    disn=tmp[,c("DISTRITO.N.4.0")],
                    munn=tmp[,c("MUNICIPIO.N.6.0")],
                    seccion=tmp[,c("SECCION.N.6.0")],
                    ptot=tmp[,c("POB_TOT.N.18.0")])  ## FALTA INCLUIR p18
pob05 <- rbind(pob05, tmp2)
#
edo <- "cam"; fl <- "04_camp_pob.csv"
tmp <- read.csv( paste(c05,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    munn=tmp[,c("MUNICIPIO")],
                    seccion=tmp[,c("SECCION")],
                    ptot=tmp[,c("POB_TOT")])  ## FALTA INCLUIR p18
pob05 <- rbind(pob05, tmp2)
#
edo <- "coa"; fl <- "05_coah_pob.csv"
tmp <- read.csv( paste(c05,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    munn=tmp[,c("MUNICIPIO")],
                    seccion=tmp[,c("SECCION")],
                    ptot=tmp[,c("POB_TOT")])  ## FALTA INCLUIR p18
pob05 <- rbind(pob05, tmp2)
#
edo <- "col"; fl <- "06_col_pob.csv"
tmp <- read.csv( paste(c05,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    munn=tmp[,c("MUNICIPIO")],
                    seccion=tmp[,c("SECCION")],
                    ptot=tmp[,c("POB_TOT")])  ## FALTA INCLUIR p18
pob05 <- rbind(pob05, tmp2)
#
edo <- "cps"; fl <- "07_chiap_pob.csv"
tmp <- read.csv( paste(c05,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    munn=tmp[,c("MUNICIPIO")],
                    seccion=tmp[,c("SECCION")],
                    ptot=tmp[,c("POB_TOT")])  ## FALTA INCLUIR p18
pob05 <- rbind(pob05, tmp2)
#
edo <- "cua"; fl <- "08_chih_pob.csv"
tmp <- read.csv( paste(c05,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    munn=tmp[,c("MUNICIPIO")],
                    seccion=tmp[,c("SECCION")],
                    ptot=tmp[,c("POB_TOT")])  ## FALTA INCLUIR p18
pob05 <- rbind(pob05, tmp2)
#
edo <- "df"; fl <- "09_df_pob.csv"
tmp <- read.csv( paste(c05,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    munn=tmp[,c("MUNICIPIO")],
                    seccion=tmp[,c("SECCION")],
                    ptot=tmp[,c("POB_TOT")])  ## FALTA INCLUIR p18
pob05 <- rbind(pob05, tmp2)
#
edo <- "dgo"; fl <- "10_dgo_pob.csv"
tmp <- read.csv( paste(c05,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    munn=tmp[,c("MUNICIPIO")],
                    seccion=tmp[,c("SECCION")],
                    ptot=tmp[,c("POB_TOT")])  ## FALTA INCLUIR p18
pob05 <- rbind(pob05, tmp2)
#
edo <- "gua"; fl <- "11_gto_pob.csv"
tmp <- read.csv( paste(c05,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    munn=tmp[,c("MUNICIPIO")],
                    seccion=tmp[,c("SECCION")],
                    ptot=tmp[,c("POB_TOT")])  ## FALTA INCLUIR p18
pob05 <- rbind(pob05, tmp2)
#
edo <- "gue"; fl <- "12_gro_pob.csv"
tmp <- read.csv( paste(c05,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    munn=tmp[,c("MUNICIPIO")],
                    seccion=tmp[,c("SECCION")],
                    ptot=tmp[,c("POB_TOT")])  ## FALTA INCLUIR p18
pob05 <- rbind(pob05, tmp2)
#
edo <- "hgo"; fl <- "13_hgo_pob.csv"
tmp <- read.csv( paste(c05,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    munn=tmp[,c("MUNICIPIO")],
                    seccion=tmp[,c("SECCION")],
                    ptot=tmp[,c("POB_TOT")])  ## FALTA INCLUIR p18
pob05 <- rbind(pob05, tmp2)
#
edo <- "jal"; fl <- "14_jal_pob.csv"
tmp <- read.csv( paste(c05,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    munn=tmp[,c("MUNICIPIO")],
                    seccion=tmp[,c("SECCION")],
                    ptot=tmp[,c("POB_TOT")])  ## FALTA INCLUIR p18
pob05 <- rbind(pob05, tmp2)
#
edo <- "mex"; fl <- "_5_mex_pob.csv"
tmp <- read.csv( paste(c05,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    munn=tmp[,c("MUNICIPIO")],
                    seccion=tmp[,c("SECCION")],
                    ptot=tmp[,c("POB_TOT")])  ## FALTA INCLUIR p18
pob05 <- rbind(pob05, tmp2)
#
edo <- "mic"; fl <- "16_mich_pob.csv"
tmp <- read.csv( paste(c05,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    munn=tmp[,c("MUNICIPIO")],
                    seccion=tmp[,c("SECCION")],
                    ptot=tmp[,c("POB_TOT")])  ## FALTA INCLUIR p18
pob05 <- rbind(pob05, tmp2)
#
edo <- "mor"; fl <- "17_mor_pob.csv"
tmp <- read.csv( paste(c05,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    munn=tmp[,c("MUNICIPIO")],
                    seccion=tmp[,c("SECCION")],
                    ptot=tmp[,c("POB_TOT")])  ## FALTA INCLUIR p18
pob05 <- rbind(pob05, tmp2)
#
edo <- "nay"; fl <- "18_nay_pob.csv"
tmp <- read.csv( paste(c05,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    munn=tmp[,c("MUNICIPIO")],
                    seccion=tmp[,c("SECCION")],
                    ptot=tmp[,c("POB_TOT")])  ## FALTA INCLUIR p18
pob05 <- rbind(pob05, tmp2)
#
edo <- "nl"; fl <- "19_nl_pob.csv"
tmp <- read.csv( paste(c05,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    munn=tmp[,c("MUNICIPIO")],
                    seccion=tmp[,c("SECCION")],
                    ptot=tmp[,c("POB_TOT")])  ## FALTA INCLUIR p18
pob05 <- rbind(pob05, tmp2)
#
edo <- "oax"; fl <- "20_oax_pob.csv"
tmp <- read.csv( paste(c05,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    munn=tmp[,c("MUNICIPIO")],
                    seccion=tmp[,c("SECCION")],
                    ptot=tmp[,c("POB_TOT")])  ## FALTA INCLUIR p18
pob05 <- rbind(pob05, tmp2)
#
edo <- "pue"; fl <- "21_pue_pob.csv"
tmp <- read.csv( paste(c05,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    munn=tmp[,c("MUNICIPIO")],
                    seccion=tmp[,c("SECCION")],
                    ptot=tmp[,c("POB_TOT")])  ## FALTA INCLUIR p18
pob05 <- rbind(pob05, tmp2)
#
edo <- "que"; fl <- "22_qro_pob.csv"
tmp <- read.csv( paste(c05,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    munn=tmp[,c("MUNICIPIO")],
                    seccion=tmp[,c("SECCION")],
                    ptot=tmp[,c("POB_TOT")])  ## FALTA INCLUIR p18
pob05 <- rbind(pob05, tmp2)
#
edo <- "qui"; fl <- "23_qroo_pob.csv"
tmp <- read.csv( paste(c05,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    munn=tmp[,c("MUNICIPIO")],
                    seccion=tmp[,c("SECCION")],
                    ptot=tmp[,c("POB_TOT")])  ## FALTA INCLUIR p18
pob05 <- rbind(pob05, tmp2)
#
edo <- "san"; fl <- "24_slp_pob.csv"
tmp <- read.csv( paste(c05,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    munn=tmp[,c("MUNICIPIO")],
                    seccion=tmp[,c("SECCION")],
                    ptot=tmp[,c("POB_TOT")])  ## FALTA INCLUIR p18
pob05 <- rbind(pob05, tmp2)
#
edo <- "sin"; fl <- "_5_sin_pob.csv"
tmp <- read.csv( paste(c05,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    munn=tmp[,c("MUNICIPIO")],
                    seccion=tmp[,c("SECCION")],
                    ptot=tmp[,c("POB_TOT")])  ## FALTA INCLUIR p18
pob05 <- rbind(pob05, tmp2)
#
edo <- "son"; fl <- "26_son_pob.csv"
tmp <- read.csv( paste(c05,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    munn=tmp[,c("MUNICIPIO")],
                    seccion=tmp[,c("SECCION")],
                    ptot=tmp[,c("POB_TOT")])  ## FALTA INCLUIR p18
pob05 <- rbind(pob05, tmp2)
#
edo <- "tab"; fl <- "27_tab_pob.csv"
tmp <- read.csv( paste(c05,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    munn=tmp[,c("MUNICIPIO")],
                    seccion=tmp[,c("SECCION")],
                    ptot=tmp[,c("POB_TOT")])  ## FALTA INCLUIR p18
pob05 <- rbind(pob05, tmp2)
#
edo <- "tam"; fl <- "28_tamp_pob.csv"
tmp <- read.csv( paste(c05,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    munn=tmp[,c("MUNICIPIO")],
                    seccion=tmp[,c("SECCION")],
                    ptot=tmp[,c("POB_TOT")])  ## FALTA INCLUIR p18
pob05 <- rbind(pob05, tmp2)
#
edo <- "tla"; fl <- "29_tlax_pob.csv"
tmp <- read.csv( paste(c05,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    munn=tmp[,c("MUNICIPIO")],
                    seccion=tmp[,c("SECCION")],
                    ptot=tmp[,c("POB_TOT")])  ## FALTA INCLUIR p18
pob05 <- rbind(pob05, tmp2)
#
edo <- "ver"; fl <- "30_ver_pob.csv"
tmp <- read.csv( paste(c05,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    munn=tmp[,c("MUNICIPIO")],
                    seccion=tmp[,c("SECCION")],
                    ptot=tmp[,c("POB_TOT")])  ## FALTA INCLUIR p18
pob05 <- rbind(pob05, tmp2)
#
edo <- "yuc"; fl <- "31_yuc_pob.csv"
tmp <- read.csv( paste(c05,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    munn=tmp[,c("MUNICIPIO")],
                    seccion=tmp[,c("SECCION")],
                    ptot=tmp[,c("POB_TOT")])  ## FALTA INCLUIR p18
pob05 <- rbind(pob05, tmp2)
#
edo <- "zac"; fl <- "_2_zac_pob.csv"
tmp <- read.csv( paste(c05,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    munn=tmp[,c("MUNICIPIO")],
                    seccion=tmp[,c("SECCION")],
                    ptot=tmp[,c("POB_TOT")])  ## FALTA INCLUIR p18
pob05 <- rbind(pob05, tmp2)
table(pob05$seccion==0)
str(pob05$seccion)

## poblacion del censo 2010
c10 <- c("~/Dropbox/data/mapas/seccionesIfe")
#c10 <- c("d:/01/Dropbox/data/mapas/seccionesIfe")
#
edo <- "ags"; fl <- "secciones_01.csv"
tmp <- read.csv( paste(c10,edo,fl,sep="/"), header=TRUE)
pob10 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    seccion=tmp[,c("CLAVEGEO")],
                    ptot=tmp[,c("POBTOT")],
                    p18=tmp[,c("P_18YMAS")])
pob10$munn <- as.integer(pob10$seccion/100000) - as.integer(pob10$seccion/100000000)*1000
pob10$seccion <- pob10$seccion - as.integer(pob10$seccion/10000)*10000
#
edo <- "bc"; fl <- "secciones_02.csv"
tmp <- read.csv( paste(c10,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    seccion=tmp[,c("CLAVEGEO")],
                    ptot=tmp[,c("POBTOT")],
                    p18=tmp[,c("P_18YMAS")])
tmp2$munn <- as.integer(tmp2$seccion/100000) - as.integer(tmp2$seccion/100000000)*1000
tmp2$seccion <- tmp2$seccion - as.integer(tmp2$seccion/10000)*10000
pob10 <- rbind(pob10, tmp2)
#
edo <- "bcs"; fl <- "secciones_03.csv"
tmp <- read.csv( paste(c10,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    seccion=tmp[,c("CLAVEGEO")],
                    ptot=tmp[,c("POBTOT")],
                    p18=tmp[,c("P_18YMAS")])
tmp2$munn <- as.integer(tmp2$seccion/100000) - as.integer(tmp2$seccion/100000000)*1000
tmp2$seccion <- tmp2$seccion - as.integer(tmp2$seccion/10000)*10000
pob10 <- rbind(pob10, tmp2)
#
edo <- "cam"; fl <- "secciones_04.csv"
tmp <- read.csv( paste(c10,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    seccion=tmp[,c("CLAVEGEO")],
                    ptot=tmp[,c("POBTOT")],
                    p18=tmp[,c("P_18YMAS")])
tmp2$munn <- as.integer(tmp2$seccion/100000) - as.integer(tmp2$seccion/100000000)*1000
tmp2$seccion <- tmp2$seccion - as.integer(tmp2$seccion/10000)*10000
pob10 <- rbind(pob10, tmp2)
#
edo <- "coa"; fl <- "secciones_05.csv"
tmp <- read.csv( paste(c10,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    seccion=tmp[,c("CLAVEGEO")],
                    ptot=tmp[,c("POBTOT")],
                    p18=tmp[,c("P_18YMAS")])
tmp2$munn <- as.integer(tmp2$seccion/100000) - as.integer(tmp2$seccion/100000000)*1000
tmp2$seccion <- tmp2$seccion - as.integer(tmp2$seccion/10000)*10000
pob10 <- rbind(pob10, tmp2)
#
edo <- "col"; fl <- "secciones_06.csv"
tmp <- read.csv( paste(c10,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    seccion=tmp[,c("CLAVEGEO")],
                    ptot=tmp[,c("POBTOT")],
                    p18=tmp[,c("P_18YMAS")])
tmp2$munn <- as.integer(tmp2$seccion/100000) - as.integer(tmp2$seccion/100000000)*1000
tmp2$seccion <- tmp2$seccion - as.integer(tmp2$seccion/10000)*10000
pob10 <- rbind(pob10, tmp2)
#
edo <- "cps"; fl <- "secciones_07.csv"
tmp <- read.csv( paste(c10,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    seccion=tmp[,c("CLAVEGEO")],
                    ptot=tmp[,c("POBTOT")],
                    p18=tmp[,c("P_18YMAS")])
tmp2$munn <- as.integer(tmp2$seccion/100000) - as.integer(tmp2$seccion/100000000)*1000
tmp2$seccion <- tmp2$seccion - as.integer(tmp2$seccion/10000)*10000
#head(tmp2[tmp2$edon==7,]) # problemo: chiapas viene duplicado en pob10
tmp2 <- tmp2[which(duplicated(tmp2$seccion)==FALSE),] # por alguna razón los datos de chiapas vienen duplicados, esto elimina
pob10 <- rbind(pob10, tmp2)
#
edo <- "cua"; fl <- "secciones_08.csv"
tmp <- read.csv( paste(c10,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    seccion=tmp[,c("CLAVEGEO")],
                    ptot=tmp[,c("POBTOT")],
                    p18=tmp[,c("P_18YMAS")])
tmp2$munn <- as.integer(tmp2$seccion/100000) - as.integer(tmp2$seccion/100000000)*1000
tmp2$seccion <- tmp2$seccion - as.integer(tmp2$seccion/10000)*10000
pob10 <- rbind(pob10, tmp2)
#
edo <- "df"; fl <- "secciones_09.csv"
tmp <- read.csv( paste(c10,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    seccion=tmp[,c("CLAVEGEO")],
                    ptot=tmp[,c("POBTOT")],
                    p18=tmp[,c("P_18YMAS")])
tmp2$munn <- as.integer(tmp2$seccion/100000) - as.integer(tmp2$seccion/100000000)*1000
tmp2$seccion <- tmp2$seccion - as.integer(tmp2$seccion/10000)*10000
pob10 <- rbind(pob10, tmp2)
#
edo <- "dgo"; fl <- "secciones_10.csv"
tmp <- read.csv( paste(c10,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    seccion=tmp[,c("CLAVEGEO")],
                    ptot=tmp[,c("POBTOT")],
                    p18=tmp[,c("P_18YMAS")])
tmp2$munn <- as.integer(tmp2$seccion/100000) - as.integer(tmp2$seccion/100000000)*1000
tmp2$seccion <- tmp2$seccion - as.integer(tmp2$seccion/10000)*10000
pob10 <- rbind(pob10, tmp2)
#
edo <- "gua"; fl <- "secciones_11.csv"
tmp <- read.csv( paste(c10,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    seccion=tmp[,c("CLAVEGEO")],
                    ptot=tmp[,c("POBTOT")],
                    p18=tmp[,c("P_18YMAS")])
tmp2$munn <- as.integer(tmp2$seccion/100000) - as.integer(tmp2$seccion/100000000)*1000
tmp2$seccion <- tmp2$seccion - as.integer(tmp2$seccion/10000)*10000
pob10 <- rbind(pob10, tmp2)
#
edo <- "gue"; fl <- "secciones_12.csv"
tmp <- read.csv( paste(c10,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    seccion=tmp[,c("CLAVEGEO")],
                    ptot=tmp[,c("POBTOT")],
                    p18=tmp[,c("P_18YMAS")])
tmp2$munn <- as.integer(tmp2$seccion/100000) - as.integer(tmp2$seccion/100000000)*1000
tmp2$seccion <- tmp2$seccion - as.integer(tmp2$seccion/10000)*10000
pob10 <- rbind(pob10, tmp2)
#
edo <- "hgo"; fl <- "secciones_13.csv"
tmp <- read.csv( paste(c10,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    seccion=tmp[,c("CLAVEGEO")],
                    ptot=tmp[,c("POBTOT")],
                    p18=tmp[,c("P_18YMAS")])
tmp2$munn <- as.integer(tmp2$seccion/100000) - as.integer(tmp2$seccion/100000000)*1000
tmp2$seccion <- tmp2$seccion - as.integer(tmp2$seccion/10000)*10000
pob10 <- rbind(pob10, tmp2)
#
edo <- "jal"; fl <- "secciones_14.csv"
tmp <- read.csv( paste(c10,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    seccion=tmp[,c("CLAVEGEO")],
                    ptot=tmp[,c("POBTOT")],
                    p18=tmp[,c("P_18YMAS")])
tmp2$munn <- as.integer(tmp2$seccion/100000) - as.integer(tmp2$seccion/100000000)*1000
tmp2$seccion <- tmp2$seccion - as.integer(tmp2$seccion/10000)*10000
pob10 <- rbind(pob10, tmp2)
#
edo <- "mex"; fl <- "secciones_15.csv"
tmp <- read.csv( paste(c10,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    seccion=tmp[,c("CLAVEGEO")],
                    ptot=tmp[,c("POBTOT")],
                    p18=tmp[,c("P_18YMAS")])
tmp2$munn <- as.integer(tmp2$seccion/100000) - as.integer(tmp2$seccion/100000000)*1000
tmp2$seccion <- tmp2$seccion - as.integer(tmp2$seccion/10000)*10000
pob10 <- rbind(pob10, tmp2)
#
edo <- "mic"; fl <- "secciones_16.csv"
tmp <- read.csv( paste(c10,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    seccion=tmp[,c("CLAVEGEO")],
                    ptot=tmp[,c("POBTOT")],
                    p18=tmp[,c("P_18YMAS")])
tmp2$munn <- as.integer(tmp2$seccion/100000) - as.integer(tmp2$seccion/100000000)*1000
tmp2$seccion <- tmp2$seccion - as.integer(tmp2$seccion/10000)*10000
pob10 <- rbind(pob10, tmp2)
#
edo <- "mor"; fl <- "secciones_17.csv"
tmp <- read.csv( paste(c10,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    seccion=tmp[,c("CLAVEGEO")],
                    ptot=tmp[,c("POBTOT")],
                    p18=tmp[,c("P_18YMAS")])
tmp2$munn <- as.integer(tmp2$seccion/100000) - as.integer(tmp2$seccion/100000000)*1000
tmp2$seccion <- tmp2$seccion - as.integer(tmp2$seccion/10000)*10000
pob10 <- rbind(pob10, tmp2)
#
edo <- "nay"; fl <- "secciones_18.csv"
tmp <- read.csv( paste(c10,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    seccion=tmp[,c("CLAVEGEO")],
                    ptot=tmp[,c("POBTOT")],
                    p18=tmp[,c("P_18YMAS")])
tmp2$munn <- as.integer(tmp2$seccion/100000) - as.integer(tmp2$seccion/100000000)*1000
tmp2$seccion <- tmp2$seccion - as.integer(tmp2$seccion/10000)*10000
pob10 <- rbind(pob10, tmp2)
#
edo <- "nl"; fl <- "secciones_19.csv"
tmp <- read.csv( paste(c10,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    seccion=tmp[,c("CLAVEGEO")],
                    ptot=tmp[,c("POBTOT")],
                    p18=tmp[,c("P_18YMAS")])
tmp2$munn <- as.integer(tmp2$seccion/100000) - as.integer(tmp2$seccion/100000000)*1000
tmp2$seccion <- tmp2$seccion - as.integer(tmp2$seccion/10000)*10000
pob10 <- rbind(pob10, tmp2)
#
edo <- "oax"; fl <- "secciones_20.csv"
tmp <- read.csv( paste(c10,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    seccion=tmp[,c("CLAVEGEO")],
                    ptot=tmp[,c("POBTOT")],
                    p18=tmp[,c("P_18YMAS")])
tmp2$munn <- as.integer(tmp2$seccion/100000) - as.integer(tmp2$seccion/100000000)*1000
tmp2$seccion <- tmp2$seccion - as.integer(tmp2$seccion/10000)*10000
pob10 <- rbind(pob10, tmp2)
#
edo <- "pue"; fl <- "secciones_21.csv"
tmp <- read.csv( paste(c10,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    seccion=tmp[,c("CLAVEGEO")],
                    ptot=tmp[,c("POBTOT")],
                    p18=tmp[,c("P_18YMAS")])
tmp2$munn <- as.integer(tmp2$seccion/100000) - as.integer(tmp2$seccion/100000000)*1000
tmp2$seccion <- tmp2$seccion - as.integer(tmp2$seccion/10000)*10000
pob10 <- rbind(pob10, tmp2)
#
edo <- "que"; fl <- "secciones_22.csv"
tmp <- read.csv( paste(c10,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    seccion=tmp[,c("CLAVEGEO")],
                    ptot=tmp[,c("POBTOT")],
                    p18=tmp[,c("P_18YMAS")])
tmp2$munn <- as.integer(tmp2$seccion/100000) - as.integer(tmp2$seccion/100000000)*1000
tmp2$seccion <- tmp2$seccion - as.integer(tmp2$seccion/10000)*10000
pob10 <- rbind(pob10, tmp2)
#
edo <- "qui"; fl <- "secciones_23.csv"
tmp <- read.csv( paste(c10,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    seccion=tmp[,c("CLAVEGEO")],
                    ptot=tmp[,c("POBTOT")],
                    p18=tmp[,c("P_18YMAS")])
tmp2$munn <- as.integer(tmp2$seccion/100000) - as.integer(tmp2$seccion/100000000)*1000
tmp2$seccion <- tmp2$seccion - as.integer(tmp2$seccion/10000)*10000
pob10 <- rbind(pob10, tmp2)
#
edo <- "san"; fl <- "secciones_24.csv"
tmp <- read.csv( paste(c10,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    seccion=tmp[,c("CLAVEGEO")],
                    ptot=tmp[,c("POBTOT")],
                    p18=tmp[,c("P_18YMAS")])
tmp2$munn <- as.integer(tmp2$seccion/100000) - as.integer(tmp2$seccion/100000000)*1000
tmp2$seccion <- tmp2$seccion - as.integer(tmp2$seccion/10000)*10000
pob10 <- rbind(pob10, tmp2)
#
edo <- "sin"; fl <- "secciones_25.csv"
tmp <- read.csv( paste(c10,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    seccion=tmp[,c("CLAVEGEO")],
                    ptot=tmp[,c("POBTOT")],
                    p18=tmp[,c("P_18YMAS")])
tmp2$munn <- as.integer(tmp2$seccion/100000) - as.integer(tmp2$seccion/100000000)*1000
tmp2$seccion <- tmp2$seccion - as.integer(tmp2$seccion/10000)*10000
pob10 <- rbind(pob10, tmp2)
#
edo <- "son"; fl <- "secciones_26.csv"
tmp <- read.csv( paste(c10,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    seccion=tmp[,c("CLAVEGEO")],
                    ptot=tmp[,c("POBTOT")],
                    p18=tmp[,c("P_18YMAS")])
tmp2$munn <- as.integer(tmp2$seccion/100000) - as.integer(tmp2$seccion/100000000)*1000
tmp2$seccion <- tmp2$seccion - as.integer(tmp2$seccion/10000)*10000
pob10 <- rbind(pob10, tmp2)
#
edo <- "tab"; fl <- "secciones_27.csv"
tmp <- read.csv( paste(c10,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    seccion=tmp[,c("CLAVEGEO")],
                    ptot=tmp[,c("POBTOT")],
                    p18=tmp[,c("P_18YMAS")])
tmp2$munn <- as.integer(tmp2$seccion/100000) - as.integer(tmp2$seccion/100000000)*1000
tmp2$seccion <- tmp2$seccion - as.integer(tmp2$seccion/10000)*10000
pob10 <- rbind(pob10, tmp2)
#
edo <- "tam"; fl <- "secciones_28.csv"
tmp <- read.csv( paste(c10,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    seccion=tmp[,c("CLAVEGEO")],
                    ptot=tmp[,c("POBTOT")],
                    p18=tmp[,c("P_18YMAS")])
tmp2$munn <- as.integer(tmp2$seccion/100000) - as.integer(tmp2$seccion/100000000)*1000
tmp2$seccion <- tmp2$seccion - as.integer(tmp2$seccion/10000)*10000
pob10 <- rbind(pob10, tmp2)
#
edo <- "tla"; fl <- "secciones_29.csv"
tmp <- read.csv( paste(c10,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    seccion=tmp[,c("CLAVEGEO")],
                    ptot=tmp[,c("POBTOT")],
                    p18=tmp[,c("P_18YMAS")])
tmp2$munn <- as.integer(tmp2$seccion/100000) - as.integer(tmp2$seccion/100000000)*1000
tmp2$seccion <- tmp2$seccion - as.integer(tmp2$seccion/10000)*10000
pob10 <- rbind(pob10, tmp2)
#
edo <- "ver"; fl <- "secciones_30.csv"
tmp <- read.csv( paste(c10,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    seccion=tmp[,c("CLAVEGEO")],
                    ptot=tmp[,c("POBTOT")],
                    p18=tmp[,c("P_18YMAS")])
tmp2$munn <- as.integer(tmp2$seccion/100000) - as.integer(tmp2$seccion/100000000)*1000
tmp2$seccion <- tmp2$seccion - as.integer(tmp2$seccion/10000)*10000
pob10 <- rbind(pob10, tmp2)
#
edo <- "yuc"; fl <- "secciones_31.csv"
tmp <- read.csv( paste(c10,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    seccion=tmp[,c("CLAVEGEO")],
                    ptot=tmp[,c("POBTOT")],
                    p18=tmp[,c("P_18YMAS")])
tmp2$munn <- as.integer(tmp2$seccion/100000) - as.integer(tmp2$seccion/100000000)*1000
tmp2$seccion <- tmp2$seccion - as.integer(tmp2$seccion/10000)*10000
pob10 <- rbind(pob10, tmp2)
#
edo <- "zac"; fl <- "secciones_32.csv"
tmp <- read.csv( paste(c10,edo,fl,sep="/"), header=TRUE)
tmp2 <- data.frame(edon=tmp[,c("ENTIDAD")],
                    disn=tmp[,c("DISTRITO")],
                    seccion=tmp[,c("CLAVEGEO")],
                    ptot=tmp[,c("POBTOT")],
                    p18=tmp[,c("P_18YMAS")])
tmp2$munn <- as.integer(tmp2$seccion/100000) - as.integer(tmp2$seccion/100000000)*1000
tmp2$seccion <- tmp2$seccion - as.integer(tmp2$seccion/10000)*10000
pob10 <- rbind(pob10, tmp2)
#
# SOME STATS
tmp <- pob10$ptot[pob10$ptot!=0]
print( paste("Median section population in 2010 was", median(tmp), ". Minimun was", min(tmp), ", maximum was", max(tmp)) )

# fusiona pob05 y pob10
pob <- pob10; colnames(pob) <- c("edon", "disn10", "seccion",  "ptot10", "p1810", "munn")
#summary(pob$seccion)
#pob$tmp <- pob$edon*10000 + pob$seccion
#summary(pob05$seccion); colnames(pob05)
#pob05$tmp <- pob05$edon*10000 + pob05$seccion;
#tmp <- pob05[,c("tmp", "ptot", "seccion")]; colnames(tmp) <- c("tmp","ptot05", "seccion05")
tmp <- pob05[, c("edon","seccion","disn","ptot")]; colnames(tmp) <- c("edon", "seccion", "disn05", "ptot05")
pob <- merge(x = pob, y = tmp, by = c("edon","seccion"), all = TRUE)
dim(pob05); dim(pob10); dim(pob); # debug
pob <- pob[, c("edon","seccion","disn05","disn10","ptot05","ptot10")] # drops munn and p1810
#
# sections that are absent from one or the other popuation file
pob$dcheck <- 0
pob$dcheck[which(is.na(pob$ptot05)==TRUE | is.na(pob$ptot10)==TRUE)] <- 1
table(pob$dcheck)
pob[is.na(pob)==TRUE] <- 0 # cambia los NAs por ceros para las sumas
# Las secciones cambiadas involucran a millones de habitantes
sum(pob$ptot05[pob$dcheck==1]); sum(pob$ptot10[pob$dcheck==1])
## # list them by state
## for(i in 1:32){
##     print(paste("edon =", i));
##     print(pob$seccion[pob$edon==i & pob$dcheck==1]);
## }
#
# pob$disn05 - pob$disn10 # suggests pob05 reports 1997 districts and pob10 reports 2005 districts
#
## merge pob with eq
tmp <- pob[,c("edon","seccion","ptot05","ptot10")];
eq <- merge(x = eq, y = tmp, by = c("edon", "seccion"), all.x = TRUE)
# SUMAR POBLACION DE LOS DISTRITOS
# así se hace en R un by yr mo: egen e12=sum(invested) de stata
table(eq$dis2012 - eq$dis2006) # ojo: hay secciones que cambian de distrito sin que medie redistritación (pocas, análisis de la info en objeto eq confirmaría que se trata de ajustes a límites municipales). Las ignoro.
#
eq[,grep(x = colnames(eq), pattern = "ptot")][is.na(eq[,grep(x = colnames(eq), pattern = "ptot")])==TRUE] <- 0 # replace NAs with zero in population
#
# seats apportioned by state
tmp <- eq[,c("edon","dis1994","dis1997","dis2000","dis2003","dis2006", "dis2009", "dis2012","dis2013.1")]; colnames(tmp)[9] <- "dis2013"
tmp$dis1994   <- ave(x = tmp$dis1994,   as.factor(tmp$edon),   FUN = function(x) max(x), na.rm=TRUE)
tmp$dis1997   <- ave(x = tmp$dis1997,   as.factor(tmp$edon),   FUN = function(x) max(x), na.rm=TRUE)
tmp$dis2000   <- ave(x = tmp$dis2000,   as.factor(tmp$edon),   FUN = function(x) max(x), na.rm=TRUE)
tmp$dis2003   <- ave(x = tmp$dis2003,   as.factor(tmp$edon),   FUN = function(x) max(x), na.rm=TRUE)
tmp$dis2006   <- ave(x = tmp$dis2006,   as.factor(tmp$edon),   FUN = function(x) max(x), na.rm=TRUE)
tmp$dis2009   <- ave(x = tmp$dis2009,   as.factor(tmp$edon),   FUN = function(x) max(x), na.rm=TRUE)
tmp$dis2012   <- ave(x = tmp$dis2012,   as.factor(tmp$edon),   FUN = function(x) max(x), na.rm=TRUE)
tmp$dis2013   <- ave(x = tmp$dis2013,   as.factor(tmp$edon),   FUN = function(x) max(x), na.rm=TRUE)
tmp$dis2015   <- tmp$dis2012
tmp <- tmp[duplicated(tmp$edon)==FALSE,] # drops redundant rows after aggregating states
app.edos <- tmp
head(app.edos)
#
# aggregate state and national pop
tmp <- eq[,c("edon","ptot05","ptot10")]
tmp$ptot05 <- ave(tmp$ptot05, as.factor(tmp$edon), FUN=sum, na.rm=TRUE) # state totals
tmp$ptot10 <- ave(tmp$ptot10, as.factor(tmp$edon), FUN=sum, na.rm=TRUE)
tmp <- tmp[duplicated(tmp$edon)==FALSE,] # drops redundant rows after aggregating states
tmp <- tmp[order(tmp$edon),] # sort
tmp2 <- tmp # will be used below
tmp.b <- (tmp$ptot10 * 2005 - tmp$ptot05 * 2010) / (2005 - 2010); tmp.a <- (tmp$ptot05 - tmp.b) / 2005; # interpolation parameters
tmp$ptot1994 <- round(x = tmp.a * 1994 + tmp.b, digits = 0);
tmp$ptot1997 <- round(x = tmp.a * 1997 + tmp.b, digits = 0);
tmp$ptot2000 <- round(x = tmp.a * 2000 + tmp.b, digits = 0);
tmp$ptot2003 <- round(x = tmp.a * 2003 + tmp.b);
tmp$ptot2006 <- round(x = tmp.a * 2006 + tmp.b);
tmp$ptot2009 <- round(x = tmp.a * 2009 + tmp.b);
tmp$ptot2010 <- round(x = tmp.a * 2010 + tmp.b);
tmp$ptot2012 <- round(x = tmp.a * 2012 + tmp.b);
tmp$ptot2013 <- round(x = tmp.a * 2013 + tmp.b);
tmp$ptot2015 <- round(x = tmp.a * 2015 + tmp.b);
tmp$ptot2018 <- round(x = tmp.a * 2018 + tmp.b);
#tmp$ptot1995 <- round(x = tmp.a * 1995 + tmp.b); # uncomment to compare extra-  and intra-polations, should be dropped
tmp$ptot05 <- tmp$ptot10 <- NULL;
pob.edos <- tmp
pob.nal <- apply(X = pob.edos, MARGIN = 2, sum); pob.nal <- pob.nal[-1]
head(pob.edos)
#
# do state population using conteo 1995 and censo 2000
tmp <- tmp2; rm(tmp2)
# 1995 conteo
pd <- "~/Dropbox/data/elecs/MXelsCalendGovt/censos/conteo1995"
tmp95 <- read.csv(file = paste(pd, "ptot1995.csv", sep = "/"))
tmp95 <- tmp95[tmp95$inegi==0,]
tmp95 <- tmp95[-which(tmp95$mun=="Otros Municipios"),]
tmp95 <- tmp95[,c("edon","ptot")]
tmp$ptot95 <- tmp95$ptot
rm(tmp95)
# 2000 census
pd <- "~/Dropbox/data/elecs/MXelsCalendGovt/censos"
tmp00 <- read.csv(file = paste(pd, "ptot2000mu.csv", sep = "/"))
tmp00$edon <- as.integer(tmp00$edomun / 1000)
tmp00$ptot <- ave(tmp00$ptot, as.factor(tmp00$edon), FUN=sum, na.rm=TRUE) # state totals
tmp00 <- tmp00[duplicated(tmp00$edon)==FALSE,] # drops redundant rows after aggregating states
tmp00 <- tmp00[,c("edon","ptot")]
tmp$ptot00 <- tmp00$ptot
rm(tmp00)
#
tmp.b <- (tmp$ptot00 * 1995 - tmp$ptot95 * 2000) / (1995 - 2000); tmp.a <- (tmp$ptot95 - tmp.b) / 1995; # interpolation parameters
tmp$ptot1994 <- round(x = tmp.a * 1994 + tmp.b, digits = 0);
tmp$ptot1997 <- round(x = tmp.a * 1997 + tmp.b, digits = 0);
tmp$ptot2000 <- tmp$ptot00;
tmp.b <- (tmp$ptot05 * 2000 - tmp$ptot00 * 2005) / (2000 - 2005); tmp.a <- (tmp$ptot00 - tmp.b) / 2000; # interpolation parameters
tmp$ptot2003 <- round(x = tmp.a * 2003 + tmp.b, digits = 0);
tmp.b <- (tmp$ptot10 * 2005 - tmp$ptot05 * 2010) / (2005 - 2010); tmp.a <- (tmp$ptot05 - tmp.b) / 2005; # interpolation parameters
tmp$ptot2006 <- round(x = tmp.a * 2006 + tmp.b, digits = 0);
tmp$ptot2009 <- round(x = tmp.a * 2009 + tmp.b, digits = 0);
tmp$ptot2010 <- round(x = tmp.a * 2010 + tmp.b, digits = 0);
tmp$ptot2012 <- round(x = tmp.a * 2012 + tmp.b, digits = 0);
tmp$ptot2013 <- round(x = tmp.a * 2013 + tmp.b, digits = 0);
tmp$ptot2015 <- round(x = tmp.a * 2015 + tmp.b, digits = 0);
tmp$ptot2018 <- round(x = tmp.a * 2018 + tmp.b, digits = 0);
#
## # COMPARE conteo1995 and censo2000 with 05-10 extrapolations by state (needs a commented line above)
## edos <- c("ags", "bc", "bcs", "cam", "coa", "col", "cps", "cua", "df", "dgo", "gua", "gue", "hgo", "jal", "mex", "mic", "mor", "nay", "nl", "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac")
## data.frame(edo=edos, est95=pob.edos$ptot1995, real95=tmp$ptot95, dif=(pob.edos$ptot1995 - tmp$ptot95)*100 / tmp$ptot95)
## #
## # this is what comes out
##    edo    est95   real95         dif
## 1  ags   826256   862720  -4.2266320
## 2   bc  2223267  2112140   5.2613463
## 3  bcs   262458   375494 -30.1032773
## 4  cam   619308   642516  -3.6120501
## 5  coa  1988818  2173775  -8.5085623
## 6  col   402878   488028 -17.4477694
## 7  cps  3287217  3584786  -8.3008860
## 8  cua  2911402  2793537   4.2192031
## 9   df  8460588  8489007  -0.3347741
## 10 dgo  1254046  1431748 -12.4115417
## 11 gua  3708692  4406568 -15.8371776
## 12 gue  2568070  2916567 -11.9488769
## 13 hgo  1706506  2112473 -19.2176184
## 14 jal  5554975  5991176  -7.2807242
## 15 mex 11666516 11707964  -0.3540154
## 16 mic  3196145  3870604 -17.4251616
## 17 mor  1284243  1442662 -10.9810198
## 18 nay   679094   896702 -24.2675939
## 19  nl  3290960  3550114  -7.2998783
## 20 oax  2916539  3228895  -9.6737738
## 21 pue  4589741  4624365  -0.7487298
## 22 que  1138543  1250476  -8.9512314
## 23 qui   754771   703536   7.2824987
## 24 san  2060206  2200763  -6.3867395
## 25 sin  2289804  2425675  -5.6013687
## 26 son  1859623  2085536 -10.8323712
## 27 tab  1492701  1748769 -14.6427573
## 28 tam  2535606  2527328   0.3275396
## 29 tla   864749   883924  -2.1693041
## 30 ver  6044254  6737324 -10.2870220
## 31 yuc  1545690  1556622  -0.7022900
## 32 zac  1121740  1336496 -16.0685853
#
## # compare 2006 population with estimate using 1995-2000 rate of change (what ife used in 2004-5)
## tmp.b <- (pob.edos$ptot2000 * 1994 - pob.edos$ptot1994 * 2000) / (1994 - 2000); tmp.a <- (pob.edos$ptot1994 - tmp.b) / 1994; # interpolation parameters
## pob.edos$est2006 <- round(x = tmp.a * 2006 + tmp.b, digits = 1);
## pob.edos$dif <- (pob.edos$est2006 - pob.edos$ptot2006)*100 / pob.edos$ptot2006
## THIS IS WHAT COMES OUT data.frame(edon=pob.edos$edon, dif=pob.edos$dif)
## edon,          dif
##    1,  -4.33008486
##    2,   1.06826249
##    3, -10.21035445
##    4,  -2.57395818
##    5,  -3.87353791
##    6,   4.04408494
##    7,  -1.58995631
##    8,   2.73948464
##    9,  -0.02551747
##   10,  -4.10863132
##   11,  -0.82865752
##   12,   3.32601978
##   13,  -1.08250343
##   14,  -2.22406647
##   15,   3.67355813
##   16,   1.99544108
##   17,   2.71557604
##   18,  -2.90537019
##   19,  -2.68411760
##   20,   3.49874602
##   21,   2.87413830
##   22,  -3.35727958
##   23,  -7.89934573
##   24,  -1.13513547
##   25,   1.13399735
##   26,  -3.00998413
##   27,   1.16708568
##   28,  -1.58816778
##   29,  -2.88832974
##   30,  -1.41134379
##   31,  -3.58332512
##   32,  -1.30289229
#
tmp$ptot05 <- tmp$ptot10 <- tmp$ptot00 <- tmp$ptot95 <- NULL
#
pob.edos <- tmp # this replaces the 2005--2010 state extrapolations prepared above with 1995--2000--2005--2010 interpolations (used only for state aggregates, not section pop (no section-level data for 1995 and 2000)
pob.nal <- apply(X = pob.edos, MARGIN = 2, sum); pob.nal <- pob.nal[-1]
head(pob.edos)
#
# turn them into target size
#
app.structures <- app.edos[,c(1:3,6,9)];
names(app.structures) <- c("edon","map1979","map1997","map2006","map2015p")
head(app.structures)
#
target.edos.func <- function(apportionment2use = "s"){
    # apportionment2use <- "s" # debug
    if (apportionment2use == "h"){
        apportionment <- app.structures$map1979
    } else {
        if (apportionment2use == "l"){
            apportionment <- app.structures$map1997
        } else {
            if (apportionment2use == "s"){
                apportionment <- app.structures$map2006
            } else {
                if (apportionment2use == "r"){
                    apportionment <- app.structures$map2015p
                } else {
                    print("Valid apportionment2use options are 'h' for map1979 (heroles), 'l' for map1997 (lujambio), 's' for map2006 (sanchez), and 'r' for map2015p (rojano)"); apportionment <- rep(0, 32)}
            }
        }
    }
    target.edos <- pob.edos;
    # j <- 1 # debug
    for (j in 2:ncol(target.edos)){
        target.edos[,j] <- round( target.edos[,j] / apportionment, digits = 0 )
    }
    # head(target.edos) # debug
    return(target.edos)
    ## END FUNCTION. USE IT INSTEAD IF A STATIC target.edos BELOW
}
#
head( target.edos.func(apportionment2use = "s") ) # debug
#
target.nal <- pob.nal / 300
target.nal
#
# state deputy apportionment quotas
natQ <- pob.nal / 300; names(natQ) <- gsub(x = names(natQ), pattern = "ptot", replacement = "q")
stateQ <- pob.edos; colnames(stateQ) <- gsub(x = colnames(stateQ), pattern = "ptot", replacement = "q")
for (j in 2:ncol(pob.edos)){
    stateQ[,j] <- pob.edos[,j] / natQ[j-1];
}
rm(j)
# state over/under-representation
tmp6a <- app.edos[,-1]; tmp6s <- stateQ[,-1]
state.overep <- round( tmp6a - tmp6s, digits = 2); colnames(state.overep) <- gsub(x = colnames(state.overep), pattern = "dis", replacement = "dif")
state.overep.rel <- round( state.overep / tmp6a, digits = 2)
state.overep <- cbind(edon = app.edos$edon, state.overep)
state.overep.rel <- cbind(edon= app.edos$edon, state.overep.rel)
rm(tmp6a, tmp6s)
head(state.overep)
#
# aggregates district populations
#
y <- 2012 # sum seccion populations into districts actually used in 2012 election
tmp <- eq[,c("edon","dis2012","ptot05","ptot10")]; colnames(tmp)[2] <- "disn"
tmp$ptot05 <- ave(tmp$ptot05, as.factor(tmp$edon+tmp$disn/100), FUN=sum, na.rm=TRUE)
tmp$ptot10 <- ave(tmp$ptot10, as.factor(tmp$edon+tmp$disn/100), FUN=sum, na.rm=TRUE)
tmp <- tmp[duplicated(tmp$edon+tmp$disn/100)==FALSE,] # drops redundabt rows after aggregating districts from secciones
print(paste("Total 2005 population unassigned to a district in", y, "election =", sum(tmp$ptot05[which(tmp$disn==0)])))
print(paste("Total 2010 population unassigned to a district in", y, "election =", sum(tmp$ptot10[which(tmp$disn==0)])))
tmp <- tmp[-which(tmp$disn==0),] # removes secciones not assigned to some district
tmp <- tmp[order(tmp$edon, tmp$disn),]
tmp.b <- (tmp$ptot10 * 2005 - tmp$ptot05 * 2010) / (2005 - 2010); tmp.a <- (tmp$ptot05 - tmp.b) / 2005
tmp$ptot1994 <- round(x = tmp.a * 1994 + tmp.b, digits = 0);
tmp$ptot1997 <- round(x = tmp.a * 1997 + tmp.b, digits = 0);
tmp$ptot2000 <- round(x = tmp.a * 2000 + tmp.b, digits = 0);
tmp$ptot2003 <- round(x = tmp.a * 2003 + tmp.b, digits = 0);
tmp$ptot2006 <- round(x = tmp.a * 2006 + tmp.b, digits = 0);
tmp$ptot2009 <- round(x = tmp.a * 2009 + tmp.b, digits = 0);
tmp$ptot2010 <- round(x = tmp.a * 2010 + tmp.b, digits = 0);
tmp$ptot2012 <- round(x = tmp.a * 2012 + tmp.b, digits = 0);
tmp$ptot2013 <- round(x = tmp.a * 2013 + tmp.b, digits = 0);
tmp$ptot2015 <- round(x = tmp.a * 2015 + tmp.b, digits = 0);
tmp$ptot2018 <- round(x = tmp.a * 2018 + tmp.b, digits = 0);
tmp$ptot05 <- tmp$ptot10 <- NULL;
tmp$ptot05 <- tmp$ptot10 <- NULL;
pob.distMap2006 <- tmp
head(tmp)
#
# 2013.1    sum seccion populations into first redistricting proposal
y <- 2013
tmp <- eq[,c("edon","dis2013.1","ptot05","ptot10")]; colnames(tmp)[2] <- "disn"
tmp$ptot05 <- ave(tmp$ptot05, as.factor(tmp$edon+tmp$disn/100), FUN=sum, na.rm=TRUE)
tmp$ptot10 <- ave(tmp$ptot10, as.factor(tmp$edon+tmp$disn/100), FUN=sum, na.rm=TRUE)
tmp <- tmp[duplicated(tmp$edon+tmp$disn/100)==FALSE,] # drops redundabt rows after aggregating districts from secciones
print(paste("Total 2005 population unassigned to a district in", y, "=", sum(tmp$ptot05[which(tmp$disn==0)])))
print(paste("Total 2010 population unassigned to a district in", y, "=", sum(tmp$ptot10[which(tmp$disn==0)])))
tmp <- tmp[-which(tmp$disn==0),] # removes secciones not assigned to some district
tmp <- tmp[order(tmp$edon, tmp$disn),]
tmp.b <- (tmp$ptot10 * 2005 - tmp$ptot05 * 2010) / (2005 - 2010); tmp.a <- (tmp$ptot05 - tmp.b) / 2005
tmp$ptot1994 <- round(x = tmp.a * 1994 + tmp.b, digits = 0);
tmp$ptot1997 <- round(x = tmp.a * 1997 + tmp.b, digits = 0);
tmp$ptot2000 <- round(x = tmp.a * 2000 + tmp.b, digits = 0);
tmp$ptot2003 <- round(x = tmp.a * 2003 + tmp.b, digits = 0);
tmp$ptot2006 <- round(x = tmp.a * 2006 + tmp.b, digits = 0);
tmp$ptot2009 <- round(x = tmp.a * 2009 + tmp.b, digits = 0);
tmp$ptot2010 <- round(x = tmp.a * 2010 + tmp.b, digits = 0);
tmp$ptot2012 <- round(x = tmp.a * 2012 + tmp.b, digits = 0);
tmp$ptot2013 <- round(x = tmp.a * 2013 + tmp.b, digits = 0);
tmp$ptot2015 <- round(x = tmp.a * 2015 + tmp.b, digits = 0);
tmp$ptot2018 <- round(x = tmp.a * 2018 + tmp.b, digits = 0);
tmp$ptot05 <- tmp$ptot10 <- NULL;
tmp$ptot05 <- tmp$ptot10 <- NULL;
pob.distMap2015p1 <- tmp
#
# 2013.3        sum seccion populations into third (final) redistricting proposal
y <- 2013
tmp <- eq[,c("edon","dis2013.3","ptot05","ptot10")]; colnames(tmp)[2] <- "disn"
tmp$ptot05 <- ave(tmp$ptot05, as.factor(tmp$edon+tmp$disn/100), FUN=sum, na.rm=TRUE)
tmp$ptot10 <- ave(tmp$ptot10, as.factor(tmp$edon+tmp$disn/100), FUN=sum, na.rm=TRUE)
tmp <- tmp[duplicated(tmp$edon+tmp$disn/100)==FALSE,] # drops redundabt rows after aggregating districts from secciones
print(paste("Total 2005 population unassigned to a district in", y, "=", sum(tmp$ptot05[which(tmp$disn==0)])))
print(paste("Total 2010 population unassigned to a district in", y, "=", sum(tmp$ptot10[which(tmp$disn==0)])))
tmp <- tmp[-which(tmp$disn==0),] # removes secciones not assigned to some district
tmp <- tmp[order(tmp$edon, tmp$disn),]
tmp.b <- (tmp$ptot10 * 2005 - tmp$ptot05 * 2010) / (2005 - 2010); tmp.a <- (tmp$ptot05 - tmp.b) / 2005
tmp$ptot1994 <- round(x = tmp.a * 1994 + tmp.b, digits = 0);
tmp$ptot1997 <- round(x = tmp.a * 1997 + tmp.b, digits = 0);
tmp$ptot2000 <- round(x = tmp.a * 2000 + tmp.b, digits = 0);
tmp$ptot2003 <- round(x = tmp.a * 2003 + tmp.b, digits = 0);
tmp$ptot2006 <- round(x = tmp.a * 2006 + tmp.b, digits = 0);
tmp$ptot2009 <- round(x = tmp.a * 2009 + tmp.b, digits = 0);
tmp$ptot2010 <- round(x = tmp.a * 2010 + tmp.b, digits = 0);
tmp$ptot2012 <- round(x = tmp.a * 2012 + tmp.b, digits = 0);
tmp$ptot2013 <- round(x = tmp.a * 2013 + tmp.b, digits = 0);
tmp$ptot2015 <- round(x = tmp.a * 2015 + tmp.b, digits = 0);
tmp$ptot2018 <- round(x = tmp.a * 2018 + tmp.b, digits = 0);
tmp$ptot05 <- tmp$ptot10 <- NULL;
tmp$ptot05 <- tmp$ptot10 <- NULL;
pob.distMap2015p3 <- tmp
#
head(pob.distMap2015p3)
dim(pob)
#
# clean
rm(tmp.a, tmp.b, tmp, i, fl, pob, pob05, pob10, edo, y, tempobj, c05, c10)
ls()
#

#################################################
# prepare district populations to target ratios
#################################################
save.image(file = "tmp.RData") # debug
rm(list = ls())                # debug
wd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/git-repo/mex-open-map/data/")
setwd(wd)
load(file = "tmp.RData")       # debug

tmp <- tmp2 <- pob.distMap2006
head(tmp)
i <- 2 # debug
for (i in 1:300){
    tmp[i, 3:13] <-   tmp[i, 3:13] / target.edos.func("s")[tmp$edon[i], 2:12];
    tmp2[i, 3:13] <- tmp2[i, 3:13] / target.nal;
}
colnames(tmp)[3:13] <- paste("rels",  c(seq(from = 1994, to = 2009, by = 3), 2010, 2012, 2013, 2015, 2018), sep = "")
colnames(tmp2)[3:13] <- paste("reln", c(seq(from = 1994, to = 2009, by = 3), 2010, 2012, 2013, 2015, 2018), sep = "")
pob.distMap2006 <- cbind(pob.distMap2006, tmp[,3:13], tmp2[,3:13])
head(pob.distMap2006)
#
tmp <- tmp2 <- pob.distMap2015p1
for (i in 1:300){
    tmp[i, 3:13] <-   tmp[i, 3:13] / target.edos.func("r")[tmp$edon[i], 2:12];
    tmp2[i, 3:13] <- tmp2[i, 3:13] / target.nal;
}
colnames(tmp)[3:13] <- paste("rels",  c(seq(from = 1994, to = 2009, by = 3), 2010, 2012, 2013, 2015, 2018), sep = "")
colnames(tmp2)[3:13] <- paste("reln", c(seq(from = 1994, to = 2009, by = 3), 2010, 2012, 2013, 2015, 2018), sep = "")
pob.distMap2015p1 <- cbind(pob.distMap2015p1, tmp[,3:13], tmp2[,3:13])
head(pob.distMap2015p1)
#
tmp <- tmp2 <- pob.distMap2015p3
for (i in 1:300){
    tmp[i, 3:13] <-   tmp[i, 3:13] / target.edos.func("r")[tmp$edon[i], 2:12];
    tmp2[i, 3:13] <- tmp2[i, 3:13] / target.nal;
}
colnames(tmp)[3:13] <- paste("rels",  c(seq(from = 1994, to = 2009, by = 3), 2010, 2012, 2013, 2015, 2018), sep = "")
colnames(tmp2)[3:13] <- paste("reln", c(seq(from = 1994, to = 2009, by = 3), 2010, 2012, 2013, 2015, 2018), sep = "")
pob.distMap2015p3 <- cbind(pob.distMap2015p3, tmp[,3:13], tmp2[,3:13])
head(pob.distMap2015p3)
#
rm(tmp, tmp2)

#
# paste district populations to election objects
tmp <- pob.distMap2006[, c("edon","disn","ptot2006","rels2006","reln2006")]; colnames(tmp) <- c("edon", "disn", "ptot", "rels", "reln")
df2006d0 <- merge(x = df2006d0, y = tmp, by = c("edon", "disn"))
df2006s0 <- merge(x = df2006s0, y = tmp, by = c("edon", "disn"))
tmp <- pob.distMap2015p1[, c("edon","disn","ptot2006","rels2006","reln2006")]; colnames(tmp) <- c("edon", "disn", "ptot", "rels", "reln")
df2006d1 <- merge(x = df2006d1, y = tmp, by = c("edon", "disn"))
df2006s1 <- merge(x = df2006s1, y = tmp, by = c("edon", "disn"))
tmp <- pob.distMap2015p3[, c("edon","disn","ptot2006","rels2006","reln2006")]; colnames(tmp) <- c("edon", "disn", "ptot", "rels", "reln")
df2006d3 <- merge(x = df2006d3, y = tmp, by = c("edon", "disn"))
df2006s3 <- merge(x = df2006s3, y = tmp, by = c("edon", "disn"))
#
tmp <- pob.distMap2006[, c("edon","disn","ptot2009","rels2009","reln2009")]; colnames(tmp) <- c("edon", "disn", "ptot", "rels", "reln")
df2009d0 <- merge(x = df2009d0, y = tmp, by = c("edon", "disn"))
df2009s0 <- merge(x = df2009s0, y = tmp, by = c("edon", "disn"))
tmp <- pob.distMap2015p1[, c("edon","disn","ptot2009","rels2009","reln2009")]; colnames(tmp) <- c("edon", "disn", "ptot", "rels", "reln")
df2009d1 <- merge(x = df2009d1, y = tmp, by = c("edon", "disn"))
df2009s1 <- merge(x = df2009s1, y = tmp, by = c("edon", "disn"))
tmp <- pob.distMap2015p3[, c("edon","disn","ptot2009","rels2009","reln2009")]; colnames(tmp) <- c("edon", "disn", "ptot", "rels", "reln")
df2009d3 <- merge(x = df2009d3, y = tmp, by = c("edon", "disn"))
df2009s3 <- merge(x = df2009s3, y = tmp, by = c("edon", "disn"))
#
tmp <- pob.distMap2006[, c("edon","disn","ptot2012","rels2012","reln2012")]; colnames(tmp) <- c("edon", "disn", "ptot", "rels", "reln")
df2012d0 <- merge(x = df2012d0, y = tmp, by = c("edon", "disn"))
df2012s0 <- merge(x = df2012s0, y = tmp, by = c("edon", "disn"))
tmp <- pob.distMap2015p1[, c("edon","disn","ptot2012","rels2012","reln2012")]; colnames(tmp) <- c("edon", "disn", "ptot", "rels", "reln")
df2012d1 <- merge(x = df2012d1, y = tmp, by = c("edon", "disn"))
df2012s1 <- merge(x = df2012s1, y = tmp, by = c("edon", "disn"))
tmp <- pob.distMap2015p3[, c("edon","disn","ptot2012","rels2012","reln2012")]; colnames(tmp) <- c("edon", "disn", "ptot", "rels", "reln")
df2012d3 <- merge(x = df2012d3, y = tmp, by = c("edon", "disn"))
df2012s3 <- merge(x = df2012s3, y = tmp, by = c("edon", "disn"))

## COMPUTE RELATIVE REPRESENTATION INDEX FOR DISTRICTS VIS-A-VIS STATE/NAT AVERAGES (ANSOLABEHERE ET AL 2002)
tmp <- df2006d0; tmp$rris <- 1/tmp$rels; tmp$rrin <- 1/tmp$reln; df2006d0 <- tmp
tmp <- df2006d1; tmp$rris <- 1/tmp$rels; tmp$rrin <- 1/tmp$reln; df2006d1 <- tmp
tmp <- df2006d3; tmp$rris <- 1/tmp$rels; tmp$rrin <- 1/tmp$reln; df2006d3 <- tmp
#
tmp <- df2009d0; tmp$rris <- 1/tmp$rels; tmp$rrin <- 1/tmp$reln; df2009d0 <- tmp
tmp <- df2009d1; tmp$rris <- 1/tmp$rels; tmp$rrin <- 1/tmp$reln; df2009d1 <- tmp
tmp <- df2009d3; tmp$rris <- 1/tmp$rels; tmp$rrin <- 1/tmp$reln; df2009d3 <- tmp
#
tmp <- df2012d0; tmp$rris <- 1/tmp$rels; tmp$rrin <- 1/tmp$reln; df2012d0 <- tmp
tmp <- df2012d1; tmp$rris <- 1/tmp$rels; tmp$rrin <- 1/tmp$reln; df2012d1 <- tmp
tmp <- df2012d3; tmp$rris <- 1/tmp$rels; tmp$rrin <- 1/tmp$reln; df2012d3 <- tmp
#
# prepare (until now, non-existant) df2015 with rri
tmp <- pob.distMap2006
tmp <- tmp[,c("edon","disn","ptot2015","rels2015","reln2015")]; colnames(tmp) <- sub(pattern = "2015", replacement = "", colnames(tmp))
tmp$rris <- 1/tmp$rels; tmp$rrin <- 1/tmp$reln;
df2015d0 <- tmp
#
tmp <- pob.distMap2015p3
tmp <- tmp[,c("edon","disn","ptot2015","rels2015","reln2015")]; colnames(tmp) <- sub(pattern = "2015", replacement = "", colnames(tmp))
tmp$rris <- 1/tmp$rels; tmp$rrin <- 1/tmp$reln;
df2015d3 <- tmp
#
# prepare (until now, non-existant) df2018 with rri
tmp <- pob.distMap2006
tmp <- tmp[,c("edon","disn","ptot2018","rels2018","reln2018")]; colnames(tmp) <- sub(pattern = "2018", replacement = "", colnames(tmp))
tmp$rris <- 1/tmp$rels; tmp$rrin <- 1/tmp$reln;
df2018d0 <- tmp
#
tmp <- pob.distMap2015p3
tmp <- tmp[,c("edon","disn","ptot2018","rels2018","reln2018")]; colnames(tmp) <- sub(pattern = "2018", replacement = "", colnames(tmp))
tmp$rris <- 1/tmp$rels; tmp$rrin <- 1/tmp$reln;
df2018d3 <- tmp
#
# prepare (until now, non-existant) df2000 with rri
tmp <- pob.distMap2006
tmp <- tmp[,c("edon","disn","ptot2000","rels2000","reln2000")]; colnames(tmp) <- sub(pattern = "2000", replacement = "", colnames(tmp))
tmp$rris <- 1/tmp$rels; tmp$rrin <- 1/tmp$reln;
df2000d0 <- tmp
#
tmp <- pob.distMap2015p3
tmp <- tmp[,c("edon","disn","ptot2000","rels2000","reln2000")]; colnames(tmp) <- sub(pattern = "2000", replacement = "", colnames(tmp))
tmp$rris <- 1/tmp$rels; tmp$rrin <- 1/tmp$reln;
df2000d3 <- tmp
#
# prepare fake df2010 with rri (for use when graphing rris only, no elec results, but with structure that code below recognizes)
tmp <- pob.distMap2006
tmp <- tmp[,c("edon","disn","ptot2010","rels2010","reln2010")]; colnames(tmp) <- sub(pattern = "2010", replacement = "", colnames(tmp))
tmp$rris <- 1/tmp$rels; tmp$rrin <- 1/tmp$reln;
df2010d0 <- tmp
#
tmp <- pob.distMap2015p3
tmp <- tmp[,c("edon","disn","ptot2010","rels2010","reln2010")]; colnames(tmp) <- sub(pattern = "2010", replacement = "", colnames(tmp))
tmp$rris <- 1/tmp$rels; tmp$rrin <- 1/tmp$reln;
df2010d3 <- tmp
head(df2010d3)
#
# this block shows that inverting mu rels achieves rris
## # select year and map
## tmp.dis <- pob.distMap2006 # df2012d0 has target pop instead
## tmp.st  <- pob.edos
## tmp.nat <- apply(tmp.st, 2, sum)
## tmp.ndis  <- df2012s0
## #
## tmp <- tmp.dis[,c("edon","disn")] # object tha will receive index
## tmp$rris <- tmp$rrin <- NA
## for (i in 1:300){
##     #i <- 1 # debug
##     tmp$rris[i] <- (1 / tmp.dis$ptot2012[i]) / (tmp.ndis$ndis[which(tmp.ndis$edon==tmp$edon[i])] / tmp.st$ptot2012[which(tmp.st$edon==tmp$edon[i])])
##     tmp$rrin[i] <- (1 / tmp.dis$ptot2012[i]) / (                                             300 / tmp.nat[which(names(tmp.nat)=="ptot2012")]      )
## }
## # my reln and rels are 1/rrin and 1/rris
## head(1/tmp)
## head(tmp.dis)
## #
## head(df2012d0[order(df2012d0$edon, df2012d0$disn),])
#

summary(df2015d0$ptot)
tail(df2015d0[order(df2015d0$ptot),])

################################################################################
## ADDS MARGIN (positive if winner, negative  difference to winner otherwise) ##
################################################################################
# 2006 easy, no partial coalitions
addMargin <- function(object = df2006d0, votecols = 3:7){
    work <- object;
    work$prdcm <- work$pricm <- work$panm <- 0;
    for (i in 1:nrow(work)){
        vots <- work[i,votecols]; work.row <- work[i,];
        index.1st <- which(-rank(vots, ties.method = "max") + length(votecols) + 1 == 1);
        index.2nd <- which(-rank(vots, ties.method = "max") + length(votecols) + 1 == 2);
        vot1 <- as.numeric(vots[index.1st]); vot2 <- as.numeric(vots[index.2nd]);
        work.row$panm  <- ifelse(work.row$panw ==1, vot1 - vot2, work.row$pan  - vot1);
        work.row$pricm <- ifelse(work.row$pricw==1, vot1 - vot2, work.row$pric - vot1);
        work.row$prdcm <- ifelse(work.row$prdcw==1, vot1 - vot2, work.row$prdc - vot1);
        work.row$panm <- round(work.row$panm / work.row$efec, digits = 3);
        work.row$pricm <- round(work.row$pricm / work.row$efec, digits = 3);
        work.row$prdcm <- round(work.row$prdcm / work.row$efec, digits = 3);
        work[i,] <- work.row;
    }
    return(work)
}
#
df2006d0 <- addMargin(object = df2006d0, votecols = 3:7)
df2006d1 <- addMargin(object = df2006d1, votecols = 3:7)
df2006d3 <- addMargin(object = df2006d3, votecols = 3:7)
#
# 2009 harder, partial coalitions
addMargin <- function(object = df2009d0, votecols = 3:9){
    # i <- 253; object <- df2009d3; votecols <- 3:9 ## DEBUG ##
    work <- object;
    work$prdm <- work$prim <- work$panm <- 0;
    for (i in 1:nrow(work)){
        work.row <- work[i,];
        # consolidate coalition vote
        pri2coal <- work.row$pri / (work.row$pri + work.row$pvem + 1); # 1 to avoid indeterminacy
        work.row$pri <- ifelse(work.row$shSecCoalPri<.5, work.row$pri + pri2coal*work.row$pric, work.row$pri + work.row$pric + work.row$pvem);
        work.row$pvem <- ifelse(work.row$shSecCoalPri<.5, work.row$pvem + (1 - pri2coal)*work.row$pric, 0);
        work.row$pric <- 0;
        vots <- work.row[votecols];
        index.1st <- which(-rank(vots, ties.method = "max") + length(votecols) + 1 == 1);
        index.2nd <- which(-rank(vots, ties.method = "max") + length(votecols) + 1 == 2);
        vot1 <- as.numeric(vots[index.1st]); vot2 <- as.numeric(vots[index.2nd]); # votes won by 1st and 2nd places
        work.row$panm <- ifelse(work.row$panw==1, vot1 - vot2, work.row$pan  - vot1);
        work.row$prim <- ifelse( (work.row$priw==1 | work.row$pricw==1), vot1 - vot2, work.row$pri - vot1);
        work.row$prdm <- ifelse(work.row$prdw==1, vot1 - vot2, work.row$prd - vot1);
        work.row$panm <- round(work.row$panm / work.row$efec, digits = 3);        # computes shares
        work.row$prim <- round(work.row$prim / work.row$efec, digits = 3);
        work.row$prdm <- round(work.row$prdm / work.row$efec, digits = 3);
        work[i,] <- work.row;
    }
    return(work)
}
#
df2009d0 <- addMargin(object = df2009d0)
df2009d1 <- addMargin(object = df2009d1)
df2009d3 <- addMargin(object = df2009d3)
#
# head(addMargin(object = df2009d0)) # debug
#
# 2012, partial coalition present
addMargin <- function(object = df2012d3, votecols = 3:8){
    # i <- 1; object <- df2012d3; votecols <- 3:8 ## DEBUG ##
    work <- object;
    work$prdcm <- work$prim <- work$panm <- 0;
    for (i in 1:nrow(work)){
        work.row <- work[i,];
        # consolidate coalition vote
        pri2coal <- work.row$pri / (work.row$pri + work.row$pvem + 1); # 1 to avoid indeterminacy
        work.row$pri <- ifelse(work.row$shSecCoalPri<.5, work.row$pri + pri2coal*work.row$pric, work.row$pri + work.row$pric + work.row$pvem);
        work.row$pvem <- ifelse(work.row$shSecCoalPri<.5, work.row$pvem + (1 - pri2coal)*work.row$pric, 0);
        work.row$pric <- 0
        vots <- work.row[votecols];
        index.1st <- which(-rank(vots, ties.method = "max") + length(votecols) + 1 == 1);
        index.2nd <- which(-rank(vots, ties.method = "max") + length(votecols) + 1 == 2);
        vot1 <- as.numeric(vots[index.1st]); vot2 <- as.numeric(vots[index.2nd]); # votes won by 1st and 2nd places
        work.row$panm <- ifelse(work.row$panw==1, vot1 - vot2, work.row$pan  - vot1);
        work.row$prim <- ifelse( (work.row$priw==1 | work.row$pricw==1), vot1 - vot2, work.row$pri - vot1);
        work.row$prdcm <- ifelse(work.row$prdcw==1, vot1 - vot2, work.row$prdc - vot1);
        work.row$panm <- round(work.row$panm / work.row$efec, digits = 3);        # computes shares
        work.row$prim <- round(work.row$prim / work.row$efec, digits = 3);
        work.row$prdcm <- round(work.row$prdcm / work.row$efec, digits = 3);
        work[i,] <- work.row;
    }
    return(work)
}
#
df2012d0 <- addMargin(object = df2012d0)
df2012d1 <- addMargin(object = df2012d1)
df2012d3 <- addMargin(object = df2012d3)
head(df2012d0)
df2012d0$prim
#
# head(addMargin(object = df2012d3)) # debug
#
# SORT ELEC.DISTRICT DATA
df2006d0 <- df2006d0[order(df2006d0$edon, df2006d0$disn),]
df2006d1 <- df2006d1[order(df2006d1$edon, df2006d1$disn),]
df2006d3 <- df2006d3[order(df2006d3$edon, df2006d3$disn),]
df2009d0 <- df2009d0[order(df2009d0$edon, df2009d0$disn),]
df2009d1 <- df2009d1[order(df2009d1$edon, df2009d1$disn),]
df2009d3 <- df2009d3[order(df2009d3$edon, df2009d3$disn),]
df2012d0 <- df2012d0[order(df2012d0$edon, df2012d0$disn),]
df2012d1 <- df2012d1[order(df2012d1$edon, df2012d1$disn),]
df2012d3 <- df2012d3[order(df2012d3$edon, df2012d3$disn),]
#
df2006s0 <- df2006s0[order(df2006s0$edon, df2006s0$disn),]
df2006s1 <- df2006s1[order(df2006s1$edon, df2006s1$disn),]
df2006s3 <- df2006s3[order(df2006s3$edon, df2006s3$disn),]
df2009s0 <- df2009s0[order(df2009s0$edon, df2009s0$disn),]
df2009s1 <- df2009s1[order(df2009s1$edon, df2009s1$disn),]
df2009s3 <- df2009s3[order(df2009s3$edon, df2009s3$disn),]
df2012s0 <- df2012s0[order(df2012s0$edon, df2012s0$disn),]
df2012s1 <- df2012s1[order(df2012s1$edon, df2012s1$disn),]
df2012s3 <- df2012s3[order(df2012s3$edon, df2012s3$disn),]
#
# consolidate coalition victories
df2009d0$priw <- df2009d0$priw + df2009d0$pricw
df2009d1$priw <- df2009d1$priw + df2009d1$pricw
df2009d3$priw <- df2009d3$priw + df2009d3$pricw
df2009d0$pricw <- NULL
df2009d1$pricw <- NULL
df2009d3$pricw <- NULL
#
df2012d0$priw <- df2012d0$priw + df2012d0$pricw
df2012d1$priw <- df2012d1$priw + df2012d1$pricw
df2012d3$priw <- df2012d3$priw + df2012d3$pricw
df2012d0$pricw <- NULL
df2012d1$pricw <- NULL
df2012d3$pricw <- NULL
#
# remove pricm and prdcm to match other years' labels
colnames(df2006d0)[which( names(df2006d0)=="pricm" )] <- "prim"
colnames(df2006d1)[which( names(df2006d1)=="pricm" )] <- "prim"
colnames(df2006d3)[which( names(df2006d3)=="pricm" )] <- "prim"
colnames(df2006d0)[which( names(df2006d0)=="prdcm" )] <- "prdm"
colnames(df2006d1)[which( names(df2006d1)=="prdcm" )] <- "prdm"
colnames(df2006d3)[which( names(df2006d3)=="prdcm" )] <- "prdm"
colnames(df2012d0)[which( names(df2012d0)=="prdcm" )] <- "prdm"
colnames(df2012d1)[which( names(df2012d1)=="prdcm" )] <- "prdm"
colnames(df2012d3)[which( names(df2012d3)=="prdcm" )] <- "prdm"
#
colnames(df2006d0)[which( names(df2006d0)=="pricw" )] <- "priw"
colnames(df2006d1)[which( names(df2006d1)=="pricw" )] <- "priw"
colnames(df2006d3)[which( names(df2006d3)=="pricw" )] <- "priw"
colnames(df2006d0)[which( names(df2006d0)=="prdcw" )] <- "prdw"
colnames(df2006d1)[which( names(df2006d1)=="prdcw" )] <- "prdw"
colnames(df2006d3)[which( names(df2006d3)=="prdcw" )] <- "prdw"
colnames(df2012d0)[which( names(df2012d0)=="prdcw" )] <- "prdw"
colnames(df2012d1)[which( names(df2012d1)=="prdcw" )] <- "prdw"
colnames(df2012d3)[which( names(df2012d3)=="prdcw" )] <- "prdw"
#


#################################################################
#################################################################
## compute district similarity index (cox+katz book pp. 15-17) ##
#################################################################
#################################################################
tmp <- eq[,c("edon","seccion","dis2012","dis2013.1","dis2013.3","ptot10")];
#table(tmp$dis2012==0, tmp$dis2013.1==0)
tmp <- tmp[-which(tmp$dis2012==0 | tmp$dis2013.1==0 | tmp$dis2013.3==0),] # drop secciones unasiggned to any of three districting plans (these should disappear as reseccionamiento info is incorporated)
tmp$dis2012 <- round(tmp$edon + tmp$dis2012/100, digits = 2)
tmp$dis2013.1 <- round(tmp$edon + tmp$dis2013.1/100, digits = 2)
tmp$dis2013.3 <- round(tmp$edon + tmp$dis2013.3/100, digits = 2)
#
allnew <- as.numeric(names(table(tmp$dis2013.1)))
#
similarityMap2015p1with2012   <- data.frame(dis2013.1 = allnew, simSec = rep(0,300), simPob = rep(0,300), with = rep(0,300));
similarityMap2015p3with2012   <- data.frame(dis2013.3 = allnew, simSec = rep(0,300), simPob = rep(0,300), with = rep(0,300));
similarityMap2015p3with2015p1 <- data.frame(dis2013.3 = allnew, simSec = rep(0,300), simPob = rep(0,300), with = rep(0,300)); # NOT YET COMPUTED BELOW...
rownames(similarityMap2015p1with2012) <- rownames(similarityMap2015p3with2012) <- rownames(similarityMap2015p3with2015p1) <- allnew;
#
#i <- 1
for (i in 1:300){
    tmp2 <- tmp[ which(tmp$dis2013.1==allnew[i]), ]; # subset secciones from dis2013.1==i
    #head(tmp2)
    # common secciones
    c <- table(tmp2$dis2012);
    tmp3 <- as.numeric( names(   c[which( c == max(c) )]  )[1] ); # if several, take first
    c <- max(c);
    similarityMap2015p1with2012$with[i] <- tmp3;
    n <- nrow(tmp2);
    p <- nrow( tmp[ which(tmp$dis2012==tmp3), ] ); # subset secciones from dis2012==with
    similarityMap2015p1with2012$simSec[i] <- c / (p + n - c); # share of sections, cox+katz use population share
    #
    # common population
    c <- sum(tmp2$ptot10[tmp2$dis2012 == tmp3]);
    n <- sum(tmp2$ptot10);
    p <- sum(tmp$ptot10[ which(tmp$dis2012==tmp3) ] );
    similarityMap2015p1with2012$simPob[i] <- c / (p + n - c); # share of sections, cox+katz use population share
    #
    tmp2 <- tmp[ which(tmp$dis2013.3==allnew[i]), ]; # subset secciones from dis2013.3==i
    #head(tmp2)
    # common secciones
    c <- table(tmp2$dis2012);
    tmp3 <- as.numeric( names(   c[which( c == max(c) )]  )[1] ); # if several, take first
    c <- max(c);
    similarityMap2015p3with2012$with[i] <- tmp3;
    n <- nrow(tmp2);
    p <- nrow( tmp[ which(tmp$dis2012==tmp3), ] ); # subset secciones from dis2012==with
    similarityMap2015p3with2012$simSec[i] <- c / (p + n - c); # share of sections, cox+katz use population share
    #
    # common population
    c <- sum(tmp2$ptot10[tmp2$dis2012 == tmp3]);
    n <- sum(tmp2$ptot10);
    p <- sum(tmp$ptot10[ which(tmp$dis2012==tmp3) ] );
    similarityMap2015p3with2012$simPob[i] <- c / (p + n - c); # share of sections, cox+katz use population share
    #
    tmp2 <- tmp[ which(tmp$dis2013.3==allnew[i]), ]; # subset secciones from dis2013.3==i
    #head(tmp2)
    # common secciones
    c <- table(tmp2$dis2013.1);
    tmp3 <- as.numeric( names(   c[which( c == max(c) )]  )[1] ); # if several, take first
    c <- max(c);
    similarityMap2015p3with2015p1$with[i] <- tmp3;
    n <- nrow(tmp2);
    p <- nrow( tmp[ which(tmp$dis2013.1==tmp3), ] ); # subset secciones from dis2013.1==with
    similarityMap2015p3with2015p1$simSec[i] <- c / (p + n - c); # share of sections, cox+katz use population share
    #
    # common population
    c <- sum(tmp2$ptot10[tmp2$dis2013.1 == tmp3]);
    n <- sum(tmp2$ptot10);
    p <- sum(tmp$ptot10[ which(tmp$dis2013.1==tmp3) ] );
    similarityMap2015p3with2015p1$simPob[i] <- c / (p + n - c); # share of sections, cox+katz use population share
}
#head(similarityMap2015p3with2012)
#
print( paste("Largest pop. change from d1 to d3 was", round( max( abs (similarityMap2015p1with2012$simPob - similarityMap2015p3with2012$simPob) ), digits = 2) ) )
print( paste("Mean pop. change was", round ( mean( abs (similarityMap2015p1with2012$simPob - similarityMap2015p3with2012$simPob) ) , digits = 2 ) ) )
print( paste("Smallest was", round( min( abs (similarityMap2015p1with2012$simPob - similarityMap2015p3with2012$simPob) ), digits = 2) ) )
#
quantile(similarityMap2015p3with2015p1$simPob)
table(similarityMap2015p3with2015p1$simPob==1)
rm(i, n, natQ, stateQ, tmp, tmp2, tmp3)



##############################################################################################################################################
## 2006 MAP DISTRICT VOLATILITY AND MEAN MARGIN (HOW CAN THEY BE COMPOUNDED IN A MEASURE? MG/VOL SEEMS NOT TO WORK ... FINANCE LITERATURE?) ##
##############################################################################################################################################
head(df2006d0) # debug
m1 <- df2006d0[,c("panm","prim","prdm")]
m2 <- df2009d0[,c("panm","prim","prdm")]
m3 <- df2012d0[,c("panm","prim","prdm")]
#
v1 <- df2006d0[,c("pan","pric","prdc","panal","asdc")] / df2006d0$efec
v2 <- df2009d0[,c("pan","pri","prd","pvem","panal","ptc")] / df2009d0$efec
v3 <- df2012d0[,c("pan","pri","prdc","pvem","panal")] / df2012d0$efec
#
# homogenize columns
v1 <- cbind(v1, pvem = rep(0,300), pt = rep(0,300)); v1 <- v1[,c("pan","pric","prdc","pvem","pt","panal","asdc")]
v2 <- cbind(v2, asdc = rep(0,300)); v2 <- v2[,c("pan","pri","prd","pvem","ptc","panal","asdc")]
v3 <- cbind(v3, asdc = rep(0,300), pt = rep(0,300)); v3 <- v3[,c("pan","pri","prdc","pvem","pt","panal","asdc")]
#
tmp <- cbind( (v2 - v1), (v3 - v2)); tmp2 <- tmp^2
vol.sq  <- apply(X = tmp2, MARGIN = 1, FUN = function(X) sqrt(sum(X) / 2) )
vol.max <- apply(X = tmp, MARGIN = 1, FUN = function(X) max(abs(X)) )
#
tmp <- cbind( m1$panm, m2$panm, m3$panm);
mean.margin <- apply( tmp, 1, mean)
tmp <- cbind( m1$prim, m2$prim, m3$prim);
head(tmp)
mean.margin <- cbind( mean.margin, apply( tmp, 1, mean))
tmp <- cbind( m1$prdm, m2$prdm, m3$prdm);
mean.margin <- cbind( mean.margin, apply( tmp, 1, mean))
colnames(mean.margin) <- c("pan","pri","prd")
#
head(vol.sq)
head(mean.margin)
summary(vol.sq)
summary(mean.margin)
#
district.volatility <- list(vol.max=vol.max, vol.sq=vol.sq); rm(vol.max, vol.sq)
rm(tmp,tmp2,v1,v2,v3,m1,m2,m3)



# DISTRICT ELASTICITY TO STATE CHANGE (REGRESS EACH STATE'S SECTIONS ON DISTRICT DUMMIES)
# RUN THIS FROM THIS POINT IF DATA CHANGES... ELSE LOAD ELASTICITY RESULTS
#
## cd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/code/")
## source(file = paste(cd, "elasPrep.r", sep = ""), echo = TRUE)
## rm(cd)
#
load(file = "disElast.RData")       # debug
#
# extract regression constants and standardize dis.elasticity colnames
tmp.pan <- tmp.pri <- tmp.prd <- vector() # will receive constants
for (i in 1:32){
    ndis <- length( grep("stch.*", colnames(dis.elasticity$regs.d0$pan[[i]])) ); # determine how many districts in state
    tmp.pan <- c(tmp.pan, apply(X=dis.elasticity$regs.d0$pan[[i]][,1:ndis], MARGIN=2, median));
    tmp.pri <- c(tmp.pri, apply(X=dis.elasticity$regs.d0$pri[[i]][,1:ndis], MARGIN=2, median));
    tmp.prd <- c(tmp.prd, apply(X=dis.elasticity$regs.d0$prd[[i]][,1:ndis], MARGIN=2, median));
}
const.d0 <- data.frame(district=dis.elasticity$elas.d0$district, pan=tmp.pan, pri=tmp.pri, prd=tmp.prd)
#
tmp.pan <- tmp.pri <- tmp.prd <- vector() # will receive constants
i <- 1
for (i in 1:32){
    ndis <- length( grep("beta.*", colnames(dis.elasticity$regs.d1$pan[[i]])) )/2; # determine how many districts in state
    colnames(dis.elasticity$regs.d1$pan[[i]])[1:ndis] <- paste("d", 1:ndis, sep=""); # makes colnames like d0's
    colnames(dis.elasticity$regs.d1$pan[[i]])[(ndis+1):(2*ndis)] <- paste("stch", 1:ndis, sep=""); # makes colnames like d0's
    tmp.pan <- c(tmp.pan, apply(X=dis.elasticity$regs.d1$pan[[i]][,1:ndis], MARGIN=2, median));
    tmp.pri <- c(tmp.pri, apply(X=dis.elasticity$regs.d1$pri[[i]][,1:ndis], MARGIN=2, median));
    tmp.prd <- c(tmp.prd, apply(X=dis.elasticity$regs.d1$prd[[i]][,1:ndis], MARGIN=2, median));
}
const.d1 <- data.frame(district=dis.elasticity$elas.d1$district, pan=tmp.pan, pri=tmp.pri, prd=tmp.prd)
#
tmp.pan <- tmp.pri <- tmp.prd <- vector() # will receive constants
for (i in 1:32){
    ndis <- length( grep("beta.*", colnames(dis.elasticity$regs.d3$pan[[i]])) )/2; # determine how many districts in state
    colnames(dis.elasticity$regs.d3$pan[[i]])[1:ndis] <- paste("d", 1:ndis, sep=""); # makes colnames like d0's
    colnames(dis.elasticity$regs.d3$pan[[i]])[(ndis+1):(2*ndis)] <- paste("stch", 1:ndis, sep=""); # makes colnames like d0's
    tmp.pan <- c(tmp.pan, apply(X=dis.elasticity$regs.d3$pan[[i]][,1:ndis], MARGIN=2, median));
    tmp.pri <- c(tmp.pri, apply(X=dis.elasticity$regs.d3$pri[[i]][,1:ndis], MARGIN=2, median));
    tmp.prd <- c(tmp.prd, apply(X=dis.elasticity$regs.d3$prd[[i]][,1:ndis], MARGIN=2, median));
}
const.d3 <- data.frame(district=dis.elasticity$elas.d3$district, pan=tmp.pan, pri=tmp.pri, prd=tmp.prd)
#
dis.elasticity <- c(dis.elasticity, list(const.d0=const.d0, const.d1=const.d1, const.d3=const.d3))
#
rm(i, ndis, tmp.pan, tmp.pri, tmp.prd, const.d0, const.d1, const.d3)
#

sd((pob.edos$ptot2006 - pob.edos$ptot2000) * 100/pob.edos$ptot2000)

#################################
#################################
##  Graph district elasticity  ##
#################################
#################################
## library(Cairo)
## #
## setwd(gd)
## types <- c("pdf", "png", "svg"); type <- types[1] # select type here
## Cairo(file=paste("elast.pri.d0", ".", type, sep=""),
##       type=type,
##       units="in",
##       width=10,
##       height=10,
##       dpi = 96)
plot(c(-.3,.3),c(-.3,.3), type="n",
     main = "District elasticity 2006-2012, PRI",
     xlab = "change in state congressional vote %",
     ylab = "change in district vote %")
for (i in 1:300){
#    abline(a=dis.elasticity$const.d0$pan[i], b=dis.elasticity$elas.d0$pan[i], col="blue", lwd=.25)
    abline(a=dis.elasticity$const.d0$pri[i], b=dis.elasticity$elas.d0$pri[i], col="red", lwd=.25)
#    abline(a=dis.elasticity$const.d0$prd[i], b=dis.elasticity$elas.d0$prd[i], col="gold", lwd=.25)
}
abline(v=0); abline(h=0)
abline(a=0, b=1, lty=2)
## dev.off()
## setwd(wd)

# ---------------------------------------------------------------------
# SUMMARIZE NAT VOTE-SEAT SHARES, PLOT RRIS (ANSOLABEHERE ET AL 2002)
# ---------------------------------------------------------------------
# merges pri+green all years nationwide
# seats MR + PR
seats.mix.0612 <- data.frame(pty=c("pan","pri-green","left","pt-c","panal","pasc"),
                             dip06=c(206,123,157,0,10,4)/500,
                             dip09=c(143,258,71,19,9,0)/500,
                             dip12=c(114,241,135,0,10,0)/500)
# seats MR only
s1 <- df2006d0[,c("panw","priw","prdw","panalw","asdcw")]     #/ df2006d0$efec
s2 <- df2009d0[,c("panw","priw","prdw","pvemw","panalw","ptcw")] #/ df2009d0$efec
s3 <- df2012d0[,c("panw","priw","prdw","pvemw","panalw")]      #/ df2012d0$efec
# homogenize columns
s1 <- cbind(s1, pvemw = rep(0,300), ptcw = rep(0,300)); s1 <- s1[,c("panw","priw","prdw","pvemw","ptcw","panalw","asdcw")]
s2 <- cbind(s2, asdcw = rep(0,300)); s2 <- s2[,c("panw","priw","prdw","pvemw","ptcw","panalw","asdcw")]
s3 <- cbind(s3, asdcw = rep(0,300), ptcw = rep(0,300)); s3 <- s3[,c("panw","priw","prdw","pvemw","ptcw","panalw","asdcw")]
# sum pri and green regardless of partial coal
s2$priw <- s2$priw+s2$pvemw; s2$pvemw <- NULL
s3$priw <- s3$priw+s3$pvemw; s3$pvemw <- NULL
s1$pvemw <- s2$pvemw <- s3$pvemw <- NULL
seats.mr.0612 <- data.frame(pty=c("pan","pri-green","left","pt-c","panal","pasc"),
                           dip06=apply(s1, 2, sum)/300,
                           dip09=apply(s2, 2, sum)/300,
                           dip12=apply(s3, 2, sum)/300)
# national votes
v1 <- df2006d0[,c("pan","pric","prdc","panal","asdc")]     #/ df2006d0$efec
v2 <- df2009d0[,c("pan","pri","prd","pvem","panal","ptc")] #/ df2009d0$efec
v3 <- df2012d0[,c("pan","pri","prdc","pvem","panal")]      #/ df2012d0$efec
# homogenize columns
v1 <- cbind(v1, pvem = rep(0,300), pt = rep(0,300)); v1 <- v1[,c("pan","pric","prdc","pvem","pt","panal","asdc")]
v2 <- cbind(v2, asdc = rep(0,300)); v2 <- v2[,c("pan","pri","prd","pvem","ptc","panal","asdc")]
v3 <- cbind(v3, asdc = rep(0,300), pt = rep(0,300)); v3 <- v3[,c("pan","pri","prdc","pvem","pt","panal","asdc")]
# sum pri and green regardless of partial coal
v2$pri <- v2$pri+v2$pvem; v2$pvem <- NULL
v3$pri <- v3$pri+v3$pvem; v3$pvem <- NULL
v1$pvem <- v2$pvem <- v3$pvem <- NULL
nat.vot.0612 <- data.frame(pty=c("pan","pri-green","left","pt-c","panal","pasc"),
                           dip06=apply(v1, 2, sum),
                           dip09=apply(v2, 2, sum),
                           dip12=apply(v3, 2, sum))
nat.vot.0612$dip06 <- nat.vot.0612$dip06 / apply(nat.vot.0612[2:4], 2, sum)[1]
nat.vot.0612$dip09 <- nat.vot.0612$dip09 / apply(nat.vot.0612[2:4], 2, sum)[2]
nat.vot.0612$dip12 <- nat.vot.0612$dip12 / apply(nat.vot.0612[2:4], 2, sum)[3]
rm(v1,v2,v3,s1,s2,s3)
#
# plot votes-seats SMD and mix comparison 2006--2012
#pdf(file = paste(gd, "votes-seatsSMDandMix0612.pdf", sep=""))
plot(nat.vot.0612$dip12, seats.mr.0612$dip12, xlim = c(0,.62), ylim = c(0,.62),
     xlab = "vote shares", ylab = "seat shares", type="n")
abline(a=0, b=1, lty = 2)
arrows(nat.vot.0612$dip06[nat.vot.0612$dip06>0], seats.mr.0612$dip06[nat.vot.0612$dip06>0],
       nat.vot.0612$dip06[nat.vot.0612$dip06>0], seats.mix.0612$dip06[nat.vot.0612$dip06>0],
       length = .1, col = "gray50")
points(nat.vot.0612$dip06[nat.vot.0612$dip06>0], seats.mr.0612$dip06[nat.vot.0612$dip06>0], pch=19, col = "gray50")
points(nat.vot.0612$dip06[nat.vot.0612$dip06>0], seats.mix.0612$dip06[nat.vot.0612$dip06>0], pch=19)
arrows(nat.vot.0612$dip09[nat.vot.0612$dip09>0], seats.mr.0612$dip09[nat.vot.0612$dip09>0],
       nat.vot.0612$dip09[nat.vot.0612$dip09>0], seats.mix.0612$dip09[nat.vot.0612$dip09>0],
       length = .1, col = "gray50")
points(nat.vot.0612$dip09[nat.vot.0612$dip09>0], seats.mr.0612$dip09[nat.vot.0612$dip09>0], pch=19, col = "gray50")
points(nat.vot.0612$dip09[nat.vot.0612$dip09>0], seats.mix.0612$dip09[nat.vot.0612$dip09>0], pch=19)
arrows(nat.vot.0612$dip12[nat.vot.0612$dip12>0], seats.mr.0612$dip12[nat.vot.0612$dip12>0],
       nat.vot.0612$dip12[nat.vot.0612$dip12>0], seats.mix.0612$dip12[nat.vot.0612$dip12>0],
       length = .1, col = "gray50")
points(nat.vot.0612$dip12[nat.vot.0612$dip12>0], seats.mr.0612$dip12[nat.vot.0612$dip12>0], pch=19, col = "gray50")
points(nat.vot.0612$dip12[nat.vot.0612$dip12>0], seats.mix.0612$dip12[nat.vot.0612$dip12>0], pch=19)
legend("bottomright", legend = c("SMD only","SMD + PR mix"), pch = c(19,19), col = c("gray50", "black"))
#dev.off()
#
# plot rris 2006-2015 with d0 and d3
m <- min(c(df2006d0$rris, df2009d0$rris, df2012d0$rris, df2015d0$rris, df2018d0$rris, df2006d3$rris, df2009d3$rris, df2012d3$rris, df2015d3$rris, df2018d3$rris)); M <- max(c(df2006d0$rris, df2009d0$rris, df2012d0$rris, df2015d0$rris, df2018d0$rris, df2006d3$rris, df2009d3$rris, df2012d3$rris, df2015d3$rris, df2018d3$rris)) # min and max for two graphs
# with d0
tmp <- c(df2006d0$rris, df2009d0$rris, df2012d0$rris, df2015d0$rris, df2018d0$rris)
rris <- data.frame(yr = c(rep(2006,300), rep(2009,300), rep(2012,300), rep(2015,300), rep(2018,300)), rri=tmp)
tmp <- boxplot(rri ~ yr, data = rris) # produce boxplot data
#pdf(file = paste(gd, "rris0618d0.pdf", sep=""))
bxp(tmp, pars = list(ylim=c(m,M)), , xlab = "year", ylab = "district relative representation index (RRI)", main = "2006 map (drawn with 2000 census)") # plot with this command that allows changing range
#dev.off()
# with d3
tmp <- c(df2006d3$rris, df2009d3$rris, df2012d3$rris, df2015d3$rris, df2018d3$rris)
rris <- data.frame(yr = c(rep(2006,300), rep(2009,300), rep(2012,300), rep(2015,300), rep(2018,300)), rri=tmp)
tmp <- boxplot(rri ~ yr, data = rris) # produce boxplot data
#pdf(file = paste(gd, "rris0618d3.pdf", sep=""))
bxp(tmp, pars = list(ylim=c(m,M)), , xlab = "year", ylab = "district relative representation index (RRI)", main = "2015 map (drawn with 2010 census)") # plot with this command that
#dev.off()
#
# summarize rris
round(quantile(df2006d0$rris, probs = c(0, .05, .25, .5, .75, .95, 1)), 2)
round(quantile(df2009d0$rris, probs = c(0, .05, .25, .5, .75, .95, 1)), 2)
round(quantile(df2012d0$rris, probs = c(0, .05, .25, .5, .75, .95, 1)), 2)
round(quantile(df2015d0$rris, probs = c(0, .05, .25, .5, .75, .95, 1)), 2)

df2012d0[3,]
pob.edos[1,]
app.edos[1,]

# GRAPH MALAPPORTIONMENT
yr <- 2006; ds <- "2006 map"; dss <- "d0" # for labels and names
tmp <- df2006d0 ## choose object
head(tmp)
what2plot <- (tmp$rels - 1)*100
###for 2000 and 2015 use line below (unless df2000 and df2015 objects exist)
# what2plot <- (pob.distMap2006$rels2015[order(pob.distMap2006$edon, pob.distMap2006$disn)] -1)*100
# what2plot <- (pob.distMap2015p3$rels2015[order(pob.distMap2015p3$edon, pob.distMap2015p3$disn)] -1)*100
edon2plot <- tmp$edon
disn2plot <- tmp$disn
#
setwd(wd)
# posiciones para los nombres de los estados y los colores
tmp <- data.frame(index=1:300, edon=edon2plot)
tmp$pos <- ave(tmp$index, as.factor(tmp$edon), FUN=mean, na.rm=TRUE)
clr.300 <- (tmp$edon/4 - as.integer(tmp$edon/4))*4 +1 # four-color sequence by state
library(RColorBrewer); clr.4 <- brewer.pal(5, "Set1")[2:5]; # prepare 4 nice colors for plot
library(plyr); clr.300 <- mapvalues(clr.300, from = c(2,3,4,1), to = clr.4);
clr.32 <- clr.300[duplicated(tmp$edon)==FALSE]
#
nom.pos <- tmp$pos[duplicated(tmp$edon)==FALSE]
#
library(Cairo)
#
## setwd(gd)
## types <- c("pdf", "png", "svg"); type <- types[1] # select type here
## Cairo(file=paste("malapp", yr, dss, ".", type, sep=""),
##       type=type,
##       units="in",
##       width=10,
##       height=6,
##       dpi = 96)
plot(what2plot, ylab = "malapportionment (%)", xlab = "districts",
#     ylim = c(min(what2plot), max(what2plot)+25),
     ylim = c(-50, 160+25),
#     type="n", main = paste("Pop.-target diff. of", ds, "in year", yr), axes = FALSE)
     type="n", main = "2006 map whan inaugurated", axes = FALSE)
axis(1, at = c(1, seq(from = 50, to = 300, by = 50)), cex.axis = .85)
#axis(2, at = seq(from = -60, to = 120, by = 10), labels = FALSE)
axis(2, at = seq(from = -60, to = 140, by = 20), cex.axis = .85)
polygon(x = c(-10,310,310,-10), y = c(-15,-15,15,15), lty = 0, col = "grey90")
abline(h=0, lty=2)
abline(h=c(seq(from = -60, to = -10, by = 10), seq(from = 10, to = 140, by = 10)), col="grey")
points(what2plot,
     col = clr.300, pch=20)
text(what2plot, labels=disn2plot, cex = .25, col="white")
#text(nom.pos, rep( c(max(what2plot)+24, max(what2plot)+20, max(what2plot)+16, max(what2plot)+11), times=8), labels = edos, cex = .75, col=clr.32)
text(nom.pos, rep( c(150+30, 150+20, 150+10, 150), times=8), labels = edos, cex = .75, col=clr.32)
## dev.off()
## setwd(wd)
#
# compute mean absolute malapportionment
quantile(abs(what2plot))
#
rm(yr, ds, dss, what2plot)


# cross tabulate pts ratio and ptn ratio with margin
object <- df2006d3; yr <- 2006; ds <- "2015 map"; dss <- "d3"; bs <- "(state base)"; bss <- "sta";
marg2graph <- apply(X = object[,c("panm","prim","prdm")], MARGIN = 1, max)
mal2graph <- (object$rels - 1) * 100 # use reln for nation base, rels for state base
## library(Cairo)
## setwd(gd)
## types <- c("pdf", "png", "svg"); type <- types[1] # select type here
## Cairo(file=paste("malmg", yr, dss, bss, ".", type, sep=""),
##       type=type,
##       units="in",
##       width=10,
##       height=8,
##       dpi = 96)
plot(mal2graph, marg2graph, type = "n",
     ylim = c(0,.6), xlim = c(-50, 125),
     main = paste(yr, "election in", ds, bs),
     xlab = "Malapportionment (%)", ylab = "Winner's margin")
axis(1, at = seq(-50, 120, 10), labels = FALSE)
tmp <- marg2graph; marg2graph <- marg2graph[which(object$priw==1)]
tmp2 <- mal2graph; mal2graph <- mal2graph[object$priw==1]
tmp3 <- lm(formula = marg2graph ~ 1 + mal2graph)
points(mal2graph, marg2graph, col = "red", pch = 19, cex = .5)
abline(tmp3, col = "red");
#
marg2graph <- tmp; mal2graph <- tmp2;
marg2graph <- marg2graph[which(object$panw==1)]
mal2graph <- mal2graph[object$panw==1]
tmp3 <- lm(formula = marg2graph ~ 1 + mal2graph)
points(mal2graph, marg2graph, col = "blue", pch = 19, cex = .5)
abline(tmp3, col = "blue");
#
marg2graph <- tmp; mal2graph <- tmp2;
marg2graph <- marg2graph[which(object$prdw==1)]
mal2graph <- mal2graph[object$prdw==1]
tmp3 <- lm(formula = marg2graph ~ 1 + mal2graph)
points(mal2graph, marg2graph, col = "gold", pch = 19, cex = .5)
abline(tmp3, col = "gold");
## dev.off()
## setwd(wd)
rm(marg2graph, mal2graph, tmp, tmp2, tmp3)


# plot state deputy over/under-representation in time
#
library(RColorBrewer) # prepare 32 colors for plot
tmp <- brewer.pal(8, "Set2")
tmp2 <- colorRampPalette(tmp) # returns a function
col.plot <- tmp2(32)
#
yrs <- as.numeric( gsub(x = colnames(state.overep), pattern = "dif", replacement = "") )
#select <- which(yrs>1993); y.plot <- state.overep[,select]; x.plot <- yrs[select];
select <- which(yrs>1993); y.plot <- state.overep[state.overep$dif2006<0,select]; x.plot <- yrs[select];
data.frame(cbind(app.edos$dis2012[state.overep$dif2006<0], pob.edos$ptot2009[state.overep$dif2006<0], edos[state.overep$dif2006<0]))
sum(app.edos$dis2012[state.overep$dif2006<0])
y.plot; edos[state.overep$dif2006<0]
#
## library(Cairo)
## setwd(gd)
## types <- c("pdf", "png", "svg"); type <- types[1] # select type here
## Cairo(file=paste("statesUnderOverRep.", type, sep=""),
##       type=type,
##       units="in",
##       width=7,
##       height=10,
##       dpi = 96)
plot(c(1996, max(x.plot)+1), c(min(y.plot[,2])-.5, max(y.plot[,9])), type = "n", axes = FALSE,
     xlab = "", ylab = "State under-/over-representation (deputies)", main = "States' representation in Congress")
axis(1, seq(1997, 2015, 3))
axis(1, at = c(x.plot, 1996, 2005, 2013), labels = FALSE)
axis(2, -6:5)
abline(h=-6:5, col = "grey"); abline(h=0, col = "red")
abline(v=c(1996,2005,2013), col = "grey", lty = 2)
for (i in 1:32){
    lines(x.plot, y.plot[i,], col = col.plot[i]);
    points(x.plot[-which(x.plot==2013)], y.plot[i,-which(x.plot==2013)], pch = 19, cex = .5, col = col.plot[i]);
}
text(1996, -6.25, "R", cex = .75); text(2005, -6.25, "R", cex = .75); text(2013, -6.25, "F", cex = .75)
select <- which(y.plot[,which(x.plot==2003)]==min(y.plot[,which(x.plot==2003)])); text(x = 2003, y = y.plot[select, which(x.plot==2003)], labels = edos[select], cex = .75, pos = 4, col = col.plot[select])
select <- which(y.plot[,which(x.plot==2003)]==max(y.plot[,which(x.plot==2003)])); text(x = 2003, y = y.plot[select, which(x.plot==2003)], labels = edos[select], cex = .75, pos = 3, col = col.plot[select])
select <- 30; text(x = 2003, y = y.plot[select, which(x.plot==2003)], labels = edos[select], cex = .75, pos = 3, col = col.plot[select])
select <- 2; text(x = 2003, y = y.plot[select, which(x.plot==2003)], labels = edos[select], cex = .75, pos = 1, col = col.plot[select])
select <- 16; text(x = 2003, y = y.plot[select, which(x.plot==2003)], labels = edos[select], cex = .75, pos = 3, col = col.plot[select])
select <- c(20,21,25,30); text(x = 2015, y = y.plot[select, which(x.plot==2015)]+c(.22,-.14,0,.12), labels = edos[select], cex = .75, pos = 4, col = col.plot[select])
select <- c(7,11,14,15,22,23,28); text(x = 2015, y = y.plot[select, which(x.plot==2015)]+c(-.24,0,.1,.03,-.11,-.1,0), labels = edos[select], cex = .75, pos = 4, col = col.plot[select])
## dev.off()
## setwd(wd)
#

# same as above in relative terms
select <- which(yrs>1993); y.plot <- state.overep.rel[,select]*100; x.plot <- yrs[select];
#
## library(Cairo)
## setwd(gd)
## types <- c("pdf", "png", "svg"); type <- types[1] # select type here
## Cairo(file=paste("statesUnderOverRep-rel.", type, sep=""),
##       type=type,
##       units="in",
##       width=7,
##       height=10,
##       dpi = 96)
plot(c(1996, max(x.plot)+1), c(min(y.plot[-1,]), 40), type = "n", axes = FALSE,
     xlab = "", ylab = "State % under-/over-representation (deputies)", main = "Relative to their size")
axis(1, seq(1997, 2015, 3))
axis(1, at = c(x.plot, 1996, 2005, 2013), labels = FALSE)
axis(2, seq(-60,60,20))
abline(h=seq(-60,60,10), col = "grey"); abline(h=0, col = "red")
abline(v=c(1996,2005,2013), col = "grey", lty = 2)
for (i in 1:32){
    lines(x.plot, y.plot[i,], col = col.plot[i]);
    points(x.plot[-which(x.plot==2013)], y.plot[i,-which(x.plot==2013)], pch = 19, cex = .5, col = col.plot[i]);
}
text(1996, -58, "R", cex = .75); text(2005, -58, "R", cex = .75); text(2013, -58, "F", cex = .75)
select <- c(9,17,20,21,25); text(x = 2015, y = y.plot[select, which(x.plot==2015)]+c(0,-.5,0,.5,0), labels = edos[select], cex = .75, pos = 4, col = col.plot[select])
select <- c(7,11,14,28); text(x = 2015, y = y.plot[select, which(x.plot==2015)]+c(-.7,.9,0,0), labels = edos[select], cex = .75, pos = 4, col = col.plot[select])
select <- c(3,6); text(x = 1997, y = y.plot[select, which(x.plot==1997)], labels = edos[select], cex = .75, pos = 3, col = col.plot[select])
select <- c(2,23,32); text(x = 2003, y = y.plot[select, which(x.plot==2003)], labels = edos[select], cex = .75, pos = 4, col = col.plot[select])
select <- c(22); text(x = 2012, y = y.plot[select, which(x.plot==2012)], labels = edos[select], cex = .75, pos = 4, col = col.plot[select])
## dev.off()
## setwd(wd)
#
# -rank(y.plot[,which(x.plot==2012)], ties.method = "max")+33
# which(y.plot[,which(x.plot==2012)] < y.plot[,which(x.plot==2013)] & y.plot[,which(x.plot==2015)] < y.plot[,which(x.plot==2013)]

rm(tmp,tmp2,i,yrs,x.plot,y.plot,yr,select,dss,ds,edon2plot,object,nom.pos,clr.300,clr.32,bs,bss,col.plot)


# Rojano's plot of cost functions by state (counterproposals to 2nd scenario, from which 3rd was made)
#colColumns <- c("black","blue","red","gold","yellow","green","orange","cyan","brown","blue","red","gold","gold","yellow","green","orange","cyan","grey")
wd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/data/")
gd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/graphs/")
setwd(wd);
ptyProp2to3 <- read.csv(file = "propsAl2doEscenarioGrafRojano.csv")
# one prop per party (best)
tmp <- ptyProp2to3[,grep("pan[NL]", colnames(ptyProp2to3))]; tmp[is.na(tmp)==TRUE] <- 1000 # mess to work with NAs
ptyProp2to3$pan <- apply(tmp, 1, function(x) min(x, na.rm = TRUE)); ptyProp2to3$pan[ptyProp2to3$pan==1000] <- NA
tmp <- ptyProp2to3[,grep("pri[NL]", colnames(ptyProp2to3))]; tmp[is.na(tmp)==TRUE] <- 1000 # mess to work with NAs
ptyProp2to3$pri <- apply(tmp, 1, function(x) min(x, na.rm = TRUE)); ptyProp2to3$pri[ptyProp2to3$pri==1000] <- NA
tmp <- ptyProp2to3[,grep("prd[NL]", colnames(ptyProp2to3))]; tmp[is.na(tmp)==TRUE] <- 1000 # mess to work with NAs
ptyProp2to3$prd <- apply(tmp, 1, function(x) min(x, na.rm = TRUE)); ptyProp2to3$prd[ptyProp2to3$prd==1000] <- NA
tmp <- ptyProp2to3[,grep("pt[NL]", colnames(ptyProp2to3))]; tmp[is.na(tmp)==TRUE] <- 1000 # mess to work with NAs
ptyProp2to3$pt <- apply(tmp, 1, function(x) min(x, na.rm = TRUE)); ptyProp2to3$pt[ptyProp2to3$pt==1000] <- NA
tmp <- ptyProp2to3[,grep("pvem[NL]", colnames(ptyProp2to3))]; tmp[is.na(tmp)==TRUE] <- 1000 # mess to work with NAs
ptyProp2to3$pvem <- apply(tmp, 1, function(x) min(x, na.rm = TRUE)); ptyProp2to3$pvem[ptyProp2to3$pvem==1000] <- NA
tmp <- ptyProp2to3[,grep("mc[NL]", colnames(ptyProp2to3))]; tmp[is.na(tmp)==TRUE] <- 1000 # mess to work with NAs
ptyProp2to3$mc <- apply(tmp, 1, function(x) min(x, na.rm = TRUE)); ptyProp2to3$mc[ptyProp2to3$mc==1000] <- NA
tmp <- ptyProp2to3[,grep("panal[NL]", colnames(ptyProp2to3))]; tmp[is.na(tmp)==TRUE] <- 1000 # mess to work with NAs
ptyProp2to3$panal <- apply(tmp, 1, function(x) min(x, na.rm = TRUE)); ptyProp2to3$panal[ptyProp2to3$panal==1000] <- NA
## tmp <- ptyProp2to3[,grep("clv|jle", colnames(ptyProp2to3))]; tmp[is.na(tmp)==TRUE] <- 1000 # mess to work with NAs
## ptyProp2to3$ife <- apply(tmp, 1, function(x) min(x, na.rm = TRUE)); ptyProp2to3$ife[ptyProp2to3$ife==1000] <- NA
ptyProp2to3 <- ptyProp2to3[,c("edo","escen1","escen2","escen3","pan","pri","prd","pt","pvem","mc","panal")]
# group minors
tmp <- ptyProp2to3[,grep("pt|pvem|mc|panal", colnames(ptyProp2to3))]; tmp[is.na(tmp)==TRUE] <- 1000 # mess to work with NAs
ptyProp2to3$minor <- apply(tmp, 1, function(x) min(x, na.rm = TRUE)); ptyProp2to3$minor[ptyProp2to3$minor==1000] <- NA
ptyProp2to3 <- ptyProp2to3[,c("edo","escen1","escen2","escen3","pan","pri","prd","minor")]
head(ptyProp2to3)
#
# Make changes relative, prop2=100
tmp <- ptyProp2to3[,c("escen1","escen2","escen3","pan","pri","prd","minor")]
tmp <- tmp * 100/ptyProp2to3$escen2
ptyProp2to3[,c("escen1","escen2","escen3","pan","pri","prd","minor")] <- tmp
#
#require(plotrix)
colColumns <- c("blue","red","gold","grey")
## library(Cairo)
## setwd(gd)
## types <- c("pdf", "png", "svg"); type <- types[1] # select type here
## Cairo(file=paste("propsAndCost.", type, sep=""),
##       type=type,
##       units="in",
##       width=9,
##       height=10,
##       dpi = 96)
#plot( c(1,32), c(min(tmp, na.rm = TRUE),120), type = "n", axes = FALSE, xlab = "", ylab = "cost function (proposal 2 = 100)")
plot( c(1,32), c(min(tmp, na.rm = TRUE),max(tmp, na.rm = TRUE)), type = "n", axes = FALSE,
     xlab = "", ylab = "\"cost\" function (proposal 2 = 100)", main = "Proposals and counterproposals")
## par(bty="n") # deleting the box
## gap.plot( 1:32, ptyProp2to3$pan, col = colColumns[1], xlab = "", ylab = "cost function (proposal 2 = 100)", 
##          gap = c(125,145), gap.axis = "y", ylim = range(ptyProp2to3[,c("pan","pri","prd","minor")],na.rm = TRUE),
##          ytics = c(80,90,100,110,120,150), yticlab = c(80,90,100,110,120,150))
## gap.plot( 1:32, ptyProp2to3$minor, col = colColumns[4],  
##          gap = c(125,145), gap.axis = "y", ylim = range(ptyProp2to3[,c("pan","pri","prd","minor")],na.rm = TRUE), add = TRUE)
## abline(h=seq(124.99,126.2,.001), col="white")  # hiding vertical lines
## axis.break(2,125,style="slash")               # plotting slashes for breakpoints
axis( 1, at = 1:32, labels = ptyProp2to3$edo, tick = FALSE, las=3)
axis( 2, at = seq(80, 160, 10), labels = FALSE);  axis( 2, at = seq(80, 160, 20), labels = seq(80, 160, 20));  
abline(v = 1:32, col = "grey"); abline(h=100, col="black")
#
yStuff <- ptyProp2to3[,c('pan','pri','prd','minor')]; yStuff[is.na(yStuff)==TRUE] <- 0
dup.dummies <- matrix(0, nrow=nrow(yStuff), ncol=ncol(yStuff))
i <- 4;
for (i in 1:32){
    tmp.dup <- duplicated(t(yStuff[i,])) | duplicated(t(yStuff[i,]), fromLast = TRUE) # for some reason will not work unless rows transposed
    dup.dummies[i,tmp.dup==TRUE] <- 1
}
yStuff.unique <- (1-dup.dummies)*yStuff;
yStuff.dupli <- dup.dummies*yStuff;
for (i in 1:32){
    points( rep(i,4), yStuff.unique[i,], col = colColumns, pch = 19, cex=1.25) 
    sunflowerplot( rep(i,4), yStuff.dupli[i,], col = "brown", seg.col = "brown", cex = 1.5, add = TRUE ) # seg.col = colColumns ESTO NO JALA, NO RECONOCE COLORES DE QUIENES SE EMPALMAN
}
points( 1:32, ptyProp2to3$escen1, pch = 20); text(1:32, ptyProp2to3$escen1, labels="1", cex = .35, col="white")
points( 1:32, ptyProp2to3$escen2, pch = 20); text(1:32, ptyProp2to3$escen2, labels="2", cex = .35, col="white")
points( 1:32, ptyProp2to3$escen3, pch = 20); text(1:32, ptyProp2to3$escen3, labels="3", cex = .35, col="white")
## dev.off()
## setwd(wd)
rm(colColumns, dup.dummies, i, ptyProp2to3, tmp, tmp.dup, yStuff, yStuff.dupli, yStuff.unique)


pob$chg <- (pob$ptot10 - pob$ptot05) / (pob$ptot05 + 1)  # +1 para que los que salen de cero no se indeterminen
pob$chg[pob$ptot05==0]
summary(pob$chg)
colnames(pob)

pob[pob$edon==2 & pob$secn==1526,]





## Resumen de la lista nominal en las secciones 2012
print(paste("No. de secciones en 2012 =", nrow(e12)))
summary(e12$lisnom)
## Objetivo poblacional con lisnom # hay que hacerlo con ptot...
target.lisnom <- sum(e12$lisnom)/300 # lisnom that each district would have if all equal
print( paste("lista nominal que distritos homogeneos tendrian =", round(target.lisnom, digits = 0)) )

# así se hace en R un by yr mo: egen e12=sum(invested) de stata
e12$totln <- ave(e12$lisnom, as.factor(e12$edon+e12$disn/100), FUN=sum, na.rm=TRUE)

ln2012 <- e12[duplicated(e12$edon+e12$disn/100)== FALSE,]
ln2012 <- ln2012[,c("edon","disn","munn","totln")]; colnames(ln2012) <- c("edon","disn","munn","lisnom")

## resumen de los distritos en 2012
summary(ln2012$lisnom)
ln2012$rel <- (ln2012$lisnom - target.lisnom)*100/target.lisnom


## ## 2009
## tmp <- read.csv("dfSeccion2009.csv", header=TRUE)
## tmp <- tmp[order(tmp$edon, tmp$disn, tmp$seccion),]

## ## Resumen de la lista nominal en las secciones
## print(paste("No. de secciones =", nrow(tmp)))
## summary(tmp$lisnom)
## ## Objetivo poblacional
## target.lisnom <- sum(tmp$lisnom)/300 # lisnom that each district would have if all equal
## print( paste("lista nominal que distritos homogeneos tendrian =", target.lisnom) )

## # así se hace en R un by yr mo: egen tmp=sum(invested) de stata
## tmp$totln <- ave(tmp$lisnom, as.factor(tmp$edon+tmp$disn/100), FUN=sum, na.rm=TRUE)

## ln2009 <- tmp[duplicated(tmp$edon+tmp$disn/100)== FALSE,]
## ln2009 <- ln2009[,c("edon","disn","munn","totln")]; colnames(ln2009) <- c("edon","disn","munn","lisnom")

## ## resumen de los distritos en 2009
## summary(ln2009$lisnom)
## ln2009$rel <- (ln2009$lisnom - target.lisnom)*100/target.lisnom


## ## 2006
## tmp <- read.csv("dfSeccion2006.csv", header=TRUE)
## tmp <- tmp[order(tmp$edon, tmp$disn, tmp$seccion),]

## colnames(tmp) # 2006 no tiene lisnom

## ## Resumen de la lista nominal en las secciones
## print(paste("No. de secciones =", nrow(tmp)))
## summary(tmp$lisnom)
## ## Objetivo poblacional
## target.lisnom <- sum(tmp$lisnom)/300 # lisnom that each district would have if all equal
## print( paste("lista nominal que distritos homogeneos tendrian =", target.lisnom) )

## # así se hace en R un by yr mo: egen tmp=sum(invested) de stata
## tmp$totln <- ave(tmp$lisnom, as.factor(tmp$edon+tmp$disn/100), FUN=sum, na.rm=TRUE)

## ln2006 <- tmp[duplicated(tmp$edon+tmp$disn/100)== FALSE,]
## ln2006 <- ln2006[,c("edon","disn","munn","totln")]; colnames(ln2006) <- c("edon","disn","munn","lisnom")

## ## resumen de los distritos en 2006
## summary(ln2006$lisnom)
## ln2006$rel <- (ln2006$lisnom - target.lisnom)*100/target.lisnom


## ## 2003
## tmp <- read.csv("dfSeccion2003.csv", header=TRUE)
## tmp <- tmp[order(tmp$edon, tmp$disn, tmp$seccion),]

## colnames(tmp) ## 2003 no tiene lista nominal

## ## Resumen de la lista nominal en las secciones
## print(paste("No. de secciones =", nrow(tmp)))
## summary(tmp$lisnom)
## ## Objetivo poblacional
## target.lisnom <- sum(tmp$lisnom)/300 # lisnom that each district would have if all equal
## print( paste("lista nominal que distritos homogeneos tendrian =", target.lisnom) )

## # así se hace en R un by yr mo: egen tmp=sum(invested) de stata
## tmp$totln <- ave(tmp$lisnom, as.factor(tmp$edon+tmp$disn/100), FUN=sum, na.rm=TRUE)

## ln2003 <- tmp[duplicated(tmp$edon+tmp$disn/100)== FALSE,]
## ln2003 <- ln2003[,c("edon","disn","munn","totln")]; colnames(ln2003) <- c("edon","disn","munn","lisnom")

## ## resumen de los distritos en 2003
## summary(ln2003$lisnom)
## ln2003$rel <- (ln2003$lisnom - target.lisnom)*100/target.lisnom



setwd(wd)
# posiciones para los nombres de los estados y los colores
tmp <- data.frame(index=1:300, edon=ln2012$edon)
tmp$pos <- ave(tmp$index, as.factor(tmp$edon), FUN=mean, na.rm=TRUE)
clr.300 <- (tmp$edon/4 - as.integer(tmp$edon/4))*4 +1 # four-color sequence by state
clr.32 <- clr.300[duplicated(tmp$edon)==FALSE]

nom.pos <- tmp$pos[duplicated(tmp$edon)==FALSE]
edos <- c("ags", "bcn", "bcs", "cam", "coa", "col", "cps", "cua", "df", "dgo", "gua", "gue", "hgo", "jal", "mex", "mic", "mor", "nay", "nl", "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac")


library(Cairo)

## setwd(paste(wd, "/graphs/", sep=""))
## types <- c("pdf", "png", "svg"); type <- types[2] # select type here
## Cairo(file=paste("lisnom.dis.2012.", type, sep=""),
##       type=type,
##       units="in",
##       width=10,
##       height=6,
##       dpi = 96)
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
## dev.off()
## setwd(wd)

# min-max, ets spreads
min(ln2012$lisnom)
max(ln2012$lisnom)
print(paste("min-max spread:", max(ln2012$lisnom)/min(ln2012$lisnom)))

quantile(ln2012$lisnom, probs=.1)
quantile(ln2012$lisnom, probs=.9)
print(paste("interquartile spread:", quantile(ln2012$lisnom, probs=.9)/quantile(ln2012$lisnom, probs=.1)))

quantile(ln2012$lisnom, probs=.25)
quantile(ln2012$lisnom, probs=.75)
print(paste("interquartile spread:", quantile(ln2012$lisnom, probs=.75)/quantile(ln2012$lisnom, probs=.25)))

summary(ln2012$lisnom)
