rm(list=ls())

wd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/data/")
dd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/elecReturns/") # data directory
setwd(wd)

#########################################################################################
# IMPORTS AND PREPARES OBJECT MAPPING SECTIONS TO 1977, 1997, 2004, AND 2013 DISTRICTS ##
#########################################################################################
#
cd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/code/")
source(file = paste(cd, "eqPrep.r", sep = ""), echo = TRUE)
rm(cd)
#
####################################################################################
## IMPORTS 2013 DISTRICT DATA --- no longer needed, info is included in object eq ##
####################################################################################
## #source("codeFor2013districts.r") # RUN CODE TO CODE 3er ESCENARIO DISTRICTS
## load(file="dis2013.RData") 
## head(dis2013)



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
dd2 <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/data/") # prepared data directory
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
head(tmp)
# SUMAR POBLACION DE LOS DISTRITOS
# así se hace en R un by yr mo: egen e12=sum(invested) de stata
table(eq$dis2012 - eq$dis2006) # ojo: hay secciones que cambian de distrito sin que medie redistritación (pocos, los ignoro por usar sólo dis2012)
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
tmp.b <- (tmp$ptot10 * 2005 - tmp$ptot05 * 2010) / (2005 - 2010); tmp.a <- (tmp$ptot05 - tmp.b) / 2005; # interpolation parameters
tmp$ptot1994 <- round(x = tmp.a * 1994 + tmp.b, digits = 0);
tmp$ptot1997 <- round(x = tmp.a * 1997 + tmp.b, digits = 0);
tmp$ptot2000 <- round(x = tmp.a * 2000 + tmp.b, digits = 0);
tmp$ptot2003 <- round(x = tmp.a * 2003 + tmp.b);
tmp$ptot2006 <- round(x = tmp.a * 2006 + tmp.b);
tmp$ptot2009 <- round(x = tmp.a * 2009 + tmp.b);
tmp$ptot2012 <- round(x = tmp.a * 2012 + tmp.b);
tmp$ptot2013 <- round(x = tmp.a * 2013 + tmp.b);
tmp$ptot05 <- tmp$ptot10 <- NULL;
pob.edos <- tmp
pob.nal <- apply(X = pob.edos, MARGIN = 2, sum); pob.nal <- pob.nal[-1]
#
# turn them into target size
#
target.edos <- pob.edos; target.nal <- pob.nal;
for (i in 1:32){
    target.edos[i,-1] <- round( pob.edos[i,-1] / app.edos[i,-1], digits = 0 )
}
head(target.edos)
target.nal <- pob.nal / 300
target.nal
#
# aggregates district populations
#
## y <- 1994
## tmp <- eq[,c("edon","dis1994","ptot05","ptot10")]; colnames(tmp)[2] <- "dis"
## tmp$ptot05 <- ave(tmp$ptot05, as.factor(tmp$edon+tmp$dis/100), FUN=sum, na.rm=TRUE)
## tmp$ptot10 <- ave(tmp$ptot10, as.factor(tmp$edon+tmp$dis/100), FUN=sum, na.rm=TRUE)
## tmp <- tmp[duplicated(tmp$edon+tmp$dis/100)==FALSE,] # drops redundabt rows after aggregating districts from secciones
## print(paste("Total 2005 population unassigned to a district in", y, "election =", sum(tmp$ptot05[which(tmp$dis==0)])))
## print(paste("Total 2010 population unassigned to a district in", y, "election =", sum(tmp$ptot10[which(tmp$dis==0)])))
## tmp <- tmp[-which(tmp$dis==0),] # removes secciones not assigned to some district
## tmp <- tmp[order(tmp$edon, tmp$dis),]
## tmp.b <- (tmp$ptot10 * 2005 - tmp$ptot05 * 2010) / (2005 - 2010); tmp.a <- (tmp$ptot05 - tmp.b) / 2005
## tmp$ptot1994 <- round(x = tmp.a * 1994 + tmp.b, digits = 0);
## tmp$ptot1997 <- round(x = tmp.a * 1997 + tmp.b, digits = 0);
## tmp$ptot2000 <- round(x = tmp.a * 2000 + tmp.b, digits = 0);
## tmp$ptot2003 <- round(x = tmp.a * 2003 + tmp.b, digits = 0);
## tmp$ptot2006 <- round(x = tmp.a * 2006 + tmp.b, digits = 0);
## tmp$ptot2009 <- round(x = tmp.a * 2009 + tmp.b, digits = 0);
## tmp$ptot2012 <- round(x = tmp.a * 2012 + tmp.b, digits = 0);
## tmp$ptot2013 <- round(x = tmp.a * 2013 + tmp.b, digits = 0);
## tmp$ptot05 <- tmp$ptot10 <- NULL;
## tmp$ptot05 <- tmp$ptot10 <- NULL;
## pob.dist1994 <- tmp
## #
## y <- 1997
## tmp <- eq[,c("edon","dis1997","ptot05","ptot10")]; colnames(tmp)[2] <- "dis"
## tmp$ptot05 <- ave(tmp$ptot05, as.factor(tmp$edon+tmp$dis/100), FUN=sum, na.rm=TRUE)
## tmp$ptot10 <- ave(tmp$ptot10, as.factor(tmp$edon+tmp$dis/100), FUN=sum, na.rm=TRUE)
## tmp <- tmp[duplicated(tmp$edon+tmp$dis/100)==FALSE,] # drops redundabt rows after aggregating districts from secciones
## print(paste("Total 2005 population unassigned to a district in", y, "election =", sum(tmp$ptot05[which(tmp$dis==0)])))
## print(paste("Total 2010 population unassigned to a district in", y, "election =", sum(tmp$ptot10[which(tmp$dis==0)])))
## tmp <- tmp[-which(tmp$dis==0),] # removes secciones not assigned to some district
## tmp <- tmp[order(tmp$edon, tmp$dis),]
## tmp.b <- (tmp$ptot10 * 2005 - tmp$ptot05 * 2010) / (2005 - 2010); tmp.a <- (tmp$ptot05 - tmp.b) / 2005
## tmp$ptot1994 <- round(x = tmp.a * 1994 + tmp.b, digits = 0);
## tmp$ptot1997 <- round(x = tmp.a * 1997 + tmp.b, digits = 0);
## tmp$ptot2000 <- round(x = tmp.a * 2000 + tmp.b, digits = 0);
## tmp$ptot2003 <- round(x = tmp.a * 2003 + tmp.b, digits = 0);
## tmp$ptot2006 <- round(x = tmp.a * 2006 + tmp.b, digits = 0);
## tmp$ptot2009 <- round(x = tmp.a * 2009 + tmp.b, digits = 0);
## tmp$ptot2012 <- round(x = tmp.a * 2012 + tmp.b, digits = 0);
## tmp$ptot2013 <- round(x = tmp.a * 2013 + tmp.b, digits = 0);
## tmp$ptot05 <- tmp$ptot10 <- NULL;
## tmp$ptot05 <- tmp$ptot10 <- NULL;
## pob.dist1997 <- tmp
#
## y <- 2000
## tmp <- eq[,c("edon","dis2000","ptot05","ptot10")]; colnames(tmp)[2] <- "dis"
## tmp$ptot05 <- ave(tmp$ptot05, as.factor(tmp$edon+tmp$dis/100), FUN=sum, na.rm=TRUE)
## tmp$ptot10 <- ave(tmp$ptot10, as.factor(tmp$edon+tmp$dis/100), FUN=sum, na.rm=TRUE)
## tmp <- tmp[duplicated(tmp$edon+tmp$dis/100)==FALSE,] # drops redundabt rows after aggregating districts from secciones
## print(paste("Total 2005 population unassigned to a district in", y, "election =", sum(tmp$ptot05[which(tmp$dis==0)])))
## print(paste("Total 2010 population unassigned to a district in", y, "election =", sum(tmp$ptot10[which(tmp$dis==0)])))
## tmp <- tmp[-which(tmp$dis==0),] # removes secciones not assigned to some district
## tmp <- tmp[order(tmp$edon, tmp$dis),]
## tmp.b <- (tmp$ptot10 * 2005 - tmp$ptot05 * 2010) / (2005 - 2010); tmp.a <- (tmp$ptot05 - tmp.b) / 2005
## tmp$ptot1994 <- round(x = tmp.a * 1994 + tmp.b, digits = 0);
## tmp$ptot1997 <- round(x = tmp.a * 1997 + tmp.b, digits = 0);
## tmp$ptot2000 <- round(x = tmp.a * 2000 + tmp.b, digits = 0);
## tmp$ptot2003 <- round(x = tmp.a * 2003 + tmp.b, digits = 0);
## tmp$ptot2006 <- round(x = tmp.a * 2006 + tmp.b, digits = 0);
## tmp$ptot2009 <- round(x = tmp.a * 2009 + tmp.b, digits = 0);
## tmp$ptot2012 <- round(x = tmp.a * 2012 + tmp.b, digits = 0);
## tmp$ptot2013 <- round(x = tmp.a * 2013 + tmp.b, digits = 0);
## tmp$ptot05 <- tmp$ptot10 <- NULL;
## tmp$ptot05 <- tmp$ptot10 <- NULL;
## pob.dist2000 <- tmp
#
## y <- 2003
## tmp <- eq[,c("edon","dis2003","ptot05","ptot10")]; colnames(tmp)[2] <- "dis"
## tmp$ptot05 <- ave(tmp$ptot05, as.factor(tmp$edon+tmp$dis/100), FUN=sum, na.rm=TRUE)
## tmp$ptot10 <- ave(tmp$ptot10, as.factor(tmp$edon+tmp$dis/100), FUN=sum, na.rm=TRUE)
## tmp <- tmp[duplicated(tmp$edon+tmp$dis/100)==FALSE,] # drops redundabt rows after aggregating districts from secciones
## print(paste("Total 2005 population unassigned to a district in", y, "election =", sum(tmp$ptot05[which(tmp$dis==0)])))
## print(paste("Total 2010 population unassigned to a district in", y, "election =", sum(tmp$ptot10[which(tmp$dis==0)])))
## tmp <- tmp[-which(tmp$dis==0),] # removes secciones not assigned to some district
## tmp <- tmp[order(tmp$edon, tmp$dis),]
## tmp.b <- (tmp$ptot10 * 2005 - tmp$ptot05 * 2010) / (2005 - 2010); tmp.a <- (tmp$ptot05 - tmp.b) / 2005
## tmp$ptot1994 <- round(x = tmp.a * 1994 + tmp.b, digits = 0);
## tmp$ptot1997 <- round(x = tmp.a * 1997 + tmp.b, digits = 0);
## tmp$ptot2000 <- round(x = tmp.a * 2000 + tmp.b, digits = 0);
## tmp$ptot2003 <- round(x = tmp.a * 2003 + tmp.b, digits = 0);
## tmp$ptot2006 <- round(x = tmp.a * 2006 + tmp.b, digits = 0);
## tmp$ptot2009 <- round(x = tmp.a * 2009 + tmp.b, digits = 0);
## tmp$ptot2012 <- round(x = tmp.a * 2012 + tmp.b, digits = 0);
## tmp$ptot2013 <- round(x = tmp.a * 2013 + tmp.b, digits = 0);
## tmp$ptot05 <- tmp$ptot10 <- NULL;
## tmp$ptot05 <- tmp$ptot10 <- NULL;
## pob.dist2003 <- tmp
## #
## y <- 2006
## tmp <- eq[,c("edon","dis2006","ptot05","ptot10")]; colnames(tmp)[2] <- "dis"
## tmp$ptot05 <- ave(tmp$ptot05, as.factor(tmp$edon+tmp$dis/100), FUN=sum, na.rm=TRUE)
## tmp$ptot10 <- ave(tmp$ptot10, as.factor(tmp$edon+tmp$dis/100), FUN=sum, na.rm=TRUE)
## tmp <- tmp[duplicated(tmp$edon+tmp$dis/100)==FALSE,] # drops redundabt rows after aggregating districts from secciones
## print(paste("Total 2005 population unassigned to a district in", y, "election =", sum(tmp$ptot05[which(tmp$dis==0)])))
## print(paste("Total 2010 population unassigned to a district in", y, "election =", sum(tmp$ptot10[which(tmp$dis==0)])))
## tmp <- tmp[-which(tmp$dis==0),] # removes secciones not assigned to some district
## tmp <- tmp[order(tmp$edon, tmp$dis),]
## tmp.b <- (tmp$ptot10 * 2005 - tmp$ptot05 * 2010) / (2005 - 2010); tmp.a <- (tmp$ptot05 - tmp.b) / 2005
## tmp$ptot1994 <- round(x = tmp.a * 1994 + tmp.b, digits = 0);
## tmp$ptot1997 <- round(x = tmp.a * 1997 + tmp.b, digits = 0);
## tmp$ptot2000 <- round(x = tmp.a * 2000 + tmp.b, digits = 0);
## tmp$ptot2003 <- round(x = tmp.a * 2003 + tmp.b, digits = 0);
## tmp$ptot2006 <- round(x = tmp.a * 2006 + tmp.b, digits = 0);
## tmp$ptot2009 <- round(x = tmp.a * 2009 + tmp.b, digits = 0);
## tmp$ptot2012 <- round(x = tmp.a * 2012 + tmp.b, digits = 0);
## tmp$ptot2013 <- round(x = tmp.a * 2013 + tmp.b, digits = 0);
## tmp$ptot05 <- tmp$ptot10 <- NULL;
## tmp$ptot05 <- tmp$ptot10 <- NULL;
## pob.dist2006 <- tmp
## #
## y <- 2009
## tmp <- eq[,c("edon","dis2009","ptot05","ptot10")]; colnames(tmp)[2] <- "dis"
## tmp$ptot05 <- ave(tmp$ptot05, as.factor(tmp$edon+tmp$dis/100), FUN=sum, na.rm=TRUE)
## tmp$ptot10 <- ave(tmp$ptot10, as.factor(tmp$edon+tmp$dis/100), FUN=sum, na.rm=TRUE)
## tmp <- tmp[duplicated(tmp$edon+tmp$dis/100)==FALSE,] # drops redundabt rows after aggregating districts from secciones
## print(paste("Total 2005 population unassigned to a district in", y, "election =", sum(tmp$ptot05[which(tmp$dis==0)])))
## print(paste("Total 2010 population unassigned to a district in", y, "election =", sum(tmp$ptot10[which(tmp$dis==0)])))
## tmp <- tmp[-which(tmp$dis==0),] # removes secciones not assigned to some district
## tmp <- tmp[order(tmp$edon, tmp$dis),]
## tmp.b <- (tmp$ptot10 * 2005 - tmp$ptot05 * 2010) / (2005 - 2010); tmp.a <- (tmp$ptot05 - tmp.b) / 2005
## tmp$ptot1994 <- round(x = tmp.a * 1994 + tmp.b, digits = 0);
## tmp$ptot1997 <- round(x = tmp.a * 1997 + tmp.b, digits = 0);
## tmp$ptot2000 <- round(x = tmp.a * 2000 + tmp.b, digits = 0);
## tmp$ptot2003 <- round(x = tmp.a * 2003 + tmp.b, digits = 0);
## tmp$ptot2006 <- round(x = tmp.a * 2006 + tmp.b, digits = 0);
## tmp$ptot2009 <- round(x = tmp.a * 2009 + tmp.b, digits = 0);
## tmp$ptot2012 <- round(x = tmp.a * 2012 + tmp.b, digits = 0);
## tmp$ptot2013 <- round(x = tmp.a * 2013 + tmp.b, digits = 0);
## tmp$ptot05 <- tmp$ptot10 <- NULL;
## tmp$ptot05 <- tmp$ptot10 <- NULL;
## pob.dist2009 <- tmp
#
y <- 2012
tmp <- eq[,c("edon","dis2012","ptot05","ptot10")]; colnames(tmp)[2] <- "dis"
tmp$ptot05 <- ave(tmp$ptot05, as.factor(tmp$edon+tmp$dis/100), FUN=sum, na.rm=TRUE)
tmp$ptot10 <- ave(tmp$ptot10, as.factor(tmp$edon+tmp$dis/100), FUN=sum, na.rm=TRUE)
tmp <- tmp[duplicated(tmp$edon+tmp$dis/100)==FALSE,] # drops redundabt rows after aggregating districts from secciones
print(paste("Total 2005 population unassigned to a district in", y, "election =", sum(tmp$ptot05[which(tmp$dis==0)])))
print(paste("Total 2010 population unassigned to a district in", y, "election =", sum(tmp$ptot10[which(tmp$dis==0)])))
tmp <- tmp[-which(tmp$dis==0),] # removes secciones not assigned to some district
tmp <- tmp[order(tmp$edon, tmp$dis),]
tmp.b <- (tmp$ptot10 * 2005 - tmp$ptot05 * 2010) / (2005 - 2010); tmp.a <- (tmp$ptot05 - tmp.b) / 2005
tmp$ptot1994 <- round(x = tmp.a * 1994 + tmp.b, digits = 0);
tmp$ptot1997 <- round(x = tmp.a * 1997 + tmp.b, digits = 0);
tmp$ptot2000 <- round(x = tmp.a * 2000 + tmp.b, digits = 0);
tmp$ptot2003 <- round(x = tmp.a * 2003 + tmp.b, digits = 0);
tmp$ptot2006 <- round(x = tmp.a * 2006 + tmp.b, digits = 0);
tmp$ptot2009 <- round(x = tmp.a * 2009 + tmp.b, digits = 0);
tmp$ptot2012 <- round(x = tmp.a * 2012 + tmp.b, digits = 0);
tmp$ptot2013 <- round(x = tmp.a * 2013 + tmp.b, digits = 0);
tmp$ptot05 <- tmp$ptot10 <- NULL;
tmp$ptot05 <- tmp$ptot10 <- NULL;
pob.dist2012 <- tmp
#
# 2013.1
y <- 2013
tmp <- eq[,c("edon","dis2013.1","ptot05","ptot10")]; colnames(tmp)[2] <- "dis"
tmp$ptot05 <- ave(tmp$ptot05, as.factor(tmp$edon+tmp$dis/100), FUN=sum, na.rm=TRUE)
tmp$ptot10 <- ave(tmp$ptot10, as.factor(tmp$edon+tmp$dis/100), FUN=sum, na.rm=TRUE)
tmp <- tmp[duplicated(tmp$edon+tmp$dis/100)==FALSE,] # drops redundabt rows after aggregating districts from secciones
print(paste("Total 2005 population unassigned to a district in", y, "=", sum(tmp$ptot05[which(tmp$dis==0)])))
print(paste("Total 2010 population unassigned to a district in", y, "=", sum(tmp$ptot10[which(tmp$dis==0)])))
tmp <- tmp[-which(tmp$dis==0),] # removes secciones not assigned to some district
tmp <- tmp[order(tmp$edon, tmp$dis),]
tmp.b <- (tmp$ptot10 * 2005 - tmp$ptot05 * 2010) / (2005 - 2010); tmp.a <- (tmp$ptot05 - tmp.b) / 2005
tmp$ptot1994 <- round(x = tmp.a * 1994 + tmp.b, digits = 0);
tmp$ptot1997 <- round(x = tmp.a * 1997 + tmp.b, digits = 0);
tmp$ptot2000 <- round(x = tmp.a * 2000 + tmp.b, digits = 0);
tmp$ptot2003 <- round(x = tmp.a * 2003 + tmp.b, digits = 0);
tmp$ptot2006 <- round(x = tmp.a * 2006 + tmp.b, digits = 0);
tmp$ptot2009 <- round(x = tmp.a * 2009 + tmp.b, digits = 0);
tmp$ptot2012 <- round(x = tmp.a * 2012 + tmp.b, digits = 0);
tmp$ptot2013 <- round(x = tmp.a * 2013 + tmp.b, digits = 0);
tmp$ptot05 <- tmp$ptot10 <- NULL;
tmp$ptot05 <- tmp$ptot10 <- NULL;
pob.dist2013.1 <- tmp
#
# 2013.3
y <- 2013
tmp <- eq[,c("edon","dis2013.3","ptot05","ptot10")]; colnames(tmp)[2] <- "dis"
tmp$ptot05 <- ave(tmp$ptot05, as.factor(tmp$edon+tmp$dis/100), FUN=sum, na.rm=TRUE)
tmp$ptot10 <- ave(tmp$ptot10, as.factor(tmp$edon+tmp$dis/100), FUN=sum, na.rm=TRUE)
tmp <- tmp[duplicated(tmp$edon+tmp$dis/100)==FALSE,] # drops redundabt rows after aggregating districts from secciones
print(paste("Total 2005 population unassigned to a district in", y, "=", sum(tmp$ptot05[which(tmp$dis==0)])))
print(paste("Total 2010 population unassigned to a district in", y, "=", sum(tmp$ptot10[which(tmp$dis==0)])))
tmp <- tmp[-which(tmp$dis==0),] # removes secciones not assigned to some district
tmp <- tmp[order(tmp$edon, tmp$dis),]
tmp.b <- (tmp$ptot10 * 2005 - tmp$ptot05 * 2010) / (2005 - 2010); tmp.a <- (tmp$ptot05 - tmp.b) / 2005
tmp$ptot1994 <- round(x = tmp.a * 1994 + tmp.b, digits = 0);
tmp$ptot1997 <- round(x = tmp.a * 1997 + tmp.b, digits = 0);
tmp$ptot2000 <- round(x = tmp.a * 2000 + tmp.b, digits = 0);
tmp$ptot2003 <- round(x = tmp.a * 2003 + tmp.b, digits = 0);
tmp$ptot2006 <- round(x = tmp.a * 2006 + tmp.b, digits = 0);
tmp$ptot2009 <- round(x = tmp.a * 2009 + tmp.b, digits = 0);
tmp$ptot2012 <- round(x = tmp.a * 2012 + tmp.b, digits = 0);
tmp$ptot2013 <- round(x = tmp.a * 2013 + tmp.b, digits = 0);
tmp$ptot05 <- tmp$ptot10 <- NULL;
tmp$ptot05 <- tmp$ptot10 <- NULL;
pob.dist2013.3 <- tmp
#
head(pob.dist2012)

#
# clean
rm(tmp.a, tmp.b, tmp, tmp2, i, fl, pob, pob05, pob10, edo, y, tempobj);
census <- list(c05=c05, c10=c10); rm(c05, c10)
ls()
#

# make district populations relative to target
save.image(file = "tmp.RData") # debug
rm(list = ls())                # debug
load(file = "tmp.RData")       # debug

## tmp <- tmp2 <- pob.dist1994
## for (i in 1:300){
##     tmp[i, 3:10] <-   tmp[i, 3:10] / target.edos[tmp$edon[i], 2:9];
##     tmp2[i, 3:10] <- tmp2[i, 3:10] / target.nal;
## }
## colnames(tmp)[3:10] <- paste("rels",  c(seq(from = 1994, to = 2012, by = 3), 2013.1, 2013.3), sep = "")
## colnames(tmp2)[3:10] <- paste("reln", c(seq(from = 1994, to = 2012, by = 3), 2013.1, 2013.3), sep = "")
## pob.dist1994 <- cbind(pob.dist1994, tmp[,3:10], tmp2[,3:10])
## #
## tmp <- tmp2 <- pob.dist1997
## for (i in 1:300){
##     tmp[i, 3:10] <-   tmp[i, 3:10] / target.edos[tmp$edon[i], 2:9];
##     tmp2[i, 3:10] <- tmp2[i, 3:10] / target.nal;
## }
## colnames(tmp)[3:10] <- paste("rels",  c(seq(from = 1994, to = 2012, by = 3), 2013.1, 2013.3), sep = "")
## colnames(tmp2)[3:10] <- paste("reln", c(seq(from = 1994, to = 2012, by = 3), 2013.1, 2013.3), sep = "")
## pob.dist1997 <- cbind(pob.dist1997, tmp[,3:10], tmp2[,3:10])
#
## tmp <- tmp2 <- pob.dist2000
## for (i in 1:300){
##     tmp[i, 3:10] <-   tmp[i, 3:10] / target.edos[tmp$edon[i], 2:9];
##     tmp2[i, 3:10] <- tmp2[i, 3:10] / target.nal;
## }
## colnames(tmp)[3:10] <- paste("rels",  c(seq(from = 1994, to = 2012, by = 3), 2013.1, 2013.3), sep = "")
## colnames(tmp2)[3:10] <- paste("reln", c(seq(from = 1994, to = 2012, by = 3), 2013.1, 2013.3), sep = "")
## pob.dist2000 <- cbind(pob.dist2000, tmp[,3:10], tmp2[,3:10])
## #
## tmp <- tmp2 <- pob.dist2003
## for (i in 1:300){
##     tmp[i, 3:10] <-   tmp[i, 3:10] / target.edos[tmp$edon[i], 2:9];
##     tmp2[i, 3:10] <- tmp2[i, 3:10] / target.nal;
## }
## colnames(tmp)[3:10] <- paste("rels",  c(seq(from = 1994, to = 2012, by = 3), 2013.1, 2013.3), sep = "")
## colnames(tmp2)[3:10] <- paste("reln", c(seq(from = 1994, to = 2012, by = 3), 2013.1, 2013.3), sep = "")
## pob.dist2003 <- cbind(pob.dist2003, tmp[,3:10], tmp2[,3:10])
## #
## tmp <- tmp2 <- pob.dist2006
## head(tmp)
## for (i in 1:300){
##     tmp[i, 3:10] <-   tmp[i, 3:10] / target.edos[tmp$edon[i], 2:9];
##     tmp2[i, 3:10] <- tmp2[i, 3:10] / target.nal;
## }
## colnames(tmp)[3:10] <- paste("rels",  c(seq(from = 1994, to = 2012, by = 3), 2013.1, 2013.3), sep = "")
## colnames(tmp2)[3:10] <- paste("reln", c(seq(from = 1994, to = 2012, by = 3), 2013.1, 2013.3), sep = "")
## pob.dist2006 <- cbind(pob.dist2006, tmp[,3:10], tmp2[,3:10])
## #
## tmp <- tmp2 <- pob.dist2009
## for (i in 1:300){
##     tmp[i, 3:10] <-   tmp[i, 3:10] / target.edos[tmp$edon[i], 2:9];
##     tmp2[i, 3:10] <- tmp2[i, 3:10] / target.nal;
## }
## colnames(tmp)[3:10] <- paste("rels",  c(seq(from = 1994, to = 2012, by = 3), 2013.1, 2013.3), sep = "")
## colnames(tmp2)[3:10] <- paste("reln", c(seq(from = 1994, to = 2012, by = 3), 2013.1, 2013.3), sep = "")
## pob.dist2009 <- cbind(pob.dist2009, tmp[,3:10], tmp2[,3:10])
#

tmp <- tmp2 <- pob.dist2012
i <- 1
for (i in 1:300){
    tmp[i, 3:10] <-   tmp[i, 3:10] / target.edos[tmp$edon[i], 2:9];
    tmp2[i, 3:10] <- tmp2[i, 3:10] / target.nal;
}
colnames(tmp)[3:10] <- paste("rels",  c(seq(from = 1994, to = 2012, by = 3), 2013), sep = "")
colnames(tmp2)[3:10] <- paste("reln", c(seq(from = 1994, to = 2012, by = 3), 2013), sep = "")
pob.dist2012 <- cbind(pob.dist2012, tmp[,3:10], tmp2[,3:10])
head(pob.dist2012)
#
tmp <- tmp2 <- pob.dist2013.1
for (i in 1:300){
    tmp[i, 3:10] <-   tmp[i, 3:10] / target.edos[tmp$edon[i], 2:9];
    tmp2[i, 3:10] <- tmp2[i, 3:10] / target.nal;
}
colnames(tmp)[3:10] <- paste("rels",  c(seq(from = 1994, to = 2012, by = 3), 2013), sep = "")
colnames(tmp2)[3:10] <- paste("reln", c(seq(from = 1994, to = 2012, by = 3), 2013), sep = "")
pob.dist2013.1 <- cbind(pob.dist2013.1, tmp[,3:10], tmp2[,3:10])
head(pob.dist2013.1)
#
tmp <- tmp2 <- pob.dist2013.3
for (i in 1:300){
    tmp[i, 3:10] <-   tmp[i, 3:10] / target.edos[tmp$edon[i], 2:9];
    tmp2[i, 3:10] <- tmp2[i, 3:10] / target.nal;
}
colnames(tmp)[3:10] <- paste("rels",  c(seq(from = 1994, to = 2012, by = 3), 2013), sep = "")
colnames(tmp2)[3:10] <- paste("reln", c(seq(from = 1994, to = 2012, by = 3), 2013), sep = "")
pob.dist2013.3 <- cbind(pob.dist2013.3, tmp[,3:10], tmp2[,3:10])
head(pob.dist2013.3)
#
rm(tmp, tmp2)
#
# paste district populations to election objects AQUI ES DONDE PARECE ESTAR EL PROBLEMA; LOS OBJETOS DE POOBLACION SE VEN BIEN
tmp <- pob.dist2012[, c("edon","dis","ptot2006","rels2006","reln2006")]; colnames(tmp) <- c("edon", "disn", "ptot", "rels", "reln")
df2006d0 <- merge(x = df2006d0, y = tmp, by = c("edon", "disn"))
df2006s0 <- merge(x = df2006s0, y = tmp, by = c("edon", "disn"))
tmp <- pob.dist2013.1[, c("edon","dis","ptot2006","rels2006","reln2006")]; colnames(tmp) <- c("edon", "disn", "ptot", "rels", "reln")
df2006d1 <- merge(x = df2006d1, y = tmp, by = c("edon", "disn"))
df2006s1 <- merge(x = df2006s1, y = tmp, by = c("edon", "disn"))
tmp <- pob.dist2013.3[, c("edon","dis","ptot2006","rels2006","reln2006")]; colnames(tmp) <- c("edon", "disn", "ptot", "rels", "reln")
df2006d3 <- merge(x = df2006d3, y = tmp, by = c("edon", "disn"))
df2006s3 <- merge(x = df2006s3, y = tmp, by = c("edon", "disn"))
#
tmp <- pob.dist2012[, c("edon","dis","ptot2009","rels2009","reln2009")]; colnames(tmp) <- c("edon", "disn", "ptot", "rels", "reln")
df2009d0 <- merge(x = df2009d0, y = tmp, by = c("edon", "disn"))
df2009s0 <- merge(x = df2009s0, y = tmp, by = c("edon", "disn"))
tmp <- pob.dist2013.1[, c("edon","dis","ptot2009","rels2009","reln2009")]; colnames(tmp) <- c("edon", "disn", "ptot", "rels", "reln")
df2009d1 <- merge(x = df2009d1, y = tmp, by = c("edon", "disn"))
df2009s1 <- merge(x = df2009s1, y = tmp, by = c("edon", "disn"))
tmp <- pob.dist2013.3[, c("edon","dis","ptot2009","rels2009","reln2009")]; colnames(tmp) <- c("edon", "disn", "ptot", "rels", "reln")
df2009d3 <- merge(x = df2009d3, y = tmp, by = c("edon", "disn"))
df2009s3 <- merge(x = df2009s3, y = tmp, by = c("edon", "disn"))
#
tmp <- pob.dist2012[, c("edon","dis","ptot2012","rels2012","reln2012")]; colnames(tmp) <- c("edon", "disn", "ptot", "rels", "reln")
df2012d0 <- merge(x = df2012d0, y = tmp, by = c("edon", "disn"))
df2012s0 <- merge(x = df2012s0, y = tmp, by = c("edon", "disn"))
tmp <- pob.dist2013.1[, c("edon","dis","ptot2012","rels2012","reln2012")]; colnames(tmp) <- c("edon", "disn", "ptot", "rels", "reln")
df2012d1 <- merge(x = df2012d1, y = tmp, by = c("edon", "disn"))
df2012s1 <- merge(x = df2012s1, y = tmp, by = c("edon", "disn"))
tmp <- pob.dist2013.3[, c("edon","dis","ptot2012","rels2012","reln2012")]; colnames(tmp) <- c("edon", "disn", "ptot", "rels", "reln")
df2012d3 <- merge(x = df2012d3, y = tmp, by = c("edon", "disn"))
df2012s3 <- merge(x = df2012s3, y = tmp, by = c("edon", "disn"))
#

AQUI ME QUEDE: YA TENGO POBLACIONES PARA CADA ESCENARIO ELECTORAL

tmp <- df2012d0
tmp$rels[tmp$edon==15]
tmp <- df2012d0
tmp$reln[tmp$edon==15] 

pob$chg <- (pob$ptot10 - pob$ptot05) / (pob$ptot05 + 1)  # +1 para que los que salen de cero no se indeterminen
pob$chg[pob$ptot05==0]
summary(pob$chg)
colnames(pob)

pob[pob$edon==2 & pob$secn==1526,]

Verificar si las secciones que no están en 2010 pero sí en 2005 tienen todas un distrito; compararlo con el archivo del IFE para ver si se trata de distritos 2004-05. Si fuera el caso la suma pre se vuelve fácil, creo. 

dis2013[dis2013$edon==3 & dis2013$seccion==400,]

BUSCAR CENSO 2000 POR SECCIONES




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
