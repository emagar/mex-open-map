# Purpose: Takes summary spreadsheets from INE/IFE and creates normalized proposal event database
# Input: Pre-Normalized
# Output: proposals.df


require(dplyr)
require(tidyr)
require(magrittr)
require(readr)


INE2013s1.df <- read.delim(file="Pre-Normalized APSA2014PaperTables - Obs 1st Scenario.tsv",stringsAsFactors =FALSE,na.strings="-")
INE2013s2.df <- read.delim(file="Pre-Normalized APSA2014PaperTables - Obs 2nd Scsnario .tsv",stringsAsFactors =FALSE,na.strings="-")
INE2017s1.df <- read.delim(file="Pre-Normalized Comparative Tables 2017 Federal (APSA 2019) - 1erEsc 2017.tsv",stringsAsFactors =FALSE,na.strings="-")
INE2017s2.df <- read.delim(file="Pre-Normalized Comparative Tables 2017 Federal (APSA 2019) - 2doEsc+final 2017.tsv",stringsAsFactors =FALSE,na.strings="-")



# use scather/gather to normalize data

normalizeEvent<-function(input.df) {
  # gather into key value pairs
  tmpgather.df<-gather(input.df,key="key",value="value",-Entidad)
  tmpgather.df["govlevel"]=sapply(strsplit(tmpgather.df[["key"]],'.',fixed=TRUE),function(x)x[[1]])
  tmpgather.df["actor"]=sapply(strsplit(tmpgather.df[["key"]],'.',fixed=TRUE),function(x)x[[2]])
  tmpgather.df["vtype"]<-sapply(strsplit(tmpgather.df[["key"]],'.',fixed=TRUE),function(x)if(length(x)==3){x[[3]]}else{"SCORE"})
  tmpgather.df<-subset(tmpgather.df,select=-key)
  # reassemble 
  tmpspread.df<-spread(tmpgather.df,key="vtype",value="value")
  # standardize dummy variables
  tmpspread.df["PROPOSED"]<-!is.na(tmpspread.df["SCORE"])
  if (!any(names(tmpspread.df)=="INVALID")) {
    tmpspread.df["INVALID"]<-NA
  }
  tmpspread.df[["INVALID"]]<-as.logical(tmpspread.df[["INVALID"]])
  if (!any(names(tmpspread.df)=="PAPER")) {
    tmpspread.df["PAPER"]<-NA
  }
  tmpspread.df[["PAPER"]]<-as.logical(tmpspread.df[["PAPER"]])
  return(tmpspread.df) 
}




# Process 2013 scenario 1
# standardize names
names(INE2013s1.df)[which(names(INE2013s1.df)=="Primer.Escenario")]<-"CNV.Algorithm"
names(INE2013s1.df)[which(names(INE2013s1.df)=="RECOMENDACION.CT")]<-"CNV.Recomendacion"
# Recommendation is measured at different leval, and type complicates comparison
tmp2013s1.df<- normalizeEvent(subset(INE2013s1.df,select=-CNV.Recomendacion))
tmp2013s1.df["year"]<-2013
tmp2013s1.df["stage"]<-2
tmp2013s1.df[which(tmp2013s1.df["actor"]=="Algorithm"),"stage"] <- 1


#2013 Scenario 2
# normalize names
names(INE2013s2.df)[which(names(INE2013s2.df)=="Primer.Escenario")]<-"CNV.Algorithm"
names(INE2013s2.df)[which(names(INE2013s2.df)=="Segundo.Escenario")]<-"CNV.Segundo"
names(INE2013s2.df)[which(names(INE2013s2.df)=="Tercer.Escenario")]<-"CNV.Tercer"
names(INE2013s2.df)[which(names(INE2013s2.df)=="Junta.Local.Ejecutiva")]<-"CNV.Junta"
names(INE2013s2.df)[which(names(INE2013s2.df)=="Ganador")]<-"CNV.Ganador"
tmp2013s2.df <- normalizeEvent(subset(INE2013s2.df,select=-c(CNV.Ganador,CNV.Algorithm)))
tmp2013s2.df["year"]<-2013
tmp2013s2.df["stage"]<- 4
tmp2013s2.df[which(tmp2013s2.df["actor"]=="Segundo"),"stage"] <- 3
tmp2013s2.df[which(tmp2013s2.df["actor"]=="Segundo"),"actor"] <- "INE"
tmp2013s2.df[which(tmp2013s2.df["actor"]=="Tercer"),"stage"] <- 5
tmp2013s2.df[which(tmp2013s2.df["actor"]=="Tercer"),"actor"] <- "INE"


#2017 scenario 1
# standardize names
names(INE2017s1.df)[which(names(INE2017s1.df)=="Primer.Escenario")]<-"CNV.Algorithm"
# Recommendation is measured at different leval, and type complicates comparison
tmp2017s1.df<- normalizeEvent(INE2017s1.df)
tmp2017s1.df["year"]<-2017
tmp2017s1.df["stage"]<-2
tmp2017s1.df[which(tmp2017s1.df["actor"]=="Algorithm"),"stage"] <- 1
tmp2017s1.df[which(tmp2017s1.df["Entidad"]=="CDMX"),"Entidad"] <- "Distrito Federal"

# normalize names
names(INE2017s2.df)[which(names(INE2017s2.df)=="Primer.Escenario")]<-"CNV.Algorithm"
names(INE2017s2.df)[which(names(INE2017s2.df)=="Segundo.Escenario")]<-"CNV.Segundo"
names(INE2017s2.df)[which(names(INE2017s2.df)=="Tercer.Escenario")]<-"CNV.Tercer"
tmp2017s2.df <- normalizeEvent(INE2017s2.df)
tmp2017s2.df["year"]<-2017
tmp2017s2.df["stage"]<- 4
tmp2017s2.df[which(tmp2017s2.df["actor"]=="Segundo"),"stage"] <- 3
tmp2017s2.df[which(tmp2017s2.df["actor"]=="Segundo"),"actor"] <- "INE"
tmp2017s2.df[which(tmp2017s2.df["actor"]=="Tercer"),"stage"] <- 5
tmp2017s2.df[which(tmp2017s2.df["actor"]=="Tercer"),"actor"] <- "INE"
tmp2017s2.df[which(tmp2017s2.df["Entidad"]=="CDMX"),"Entidad"] <- "Distrito Federal"

#Merge and analyze
proposals.df<-rbind(tmp2013s1.df ,tmp2013s2.df, tmp2017s1.df, tmp2017s2.df)
rm(INE2013s1.df, INE2013s2.df, INE2017s1.df, INE2017s2.df, tmp2013s1.df, tmp2013s2.df, tmp2017s1.df,tmp2017s2.df)
rm(normalizeEvent)


