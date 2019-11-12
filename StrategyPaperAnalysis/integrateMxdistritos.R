# link seccion data

library(readr)
library(tibble)
library(magrittr)

#based on 2017
statecodemap.df<-structure(list(tla = structure(1:33, .Label = c("ags", "bc", 
                                                        "bcs", "cam", "cdm", "coa", "col", "cps", "cua", "df", "dgo", 
                                                        "gua", "gue", "hgo", "jal", "mex", "mic", "mor", "nay", "nl", 
                                                        "oax", "pue", "que", "qui", "san", "sin", "son", "tab", "tam", 
                                                        "tla", "ver", "yuc", "zac"), class = "factor"), name = structure(c(1L, 
                                                                                                                           2L, 3L, 4L, 5L, 8L, 9L, 6L, 7L, 10L, 11L, 12L, 13L, 14L, 15L, 
                                                                                                                           16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 
                                                                                                                           29L, 30L, 31L, 32L, 33L), .Label = c("Aguascalientes", "Baja California", 
                                                                                                                                                                "Baja California Sur", "Campeche", "CDMX", "Chiapas", "Chihuahua", 
                                                                                                                                                                "Coahuila", "Colima", "Distrito Federal", "Durango", "Guanajuato", 
                                                                                                                                                                "Guerrero", "Hidalgo", "Jalisco", "MÃ©xico", "MichoacÃ¡n", "Morelos", 
                                                                                                                                                                "Nayarit", "Nuevo LeÃ³n", "Oaxaca", "Puebla", "QuerÃ©taro", "Quintana Roo", 
                                                                                                                                                                "San Luis PotosÃ<U+FFFD>", "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", 
                                                                                                                                                                "Tlaxcala", "Veracruz", "YucatÃ¡n", "Zacatecas"), class = "factor"), 
                       edon = structure(c(1L, 12L, 22L, 26L, NA, 27L, 28L, 29L, 
                                          30L, 31L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, NA, 13L, 
                                          14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 23L, 24L, 25L), .Label = c("1", 
                                                                                                             "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", 
                                                                                                             "2", "21", "22", "23", "24", "25", "26", "27", "28", "29", 
                                                                                                             "3", "30", "31", "32", "4", "5", "6", "7", "8", "9"), class = "factor")), row.names = c(NA, 
                                                                                                                                                                                                     -33L), class = "data.frame")
secDemo.tb <- read_csv("mxDistritos-data/dipfed2015seccion-vhat.csv")
secDemo.tb %<>% mutate(vhat.tot=vhat.morena+vhat.pri+vhat.pan)


map.ls <- grep("csv",dir("mxDistritos-data/sec2017map/",full.names=TRUE),value = TRUE)

x.df <- read_csv(map.ls[1])
extractPlans<-function(x.df){
  
  varmapx.df<-structure(c("escenario1", "mc_clv1", "morena_clv1", "pes_clv1", 
                               "pri_clv1", "pvem_clv1", "escenario2", "mc_clv2", "morena_clv2", 
                               "panal_clv2", "pes_clv2", "prd_clv2", "pri_clv2", "pt_clv2", 
                               "pvem_clv2", "escenario3", "Algorithm", "MC", "MOR", "PES", "PRI", 
                               "PVEM", "INE", "Algorithm", "MOR", "PAN", "PS", "PRD", "PRI", 
                               "PT", "PVEM", "INE", "CNV", "CLV", "CLV", "CLV", "CLV", "CLV", 
                               "CNV", "CLV", "CLV", "CLV", "CLV", "CLV", "CLV", "CLV", "CLV", 
                               "CNV", "1", "2", "2", "2", "2", "2", "3", "4", "4", "4", "4", 
                               "4", "4", "4", "4", "5"), .Dim = c(16L, 4L), .Dimnames = list(
                                 NULL, c("varname", "actor", "govlevel", "stage")))
  
  varnames<-names(x.df)
  actornames<-setdiff(names(x.df),c("edon","seccion","munn"))
  tmp.actor<-actornames[1]
  tplan<-c(by(tplan.df[,"seccion"],x.df[,tmp.actor],c), levels(x.df[,tmp.actor]))
  
  dpop <- sapply(tplan,function(x)sum(secDemo.tb[x,"efec"]))

    
  
}


             

