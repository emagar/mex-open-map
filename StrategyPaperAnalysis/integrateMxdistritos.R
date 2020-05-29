#
# Integrate @Emagar seccion database and processed map (block-equivalency file)
# Into proposal database
#
# Input: proposals.df , mxDistritosData folder
# output: propfull.df

require(readr)
require(magrittr)
require(tidyr)
require(dplyr)

#statecode mapping generated through inspection of filenames
statecodes.df <- structure(list(tla = c("ags", "bc", "bcs", "cam", "coa", "col", 
                                        "cps", "cua", "df", "dgo", "gua", "gue", "hgo", "jal", "mex", 
                                        "mic", "mor", "nay", "nl", "oax", "pue", "que", "qui", "san", 
                                        "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac"), Entidad = el <-
                                  c("Aguascalientes", "Baja California", "Baja California Sur", 
                                    "Campeche", "Chiapas", "Chihuahua", "Coahuila", "Colima", "Distrito Federal", 
                                    "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "México", 
                                    "Michoacán", "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", 
                                    "Querétaro", "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora", 
                                    "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas"
                                  ), edon = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 
                                              13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 
                                              29, 30, 31, 32)), class = c("spec_tbl_df", "tbl_df", "tbl", "data.frame"
                                              ), row.names = c(NA, -32L), spec = structure(list(cols = list(
                                                tla = structure(list(), class = c("collector_character", 
                                                                                  "collector")), name = structure(list(), class = c("collector_character", 
                                                                                                                                    "collector")), edon = structure(list(), class = c("collector_double", 
                                                                                                                                                                                      "collector"))), default = structure(list(), class = c("collector_guess", 
                                                                                                                                                                                                                                            "collector")), skip = 1), class = "col_spec"))

proposermap.df <- structure(list(proposer = c("escenario1", "escenario2", "escenario3", 
                                              "mc_clv1", "mc_clv2", "mc_cnv1", "mc_cnv2", "morena_clv1", "morena_clv2", 
                                              "morena_cnv1", "morena_cnv2", "pan_clv1", "pan_clv2", "pan_cnv1", 
                                              "pan_cnv2", "panal_clv1", "panal_clv2", "panal_cnv1", "panal_cnv2", 
                                              "pes_clv1", "pes_clv2", "pes_cnv1", "pes_cnv2", "pna_clv1", "pna_clv2", 
                                              "pna_cnv1", "pna_cnv2", "prd_clv1", "prd_clv2", "prd_cnv1", "prd_cnv2", 
                                              "pri_clv1", "pri_clv2", "pri_cnv1", "pri_cnv2", "pt_clv1", "pt_clv2", 
                                              "pt_cnv1", "pt_cnv2", "pvem_clv1", "pvem_clv2", "pvem_cnv1", 
                                              "pvem_cnv2", "escenario3_derfe"), actor = c("Algorithm", "INE", 
                                                                                          "INE", "MC", "MC", "MC", "MC", "MORENA", "MORENA", "MORENA", 
                                                                                          "MORENA", "PAN", "PAN", "PAN", "PAN", "PNA", "PNA", "PNA", "PNA", 
                                                                                          "ES", "ES", "ES", "ES", "PNA", "PNA", "PNA", "PNA", "PRD", "PRD", 
                                                                                          "PRD", "PRD", "PRI", "PRI", "PRI", "PRI", "PT", "PT", "PT", "PT", 
                                                                                          "PVEM", "PVEM", "PVEM", "PVEM", "derfe"), govlevel = c("CNV", 
                                                                                                                                                 "CNV", "CNV", "CLV", "CLV", "CNV", "CNV", "CLV", "CLV", "CNV", 
                                                                                                                                                 "CNV", "CLV", "CLV", "CNV", "CNV", "CLV", "CLV", "CNV", "CNV", 
                                                                                                                                                 "CLV", "CLV", "CNV", "CNV", "CLV", "CLV", "CNV", "CNV", "CLV", 
                                                                                                                                                 "CLV", "CNV", "CNV", "CLV", "CLV", "CNV", "CNV", "CLV", "CLV", 
                                                                                                                                                 "CNV", "CNV", "CLV", "CLV", "CNV", "CNV", "CNV"), stage = c(1, 
                                                                                                                                                                                                             3, 5, 2, 4, 2, 4, 2, 4, 2, 4, 2, 4, 2, 4, 2, 4, 2, 4, 2, 4, 2, 
                                                                                                                                                                                                             4, 2, 4, 2, 4, 2, 4, 2, 4, 2, 4, 2, 4, 2, 4, 2, 4, 2, 4, 2, 4, 
                                                                                                                                                                                                             5)), row.names = c(NA, 44L), class = "data.frame")

#
# reshapes a map file in the form of  {Enitidad, Actor, Govlevel, Stage, Proposal}
#

extractPlans<-function(.data){
  x.df<-.data
  # variable name mapping produced through inspecting: 
  # names13<- unique(as.vector(unlist(sapply(map13.ls,function(x)names(read_csv(x))))))
  # names17<- unique(as.vector(unlist(sapply(map17.ls,function(x)names(read_csv(x))))))

 # set file level parameters 
  edondef <- unique(as.vector(x.df[["edon"]]))
  if (length(edondef)!=1) {stop("contains multiple different edon values")}

  # standardize names that use . instead of _ 
  names(x.df) %<>% sub("\\.","_",.)
  actornames<-setdiff(names(x.df),c("edon","seccion","munn","inegi","ife"))
  cat(paste(actornames,sep=","))
  
  #  reashape to rows as proposals, with the proposed plan as a a variable
  kv.df <- x.df %>% gather(key="proposer",value="district",actornames) %>% group_by(proposer)
  propplans.df <- kv.df %>% summarize(plan=list(tibble(seccion,district)))
  propplans.df %<>% mutate(edon=edondef)
  
  # use maps to add linking Entidad, govlevel, actor keys
  propplans.df %<>% left_join(statecodes.df,by="edon")
  propplans.df %<>% left_join(proposermap.df,by="proposer")
  
  # patch up actors missing from map for late
  propplans.df %<>% rowwise() %>% mutate(actor=ifelse(is.na(actor),proposer,actor))
  
  return(propplans.df)
  
} 


mapfiles <- grep("csv",dir("mxDistritos-data/sec2013map/",full.names=TRUE),value = TRUE)
tmp2013.df <- bind_rows(lapply(mapfiles,function(x)extractPlans(read_csv(x)))) %>% mutate(year=2013)
mapfiles <- grep("csv",dir("mxDistritos-data/sec2017map/",full.names=TRUE),value = TRUE)
tmp2017.df <- bind_rows(lapply(mapfiles,function(x)extractPlans(read_csv(x)))) %>% mutate(year=2017)
proplans.df<- bind_rows(tmp2013.df,tmp2017.df) %>% select(-proposer,-tla)
proposals.df %<>% left_join(statecodes.df)
propfull.df <- full_join(proposals.df,proplans.df)
rm(tmp2013.df,proplans.df,mapfiles,tmp2017.df,extractPlans,proposermap.df)
#save(propfull.df,file="proposals.RData")
