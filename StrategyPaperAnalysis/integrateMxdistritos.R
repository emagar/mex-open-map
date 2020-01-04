#
# Integrate @Emagar seccion database and processed map (block-equivalency file)
# Into proposal database

require(readr)
require(magrittr)
require(tidyr)


#
# reshapes a map file in the form of  {Enitidad, Actor, Govlevel, Stage, Proposal}
#

extractPlans<-function(.data){
  x.df<-.data
  # variable name mapping produced through inspecting: 
  # names13<- unique(as.vector(unlist(sapply(map13.ls,function(x)names(read_csv(x))))))
  # names17<- unique(as.vector(unlist(sapply(map17.ls,function(x)names(read_csv(x))))))
  
  proposermap.df <-structure(list(proposer = c("escenario1", "mc_clv1", "morena_clv1", 
                                               "pes_clv1", "pri_clv1", "pvem_clv1", "escenario2", "mc_clv2", 
                                               "morena_clv2", "panal_clv2", "pes_clv2", "prd_clv2", "pri_clv2", 
                                               "pt_clv2", "pvem_clv2", "escenario3", "pan_clv1", "pan_clv2", 
                                               "pan_cnv1", "pan_cnv2", "pna_clv1", "pna_clv2", "pna_cnv1", "pna_cnv2", 
                                               "prd_clv1", "prd_clv2", "prd_cnv1", "prd_cnv2"), actor = c("Algorithm", 
                                                                                                          "MC", "MOR", "PES", "PRI", "PVEM", "INE", "Algorithm", "MOR", 
                                                                                                          "PAN", "PES", "PRD", "PRI", "PT", "PVEM", "INE", "PAN", "PAN", 
                                                                                                          "PAN", "PAN", "PNA", "PNA", "PNA", "PNA", "PRD", "PRD", "PRD", 
                                                                                                          "PRD"), govlevel = c("CNV", "CLV", "CLV", "CLV", "CLV", "CLV", 
                                                                                                                               "CNV", "CLV", "CLV", "CLV", "CLV", "CLV", "CLV", "CLV", "CLV", 
                                                                                                                               "CNV", "CLV", "CLV", "CNV", "CNV", "CLV", "CLV", "CNV", "CNV", 
                                                                                                                               "CLV", "CLV", "CNV", "CNV"), stage = c(1, 2, 2, 2, 2, 2, 3, 4, 
                                                                                                                                                                      4, 4, 4, 4, 4, 4, 4, 5, 2, 4, 2, 4, 2, 4, 2, 4, 2, 4, 2, 4)), row.names = c(NA, 
                                                                                                                                                                                                                                                  -28L), class = "data.frame")
  #statecode mapping generated through inspection of filenames
  statecodes.df <- structure(list(tla = c("ags", "bc", "bcs", "cam", "coa", "col", 
                                          "cps", "cua", "df", "dgo", "gua", "gue", "hgo", "jal", "mex", 
                                          "mic", "mor", "nay", "nl", "oax", "pue", "que", "qui", "san", 
                                          "sin", "son", "tab", "tam", "tla", "ver", "yuc", "zac"), Entidad = c("Aguascalientes", 
                                                                                                               "Baja California", "Baja California Sur", "Campeche", "Coahuila", 
                                                                                                               "Colima", "Chiapas", "Chihuahua", "Distrito Federal", "Durango", 
                                                                                                               "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "MÃ©xico", "MichoacÃ¡n", 
                                                                                                               "Morelos", "Nayarit", "Nuevo LeÃ³n", "Oaxaca", "Puebla", "QuerÃ©taro", 
                                                                                                               "Quintana Roo", "San Luis PotosÃ<U+FFFD>", "Sinaloa", "Sonora", 
                                                                                                               "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "YucatÃ¡n", 
                                                                                                               "Zacatecas"), edon = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 
                                                                                                                                      13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 
                                                                                                                                      29, 30, 31, 32)), class = c("spec_tbl_df", "tbl_df", "tbl", "data.frame"
                                                                                                                                      ), row.names = c(NA, -32L), spec = structure(list(cols = list(
                                                                                                                                        tla = structure(list(), class = c("collector_character", 
                                                                                                                                                                          "collector")), name = structure(list(), class = c("collector_character", 
                                                                                                                                                                                                                            "collector")), edon = structure(list(), class = c("collector_double", 
                                                                                                                                                                                                                                                                              "collector"))), default = structure(list(), class = c("collector_guess", 
                                                                                                                                                                                                                                                                                                                                    "collector")), skip = 1), class = "col_spec"))
  
  # set file level parameters 
  edondef <- unique(as.vector(x.df[["edon"]]))
  if (length(edondef)!=1) {stop("contains multiple different edon values")}

  # standardize names that use . instead of _ 
  names(x.df) %<>% sub("\\.","_",.)
  actornames<-setdiff(names(x.df),c("edon","seccion","munn"))
  
  #  reashape to rows as proposals, with the proposed plan as a a variable
  kv.df <- x.df %>% gather(key="proposer",value="district",actornames) %>% group_by(proposer)
  propplans.df <- kv.df %>% summarize(plan=list(tibble(seccion,district)))
  propplans.df %<>% mutate(edon=edondef)
  
  # use maps to add linking Entidad, govlevel, actor keys
  propplans.df %<>% left_join(statecodes.df,by="edon")
  propplans.df %<>% left_join(proposermap.df,by="proposer")
  
  # patch up actors missing from map for late
  propplans.df %>% filter(is.na(actor)) %>% mutate(actor=proposer)
  
  return(propplans.df)
  
} 

proposals.df <- read_csv("proposals.csv")


mapfiles <- grep("csv",dir("mxDistritos-data/sec2013map/",full.names=TRUE),value = TRUE)
tmp2013.df <- bind_rows(lapply(mapfiles,function(x)extractPlans(read_csv(x)))) %>% mutate(year=2013)
mapfiles <- grep("csv",dir("mxDistritos-data/sec2017map/",full.names=TRUE),value = TRUE)
tmp2017.df <- bind_rows(lapply(mapfiles,function(x)extractPlans(read_csv(x)))) %>% mutate(year=2017)
proplans.df<- bind_rows(tmp2013.df,tmp2017.df) %>% select(-proposer,-tla)
propfull.df <- full_join(proposals.df,proplans.df)
save(propfull.df,file="proposals.RData")

# Example of creating plan scores

#secVhat.df <- read_csv("mxDistritos-data/dipfed-seccion-vhat-2015.csv")
#secVhat.df %<>% mutate(vhat.tot=vhat.left+vhat.pri+vhat.pan)
secRaw.df <- read_csv("mxDistritos-data/dipfed-seccion-vraw-2015.csv")
#standardize variable names so parties match proposals
names(secRaw.df)[!names(secRaw.df) %in% c("edon","seccion","disn","efec")]%<>% toupper()

proposalEfecScore <- function(plan,year,edon,seccion.df) {
  # Computes maximum deviation from equal voting population in district
  
  # merge plan with appropriate seccion data
  if(is.null(plan)) {
    return (NA)
  }
  plan %<>% mutate(edon=edon)
  plan %<>% left_join(seccion.df)
  
  # score districts
  vplan <- plan %>% filter(!is.na(district) & district>0)
  vplan %<>% group_by(district)
  
  # select valid district ids
  dscores<-summarise(vplan,efec = sum(efec,na.rm=TRUE))

  #create plan scores
  pscores <- max(abs(dscores[[2]]-mean(dscores[[2]])))/mean(dscores[[2]])
  return (pscores)
}
             
# now we can use  plan scoring functions to add measure to proposals database

tmp.df <-propfull.df[1:10,]
tmp.df %<>% rowwise() %>% mutate(score.efec=proposalEfecScore(plan,year,edon,secRaw.df))


