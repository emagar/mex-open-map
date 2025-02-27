library(haven)
library(tidyr)
library(readr)
library(dplyr)
library(magrittr)

RulingParty <- read_dta("parties-data/RulingPartyLongID.dta")
grule.df <- as_tibble(RulingParty) 
grule.df %<>% rename(Entidad=state)
rm(RulingParty)

controlByWindow<-function(.data,start,end) {
  td <- .data
  tmp <- td %>% group_by(edon) %>% filter(year >=start & year < end)  %>% arrange(year) %>% summarise(partylist=list(party)) %>% rowwise()
  tmp %<>%  mutate(partyseq=list(rle(as.character(partylist))),partytab=list(sort(table(as.character(partylist)),decreasing=TRUE)))
  tmp  %<>% mutate( singlecontrol = (length(partytab)==1), primary=names(partytab)[1], secondary=names(partytab)[2], tertiary=names(partytab)[3], percentsingle=partytab[1]/sum(partytab), naltpower=(length(partyseq[["values"]])-1) )
  tmp %<>% replace_na(list( tertiary = "NONE", secondary = "NONE"))
  tmp %<>% ungroup()
  return(tmp)
}









