### PrepData
###
### Ingest and structure the core data for both Strategy, Transparency, and Algorithmic Analyses
###

## Libraries 
require(tidyverse)
require(magrittr)
require(tidylog)

## Party Type Codes

#Major - PRD, PAN, PRI / Minor - PT, PVEM, ES, MORENA, MC, PNA / Admin - CLV, Algorithm, Junta, INE, derfe / Other - PRD51
actors.df <- structure(list(actor = structure(c(2L, 7L, 9L, 10L, 11L, 13L, 
                                                 14L, 15L, 1L, 6L, 12L, 5L, 4L, 8L, 3L), .Label = c("Algorithm", 
                                                                                                    "CLV", "derfe", "ES", "INE", "Junta", "MC", "MORENA", "PAN", 
                                                                                                    "PNA", "PRD", "PRD51", "PRI", "PT", "PVEM"), class = "factor"), 
                             actortype = structure(c(1L, 3L, 2L, 3L, 2L, 2L, 3L, 3L, 1L, 
                                                     1L, 4L, 1L, 3L, 3L, 1L), .Label = c("admin", "major", "minor", 
                                                                                         "other"), class = "factor")), class = "data.frame", row.names = c(NA, 
                                                                                                                                                           -15L))                                                                                                                                                     

## Proposal events database: proposal.df
source("Normalize.R") # reads in Pre-Normalized IFE tables 
## Ruling party by year and state: grule.df , controlByWindow() helper function
source("RulingParty.R") # Bring in ruling party db
## Integrate proposed plans: profull.df
source("integrateMxdistritos.R")
rm(proposals.df)
propfull.df %<>% left_join(actors.df)
propfull.df %<>% rowwise() %>% mutate(rscore=(round(SCORE,digits=5))) # prevent plans not matching across stages because of rounding error
propfull.df %<>% rowwise() %>% mutate(planid=ifelse(is.na(rscore),NA,paste(rscore,edon,year,sep="-"))) # for network analysis



