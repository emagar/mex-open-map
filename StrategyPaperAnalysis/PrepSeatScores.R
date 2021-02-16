## FOR TESTING ONLY -- if called outside of PrepData.R
#setwd("StrategyPaperAnalysis")
#source("PrepData.R")
###

#
# PrepSeatScores
#
# Uses election return info to calculate expected seat scores to plans
#
# Depends on: PrepData.R  -- This must be run first
# Returns: planscores.df


# use a local blocks to keep environment neat, only the objects defined outside will remain after run
## BEGIN LOCAL BLOCK
planProcessing.df <- local({ 
## 
## Data cleaning functions 
##
## Supports cleaning, merging w/ raw-seccion-* files

clean_vraw<-function(x){

  # create missing coalition columns ,
  for (cmiss in setdiff(c("morenac","pric","panc","prdc"), x %>% names())) {
    x %<>% mutate({{ cmiss }} := 0)
  }

  #Adds votes received by coalition groups to the major party with which the coalition is associated.
  x %<>%
    mutate(pan = pan + panc) %>%
    mutate(prd = prd + prdc) %>%
    mutate(pri = pri + pric) %>%
    mutate(morena = morena + morenac) %>%
    rename(es = pes) %>% pivot_longer(
      .,
      cols = c("pan", "pri", "prd", "pvem", "pt", "mc", "pna", "morena", "es"),
      names_to = "actor",
      values_to = "votes"
    ) %>% #Data is in wide format, converts the data to long format
    mutate(actor = toupper(actor)) %>%  #Converts the actor names to uppercase to match with actor names in subsequent dataframes
    select(edon, seccion, actor, votes, efec, lisnom)
}

# function to check and merge plans
standardizePlan <- function(plan,edon,votedf) {
  if(is.null(plan)) { return (NULL)}
  plan %<>% mutate(edon=edon)
  plan %<>% left_join(votedf)
  plan %<>% filter(!is.na(district) & district>0)
  
  return (plan)
}

###
### Create a tibble of plans with integrated , data which can then be used for various evaluations
###

# generate table of planids and plans -- planids are repeated across actors, so select one, 
# but make sure it has a plan attached -- some of the entries repeatedly proposed by multiple parties 
# were not recorded each time in the INE systems
planProcessing.df <- propfull.df %>% filter(!is.null(plan)) %>% group_by(planid) %>% summarize(plan=head(plan,1), edon=head(edon,1))

# read  plan, and merge into a new plan column
votes.2015.df <- read_csv("mxDistritos-data/raw-seccion-2015.csv")
votes.2018.df <- read_csv("mxDistritos-data/raw-seccion-2018.csv")
votes.2018.df %<>%  clean_vraw()
votes.2015.df %<>%  clean_vraw()

planProcessing.df %>% rowwise() %>% ## RETURN VALUE FROM LOCAL BLOCK
    mutate( plan_2015 = list(standardizePlan(plan,edon,votes.2015.df)),
            plan_2018 = list(standardizePlan(plan,edon,votes.2018.df))
    ) %>% ungroup() 
}) 

vars.srclist = c("plan_2015","plan_2018")

## BEGIN LOCAL BLOCK
planProcessing.df<- local({ 
  
## Calculate functions -- used to generate district summary tables based on standardized plans
## these summary tables are then used to calculate any scores for plans
##
## USAGE NOTE: in mutate, use rowwise first
##             and  if the result is a data frame, wrap it in list()
##
##  mydata %>% rowwise() %>% mutate( answer = list(calcXXXX(plan_2015))  


# calcDistrictTotals 
#      INPUT: splan - plan produced by standardizedPlan
#      VALUE: tibble of district-level totals
calcDistrictTotals <- function (splan) {
  if(is.null(splan)) { return (NULL)}
  
  votes_total <- splan %>%   group_by(district) %>% summarize(votes_total=sum(votes,na.rm=TRUE))

 splan %<>% 
    group_by(seccion) %>% slice_head(n=1) %>% # lisnom,efec are duplicated across rows per actor
    group_by(district) %>%
    summarise(across(c(efec,lisnom), list(total = ~sum(.x,na.rm=TRUE))))
    splan %>% left_join(votes_total)
}

# calcDistrictTotals 
#      INPUT: splan - plan produced by standardizedPlan
#      VALUE: tibble of district-level totals
calcActorDistrictTotals<- function (splan) {
  if(is.null(splan)) { return (NULL)}
  
  splan %<>%   group_by(district,actor) %>% summarize(votes_total=sum(votes,na.rm=TRUE))
  splan %<>% group_by(district) %>% mutate(votes_share=votes_total/sum(votes_total,na.rm=TRUE))
  ungroup(splan)
}

# use the calc functions to extend planProcessing.df
planProcessing.df %>% # RETURN Value for local block
  rowwise() %>%
  mutate( across(
    c({{ vars.srclist}}) , 
    list( districts = ~list(calcDistrictTotals(.x)),
          actors = ~list(calcActorDistrictTotals(.x)))
  ))
}) ## END LOCAL BLOCK


###
### Add plan scores
###
## BEGIN LOCAL BLOCK
planProcessing.df<- local({ 
  
## scoring functions -- used to generate district plan scores based on district and/ districtActor summary tables

# scoreMaxPop 
#      INPUT: district level totals
#      VALUE: score representing maximum relative deviation from population equality
scoreMaxPopDev <- function(district) {
  if(is.null(district)) { return (NA)}
  district %>% 
    ungroup() %>%
    summarise(ideal = mean(lisnom_total),
              score = max(abs(lisnom_total-ideal)/ideal)
    ) %>% 
    pull(score) 
}

#scoreSeats 
#      INPUT: actor-district level totals
#      VALUE: tibble of scoring statistics

scoreSeats <- function(actordist) {
  if(is.null(actordist)) { return (NULL)}
  
  actordist %>% 
    group_by(district) %>% 
    slice_max(votes_share,n=2) %>% 
    summarize(votes_share=list(votes_share),actor=list(actor)) %>%
    rowwise() %>% 
    mutate(win_actor=actor[1], win_margin=votes_share[1]-votes_share[2] )
}

scoreComp <- function(x,threshold=.02){
  if(is.null(x)) { return (NA)}
  
  x %>% ungroup() %>%
    summarize(score=sum(win_margin<threshold)) %>%
    pull(score)
}

scoreWins <- function(x){
  if(is.null(x)) { return (NULL)}
  
  x %>% ungroup() %>%
    count(win_actor) 
}

vars.districts <- vars.srclist %>% paste("_districts",sep="")
vars.actors <- vars.srclist %>% paste("_actors",sep="")

planProcessing.df  %<>% rowwise() %>% mutate( across( 
  c({{ vars.districts }}), 
  list(
            maxPopDev = ~scoreMaxPopDev(.x)
      ))) 

planProcessing.df  %<>% rowwise() %>% mutate( across( 
  c({{ vars.actors }}), 
  list(
    winMargins = ~list(scoreSeats(.x))
  ))) 

vars.winmargins <- vars.srclist %>% paste("_actors_winMargins",sep="")

planProcessing.df  %<>% rowwise() %>% mutate( across( 
  c({{ vars.winmargins }}), 
  list(
    compCount = ~scoreComp(.x),
    actorWins = ~list(scoreWins(.x))
  ))) 

 planProcessing.df
}) ## END LOCAL BLOCK

