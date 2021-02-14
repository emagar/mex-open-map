## FOR TESTING ONLY -- if called outside of PrepData.R
setwd("StrategyPaperAnalysis")
source("PrepData.R")
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
  #Adds votes received by coalition groups to the major party with which the coalition is associated.
  x %<>%
  mutate(pan = pan + panc) %>%
  mutate(prd = prd + prdc) %>%
  mutate(pri = pri + pric) %>%
  mutate(morena = morena + morenac) %>%
  #select(., -panc, -pric, -prdc, -morenac) %>% #drop the unneeded rows
  rename(es=pes) %>% pivot_longer(., cols = c("pan", "pri", "prd", "pvem", "pt", "mc", "pna", "morena", "es"),
                            names_to = "actor",
                            values_to = "votes") %>% #Data is in wide format, converts the data to long format
  mutate(actor = toupper(actor)) %>%  #Converts the actor names to uppercase to match with actor names in subsequent dataframes
  select(edon,seccion,actor,votes,efec,lisnom)
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

# generate table of planids and plans -- planids are repeated across actors, so select one
planProcessing.df <- propfull.df %>% group_by(planid) %>% summarize(plan=head(plan,1), edon=head(edon,1))

# read  plan, and merge into a new plan column
votes.2015.df <- read_csv("mxDistritos-data/raw-seccion-2015.csv")
votes.2018.df <- read_csv("mxDistritos-data/raw-seccion-2018.csv")
votes.2018.df %<>%  mutate(prdc=0,morenac=0) %>% clean_vraw()
votes.2015.df %<>%  mutate(panc=0,morenac=0) %>% clean_vraw()

planProcessing.df %>% rowwise() %>% ## RETURN VALUE FROM LOCAL BLOCK
    mutate( plan_2015 = list(standardizePlan(plan,edon,votes.2015.df)),
            plan_2018 = list(standardizePlan(plan,edon,votes.2018.df))
    ) %>% ungroup() 
}) 

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
    c(plan_2015,plan_2018) , 
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

planProcessing.df  %<>% rowwise() %>% mutate( across( 
  c(plan_2015_districts, plan_2018_districts), 
  list(
            maxPopDev = ~scoreMaxPopDev(.x)
      ))) 

planProcessing.df  %<>% rowwise() %>% mutate( across( 
  c(plan_2015_actors, plan_2018_actors), 
  list(
    winMargins = ~list(scoreSeats(.x))
  ))) 

planProcessing.df  %<>% rowwise() %>% mutate( across( 
  c(plan_2015_actors_winMargins, plan_2018_actors_winMargins), 
  list(
    compCount = ~scoreComp(.x),
    actorWins = ~list(scoreWins(.x))
  ))) 

 planProcessing.df
}) ## END LOCAL BLOCK




planProcessing.df[1,"plan_2015_actors_winMargins"][[1]][[1]] %>% scoreWins()


### TESTING
#tstPlan <- planscores.df[[1,"plan_2015"]][[1]]
#tstPlan %>% calcDistrictTotals 
#tstPlan %>% calcActorDistrictTotals
tstscores.df <- planProcessing.df %>% 
  ungroup() %>% 
  slice_head(n=10) 

{
  tic()
  out<- capture.output(type="message",{
    # do stuff
    tst<- planProcessing.df %>% select(plan_2015_districts, plan_2018_districts) %>% rowwise() %>% mutate( 
      across( c(plan_2015_districts, plan_2018_districts), list(maxPopDev = ~scoreMaxPopDev(.x)) )
    ) 
  })
  toc()
}









#Filter for only the "best plans" (those with the lowest score)
test.df <- test.df %>%
  filter(best_plan == TRUE) %>%
  unnest(plan) #unnest the "plan" variable so that the vote share data can be merged at the seccion level

#Select plans that occurred in 2013, rename variables to match the Vote share data
test13.df <- test.df %>%
  filter(year.x == 2013) %>%
  rename(disn = district,
         edon = edon.x)

#Merge the vote share with the data for the proposals
test13.df <- left_join(test13.df, Vraw.2015, by = c("edon", "disn", "seccion"))

#Sums vote data by each district and computes the vote share. 
test13.df %<>% na.omit() %>%
  group_by(planid, edon, disn, actor.y) %>%
  summarise(., votes_2015 = sum(votes_2015),
            lisnom_2015 = sum(lisnom),
            efec_2015 = sum(efec),
            dprdc_2015 = sum(dprdc),
            dpric_2015 = sum(dpric)) %>%
  mutate(VoteShare_2015 = (votes_2015/efec_2015)*100,
         dprdc = if_else(dprdc_2015 > 0, 1, 0),
         dpric = if_else(dpric_2015 > 0, 1, 0),
         Max_VS = max(VoteShare_2015),
         Seat_Win_0 = if_else(VoteShare_2015 == Max_VS, 1, 0)) %>% #Registers which party won the district (Having the highest Vote Share in the district --> winning the seat)
  select(., -dprdc_2015, - dpric_2015, -Max_VS) %>%
  rename(actor = actor.y)


### These steps are repeated for subsequent categories of plans (plans with section 8 violations, rule violations, and finally, plans which were adopted)
####

test.df_1 <- left_join(planProp.df, proptrans.df, by = c("planid"))

#Filters for plans that won due to section 8 violations
test.df_1 <- test.df_1 %>%
  filter(win_section8 == TRUE) %>%
  unnest(plan)

test13.df_1 <- test.df_1 %>%
  filter(year.x == 2013) %>%
  rename(disn = district,
         edon = edon.x)
test13.df_1 <- left_join(test13.df_1, Vraw.2015, by = c("edon", "disn", "seccion"))
test13.df_1 %<>% na.omit() %>%
  group_by(planid, edon, disn, actor.y) %>%
  summarise(., votes_2015 = sum(votes_2015),
            lisnom_2015 = sum(lisnom),
            efec_2015 = sum(efec),
            dprdc_2015 = sum(dprdc),
            dpric_2015 = sum(dpric)) %>%
  mutate(VoteShare_2015 = (votes_2015/efec_2015)*100,
         dprdc = if_else(dprdc_2015 > 0, 1, 0),
         dpric = if_else(dpric_2015 > 0, 1, 0),
         Max_VS = max(VoteShare_2015),
         Seat_Win_1 = if_else(VoteShare_2015 == Max_VS, 1, 0)) %>%
  select(., -dprdc_2015, - dpric_2015, -Max_VS) %>%
  rename(actor = actor.y)


###
test.df_2 <- left_join(planProp.df, proptrans.df, by = c("planid"))

#Filters for plans that won due to rule violations
test.df_2 <- test.df_2 %>%
  filter(win_violation == TRUE) %>%
  unnest(plan)

test13.df_2 <- test.df_2 %>%
  filter(year.x == 2013) %>%
  rename(disn = district,
         edon = edon.x)
test13.df_2 <- left_join(test13.df_2, Vraw.2015, by = c("edon", "disn", "seccion"))
test13.df_2 %<>% na.omit() %>%
  group_by(planid, edon, disn, actor.y) %>%
  summarise(., votes_2015 = sum(votes_2015),
            lisnom_2015 = sum(lisnom),
            efec_2015 = sum(efec),
            dprdc_2015 = sum(dprdc),
            dpric_2015 = sum(dpric)) %>%
  mutate(VoteShare_2015 = (votes_2015/efec_2015)*100,
         dprdc = if_else(dprdc_2015 > 0, 1, 0),
         dpric = if_else(dpric_2015 > 0, 1, 0),
         Max_VS = max(VoteShare_2015),
         Seat_Win_2 = if_else(VoteShare_2015 == Max_VS, 1, 0)) %>%
  select(., -dprdc_2015, - dpric_2015, -Max_VS) %>%
  rename(actor = actor.y)

###
test.df_3 <- left_join(planProp.df, proptrans.df, by = c("planid"))

#Filters for plans that won at the end  of the process (regardless of how they won (best plan, section 8, rule violation))
test.df_3 <- test.df_3 %>%
  filter(win_final == TRUE) %>%
  unnest(plan)

test13.df_3 <- test.df_3 %>%
  filter(year.x == 2013) %>%
  rename(disn = district,
         edon = edon.x)
test13.df_3 <- left_join(test13.df_3, Vraw.2015, by = c("edon", "disn", "seccion"))
test13.df_3 %<>% na.omit() %>%
  group_by(planid, edon, disn, actor.y) %>%
  summarise(., votes_2015 = sum(votes_2015),
            lisnom_2015 = sum(lisnom),
            efec_2015 = sum(efec),
            dprdc_2015 = sum(dprdc),
            dpric_2015 = sum(dpric)) %>%
  mutate(VoteShare_2015 = (votes_2015/efec_2015)*100,
         dprdc = if_else(dprdc_2015 > 0, 1, 0),
         dpric = if_else(dpric_2015 > 0, 1, 0),
         Max_VS = max(VoteShare_2015),
         Seat_Win_3 = if_else(VoteShare_2015 == Max_VS, 1, 0)) %>%
  select(., -dprdc_2015, - dpric_2015, -Max_VS) %>%
  rename(actor = actor.y)

##
#Regenerates the Vote Share data in the long form and computes vote share and seat wins by party 
url.file <- "https://raw.githubusercontent.com/emagar/mxDistritos/master/data/dipfed-seccion-vraw-2015.csv"
Vraw.2015 <- read.csv(url(url.file))

Vraw.2015 <- Vraw.2015 %>%
  mutate(prd = prd + prdc) %>%
  mutate(pri = pri + pric) %>%
  select(., -ph, -prdc, -pric)

Vraw.2015 %<>% pivot_longer(., cols = c("pan", "pri", "prd", "pvem", "pt", "mc", "pna", "morena", "pes"),
                            names_to = "actor",
                            values_to = "votes_2015")
Vraw.2015 <- Vraw.2015 %>%
  mutate(actor = toupper(actor))

Vraw.2015 %<>% na.omit() %>%
  group_by(edon, disn, actor) %>%
  summarise(., votes_2015 = sum(votes_2015),
            lisnom_2015 = sum(lisnom),
            efec_2015 = sum(efec),
            dprdc_2015 = sum(dprdc),
            dpric_2015 = sum(dpric)) %>%
  mutate(VoteShare_2015 = (votes_2015/efec_2015)*100,
         dprdc = if_else(dprdc_2015 > 0, 1, 0),
         dpric = if_else(dpric_2015 > 0, 1, 0)) %>%
  select(., -dprdc_2015, - dpric_2015)

Vraw.2015 %<>% as_tibble() %>%
  group_by(edon, disn) %>%
  mutate(Max_VS = max(VoteShare_2015),
         Seat_Win = if_else(VoteShare_2015 == Max_VS, 1, 0)) %>%
  select(., -Max_VS)

#Using the Vraw datafram as a base, merges the dataframes from directly above which record the seat wins by party by district under the different win plan adoption conditions
test13.final.df <- left_join(Vraw.2015, test13.df, by = c("edon", "disn", "actor"))
test13.final.df <- left_join(test13.final.df, test13.df_1, by = c("edon", "disn", "actor"))
test13.final.df <- left_join(test13.final.df, test13.df_2, by = c("edon", "disn", "actor"))
test13.final.df <- left_join(test13.final.df, test13.df_3, by = c("edon", "disn", "actor"))


#Selects for the columns which record the seat wins under the different adoption conditons (as well as state, district, actor)
test.final.simplified <- test13.final.df %>%
  select(edon, disn, actor, Seat_Win, Seat_Win_0, Seat_Win_1, Seat_Win_2, Seat_Win_3)%>%
  mutate(Seat_Win_0 = coalesce(Seat_Win_0, Seat_Win),
         Seat_Win_1 = coalesce(Seat_Win_1, Seat_Win),
         Seat_Win_2 = coalesce(Seat_Win_2, Seat_Win), #Coalesce imputes the value from one row into another
         Seat_Win_3 = coalesce(Seat_Win_3, Seat_Win)) #Assumes that where plans do not propose a change to the electoral districts, they remain the same --> 
#the party that won the seat remains the same in that case

#Generates the final table that i have shown at meetings
test.final.simplified %>%
  group_by(actor) %>%
  summarise_at(vars(Seat_Win, Seat_Win_0, Seat_Win_1, Seat_Win_2, Seat_Win_3), list(sum)) %>%
  rename(Results_2015 = Seat_Win, Best_Plan = Seat_Win_0, Section_8 = Seat_Win_1, Rule_Violation = Seat_Win_2, Final = Seat_Win_3)-> table_13

table_13 %>% gt()
#######################
test.df <- left_join(planProp.df, proptrans.df, by = c("planid"))

test.df <- test.df %>%
  filter(best_plan == TRUE) %>%
  unnest(plan)

test17.df <- test.df %>%
  filter(year.x == 2017) %>%
  rename(disn = district,
         edon = edon.x)
test17.df <- left_join(test17.df, Vraw.2018, by = c("edon", "disn", "seccion"))

test17.df %<>% select(., -INVALID, -PAPER) %>%
  na.omit() %>%
  group_by(planid, edon, disn, actor.y) %>%
  summarise(., votes_2018 = sum(votes_2018),
            lisnom_2018 = sum(lisnom),
            efec_2018 = sum(efec),
            dpanc_2018 = sum(dpanc),
            dpric_2018 = sum(dpric),
            dmorenac_2018 = sum(dmorenac)) %>%
  mutate(VoteShare_2018 = (votes_2018/efec_2018)*100,
         dpanc = if_else(dpanc_2018 > 0, 1, 0),
         dpric = if_else(dpric_2018 > 0, 1, 0),
         dmorena = if_else(dmorenac_2018 > 0, 1, 0),
         Max_VS = max(VoteShare_2018),
         Seat_Win_0 = if_else(VoteShare_2018 == Max_VS, 1, 0)) %>%
  select(., -dpanc_2018, -dpric_2018, -dmorenac_2018) %>%
  rename(actor = actor.y)

####

test.df_1 <- left_join(planProp.df, proptrans.df, by = c("planid"))

test.df_1 <- test.df_1 %>%
  filter(win_section8 == TRUE) %>%
  unnest(plan)

test17.df_1 <- test.df_1 %>%
  filter(year.x == 2017) %>%
  rename(disn = district,
         edon = edon.x)
test17.df_1 <- left_join(test17.df_1, Vraw.2018, by = c("edon", "disn", "seccion"))
test17.df_1 %<>% select(., -INVALID, -PAPER) %>%
  na.omit() %>%
  group_by(planid, edon, disn, actor.y) %>%
  summarise(., votes_2018 = sum(votes_2018),
            lisnom_2018 = sum(lisnom),
            efec_2018 = sum(efec),
            dpanc_2018 = sum(dpanc),
            dpric_2018 = sum(dpric),
            dmorenac_2018 = sum(dmorenac)) %>%
  mutate(VoteShare_2018 = (votes_2018/efec_2018)*100,
         dpanc = if_else(dpanc_2018 > 0, 1, 0),
         dpric = if_else(dpric_2018 > 0, 1, 0),
         dmorena = if_else(dmorenac_2018 > 0, 1, 0),
         Max_VS = max(VoteShare_2018),
         Seat_Win_1 = if_else(VoteShare_2018 == Max_VS, 1, 0)) %>%
  select(., -dpanc_2018, - dpric_2018, -dmorenac_2018, -Max_VS) %>%
  rename(actor = actor.y)


###
test.df_2 <- left_join(planProp.df, proptrans.df, by = c("planid"))

test.df_2 <- test.df_2 %>%
  filter(win_violation == TRUE) %>%
  unnest(plan)

test17.df_2 <- test.df_2 %>%
  filter(year.x == 2017) %>%
  rename(disn = district,
         edon = edon.x)
test17.df_2 <- left_join(test17.df_2, Vraw.2018, by = c("edon", "disn", "seccion"))
test17.df_2 %<>% select(., -INVALID, -PAPER) %>%
  na.omit() %>%
  group_by(planid, edon, disn, actor.y) %>%
  summarise(., votes_2018 = sum(votes_2018),
            lisnom_2018 = sum(lisnom),
            efec_2018 = sum(efec),
            dpanc_2018 = sum(dpanc),
            dpric_2018 = sum(dpric),
            dmorenac_2018 = sum(dmorenac)) %>%
  mutate(VoteShare_2018 = (votes_2018/efec_2018)*100,
         dpanc = if_else(dpanc_2018 > 0, 1, 0),
         dpric = if_else(dpric_2018 > 0, 1, 0),
         dmorena = if_else(dmorenac_2018 > 0, 1, 0),
         Max_VS = max(VoteShare_2018),
         Seat_Win_2 = if_else(VoteShare_2018 == Max_VS, 1, 0)) %>%
  select(., -dpanc_2018, - dpric_2018, -dmorenac_2018, -Max_VS) %>%
  rename(actor = actor.y)

###
test.df_3 <- left_join(planProp.df, proptrans.df, by = c("planid"))

test.df_3 <- test.df_3 %>%
  filter(win_final == TRUE) %>%
  unnest(plan)

test17.df_3 <- test.df_3 %>%
  filter(year.x == 2017) %>%
  rename(disn = district,
         edon = edon.x)
test17.df_3 <- left_join(test17.df_3, Vraw.2018, by = c("edon", "disn", "seccion"))
test17.df_3 %<>% select(., -INVALID, -PAPER) %>%
  na.omit() %>%
  group_by(planid, edon, disn, actor.y) %>%
  summarise(., votes_2018 = sum(votes_2018),
            lisnom_2018 = sum(lisnom),
            efec_2018 = sum(efec),
            dpanc_2018 = sum(dpanc),
            dpric_2018 = sum(dpric),
            dmorenac_2018 = sum(dmorenac)) %>%
  mutate(VoteShare_2018 = (votes_2018/efec_2018)*100,
         dpanc = if_else(dpanc_2018 > 0, 1, 0),
         dpric = if_else(dpric_2018 > 0, 1, 0),
         dmorena = if_else(dmorenac_2018 > 0, 1, 0),
         Max_VS = max(VoteShare_2018),
         Seat_Win_3 = if_else(VoteShare_2018 == Max_VS, 1, 0)) %>%
  select(.,  -dpanc_2018, - dpric_2018, -dmorenac_2018, -Max_VS) %>%
  rename(actor = actor.y)

##
url.file <- "https://raw.githubusercontent.com/emagar/mxDistritos/master/data/dipfed-seccion-vraw-2018.csv"
Vraw.2018 <- read.csv(url(url.file))

Vraw.2018 <- Vraw.2018 %>%
  mutate(pan = pan + panc) %>%
  mutate(pri = pri + pric) %>%
  mutate(morena = morena + morenac) %>%
  select(., -panc, -pric, -morenac)

Vraw.2018 %<>% pivot_longer(., cols = c("pan", "pri", "prd", "pvem", "pt", "mc", "pna", "morena", "pes"),
                            names_to = "actor",
                            values_to = "votes_2018")
Vraw.2018 <- Vraw.2018 %>%
  mutate(actor = toupper(actor))

#Aggregate numbers by district and
#Generate values for vote share by actor
Vraw.2018 %<>% na.omit() %>%
  group_by(edon, disn, actor) %>%
  summarise(., votes_2018 = sum(votes_2018),
            lisnom_2018 = sum(lisnom),
            efec_2018 = sum(efec),
            dpanc_2018 = sum(dpanc),
            dpric_2018 = sum(dpric),
            dmorenac_2018 = sum(dmorenac)) %>%
  mutate(VoteShare_2018 = (votes_2018/efec_2018)*100,
         dpanc = if_else(dpanc_2018 > 0, 1, 0),
         dpric = if_else(dpric_2018 > 0, 1, 0),
         dmorena = if_else(dmorenac_2018 > 0, 1, 0)) %>%
  select(., -dpanc_2018, - dpric_2018, -dmorenac_2018)

Vraw.2018 %<>% as_tibble() %>%
  group_by(edon, disn) %>%
  mutate(Max_VS = max(VoteShare_2018),
         Seat_Win = if_else(VoteShare_2018 == Max_VS, 1, 0)) %>%
  select(., -Max_VS)

test17.final.df <- left_join(Vraw.2018, test17.df, by = c("edon", "disn", "actor"))
test17.final.df <- left_join(test17.final.df, test17.df_1, by = c("edon", "disn", "actor"))
test17.final.df <- left_join(test17.final.df, test17.df_2, by = c("edon", "disn", "actor"))
test17.final.df <- left_join(test17.final.df, test17.df_3, by = c("edon", "disn", "actor"))

test.final.simplified_2 <- test17.final.df %>%
  select(edon, disn, actor, Seat_Win, Seat_Win_0, Seat_Win_1, Seat_Win_2, Seat_Win_3) %>%
  mutate(Seat_Win_0 = coalesce(Seat_Win_0, Seat_Win),
         Seat_Win_1 = coalesce(Seat_Win_1, Seat_Win),
         Seat_Win_2 = coalesce(Seat_Win_2, Seat_Win),
         Seat_Win_3 = coalesce(Seat_Win_3, Seat_Win))

test.final.simplified_2 %>%
  group_by(actor) %>%
  summarise_at(vars(Seat_Win, Seat_Win_0, Seat_Win_1, Seat_Win_2, Seat_Win_3), list(sum)) %>%
  rename(Results_2018 = Seat_Win, Best_Plan = Seat_Win_0, Section_8 = Seat_Win_1, Rule_Violation = Seat_Win_2, Final = Seat_Win_3)-> table17_ 
table17_ %>% gt()

#Table:
# Filter out Table 4 row 3 and 4 as the violations
# Min scores in cases where these violations
#Unnest
#planscore functions.r, return a list

#Table:
# filter Ruke violations true
# Who proposed the rule violations where rule  violations -- did PRI propose and benefit?
```
