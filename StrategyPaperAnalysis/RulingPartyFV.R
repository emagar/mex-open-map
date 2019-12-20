library(haven)
RulingParty <- read_dta("RulingPartyLong.dta")

#create empty output
tempout <- data.frame(state=NA, year=NA, party=NA, sameparty=NA, pctsamepty=NA, single_pty_dom_90_19=NA,
                      single_pty_dom_00_17=NA, alt_power1_90_19 = NA, alt_power1_00_17=NA,
                      alt_power2_90_19 = NA, alt_power2_00_17=NA,alt_power3_90_19 = NA, alt_power3_00_17=NA,
                      primary_pol_force_90_19 = NA, primary_pol_force_00_15 = NA, secondary_pol_force_90_19 = NA, 
                      secondary_pol_force_00_15 = NA, tertiary_pol_force_90_19 = NA, tertiary_pol_force_00_15 = NA,
                      multi_comp_90_19 = NA, multi_comp_00_17 = NA)

#Run for loop
for(i in unique(RulingParty$state)){
  temp <- subset(RulingParty, RulingParty$state == i)
  temp <- temp[order(temp$year, decreasing=F),]
  
  temp$sameparty <- NA
  
  #check if same party in power as prior year:
  for(u in 2:nrow(temp)){
    temp[u,4] <- ifelse(temp[u,3]==temp[u-1,3], TRUE, FALSE)
  }
  
  #percentage of years in which same party as year b4 ruled
  temp$pctsamepty <- ifelse(length(unique(temp$sameparty))==2,1,prop.table(table(temp$sameparty))[[2]])
  
  #get table of ruling parties
  tbl <- data.frame(table(temp$party))
  #get table of number of party switches
  tbl2 <- data.frame(table(temp$sameparty))

#PARTY DOMINANCE:
  #1990-2019:
  #single party dominance (dummy - T/F - if single party ruled)
  temp$single_pty_dom_90_19 <- ifelse(nrow(tbl)==1, T, F)
  
  #IF single party did not dominate period
  if(temp$single_pty_dom_90_19 == F){
  #alternation in power x1 (dummy - T/F - if alternation took place 1+ times)
  temp$alt_power1_90_19 <- ifelse(tbl2[1,2]>=1, T, F)
  #alternation in power x2 (dummy - T/F - if alternation took place 2+ times)
  temp$alt_power2_90_19 <- ifelse(tbl2[1,2]>=2, T, F)
  #alternation in power x3 (dummy - T/F - if alternation took place 3+ times)
  temp$alt_power3_90_19 <- ifelse(tbl2[1,2]>=3, T, F)
  
  #IF single party did dominate period
  }else{
    #alternation in power x1 (dummy - T/F - if alternation took place 1+ times)
    temp$alt_power1_90_19 <- F
    #alternation in power x2 (dummy - T/F - if alternation took place 2+ times)
    temp$alt_power2_90_19 <- F
    #alternation in power x3 (dummy - T/F - if alternation took place 3+ times)
    temp$alt_power3_90_19 <- F
  }
 
  #2000-2017:
  #subset
  temp2 <- subset(temp, temp$year >= 2000 & temp$year <= 2017)
  #get table of ruling parties
  tbl <- data.frame(table(temp2$party))
  #get table of number of party switches
  tbl2 <- data.frame(table(temp2$sameparty))
  
  #single party dominance (dummy - T/F - if single party ruled)
  temp$single_pty_dom_00_17 <- ifelse(nrow(tbl)==1, T, F)
  
  #IF single party did not dominate period
  if(temp$single_pty_dom_00_17 == F){
    #alternation in power x1 (dummy - T/F - if alternation took place 1+ times)
    temp$alt_power1_00_17 <- ifelse(tbl2[1,2]>=1, T, F)
    #alternation in power x2 (dummy - T/F - if alternation took place 2+ times)
    temp$alt_power2_00_17 <- ifelse(tbl2[1,2]>=2, T, F)
    #alternation in power x3 (dummy - T/F - if alternation took place 3+ times)
    temp$alt_power3_00_17 <- ifelse(tbl2[1,2]>=3, T, F)
    
    #IF single party did dominate period
  }else{
    #alternation in power x1 (dummy - T/F - if alternation took place 1+ times)
    temp$alt_power1_00_17 <- F
    #alternation in power x2 (dummy - T/F - if alternation took place 2+ times)
    temp$alt_power2_00_17 <- F
    #alternation in power x3 (dummy - T/F - if alternation took place 3+ times)
    temp$alt_power3_00_17 <- F
  }
  
#PRIMARY POLITICAL FORCE
  #1990-2019
  temp3 <- subset(temp, !temp$year %in% c(1996,2004,2013,2017))
  
  #table of ruling parties
  ptbl <- data.frame(table(temp3$party))
  ptbl <- ptbl[order(ptbl$Freq, decreasing=T),]
  
  #states with single ruling party:
  if(nrow(ptbl)==1){
    temp$primary_pol_force_90_19 <- ptbl[1,1]
    temp$secondary_pol_force_90_19 <- NA
    temp$tertiary_pol_force_90_19 <- NA
  }
  #states with two parties
  if(nrow(ptbl)==2){
    temp$primary_pol_force_90_19 <- ptbl[1,1]
    temp$secondary_pol_force_90_19 <- ptbl[2,1]
    temp$tertiary_pol_force_90_19 <- NA
  }
  #states with three parties
  if(nrow(ptbl)>=3){
    temp$primary_pol_force_90_19 <- ptbl[1,1]
    temp$secondary_pol_force_90_19 <- ptbl[2,1]
    temp$tertiary_pol_force_90_19 <- ptbl[3,1]
  }
  
  #2000-2015
  temp4 <- subset(temp, temp$year >=2000 & temp$year <=2015 & !temp$year %in% c(1996,2004,2013,2017))
  
  #table of ruling parties
  ptbl <- data.frame(table(temp4$party))
  ptbl <- ptbl[order(ptbl$Freq, decreasing=T),]
  
  #states with single ruling party:
  if(nrow(ptbl)==1){
    temp$primary_pol_force_00_15 <- ptbl[1,1]
    temp$secondary_pol_force_00_15 <- NA
    temp$tertiary_pol_force_00_15 <- NA
  }
  #states with two parties
  if(nrow(ptbl)==2){
    temp$primary_pol_force_00_15 <- ptbl[1,1]
    temp$secondary_pol_force_00_15 <- ptbl[2,1]
    temp$tertiary_pol_force_00_15 <- NA
  }
  #states with three parties
  if(nrow(ptbl)>=3){
    temp$primary_pol_force_00_15 <- ptbl[1,1]
    temp$secondary_pol_force_00_15 <- ptbl[2,1]
    temp$tertiary_pol_force_00_15 <- ptbl[3,1]
  }
  
#multiparty competition?
  #1990-2019
  temp$multi_comp_90_19 <- ifelse(is.na(temp$tertiary_pol_force_90_19), F, T)
  #2000-2015
  temp$multi_comp_00_17 <- ifelse(is.na(temp$tertiary_pol_force_00_15), F, T)
  
#reorder temp column names to match tempout columns names
  temp <- temp[,c(1:6,10,7,11,8,12,9,13,14,17,15,18,16,19:21)]
  #table(names(temp)==names(tempout)) #check to make sure they match

  #bind to output
  tempout <- rbind(temp, tempout)
}

#remove empty row from original tempout file
tempout <- subset(tempout, !is.na(tempout$state))


#MERGE INTO ORIGINAL DATAFRAME
RulingParty$id <- paste0(RulingParty$state, RulingParty$year) #create unique identifiers for merge with new data
tempout$id <- paste0(tempout$state,tempout$year)
tempout <- tempout[,4:ncol(tempout)] #remove columns that are duplicated in RulingParty before merging

#merge data - by = common id, all.x says keep all the observations in RulingParty, no.dups = don't duplicate ID column
RulingParty <- merge(RulingParty, tempout, by="id", all.x=T, no.dups = T)

#delete ID variable
RulingParty <- RulingParty[ , -which(names(RulingParty) %in% c("id"))]

#State level meta data - i.e one row row each state, with data that is consistent across years
StateDat <- RulingParty[,c(1,5:21)]
StateDat <- subset(StateDat, !duplicated(StateDat$state))

