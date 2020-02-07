# Example of creating plan scores


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

secVhat.df <- read_csv("mxDistritos-data/dipfed-seccion-vhat-2015.csv")
#secVhat.df %<>% mutate(vhat.tot=vhat.left+vhat.pri+vhat.pan)
secRaw.df <- read_csv("mxDistritos-data/dipfed-seccion-vraw-2015.csv")
#standardize variable names so parties match proposals
names(secRaw.df)[!names(secRaw.df) %in% c("edon","seccion","disn","efec")]%<>% toupper()
tmp.df <-propfull.df[1:10,]
tmp.df %<>% rowwise() %>% mutate(score.efec=proposalEfecScore(plan,year,edon,secRaw.df))

