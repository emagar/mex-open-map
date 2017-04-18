************************************************************************
* Code and data to replicate Magar, Trelles, Altman, and McDonald 2017 *
************************************************************************

Q for Micah: what is the etiquette for replication materials. Code I prepared takes raw data, then processes it quite long in order to define the variables in the analysis. What is preferable for replication material: pre-processed data that is straightforward to use, or raw data with intricate code including all intermediate steps followed? Mike's RA no doubt encountered complication using the raw data...

********
Notation
********

Maps are named after the first congressional election that used them: the 1979, 1997, and 2006 maps. We refer to the 2013 district boundary proposal, which was never adopted for 2015 election, as the 2013 map. Three versions (the electoral board calls them scenarios) of the 2013 map are mentioned and/or analyzed: the first and second were work in progress, the third was the finalized map that was actually never adopted despite being perfectly legal. 

- d3 are district returns with 2013 proposed map (3rd and final scenario), d0 are same with 2006 map, d97 with 1997 map 
- pan, pri, etc = raw votes 
- pric, prdc, etc = coalition votes
- efec = total valid (effective) votes to compute vote shares
- nr = votes for write-in candidates (invalid under electoral law) 
- nul = void ballots 
- lisnom = registered voters
- shSecCoalPri = in counterfactual district aggregates, share of secciones with a coalition candidate 
- panw etc = dummy equal 1 if party won district
- winmg = winning margin
- ptot = total district population
- rris = Ansolabehere, Gerber, and Snyder (2002) representation index, district relative to statewide population
- rrin = Ansolabehere, Gerber, and Snyder (2002) representation index, district relative to nationwide population


********
eqPrep.r
********

analizaEscenarios.r

Usa este source code para crear distritos a partir de secciones

cd <- c("~/Dropbox/data/elecs/MXelsCalendGovt/redistrict/git-repo/mex-open-map/code/")
source(file = paste(cd, "eqPrep.r", sep = ""), echo = TRUE)
