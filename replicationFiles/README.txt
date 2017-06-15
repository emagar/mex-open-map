************************************************************************
* Code and data to replicate Magar, Trelles, Altman, and McDonald 2017 *
************************************************************************

Q for MA: I trimmed the code in order to make replication easier. Please take a look because there are certainly still redundant variables in the data---quantities that are not used to produce our results, but make interpretation easier and evaluation possible. I am unfamiliar with the etiquette for replication materials. EM

********
Notation
********

Maps are named after the first congressional election that used them: the 1979, 1997, and 2006 maps. We refer to the 2013 district boundary proposal, which was never adopted for 2015 election, as the 2013 map. Three versions (the electoral board calls them scenarios) of the 2013 map are mentioned and/or analyzed: the first and second were work in progress, the third was the finalized map that sent to the election board for approval but was actually never adopted (despite being perfectly legal). 

- d3 are district returns with 2013 proposed map (the 3rd and final scenario), d0 are same with 2006 map, d97 with 1997 map 
- pan, pri, prd etc = raw votes 
- pric, prdc, etc = coalition votes
- efec = total valid (effective) votes to compute vote shares
- nr = votes for write-in candidates (invalid under electoral law) 
- nul = void ballots 
- lisnom = registered voters
- shSecCoalPri = in counterfactual district aggregates, share of secciones with a coalition candidate 
- panw, priw etc = dummy equal 1 if party won district, 0 otherwise
- winmg = winning margin
- ptot = total population
- rris = Ansolabehere, Gerber, and Snyder (2002) district's representation index, relative to statewide population
- rrin = Ansolabehere, Gerber, and Snyder (2002) district's representation index, relative to nationwide population



****
code
****

Analysis is implemented in R. Three code files are distributed in the code/ sub-directory:

1) mainScript.r,
2) eqPrep.r and
3) linzerElas.r 

File mainScript.r invokes the other two. 

File eqPrep.r has all the details to aggregate secciones into district boundaries according to different maps.

File linzerElas.r tweaks Linzer (2012) code, extending the method to separate the components of partisan bias.

