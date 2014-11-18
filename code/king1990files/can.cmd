input can.asc;
invar year,dist,msd,mxsd,consv,conss,libv,libs,labv,labs,comv,coms,nppv,
      npps,ndpv,ndps,reconv,recons,scv,scs,bpcv,bpcs,rdcv,rdcs,rhinv,rhins,
      othv,oths,context;
output can;
outtyp f;

@
Canada.asc
Variables:  (all votes and seats in NUMBERS of..)
---------------------------------------
year     Year
dist     Number of Districts
msd      Minimum Number of Seats/District
mxsd     Maximum Number of Seats/District
consv    Conservative Votes, 1921-84
conss    Conservative Seats
libv     Liberal Votes, 1921-84
libs     Liberal Seats
labv     Labour Votes, 1921-40
labs     Labour Seats
comv     Communist Votes, 1921-40
coms     Communist Seats, only 1 seat in 1945.
nppv     National Progressive Party Votes, 1921-30
npps     National Progressive Party Seats
ndpv     CCF/NDP Votes, 1935-84
ndps     CCF/NDP Seats
reconv   Reconstruction Party Votes, 1935 only
recons   Reconstruction Party Seats
scv      Social Credit Votes, 1935-1984
scs      Social Credit Seats
bpcv     Bloc Populaire Canadien Votes, 1945
bpcs     Bloc Populaire Canadien Seats
rdcv     Ralliement des Creditistes Votes, 1965-8
rdcs     Ralliement des Creditistes Seats
rhinv    Rhinoceros Votes, 1979-84
rhins    Rhinoceros Seats
othv     Other Votes, 1921-84
oths     Other Seats
context  Contextual Column

The contextual column is coded 0=5 2-member districts;
                               1=4 2-member districts;
                               2=2 2-member districts;
                               3=abolish 2-member districts.

------------------------------------------------------------
There is one major caveat.  Although other sources cited below said that the 
  other districts are single-member (which I interpret as meaning that one 
  seat=one district), the Butler article cited below said that Canada had
  282 seats and 264 districts.  I coded the above on the bases of the other
  sources and decided that Butler must be wrong.

Canada uses a plurality election system.  Until 1921 there were five
  2-member districts; from 1925-1930 four; from 1935 to 1966 two; and 
  2-member districts were abolished in 1966 (Mackie and Rose, p. 70).

Sources:

Mackie, Thomas T., and Richard Rose.  1982.  The International Almanac
  of Electoral History, second edition.  London:  Macmillan Press Ltd.
  Pp. 70-83.

Butler, David.  1981.  "Electoral Systems."  In D. Butler, H.R. Penniman,
  and A. Ranney.  Democracy at the Polls.  Washington:  AEI.  P. 12.

1984 Rapport Statutaire Du Directeur General Des Elections Du Canada.  1984.
   Publie par le Directeur General Des Elections du Canada.  Ministre des
   Approvisionnements et Services Canada.  Appendix D.

Smith, David E.  1985.  "Party Government, Representation and National
  Integration in Canada."  In Peter Aucoin, ed.  Party Government and Regional
  Representation in Canada.  Toronto:  University of Toronto Press.  Appendix
  A.  P. 54.

Van Loon, Richard J., and Michael S. Whittington.  1981.  The Canadian 
  Political System, third edition.  Toronto:  McGraw-Hill Ryerson Limited.

@
