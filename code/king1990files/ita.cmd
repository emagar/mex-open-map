input ita.asc;
invar year,christv,christs,commv,comms,socv,socs,msiv,msis,repv,reps,
      socdv,socds,libv,libs,styrolv,styrols,mchv,mchs,csocv,
      csocs,usocv,usocs,othv,oths,dist,context;
output ita;
outtyp f;

@
ITALY


TYPE OF ELECTORAL SYSTEM  Imperiali System--a version of d'Hondt.
                          (See Appendix A in Mackie and Rose (1982)).

CHANGES IN ELECTORAL SYSTEM 1956--change in calculation of remainder.
                            (Another change was made after the 1953
                            election but it was repealed before the 1958
                            election)

  1958--electoral law was changed so majority coalition would get a bonus above
  PR.  But the big coalition did not get a majority so the law was not used. 
  However, the law was a major political issue in the campaign; the law was 
  called the Leggetruffa (the swindle law), and was probably the cause of the 
  majority coalition's defeat.  This is a nice example of electoral laws being 
  endogenous.  Even today the word Leggetruffa in Italy refers to this law
  in the 1958 election specifically.

RANGE OF ELECTIONS 1946-1987 (N=11).

PARTY INFORMATION (1) 1968 election--the United Socialist Party was a one
                      time alliance between the Socialist Party and the
                      Social Democrats.
                  (2) 1948 election--one time alliance between Communist
                      Party and the Socialists.
                  (3) South Tyrol Party is a regionally based party in
                      the North of Italy.

COLUMNS [ita.asc]
year      1    YEAR
christv    2    CHRISTIAN DEMOCRATS VOTES (V)
christs   3                        SEATS (S)
commv     4    COMMUNIST PARTY V
comms     5                    S
socv      6    SOCIALIST PARTY V
socs      7                    S
msiv      8    SOCIAL MOVEMENT[MSI] V [1948-1983]
msis      9                         S
repv     10   REPUBLICAN PARTY V
reps     11                    S
socdv    12   SOCIAL DEMOCRATS V [1948-1983]
socds    13                    S
libv     14   LIBERAL PARTY V
libs     15                 S
styrolv  16   SOUTH TYROL PARTY V [1948-1983]
styrols  17                     S
mchv 18   MONARCHISTS V [1946-1968]
mchs 19               S
csocv 20   COMMUNIST AND SOCIALIST ALLIANCE [1948 ONLY] V
csocs 21                                                S
usocv    22   UNITED SOCIALIST PARTY [1968 ONLY] V
usocs    23                                      S
othv     24   'OTHER' VOTES
oths     25           SEATS
dist     26   NUMBER OF ELECTORAL DISTRICTS[CONSTANT=32]
context  27   DUMMY VARIABLE FOR CHANGE IN 1956 CODED 1 FOR ELECTIONS
     BETWEEN 1958 AND 1983[1987], AND O OTHERWISE.
@
