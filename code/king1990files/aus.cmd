input aus.asc;
invar year,socv,socs,peopv,peops,leagv,leags,commv,comms,
      othv,oths,dist,context;
output aus;
outtyp f;

@
AUSTRIA

TYPE OF ELECTORAL SYSTEM d'Hondt--1919
                         (At the constituency level the method of
                         computing consituency totals was was changed in
                         1923 to the Hagenbach-Bishcoff. In 1968 the
                         method was changed to the Hare method)

CHANGES IN ELECTORAL SYSTEM. 1971 only major change.  The number of
                             constituencies dropped from 25 to 9.

RANGE OF ELECTIONS  1945-1983 (N=12)

PARTY INFORMATION  none.

COLUMN DESCRIPTION [aus.doc]
year     1    YEAR
socv     2    SOCIALIST PARTY VOTES (V)
socs     3                    SEATS (S)
peopv    4    PEOPLES PARTY V
peops    5                  S
leagv    6    LEAGUE OF INDEPENDENTS AND FREEDOM PARTY V [1949-1975]
leags    7                                             S
commv    8    COMMUNIST PARTY V
comms    9                    S
othv    10   'OTHER' VOTES
oths    11           SEATS
dist    12   NUMBER OF ELECTORAL DISTRICTS [UNTIL 1971 25, afterwards 9]
context 13   DUMMY VARIABLE FOR CHANGE IN 1971 CODED 1 IF 1971-1983, 0
     OTHERWISE.
@
