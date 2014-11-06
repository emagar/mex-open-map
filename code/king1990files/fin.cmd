input fin.asc;
invar year,socdv,socds,agaruv,agarus,natlcv,natlcs,fpduv,fpdus,
      sppv,spps,libpv,libps,fruralv,frurals,chrtv,chrts,
      smfv,smfs,socwv,socws,othv,oths,dist,context;
output fin;
outtyp f;
@
FINLAND (** Information for 1983 election came from the World Atlas of Elections: Voting patterns in 39 democracies.  1986.  by D. Leonard and R. Natkiel.  The Economist Publications:  London.



TYPE OF ELECTORAL SYSTEM  d'Hondt highest average.

CHANGES IN ELECTORAL SYSTEM  (1) 1936--Aland separated to become
                             a single member constituency.
                             (2) 1955--Number of constituencies changed
                             to 15.

RANGE OF ELECTIONS  1919-1983 (N=21) End of Finnish civil war to
                    1979, the last election listed in Mackie and
                    Rose(1982).

PARTY INFORMATION  (1) The forerunner to the Liberal party was the
                   National Progressive Party.  Between 1919-45 the
                   the NPP votes are recorded as Liberal votes and seats.
                   (2) In the 1933 election the National Coalition ran
                   with the right wing Patriotic Peoples Movement.  Votes
                   and seats are recorded for National Coalition.  The
                   PPM ran again in the next three elections receiving
                   14, 14 and 8 seats respectively in each election.
                   For these elections the PPM is coded as "other."
*=major party

COLUMNS IN ASCII FIlE: [Fin.asc]
year      1    YEAR
socdv     2    *SOCIAL DEMOCRATS VOTES (V)  (moderate labour party)
socds     3                     SEATS (S)
agaruv    4    *AGARIAN UNION V   (center)
agarus    5                  S
natlcv    6    *NATIONAL COALITION V  (The Conservative Party of Finland)
natlcs    7                       S
fpduv     8    *FINNISH PEOPLES DEMOCRATIC UNION (or League) V [1945-1983]
fpdus     9                                     S        [incl socwv socvs]
sppv     10   (*)SWEDISH PEOPLES PARTY V
spps     11                         S
libpv    12   *LIBERAL PEOPLES PARTY V (formerly Natl Progressive had space
                on the pol'l spectrum.  Joins Agrarian union after 1979 elen)
libps    13                         S
fruralv  14   FINNISH RURAL PARTY V [1962-1983]
frurals  15                       S
chrtv    16   CHRISTIAN LEAGUE V [1958-1983]
chrts    17                    S
smfv     18   SMALL FARMERS PARTY V [1929-1951]
smfs     19                       S
socwv    20   SOCIALIST WORKERS V [1922-1930]  becomes fpdu
socws    21                     S
othv     22   'OTHER' VOTES
oths     23           SEATS
dist     24  NUMBER OF CONSTITUENCIES [RANGE 15-16]
context  25  DUMMY VARIABLE FOR CHANGE IN NUMBER OF
             CONSTITUENCIES, CODED  1 IF 15, 0 IF 16.

@
