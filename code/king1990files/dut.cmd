input dut.asc;
invar year,hcdav,hcdas,labv,labs,cathpv,cathps,libpv,libps,dem66v,dem66s,
      arevv,arevs,christv,christs,commv,comms,pfuv,pfus,radv,rads,
      refpuv,refpus,psocv,psocs,dem70v,dem70s,cdav,cdas,farmv,farms,
      othv,oths,dist,dist2;
output dut;
outtyp f;

@
NETHERLANDS  (** Source for election of 1986:  World Atlas of Elections:  Voting patterns in 39 democracies.  By D. Leonard and R. Natkiel.  1986.  The Economist Publications:  London.  **)

TYPE OF ELECTORAL SYSTEM  d'Hondt

CHANGES IN ELECTORAL SYSTEM no major changes.

RANGE OF ELECTIONS 1946-1986 (N=13)

PARTY INFORMATION (1) CDA, formed after 1972 the election, is composed
                  of the Anti-Revolution Party, Christian Historical
                  Union and the Catholic Peoples Party.
                  (2) Columns 2 and 3 are the data for a hypothetical
                  CDA, i.e. it is the addition of the the three parties
                  in (1) before they actually formed the CDA.
                  (3) Newly formed parties: Democrats '66 (D'66)--1966
                                            Democrats '70 (D'70)--1970
                                     Radical Party after 1967 election.

COLUMNS [dut.asc]
year     1    YEAR
hcdav    2    HYPOTHETICAL CDA VOTES (V)  [see (2) above]
hcdas    3                     SEATS (S)
labv     4    LABOUR PARTY V
labs     5                 S
cathpv   6    CATHOLIC PEOPLES PARTY V [1946-1972]
cathps   7                           S
libpv    8    LIBERAL PARTY V
libps    9                  S
dem66v  10   DEMOCRATS '66 V [1971-1986]
dem66s  11                S
arevv   12   ANTI-REVOLUTIONARY PARTY V [1946-1972]
arevs   13                            S
christv 14   CHRISTIAN HISTORICAL UNION V [1946-1972]
christs 15                              S
commv   16   COMMUNIST PARTY V
comms   17                   S
pfuv    18   REFORMED POLITICAL PARTY (SGP) V
pfus    19                          S
radv    20   RADICAL POLITICAL PARTY V [1971-1986]
rads    21                           S
refpuv  22   REFORMED POLITICAL UNION V [1952-1986]
refpus  23                            S
psocv   24   PACIFIST AND SOCIALIST PARTY V [1959-1986]
psocs   25                                S
dem70v  26   DEMOCRATS '70 V [1971-1982]
dem70s  27                 S
cdav    28   CDA V [1977-1986]
cdas    29       S
farmv   30   FARMERS PARTY V [1959-1977]
farms   31                 S
othv    32   'OTHER' VOTES
oths    33           SEATS
dist    34  NUMBER OF ELECTORAL DISTRICTS              (1)
dist2   35  number of electoral districts - 2nd guess (18)

@
