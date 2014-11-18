input swi.asc;
invar year,socdv,socds,raddv,radds,cathcv,cathcs,farmv,farms,
        indpv,indps,libcv,libcs,commv,comms,protpv,protps,demv,dems,
        othv,oths,dist,context;
output swi;
outtyp f;

@
SWITZERLAND (** Source for 1983 election:  World Atlas of Elections:  Voting patterns in 39 democracies.  By D. Leonard and R. Natkiel.  1986.  The Economist Publications:  London. **)

TYPE OF ELECTORAL SYSTEM  Hagenbach-Biscoff system (1919)

CHANGES IN ELECTORAL SYSTEM  1971--before 1971 there were 24
                             electoral districts (cantons), afterwards 25.
                             (Women given franchise 1971)

RANGE OF ELECTIONS 1919-1983 (N=18)

PARTY INFORMATION none

COLUMNS [swi.doc]
year     1    YEAR
socdv    2    SOCIAL DEMOCRATS VOTES (V)
socds    3                     SEATS (S)
raddv    4    RADICAL DEMOCRATS V
radds    5                      S
cathcv   6    CATHOLIC CONSERVATIVES V
cathcs   7                           S
farmv    8    FARMERS, TRADERS AND CITIZENS [called Swiss People's       V
farms    9                                   Party from 1971]            S
indpv   10   INDEPENDENT PARTY V  [1935-1983]
indps   11                     S
libcv   12   LIBERAL CONSERVATIVES V
libcs   13                         S
commv   14   COMMUNIST PARTY V
comms   15                   S
protpv  16   PROTESTANT PEOPLES [also called Evangelical       V
protps  17                       Party]                        S
demv    18   DEMOCRATS V  [1919-1967]
dems    19             S
othv    20   'OTHER' VOTES
oths    21           SEATS
dist    22   NUMBER OF ELECTORAL DISTRICTS
context 23   DUMMY VARIABLE FOR 1971 CHANGES CODED 1 FOR ELECTIONS
          BETWEEN 1971-1979 INCLUSIVE, AND 0 OTHERWISE.

[from walter mattli]

Now let me answer your question about political parties in Switzerland.
In order for you to make a meaningful distinction between major parties
and others it might be useful to have the following information:
The Swiss political system at the national (federal) level is bicameral, i.e.,
we have the "Nationalrat" (Congress) and the "Staenderat" (Senate).
The seat distribution across the parties after the 1983 elections looked as
follows:

                                  NATIONALRAT        STAENDERAT
The parties

Extreme right:
  Nationale Aktion                     4                 -
  Vigilance                            1                 -

Right wing:
  Freisinnig Demokratische Partei(FDP) 54                14
  Schweizerische Volkspartei (SVP)     23                 5
  Liberale Partei (LPS)                 8                 3
  Landesring (LDU)                      8                 -

Middle right:
  Christlich Demokratische Volksp.(CVP)52                18
  Evangelische Volkspartei (EVP)        3                 -

Middle left / left:
  Sozialistische Demokratische P. (SPS)47                 6

Extreme left (communists and others):
  Partei der Arbeit                     1                 -
  Autonome sozialistische P. (PSA)      1                 -
  POCH                                  3                 -
  Grune Partei                          3                 -
                                     -------            -----
                       total           208                46

Now, this might be an important information:
In addition to the two chambers we have obviously the executive (i.e., the
government) called the "Bundesrat". Since 1959 the "Bundesrat" comprises
invariably 2 members of the FDP
           1 members of the SVP
           2 members of the CVP
  and      2 members of the SPS
  which sums up to 7 members.

So I would say that the major political parties are: FDP, SVP, LPS, CVP, SPS.
This might correspond to the following English names you had on the list:
Radical democrats, catholic conservatives, liberal conservatives, ?, social
democrats. To be sure you would have to compare my figures with yours.

The data problem (multicoll) is inevitable with the Swiss data on pol parties.
>From election to election there is very little in the seat distribution that
actually changes. Radical changes are unknown in Switzerland; that's why some
people call us stable...
Anyway, I hope this information is going to be of some help to you.
If you have any other question let me know.
Walter
@
