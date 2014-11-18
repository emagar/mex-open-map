input brit.asc;
invar year,tvotes,tseats,consv,conss,labv,labs,libv,libs,othv,oths,
      age,suspend,ulster;
output brit;
outtyp f;

@
Great Britain.

Year = year
tvotes = Total votes
tseats = total seats
consv = conservative votes
conss = conservative seats
labv = labor votes
labs = labor seats
libv = liberal votes
libs = liberal seats
othv = other votes
oths = other seats
age
suspend
ulster


Data for the elections from 1950 through 1979 are from 

     Mackie, Thomas T., and Rose, Richard.  1982.  THE
INTERNATIONAL ALMANAC OF ELECTORAL HISTORY, 2nd edition.
London:  MacMillan Press Ltd.  Pp. 366-367, 378-380.

Data for the 1979 and 1983 elections are from

     Leonard, Dick, and Natkiel, Richard.  1986.  WORLD ATLAS
OF ELECTIONS:  VOTING PATTERNS IN 39 DEMOCRACIES.  London:  The Economist
Publications.  Pp.  130-1.

Data for the 1987 elections are from

     "National Elections."  1987.  ELECTORAL STUDIES.  6 (3, December):  302.

The election series begins in 1950; "university voting," "the business vote,"
and two-member districts were abolished in 1948 (Mackie and Rose, 1982: 366). 
The electoral system is single-member district (650 districts), plurality
voting (Leonard and Natkiel, 1986: 130).

The data include several contextual variables.  In 1969, the voting age was
reduced from 21 to 18 (Mackie and Rose, 1982: 366); so a variable (AGE) is
included for that, coded 0 before the change and 1 after the change.  In 1972,
the separate Northern Ireland House of Commons was suspended; a variable
(SUSPEND) is included for that, coded 0 before the change and 1 after the
change.  After 1979, the total of Ulster MPs was raised to 17; a variable is
included for that (ULSTER), coded 0 before the change and 1 after the change.

There are columns for the Conservative, Labour, and Liberal parties.  The
Alliance (Liberal/Social Democrat) votes in 1983 and 1987 are reported in the
Liberal column.  The "other" category includes votes for United Ireland, the
Communist Party, the Scottish National Party, the Social Democratic and Labor
Party, the Ulster Unionists and Loyalists, the Plaid Cymru, and the parties
Mackie and Rose collected into their "other" category.  There are two 1974
elections, the first in February and the second in October.  The February
results are under "1974"; the October results are under "1974.8."

@
