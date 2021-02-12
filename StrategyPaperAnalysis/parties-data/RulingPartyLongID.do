// Adding an an ID (edon) to RulingPartyLong -->

clear

use "/Users/atrelles/Documents/mex-open-map/StrategyPaperAnalysis/RulingPartyLong.dta"

tab state
tab state, nolabel
encode state, gen (edon1)
tab edon1
tab edon1, nolabel
recode edon1 (1=1 "Aguascalientes") (2=2 "Baja California") (3=3 "Baja California Sur" )(4=9 "CDMX") (5=4 "Campeche") (6=5 "Chiapas") (7=6 "Chihuahua") (8=7 "Coahuila") (9=8 "Colima") (10=10 "Durango")(11=11 "Guanajuato")(12=12 "Guerrero")(13=13 "Hidalgo")(14=14 "Jalisco")(15=16 "Michoacán")(16=17 "Morelos")(17=15 "México")(18=18 "Nayarit")(19=19 "Nuevo León")(20=20 "Oaxaca")(21=21 "Puebla")(22=22 "Querétaro")(23=23 "Quintana Roo")(24=24 "San Luis Potosí")(25=25 "Sinaloa")(26=26 "Sonora")(27=27 "Tabasco")(28=28 "Tamaulipas")(29=29 "Tlaxcala")(30=30 "Veracruz")(31=31 "Yucatán")(32=32 "Zacatecas"), gen (edon2)

gen edon = edon2

drop edon1 edon2

tab edon


save "/Users/atrelles/Documents/mex-open-map/StrategyPaperAnalysis/RulingPartyLongID.dta", replace
