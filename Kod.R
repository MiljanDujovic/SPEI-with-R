options(max.print=2500)
# Pozivanje SPEI biblioteke-paketa
require(SPEI)
# Ucitavanje podataka
podaci = read.csv("MerenjeVlage.csv", sep = ";", stringsAsFactors = FALSE)
# Proveravanje tipa podataka
str(podaci)
# Analiza unesene tabele
summary(podaci)
# Prebacivanje sirovog datuma u formu koju R razume kao tip datum
podaci$Datum = as.Date(podaci$Datum, format="%m.%d.%Y.")
class(podaci$Datum)
# Prebacivanje sirovog vremena u formu koju R razume kao tip POSIXct
podaci$Vreme = as.POSIXct(podaci$Vreme, format = "%H:%M:%S")
class(podaci$Vreme)
# Zamena vrednosti u koloni zarad preglednosti
podaci$ID.vrednosti[podaci$ID.vrednosti == 
                      c("SM1","SM2","SM3","SM4","SM5",
                        "SM6","BT1","AH1","AT1","PP1","WS1", "WD1")] = 
  c("Vlaznost zemljista 1", "Vlaznost zemljista 2", "Vlaznost zemljista 3", 
    "Vlaznost zemljista 4", "Vlaznost zemljista 5", "Vlaznost zemljista 6", "Baterija", 
    "Vlaznost vazduha", "Temperatura vazduha", "Padavine", "Brzina vetra", "Pravac vetra")
podaci
str(podaci)
vektor = podaci[order(podaci$ID.vrednosti != "Padavine"),]
vektor
hist(vektor$Vrednost)
data(wichita)
wichita

