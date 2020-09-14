options(max.print=1000)
# Pozivanje SPEI biblioteke-paketa
require(SPEI)
#Pozivanje biblioteke koja poseduje funkcije za manipulaciju datumima i vremenom
require(lubridate)
# Ucitavanje podataka
podaci = read.csv("MeasurementExport.csv", sep = ";", stringsAsFactors = FALSE)
# Proveravanje tipa podataka
str(podaci)
# Analiza unesene tabele
summary(podaci)
# Prebacivanje sirovog datuma u formu koju R razume kao tip datum
podaci$Vreme = as.POSIXct(podaci$Vreme, format="%d.%m.%Y. %H:%M:%S", tz = "GMT")
podaci$Vrednost = as.numeric(podaci$Vrednost)
# Kreiranje nove tabele sa izdvojenim vrednostima samo za padavine
padavine = podaci[podaci$ID.vrednosti == "PP1",]
# Racunanje zbira padavina u mm za svaki dan
padavine1 <- data.frame(padavine, Dan = as.POSIXct(format(padavine$Vreme)))
padavine2 = aggregate(Vrednost ~ Dan, padavine1, sum)

# Racunanje zbira kolicine padavina u milimetrima po mesecu
padavine2$Mesec = month(padavine2$Dan, label = TRUE)
padavine2$Godina = format(padavine2$Dan, format = "%Y")
padavine3 = aggregate(Vrednost ~ Mesec + Godina, padavine2, FUN = sum)
# Kreiranje nove tabele sa izdvojenim vrednostima samo za temperature vazduha
temperatura = podaci[podaci$ID.vrednosti == "AT1",]
# Racunanje srednje temperaturne vrednosti za svaki dan u celzijusima
temperatura1 <- data.frame(temperatura, Dan = as.POSIXct(format(temperatura$Vreme)))
temperatura2 = aggregate(Vrednost ~ Dan, temperatura1, mean)
# Racunanje srednje temperaturne vrednosti po mesecu u celzijusima
temperatura2$Mesec = month(temperatura2$Dan, label = TRUE)
temperatura2$Godina = format(temperatura2$Dan, format = "%Y")
temperatura3 = aggregate(Vrednost ~ Mesec + Godina, temperatura2, FUN = mean)

for (maximum in temperatura3$Vrednost) {
  maximum = temperatura3$Vrednost+7
}
for (minimum in temperatura3$Vrednost) {
  minimum = temperatura3$Vrednost-7
}

radijacija = read.csv("Ra2Radijacija.csv", stringsAsFactors = FALSE)


harET0 = hargreaves(minimum, maximum, lat = 45.29968, Pre = padavine3$Vrednost,
                    Ra = radijacija$Values)
BilansHargreaves =padavine3$Vrednost- harET0

TabelaSPEIhargreaves = data.frame(temperatura3$Vrednost, padavine3$Vrednost,harET0, BilansHargreaves)
# Konvertovanje u vremensku seriju i grafik
TabelaSPEIhargreaves <- ts(TabelaSPEIhargreaves, start = c(2015,2), end=c(2030,2), frequency=12)
# Racunanje i prikaz standardizovanog padavinsko-evapotranspiracijskog indeksa

grafikSPEIhargreaves1 = spei(TabelaSPEIhargreaves[, "ET0_har.1"]  , 1)
grafikSPEIhargreaves1
plot.spei(grafikSPEIhargreaves1,main = "SPEI 1 Hargreaves ET metoda" )

grafikSPEIhargreaves3 = spei(TabelaSPEIhargreaves[, "ET0_har.1"]  , 3)
grafikSPEIhargreaves3
plot.spei(grafikSPEIhargreaves3,main = "SPEI 3 Hargreaves ET metoda" )

grafikSPEIhargreaves9 = spei(TabelaSPEIhargreaves[, "ET0_har.1"]  , 9)
grafikSPEIhargreaves9
plot.spei(grafikSPEIhargreaves9,main = "SPEI 9 Hargreaves ET metoda" )

grafikSPEIhargreaves12 = spei(TabelaSPEIhargreaves[, "ET0_har.1"]  , 12)
grafikSPEIhargreaves12
plot.spei(grafikSPEIhargreaves12,main = "SPEI 12 Hargreaves ET metoda" )

grafikSPEIhargreaves24 = spei(TabelaSPEIhargreaves[, "ET0_har.1"]  , 24)
grafikSPEIhargreaves24
plot.spei(grafikSPEIhargreaves24,main = "SPEI 24 Hargreaves ET metoda" )

grafikSPEIhargreaves36 = spei(TabelaSPEIhargreaves[, "ET0_har.1"]  , 36)
grafikSPEIhargreaves36
plot.spei(grafikSPEIhargreaves36,main = "SPEI 36 Hargreaves ET metoda" )


require(xlsx)
write.xlsx(BilansHargreaves, file = "excelBilansHAR3.xlsx", sheetName = "BilansHAR2", col.names = TRUE, row.names = TRUE, append = FALSE)
