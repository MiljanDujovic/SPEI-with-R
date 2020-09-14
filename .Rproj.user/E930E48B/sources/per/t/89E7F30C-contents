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

RaRadijacija = read.csv("Ra2Radijacija.csv", stringsAsFactors = FALSE)
RsRadijacija = read.csv("RsRadijacija.csv", stringsAsFactors = FALSE)
# Kreiranje nove tabele sa izdvojenim vrednostima samo za brzinu vetra
brzinavetra = podaci[podaci$ID.vrednosti == "WS1",]
# Racunanje srednje brzine vetra u m/s za svaki dan
brzinavetra1 <- data.frame(brzinavetra, Dan = as.POSIXct(format(brzinavetra$Vreme)))
brzinavetra2 = aggregate(Vrednost ~ Dan, brzinavetra1, mean)

# Racunanje srednje brzine vetra u m/s po mesecu
brzinavetra2$Mesec = format(brzinavetra2$Dan, label = TRUE, format = "%m")
brzinavetra2$Godina = format(brzinavetra2$Dan, format = "%Y")
brzinavetra3 = aggregate(Vrednost ~ Mesec + Godina, brzinavetra2, FUN = mean)

# Kreiranje nove tabele sa izdvojenim vrednostima samo za vlaznost vazduha
vlaznostvazduha = podaci[podaci$ID.vrednosti == "AH1",]
# Racunanje srednje vlaznosti vazduha u % za svaki dan
vlaznostvazduha1 <- data.frame(vlaznostvazduha, Dan = as.POSIXct(format(vlaznostvazduha$Vreme)))
vlaznostvazduha2 = aggregate(Vrednost ~ Dan, vlaznostvazduha1, mean)

# Racunanje srednje vlaznosti vazduha u % po mesecu
vlaznostvazduha2$Mesec = format(vlaznostvazduha2$Dan, label = TRUE, format = "%m")
vlaznostvazduha2$Godina = format(vlaznostvazduha2$Dan, format = "%Y")
vlaznostvazduha3 = aggregate(Vrednost ~ Mesec + Godina, vlaznostvazduha2, FUN = mean)

penmanET0 = penman(minimum,maximum, brzinavetra3$Vrednost, Rs = RaRadijacija$Values,
                   lat = 45.29968, RH = vlaznostvazduha3$Vrednost, z = 86, crop = 'short', na.rm = TRUE)


BilansPenman =padavine3$Vrednost- penmanET0
TabelaSPEIPenman = data.frame(temperatura3$Vrednost, padavine3$Vrednost,penmanET0, BilansPenman)
# Konvertovanje u vremensku seriju i grafik
TabelaSPEIPenman <- ts(TabelaSPEIPenman, start = c(2015,2), end=c(2030,2), frequency=12)
# Racunanje i prikaz standardizovanog padavinsko-evapotranspiracijskog indeksa

grafikSPEIPenman1 = spei(TabelaSPEIPenman[, "ET0_pen.1"]  , 1)
grafikSPEIPenman1
plot.spei(grafikSPEIPenman1,main = "SPEI 1 Penman-Monteith ET metoda" )

grafikSPEIPenman3 = spei(TabelaSPEIPenman[, "ET0_pen.1"]  , 3)
grafikSPEIPenman3
plot.spei(grafikSPEIPenman3,main = "SPEI 3 Penman-Monteith ET metoda" )

grafikSPEIPenman9 = spei(TabelaSPEIPenman[, "ET0_pen.1"]  , 9)
grafikSPEIPenman9
plot.spei(grafikSPEIPenman9,main = "SPEI 9 Penman-Monteith ET metoda" )

grafikSPEIPenman12 = spei(TabelaSPEIPenman[, "ET0_pen.1"]  , 12)
grafikSPEIPenman12
plot.spei(grafikSPEIPenman12,main = "SPEI 12 Penman-Monteith ET metoda" )

grafikSPEIPenman24 = spei(TabelaSPEIPenman[, "ET0_pen.1"]  , 24)
grafikSPEIPenman24
plot.spei(grafikSPEIPenman24,main = "SPEI 24 Penman-Monteith ET metoda" )

grafikSPEIPenman36 = spei(TabelaSPEIPenman[, "ET0_pen.1"]  , 36)
grafikSPEIPenman36
plot.spei(grafikSPEIPenman36,main = "SPEI 36 Penman-Monteith ET metoda" )

grafikSPEIPenman48 = spei(TabelaSPEIPenman[, "ET0_pen.1"]  , 48)
grafikSPEIPenman48
plot.spei(grafikSPEIPenman48,main = "SPEI 48 Penman-Monteith ET metoda" )

require(xlsx)
write.xlsx(BilansPenman, file = "excelBilansPEN.xlsx", sheetName = "BilansPEN", col.names = TRUE, row.names = TRUE, append = FALSE)
