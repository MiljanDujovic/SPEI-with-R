options(max.print=1000)
require(spi)
# Ucitavanje podataka
podaci = read.csv("MeasurementExport.csv", sep = ";", stringsAsFactors = FALSE)
# Proveravanje tipa podataka
str(podaci)
# Analiza unesene tabele
summary(podaci)
# Prebacivanje sirovog datuma u formu koju R razume kao tip datum
podaci$Vreme = as.POSIXct(podaci$Vreme, format="%d.%m.%Y. %H:%M:%S", tz = "GMT")
class(podaci$Datum)
podaci$Vrednost = as.numeric(podaci$Vrednost)
str(podaci)
require(lubridate)
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

datapadavine = data.frame(padavine3)
str(datapadavine)
datapadavine1 = ts(datapadavine[,-c(1,2)], start = c(2015,2), end=c(2044,12), frequency=12)
str(datapadavine1)
datapadavine2 =t(datapadavine1)
spi3=spi(datapadavine$Godina,  3)
plot(spi3, main = "Standardizovani padavinski indeks 3")

spi6=spi(datapadavine, scale = 6)
plot(spi6, main = "Standardizovani padavinski indeks 6")

spi12=spi(datapadavine, scale = 12)
plot(spi12, main = "Standardizovani padavinski indeks 12")

spi36=spi(datapadavine, scale = 36)
plot(spi36, main = "Standardizovani padavinski indeks 36")