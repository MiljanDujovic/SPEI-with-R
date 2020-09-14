options(max.print=1000)
# Pozivanje SPEI biblioteke-paketa
require(SPEI)
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

# Kreiranje nove tabele sa izdvojenim vrednostima samo za padavine
padavine = podaci[podaci$ID.vrednosti == "PP1",]

# Pokusaj racunanja zbira padavina u mm za svaki dan
padagg <- data.frame(padavine, Dan = as.POSIXct(format(padavine$Vreme)))

padagg2 = aggregate(Vrednost ~ Dan, padagg, sum)

# Racunanje zbira kolicine padavina u milimetrima po mesecu
padagg2$Mesec = month(padagg2$Dan, label = TRUE)
padagg2$Godina = format(padagg2$Dan, format = "%Y")
padagg3 = aggregate(Vrednost ~ Mesec + Godina, padagg2, FUN = sum)




# Kreiranje nove tabele sa izdvojenim vrednostima samo za temperature vazduha
temperatura = podaci[podaci$ID.vrednosti == "AT1",]


# Pokusaj racunanja srednje temperaturne vrednosti za svaki dan u celzijusima
tempagg <- data.frame(temperatura, Dan = as.POSIXct(format(temperatura$Vreme)))

tempagg2 = aggregate(Vrednost ~ Dan, tempagg, mean)

# Racunanje srednje temperaturne vrednosti po mesecu u celzijusima
tempagg2$Mesec = month(tempagg2$Dan, label = TRUE)
tempagg2$Godina = format(tempagg2$Dan, format = "%Y")
tempagg3 = aggregate(Vrednost ~ Mesec + Godina, tempagg2, FUN = mean)

# Racunanje potencijalne evapotreanspiracije za svaki mesec
tempagg3$PET = thornthwaite(tempagg3$Vrednost, lat = 45.29968)


# Racunanje mesecnog vodnog bilansa odnosno razlika mesecnih padavina i mesecne potencijalne evapotranspiracije

TemperaturnaVrednost = tempagg3$Vrednost
KolicinaPadavina = padagg3$Vrednost
Godina = tempagg3$Godina
Mesec = tempagg3$Mesec
PET = tempagg3$PET

TabelaSPEI = data.frame(Godina, Mesec,TemperaturnaVrednost ,KolicinaPadavina ,PET)
TabelaSPEI$Bilans =TabelaSPEI$KolicinaPadavina- TabelaSPEI$PET
# Konvertovanje u vremensku seriju i grafik
TabelaSPEI <- ts(TabelaSPEI[,-c(1,2)], start = c(2015,2), end=c(2030,2), frequency=12)
TabelaSPEI
plot(TabelaSPEI)
TabelaSPEI[,'Bilans']

# Racunanje i prikaz standardizovanog padavinsko-evapotranspiracijskog indeksa

grafikSPEI = spei(TabelaSPEI[, "Bilans"] , 12)
grafikSPEI
plot(grafikSPEI)

names(grafikSPEI)
summary(grafikSPEI)
plot(grafikSPEI)
str(TabelaSPEI[,'Bilans'])

require(ggplot2)
?mean.Date
?thornthwaite
