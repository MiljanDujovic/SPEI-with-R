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
# Racunanje potencijalne evapotreanspiracije za svaki mesec
temperatura3$PET = thornthwaite(temperatura3$Vrednost, lat = 45.29968)
# Racunanje mesecnog vodnog bilansa odnosno razlika mesecnih padavina i mesecne potencijalne evapotranspiracije
TemperaturnaVrednost = temperatura3$Vrednost
KolicinaPadavina = padavine3$Vrednost
Godina = temperatura3$Godina
Mesec = temperatura3$Mesec
PET = temperatura3$PET
TabelaSPEI = data.frame(Godina, Mesec,TemperaturnaVrednost ,KolicinaPadavina ,PET)

Bilans =TabelaSPEI$KolicinaPadavina- TabelaSPEI$PET

TabelaSPEI = data.frame(TabelaSPEI, Bilans)
# Konvertovanje u vremensku seriju i grafik
TabelaSPEI <- ts(TabelaSPEI[,-c(1,2)], start = c(2015,2), end=c(2030,2), frequency=12)
TabelaSPEI
plot(TabelaSPEI)
TabelaSPEI[,'Bilans']
TabelaSPEI[, "KolicinaPadavina"]
# Racunanje i prikaz standardizovanog padavinsko-evapotranspiracijskog indeksa

grafikSPEI1 = spei(TabelaSPEI[, "Bilans"]  , 2)
grafikSPEI1
plot.spei(grafikSPEI1,main = "SPEI 1 Thorntwaite ET metoda" )


grafikSPEI3 = spei(TabelaSPEI[, "Bilans"] , 3 )
grafikSPEI3
plot.spei(grafikSPEI3,main = "SPEI 3, Thorntwaite ET metoda" )

grafikSPEI9 = spei(TabelaSPEI[, "Bilans"]  , 9)
grafikSPEI9
plot.spei(grafikSPEI9,main = "SPEI 9, Thorntwaite ET metoda" )

grafikSPEI12 = spei(TabelaSPEI[, "Bilans"]  , 12 )
grafikSPEI12
plot.spei(grafikSPEI12,main = "SPEI 12, Thorntwaite ET metoda")

grafikSPEI24 = spei(TabelaSPEI[, "Bilans"]  , 24  )
grafikSPEI24
plot.spei(grafikSPEI24,main = "SPEI 24, Thorntwaite ET metoda" )


grafikSPEI36 = spei(TabelaSPEI[, "Bilans"]  , 36 )
grafikSPEI36
plot.spei(grafikSPEI36,main = "SPEI 36, Thorntwaite ET metoda" )




grafikSPEI24 = spei(TabelaSPEI[, "Bilans"]  , 36, kernel=list(type='gaussian', shift=6)  )
grafikSPEI24
plot.spei(grafikSPEI24,main = "SPEI 36, Stanica u Kaću" )

grafikSPEI24 = spei(TabelaSPEI[, "Bilans"]  , 36, kernel=list(type='triangular', shift=6)  )
grafikSPEI24
plot.spei(grafikSPEI24,main = "SPEI 36, Stanica u Kaću" )

grafikSPEI24 = spei(TabelaSPEI[, "Bilans"]  , 36, kernel=list(type='circular', shift=6)  )
grafikSPEI24
plot.spei(grafikSPEI24,main = "SPEI 36, Stanica u Kaću" )


kern.plot(36,12
          )


require(xlsx)
write.xlsx(Bilans, file = "excelBilansTHO.xlsx", sheetName = "BilansTHO", col.names = TRUE, row.names = TRUE, append = FALSE)


