options(max.print=1000)
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
require(xlsx)
write.xlsx(tempagg3, file = "exceltemperatura.xlsx", sheetName = "Temperatura", col.names = TRUE, row.names = TRUE, append = FALSE)

write.xlsx(padagg3, file = "excelpadavina.xlsx", sheetName = "Padavine", col.names = TRUE, row.names = TRUE, append = FALSE)
