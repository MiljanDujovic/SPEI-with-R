# Load data
data(wichita)
wichita
# Compute potential evapotranspiration (PET) and climatic water balance (BAL)
wichita$PET <- thornthwaite(wichita$TMED, 37.6475)
wichita
wichita$BAL <- wichita$PRCP-wichita$PET
wichita$TMED
wichita$BAL
wichita
# Convert to a ts (time series) object for convenience
wichita <- ts(wichita[,-c(1,2)], end=c(2011,10), frequency=12)
plot(wichita)
# One and tvelwe-months SPEI
spei1 <- spei(wichita[,'BAL'], 1)
plot(spei1)
spei12 <- spei(wichita[,'BAL'], 12)
plot(spei12)

class(spei1)
# Extract information from spei object: summary, call function, fitted values, and coefficients
summary(spei1)
names(spei1)
spei1$call
spei1$fitted
spei1$coefficients
spei1$kernel
# Plot spei object
par(mfrow=c(2,1))
plot(spei1, main='Wichita, SPEI-1')
plot(spei12, main='Wichita, SPEI-12')
# One and tvelwe-months SPI
spi_1 <- spi(wichita[,'PRCP'], 1)
spi_12 <- spi(wichita[,'PRCP'], 12)
par(mfrow=c(2,1))
plot(spi_1, 'Wichita, SPI-1')
plot(spi_12, 'Wichita, SPI-12')
# Time series not starting in January
par(mfrow=c(1,1))
plot(spei(ts (wichita[,'BAL'], freq=12, start = c(1980,6)), 12))
?ts
# Using a particular reference period (1980-2000) for computing the parameters
plot(spei(ts(wichita[,'BAL'], freq=12, start=c(1980,6)), 12,
          ref.start=c(1980,1), ref.end=c(2000,1)))
# Using different kernels
spei24 <- spei(wichita[,'BAL'],24)
spei24_gau <- spei(wichita[,'BAL'], 24, kernel=list(type='gaussian', shift=0))
par(mfrow=c(2,1))
plot(spei24, main='SPEI-24 with rectangular kernel')
plot(spei24_gau, main='SPEI-24 with gaussian kernel')
# Computing several time series at a time
# Dataset balance contains time series of the climatic water balance at 12 locations
data(balance)
head(balance)
bal_spei12 <- spei(balance, 12)
plot(bal_spei12)
# Using custom (user provided) parameters
coe <- spei1$coefficients
dim(coe)
spei(wichita[,'BAL'], 1, params=coe)
is.atomic(wichita)

ggplot(padavine, aes(x = padavine$Mesec, y = padavine$Vrednost)) + geom_point()
spei(padavine[,"Vrednost"], 12)
plot(spei(ts(data =  padvrem[,"padavine.Vrednost"], start=c(2015)  ), 12))
class(spei(padavine[,"Vrednost"] ))

# Zamena imena senzora u koloni ID.vrednosti 
podaci$ID.vrednosti[podaci$ID.vrednosti == 
                      c("SM1","SM2","SM3","SM4","SM5",
                        "SM6","BT1","AH1","AT1","PP1","WS1", "WD1")] = 
  c("Vlaznost zemljista 1", "Vlaznost zemljista 2", "Vlaznost zemljista 3", 
    "Vlaznost zemljista 4", "Vlaznost zemljista 5", "Vlaznost zemljista 6", "Baterija", 
    "Vlaznost vazduha", "Temperatura vazduha", "Padavine", "Brzina vetra", "Pravac vetra")