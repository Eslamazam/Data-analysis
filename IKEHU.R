setwd("/Users/eslam/Desktop/IKEHU 2021/lineaire regressie 2020")
rm(list=ls())

# Data inladen
 df = read.csv(file = "Totale verbruiken2020.csv", sep = ";")

# Filter elektra en gas
elektra= df[(grep("E", df$code)),]
gas= df[(grep("G", df$code)),]
df = elektra[,3:22]
df$volmue.maand.4 = int(as.numeric(df$volmue.maand.4))



install.packages("astsa")
install.packages("knitr")
install.packages("printr")
install.packages("plyr")
install.packages("dplyr")
install.packages("lubridate")
install.packages("gridExtra")
install.packages("reshape2")
install.packages("TTR")
install.packages("fpp2")
library(readr)
library(astsa, quietly=TRUE, warn.conflicts=FALSE)
library(ggplot2)
library(knitr)
library(printr)
library(plyr)
library(dplyr)
library(lubridate)
library(gridExtra)
library(reshape2)
library(TTR)
library(reticulate)
library(tensorflow)
library(keras)
library(fpp2)
library(stringr)
library(tseries)



# stap 1 is aanmaken van x13,x46, x79 en y.
#Vervolgens mbh van histogrammen van de 4 variabelen kijken naar de verdeling van deze waarden
#En om te kunnen weten welke waarden uitschieters zijn
#Stap 2, deze uitschieters weghalen. Het gaat hierom alleen om uitschieters die weinig vaak voorkomen maar wel invloed hebben op het gemiddele.
# stap 3, matrixplot plotten, hieruit conclusies trekken: 
#1.Er kan niet gekeken worden naar een combinatie van onafhankelijke variabelen, want deze correlereeren met elkaar
#2. Correlatie test uitvoeren. Voor y en x13 geldt: Correlatie = 0.115637 met een P-waarde van 1.248e-12, dus de correlatie waarde is significant.
# stap 4, adhv correlatie, maak ik keuze om de y variabelen te transformeren.
#stap 5 getransformeerd y en alle x'en. nog steeds niet juiste correlatie. Toch gekeken naar de residuen, residuen voldoen niet aan de voorwaarden zoals: gemiddelde moet 0 zijn.
#Stap 6, plot regel 154, 159 en 163. Dit zijn de residuen plot, daarin is te zien dat geen wolk vorm heeft dus de variantie is niet gelijk overal en het gemiddelde is niet 0 en niet normaal verdeeld.

#in verslag gaat: 
#1. correlatieplot regel 88 tm 152 in bijlage doen, hierover zeggen dat geen lineair verband tussen y en x'en en de x'en onderling correleren(dat wil je niet)
#2. bespreken waarom je welke transformaties je hebt toegepast. Nog steeds geen goeie correlaties plot gevonden. 
#3. Toch modellen aangemaakt en gekeken naar de residuen. Residuen laten zien dat deze aan geen enkele voorwaarden voldoen.



#Aanmaken x varaibelen met daarin volume van maand 1 tot en met maand 3
x13 = elektra$X1tm3

# Alleen voor x13 de waarden meenemen die onder de 2000 zijn.
x13 = x13[x13<2000]
# variabele x13 begrenzen tot 3750 waarden.
x13 = x13[1:3750]

#Aanmaken x varaibelen met daarin volume van maand 4 tot en met maand 6
x46 = elektra$X4tm6

x46 = x46[x46<1000]

x46 = x46[1:3750]
#Aanmaken x varaibelen met daarin volume van maand 7 tot en met maand 9
x79 = elektra$X7tm9

x79 = x79[x79<2000]

x79 = x79[1:3750]
#Aanmaken y varaibelen met daarin de totale volume van heel jaar 2020
y = elektra$TotaalVolume.2020

y = y[1:3750]
# nieuwe data frame aanmaken van de 4 variabelen(y,x13,x46,x79)
df2 = cbind(y,x13,x46,x79)
# Matrixplot, kijken naar correlatie onafhankelijke variabele onderling en de correlatie tussen de onfhankelijke variabelen en de afhangelijke variabelen 
pairs(df2,
      main= "Spreidingsdiagram energieverbruik 2020")
cor.test(y,x13)


transformaties
plot(x13,y,
     main = "Spreidingsdiagram van de variabelen model1"
     ,xlab = "Energieverbruik eerste kwartaal",
     ylab = "Energieverbruik van het gehele jaar")




###################################################################################################
###################################################################################################
###################################################################################################
plot(x13,log(y),
     main = "Spreidingsdiagram energieverbruik"
     ,xlab = "Energieverbruik eerste kwartaal",
     ylab = " Log energieverbruik van het gehele jaar")

qqnorm((model13$residuals),
main = "QQplot gestandaardiseerde storingen model 1 "
,xlab = "Theroretische kansen",
ylab = "gestandaardiseerde storingen model 1"
)

plot(model13$residuals~x13[1:3747],
     main = "Gestandaariseerd storingen van model 1",
     xlab = "Energieverbruik kwartaal 1",
     ylab = "Gestandaariseerde storingen")
log(y)[1:3747]

qqnorm((model46$residuals),
       main = "QQplot gestandaardiseerde storingen model 2"
       ,xlab = "Theroretische kansen",
       ylab = "gestandaardiseerde storingen model 2"
)

plot(model13$residuals~x46[1:3747],
     main = "Gestandaariseerd storingen van model 2",
     xlab = "Energieverbruik kwartaal 2",
     ylab = "Gestandaariseerde storingen")


qqnorm((model79$residuals),
       main = "QQplot gestandaardiseerde storingen model 3 "
       ,xlab = "Theroretische kansen",
       ylab = "gestandaardiseerde storingen model 3"
)

plot(model13$residuals~x79[1:3747],
     main = "Gestandaariseerd storingen van model 3",
     xlab = "Energieverbruik kwartaal 3",
     ylab = "Gestandaariseerde storingen")


plot(cooks.distance(model13),
     main = "Cook's distance model 1",
     xlab = "Index",
     ylab = "CooK's waarde")
max(cooks.distance(model13))

plot(cooks.distance(model46),
     main = "Cook's distance model 2",
     xlab = "Index",
     ylab = "CooK's waarde")
max(cooks.distance(model46))

plot(cooks.distance(model79),
     main = "Cook's distance model 3",
     xlab = "Index",
     ylab = "CooK's waarde")
max(cooks.distance(model79))


plot(log(x13),y,
     main = "Spreidingsdiagram y tegen energieverbruik eerste kwartaal")
plot(log(x13),log(y),
     main = "Spreidingsdiagram y tegen energieverbruik eerste kwartaal")

plot(x46,y,
     main = "Spreidingsdiagram y tegen energieverbruik tweede kwartaal")
plot(x46,log(y),
     main = "Spreidingsdiagram y tegen energieverbruik tweede kwartaal")
plot(log(x46),y,
     main = "Spreidingsdiagram y tegen energieverbruik tweede kwartaal")
plot(log(x46),log(y),
     main = "Spreidingsdiagram y tegen energieverbruik tweede kwartaal")

plot(x79,y,
     main = "Spreidingsdiagram y tegen energieverbruik derde kwartaal")
plot(x79,log(y),
     main = "Spreidingsdiagram y tegen energieverbruik derde kwartaal")
plot(log(x79),y,
     main = "Spreidingsdiagram y tegen energieverbruik derde kwartaal")
plot(log(x79),log(y),
     main = "Spreidingsdiagram y tegen energieverbruik derde kwartaal")
############################################# variabelen transformaties ###########################
# x13 variabele kwadratische getransformeerd
x13transform = x13^2
df3=cbind(df2,x13transform)
pairs(df3)
cor.test(y,x13transform)

# x13 variabelen logaritmsch getranformeerd
x13transform = log(x13)
df4=cbind(df2,x13transform)
pairs(df4)
cor.test(y,x13transform)

# x13 variabele wortel tranformatie
x13transform = sqrt(x13)
df5=cbind(df2,x13transform)
pairs(df5)
cor.test(y,x13transform)

# x46 variabele kwadratische getransformeerd
x46transform = x46^2
df6=cbind(df2,x46transform)
pairs(df6)
cor.test(y,x64transform)

# x46 variabelen logaritmsch getranformeerd
x46transform = log(x46)
df6=cbind(df2,x46transform)
pairs(df6)
cor.test(y,x64transform)

# x46 variabele wortel tranformatie
x46transform = sqrt(x46)
df7=cbind(df2,x46transform)
pairs(df7)
cor.test(y,x64transform)

# x79 variabele kwadratische getransformeerd
x79transform = x79^2
df8=cbind(df2,x79transform)
pairs(df8)
cor.test(y,x79transform)

# x79 variabelen logaritmsch getranformeerd
x79transform = log(x79)
df8=cbind(df2,x79transform)
pairs(df8)
cor.test(y,x79transform)

# x79 variabele wortel tranformatie
x79transform = sqrt(x79)
df8=cbind(df2,x79transform)
pairs(df8)
cor.test(y,x79transform)


# Y variabelen logaritmsch getranformeerd
ytransform = log(y)
df9=cbind(df2,ytransform)
pairs(df9)
cor.test(y,ytransform)

# Y variabele kwadratische getransformeerd
ytransform2 = y^2
df10=cbind(df2,ytransform2)
pairs(df10)
cor.test(y,ytransform)

# Y variabele wortel tranformatie
ytransform3 = sqrt(y)
df11=cbind(df2,ytransform3)
pairs(df11)
cor.test(y,ytransform)

####################################### Alsnog voorspelling maken ############################################################
# lineaire regressie modellen aanmaken
model13=lm(log(y)~x13)
model13$coefficients

hist(residuals(model13))
model46=lm(log(y)~x46)
hist(residuals(model46))
model79=lm(log(y)~x79)
hist(residuals(model79))

# residuen plot model13
plot(residuals(model13),fitted.values(model13),
     main="Gestandaardiseerde plot van model13",
     xlab = "Residuen model13",ylab = "Gestandaardiseerde storingen model13")
# residuen plot model64
plot(residuals(model46),fitted.values(model46),
     main="Gestandaardiseerde plot van model46",
     xlab = "Residuen model46",ylab = "Gestandaardiseerde storingen model46")

#residuen plot model79
plot(residuals(model79),fitted.values(model79),
     main="Gestandaardiseerde plot van model79",
     xlab = "Residuen model79",ylab = "Gestandaardiseerde storingen model79")




