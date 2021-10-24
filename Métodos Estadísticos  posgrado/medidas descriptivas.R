datos<- read.table("TABLAPESOS.txt", header=T)
attach(datos)
mean(pesos)
sd(pesos)
var(pesos)
median(pesos)
quantile(pesos)
quantile(pesos,  probs = c(0.1, 0.5, 1, 2, 5, 10, 50, 75,100)/100)

pvec <- seq(0,1,0.1)  #cuantiles de inter?s
quantile(pesos,pvec)


summary(pesos)


library(e1071) #paquete para calcular curtosis
skewness(pesos,na.rm=TRUE)
kurtosis(pesos)
desviacion<-sd(pesos,na.rm=TRUE)     #desviaci?n tipica
varianza<-desviacion^2  # varianza
mean(pesos,na.rm=TRUE)
quantile(pesos, na.rm = TRUE,probs=c(0.05,0.85,0.95))# percentiles 5%, 85% y 95%


sort(pesos)
frecuencias=as.data.frame(table(Clases=factor(cut(pesos,breaks=7))))

tablatotal=transform(frecuencias,FrecAc=cumsum(Freq),FrecRel=Freq/50,RelAcum=cumsum(Freq/50))
hist(pesos)


librerias para el resumen descriptivo
library(fBasics)
library(timeDate)
library(timeSeries)
basicStats(pesos)         # resumen de estad?sticas descriptivas


x =pesos        # c?lculo de la desviaci?n media absoluta
n = length(x)
desviacionm<-sum(abs(x-mean(x)))/(n)
desviacionm

stem(pesos) # diagrama de tallos y hojas 

data(mtcars)     # datos de carros de R
stem(mtcars$cyl) # diagrama de tallos y hojas 
stem(mtcars$mpg)


# gr?fico box plot
boxplot(pesos)



proteinas<- read.table("Consuproteinas.txt", header=T)
proteinas


skewness(proteinas$RedMeat,na.rm=TRUE)
kurtosis(proteinas$RedMeat)
desviacioncarne<-sd(proteinas$RedMeat,na.rm=TRUE)     #desviaci?n tipica
varianzacarne<-desviacioncarne^2  # varianza
mean(proteinas$RedMeat,na.rm=TRUE)
quantile(proteinas$RedMeat, na.rm = TRUE,probs=c(0.05,0.85,0.95))# percentiles 5%, 85% y 95%
tapply(proteinas$RedMeat,proteinas$Country,mean, na.rm=TRUE)    #media en funci?n de la variable pais con carne roja.
tapply(proteinas$RedMeat,proteinas$Country,sd, na.rm=TRUE)      #desviaci?n est?ndar en funci?n de la variable pais con carne roja.
tapply(proteinas$RedMeat,proteinas$Country,quantile,probs=c(0.05,0.95),na.rm=TRUE)  # cuantiles por pa?s  para la variable carnes rojas.  



par(mfrow=c(1,2))
boxplot(proteinas$RedMeat, proteinas$WhiteMeat, proteinas$Milk, proteinas$Fish,col="blue", xlab="proteinas", ylab="porcentaje",main="porcentaje de proteinas")
boxplot(proteinas$Cereals,proteinas$Starch,proteinas$Nuts)
boxplot(proteinas$RedMeat~proteinas$Country)


#tablas

table(proteinas$Milk)
table(proteinas$Country)
table(proteinas$Milk,proteinas$RedMeat)   # proteinas por pa?s

# generaci?n de tablas#

caff.marital <- matrix(c(652,1537,598,242,36,46,38,21,218,327,106,67), nrow=3,byrow=T)      # matriz de 3 filas por 4 columnas
caff.marital

colnames(caff.marital) <- c("0","1-150","151-300",">300")
rownames(caff.marital) <- c("Married","Prev.married","Single")
caff.marital
t(caff.marital)


Tabla <- table(pesos) 

Tabla # frecuencias absolutas
prop.table(Tabla)# frecuencias relativas


diagrama<-barplot(Tabla,col=rainbow(3),xlab="pesos",ylab="Frecuencias absolutas")
text(diagrama,Tabla + 1,labels=Tabla, xpd = TRUE)
title(main = "Distribuci?n de frecuencias de la variable Tipo", font.main = 4)


hist(proteinas$RedMeat, breaks = 10, freq = TRUE,    # histograma
     main = "Histograma del consumo de carnes rojas ",
     xlab="intervalos", ylab="Frecuencias")

