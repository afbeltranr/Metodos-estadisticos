datos=c("A","A", "B", "B", "B", "B", "B","I","I","I","R","I","I","I","R","R",
          "R","R","R","R","R","R","R","R","R", "M", "M", "M", "M", "M", "M",
          "M", "M", "M", "M", "M", "M", "M","A","A")

X<-table(datos)
frecuenciasrelativas<-X/40


slices=c(4,5,13,12,6)
lbls=c("Alto", "Bueno","Medio","Regular", "Insuficiente")
pie(slices, labels = lbls, main="Diagrama Circular Calificaciones")


x=c(4,5,13,12,6)
y=c("Alto", "Bueno","Medio","Regular","Insuf")
barplot(x,names.arg=y,main="Diagrama de Barras Calicaciones")

edades=c(20,17,18,25,23,15,18,22,21,22,23,23,19,20,30,25,24,25,21,24,24,21,23,22,
         26,24,23,21,21,18,21,23,24,16,17,19,26,23,24,18,21,33)

sort(edades)
frecuencias=as.data.frame(table(Clases=factor(cut(edades,breaks=6))))

tablatotal=transform(frecuencias,FrecAc=cumsum(Freq),FrecRel=Freq/42,RelAcum=cumsum(Freq/42))
hist(edades)


salarios=c(432.21,239.48,249.94,373.22,233.86,366,331.11,258.86,349.20,31.60)


mean(salarios)
sd(salarios)
max(salarios)
min(salarios)



options(scipen = 999)
library(dplyr)
library(ggplot2) 
library(readxl)
library(gmodels)
library(Hmisc)
library(ggthemes)

iris

table(iris$Species)
mad(iris$Sepal.Length) #desviaci?n media absoluta
IQR(iris$Sepal.Length) #rango intercuartil
cov(iris$Sepal.Length,iris$Sepal.Width) #covarianza
cov(iris[,1:4]) #matriz de covarianzas
cor(iris[,1:4]) #correlaci?n



sexo<-c("Hombre","Hombre","Mujer","Hombre","Mujer","Mujer")
estudios<-c("universitario","secundario","secundario",
            "postgrado","secundario","universitario")
table(sexo,estudios)



vec<-rnorm(10,20,10)
mean(vec)
vec.ruid<-c(vec,rnorm(1,300,100))  #agregar un outliera vec
mean(vec.ruid)

mean(vec,trim=0.1) #descartar 10% de los valores extremos
mean(vec.ruid,trim=0.1)

median(vec)   #mediana
median(vec.ruid)

quantile(iris$Sepal.Length,seq(0,1,0.01)) #percentil o cuantil
quantile(iris$Sepal.Length,seq(0,1,0.25))   #cuartiles 

summary(iris) #resumen descriptivo

tapply(iris$Petal.Length,iris$Species,summary)
tapply(iris$Petal.Width,iris$Species,summary)
tapply(iris$Sepal.Length,iris$Species,summary)
tapply(iris$Sepal.Width,iris$Species,summary)

# medidas de dispersi?n####

rango<-max(iris$Sepal.Length)-min(iris$Sepal.Length)  #rango

var(iris$Sepal.Length)  #varianza

sd(iris$Sepal.Length)  #desviaci?n est?ndar

#desviaci?n media absoluta

aad<-function(x,fun=median){
mean(abs(x-fun(x)))
}
aad(iris$Sepal.Length) 

aad(iris$Sepal.Length,mean)


mad(iris$Sepal.Length)   #desviaci?n media absoluta
IQR(iris$Sepal.Length)  #Rango intercuartilico


#GR?FICOS

plot(rnorm(15,10,5),col="red",type="l")
lines(rnorm(15,10,5),col="blue",type="p",pch=1)
lines(rnorm(15,10,5),col="green",type="b",pch=2)
title(main="Mi gr?fico")
legend('topright', c("lineas","puntos","ambos") ,
        lty=1:3, col=c("red", "blue","green"), bty='n', cex=.75)
        
        
hist(iris$Sepal.Length)   #histograma

hist(iris$Sepal.Length,nclass=100)    #100 intervalos de clase

plot(density(iris$Sepal.Length),main="Densidad de Sepal.Length") # gr?fico de densidad


pie(table(iris$Species))

q54q101
boxplot(iris$Sepal.Length,main="Boxplot Sepal.Length") #box-plot


boxplot(iris$Sepal.Length ~iris$Species,ylab="Sepal.Length") # box plor por especie

boxplot(x=iris[,1:4],main="Boxplots Iris") # comparar gr?ficos


# DIAGRAMAS DE DISPERSI?N

#El ancho del s?palo vs el largo del s?palo
plot(iris$Sepal.Width ~iris$Sepal.Length, col=iris$Species)
# Equivalente
plot(iris$Sepal.Length, iris$Sepal.Width,col=iris$Species,
     pch=as.numeric(iris$Species))
# Le agregamos una leyenda
legend("topright", levels(iris$Species) ,
        lty=1, col=1:3, bty="n", cex=.75)



pairs(iris[,1:4],pch=as.numeric(iris$Species),col=iris$Species)
cor(iris[,1:4]) #correlaci?n

# en tres dimensiones


install.packages("scatterplot3d",dependencies=T)
        
        
library(scatterplot3d)


scatterplot3d(iris$Petal.Width, iris$Sepal.Length,
              iris$Sepal.Width, color=as.numeric(iris$Species),
              pch=as.numeric(iris$Species))




library(MASS)
parcoord(iris[1:4], col=iris$Species,var.label=T)




iris_sample1<-iris[sample(1:dim(iris)[1],size=6,replace=F),]
rownames(iris_sample1)<-paste(as.character(iris_sample1$Species),1:6)
stars(iris_sample1[1:4]) #gr?ficos de estrellas o gr?ficos radiales



library("aplpack")
iris_sample<-iris[sample(1:dim(iris)[1],size=16,replace=F),]
faces(iris_sample[1:4],face.type=1,labels=iris_sample$Species) #caras de chernoff

getwd() #devuelve la ruta de trabajo actual


library(moments)


skewness(iris$Sepal.Length)
kurtosis(iris$Sepal.Length)


summary(iris$Sepal.Length)





tiempo <- c(1:10)
respuesta <- c(1:4, seq(10, 20, 2))  

plot(tiempo,respuesta,
     type='b',
     pch=19,
     col="black",
     main = "Respuesta vs tiempo",
     ylab = "Respuesta",
     xlab = "Tiempo")



set.seed(123)
ejemplo <- rnorm(n = 10000, mean = 0, sd = 1)
hist(ejemplo, col='orange', breaks=40, 
     ylab = "Frecuencia", main = "Histograma ejemplo")



library(ggplot2)

ggplot(iris, aes(Sepal.Length, Petal.Length)) +
        geom_point()


ggplot(iris, aes(Sepal.Length, Petal.Length, color = Species)) +
        geom_point()



ggplot(iris, aes(Sepal.Length, Petal.Length, color = Species)) +
        geom_point() +
        geom_smooth(method = "lm")


ggplot(iris, aes(Sepal.Length, Petal.Length)) +
        geom_point(aes(color = Species)) # Notar diferencia! aes() aparece en geom_point()


ggplot(iris, aes(Sepal.Length, Petal.Length)) +
        geom_point(aes(color = Species))+
        geom_smooth(method="lm")



ggplot(iris, aes(Sepal.Length, Petal.Length)) +
        geom_point()


ggplot(iris, aes(Sepal.Length, Petal.Length, color='magenta')) +
        geom_point()


ggplot(iris, aes(Sepal.Length, Petal.Length)) +
        geom_point(color='magenta')



# Gr?fico de puntos
p1 <- ggplot(iris, aes(Species, Sepal.Length)) +
        geom_point()

# Gr?fico de puntos con ruido en el eje horizontal
p2 <- ggplot(iris, aes(Species, Sepal.Length)) +
        geom_jitter(width = 0.1)

# Boxplot

p3 <- ggplot(iris, aes(Species, Sepal.Length)) +
        geom_boxplot()

# Violin 
p4 <- ggplot(iris, aes(Species, Sepal.Length)) +
        geom_violin()

# Combinando boxplot y violin

p5 <- ggplot(iris, aes(Species, Sepal.Length)) +
        geom_violin(fill='orange', alpha=0.5)+
        geom_boxplot(color="white", fill="black",
                     lwd=0.8, width=0.2 )

# Grafico de barras con medias + SEM

p6 <- ggplot(iris, aes(Species, Sepal.Length)) +
        stat_summary(fun.y = mean, geom = "bar",
                     width=0.5) +
        stat_summary(fun.data = mean_se, geom = "errorbar",
                     color="red", width=0.5)


ggplot(iris, aes(Sepal.Length, Petal.Length)) +
        geom_point(aes(color=Petal.Width))+
        facet_wrap(~Species)


library(MASS)
library(cluster) 
library(nlme)    
library(epicalc) 
library(lattice) 

fvfmnuevo<- read.table("c:/PAPER/fvfmnuevo.txt", header=T)
xyplot (FVFM ~ ETAPA |factor(PLANTA), data = fvfmnuevo)


datos<- read.table("C:/METODOSESTADISTICOS/CLASE1/datostabla.txt", header=T)

tabla<-table(datos)


library(gmodels)
CrossTable(datos$nivelescola,datos$ingresos)

chisq.test(tabla)$statistic
chisq.test(tabla)

library(DescTools)
Phi(tabla)
CramerV(tabla)
