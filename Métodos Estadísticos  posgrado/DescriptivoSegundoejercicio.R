ataques<- read.table("C:/METODOSESTADISTICOS/CLASE1/ataques.txt", header=T)

datos1 <- read.csv("C:/DISEÑO/CLASE1/ataques.csv", header = T)

datos <- read.delim('clipboard')  # se copia directamente el archivo de excel
datos<-read.table ('clipboard')

file.choose()

ejemplo<-read.table("C:\\DISEÑO\\CLASE1\\ataques.txt")

library(readxl)   # para importar archivos de excel
ejemplo1<-read_xlsx("C:/METODOSESTADISTICOS/CLASE1/GOTA.xlsx")
datos1<-read_xlsx("C:/METODOSESTADISTICOS/CLASE1/GOTA.xlsx", sheet="GOTA") # importar de un archivo excel
datos1
datos2<-read_xls("C:/METODOSESTADISTICOS/CLASE1/ALTURAEJEMPLO.xls")



##### gráficos   #########


boxplot(datos1$SEVERIDAD ~ datos1$VAR,col="blue", main = "Severidad en papa por tratamiento ", xlab = "Variedad", ylab = "Severidad")


par(mfrow=c(2,2))
boxplot(datos1$SEVERIDAD ~ datos1$VAR,col="blue", main = "Severidad en papa por tratamiento ", xlab = "Variedad", ylab = "Severidad")
boxplot(datos1$POTASIO ~ datos1$VAR,col="green", main = "Potasio en papa por tratamiento ", xlab = "Variedad", ylab = "Potasio")
boxplot(datos1$NH4 ~ datos1$VAR,col="yellow", main = "NH4 en papa por tratamiento ", xlab = "Variedad", ylab = "NH4")
boxplot(datos1$NO3 ~ datos1$VAR,col="red", main = "NO3 en papa por tratamiento ", xlab = "Variedad", ylab = "NO3")




####### crear directamente los datos en r###########

nitrogeno<-c(19.4,32.6,27.0,32.1,33.0,17.7,24.8,27.9,25.2,24.3,17.0,19.4,9.1,11.9,15.8,20.7,21.0,20.5,18.8,18.6,14.3,14.4,11.8,11.6,14.2) # contenido de Nitrógeno de plantas de trébol rojo
tratamientos<-rep(c("3DOk1","3DOk5","3DOk4","3DOk7","3DOk13"),c(5,5,5,5,5)) # inoculadas con combinaciones de cultivos decepas de Rhizobium trifolii y cepas  de Rhizobium meliloti, mg.
tratamientos<- as.factor(tratamientos) # vuelve factor la variable dietas

datos2<-data.frame(tratamientos,nitrogeno) 
datos2



attach(ataques)#evita que tenga que darle la orden con el  nombre de la tabla
detach(ataques)
Base
# detach(ataques) #Si se le quita el attach se debe especificar el nombre
# ataques$Base
summary(ataques)
Trt=as.factor(Trt)  # lo vuelve variable cualitativa requiere llamar solo la variable factor
summary(Trt)
placebo=subset(ataques,Trt==0)  #seleccionar datos
placebo
attach(placebo)
tratamiento=subset(ataques,Trt==1)
tratamiento

ataques[1:28,c(2:5,7:8)]

summary(ataques[1:28,c(2:5,7:8)])  # pacientes con placebo
summary(ataques[29:59,c(2:5,7:8)]) # pacientes con el tratamiento  progabida
summary(placebo[,c(2:5,7:8)])
summary(tratamiento[,c(2:5,7:8)])

# Medidias descriptivas por grupos
ataques
by(ataques, Trt,summary) #todas las variables  por tratamientos
by(ataques$y1,ataques$Trt, summary) # resumen estadístioo variable convulsiones en  la primera visita  por tratamiento


ataques
attach(ataques)
tapply(Age,Trt,mean, na.rm=TRUE)    #media de edad por tratamiento con todos los datos.
tapply(Age,Trt,sd, na.rm=TRUE)    #desviación estándar de edad por tratamiento con todos los datos.

tapply(Age,Trt,sum, na.rm=TRUE)    #suma de edad por tratamiento con todos los datos.




#hacer histograma
par(mfrow=c(1,2))
hist(placebo$Age,ylim=c(0,30), xlab="edad", ylab="freq",main="Placebo")
hist(tratamiento$Age,ylim=c(0,30), xlab="edad", ylab="freq",main="Tratamiento")

par(mfrow=c(1,1))

hist(placebo$Age,breaks="Sturges", col="darkblue") # gráfico solo
hist(ataques$Base, breaks=4, main = paste("Histograma tasa inicial de convulsiones" ),  xlab ="convulsiones", ylab="Frecuencias", col = "lightgreen",border = "blue")

par(mfrow=c(1,2))  # los dos gráficos en uno solo
boxplot(placebo$Age, tratamiento$Age, horizontal=T, col="green",xlab="edad", ylab="tratamiento", main="Edad por tratamiento")   # SI SE QUIERE VER EL GRÁFICO EN FORMA HORIZONTAL SINO SE SUPRIME
boxplot(placebo$Age, tratamiento$Age, col="blue", xlab="tratamiento", ylab="edad",main="Edad por tratamiento")


### OTRO EJEMPLO####
iris
boxplot(iris$Sepal.Width ~ iris$Species,col="blue", main = "Longitud de sépalo por tratamientos ", xlab = "Especie", ylab = "Longitud de sépalos")

par(mfrow=c(2,2))
boxplot(iris$Sepal.Width ~ iris$Species,col="blue", main = "Ancho de sépalo por tratamientos ", xlab = "Especie", ylab = "ancho de sépalos")
boxplot(iris$Sepal.Length ~ iris$Species,col="blue", main = "Longitud de sépalo por tratamientos ", xlab = "Especie", ylab = "Longitud de sépalos")
boxplot(iris$Petal.Length ~ iris$Species,col="blue", main = "Longitud de petalo por tratamientos ", xlab = "Especie", ylab = "Longitud de petalos")
boxplot(iris$Petal.Width ~ iris$Species,col="blue", main = "Ancho de petalo por tratamientos ", xlab = "Especie", ylab = "Ancho de petalos")


plot(Age,Base)
cor(Age, Base)

par(mfrow=c(1,1))
boxplot(ataques$y4 ~ ataques$Trt,col="blue", main = "Convulsiones en la cuarta visita por tratamientos ", xlab = "Tratamientos", ylab = "Convulsiones visita 4")



