#EJEMPLO DE CORRELACIÓN
x<-c(2,3,4,5,6,7)
y<-c(0.05,0.09,0.11,0.13,0.17,0.2)
covx<-cov(x, y, method = c("pearson"))
corx<-cor(x, y, method = c("pearson"))
corx<-cor(x, y, method = c("spearman"))
corx<-cor(x, y, method = c("kendall"))
cor.test(x, y,
         alternative = c("two.sided", "less", "greater"),
         method = c("pearson", "kendall", "spearman"),
         exact = NULL, conf.level = 0.95, continuity = FALSE, ...)



cor.test(x, y, method="kendall","two.sided")
cor.test(x, y, method="spearman")
cor.test(x, y, method="pearson")

# ejemplo Pearson
x<-c(2,3,4,5,6,7)
y<-c(0.05,0.09,0.11,0.13,0.17,0.2)
corp<-cor(x, y, method = c("pearson"))
covp<-cov(x, y, method = c("pearson")) # covarianza Pearson
cor.test(x, y, method="pearson")


sumXi <- sum(x)
sumXi2 <- sum(x^2)

sumYi <- sum(y)
sumYi2 <- sum(y^2)

sumXiYi <- sum(x*y)
r <- sumXiYi/sqrt(sumXi2*sumYi2)


# ejemplo Spearman

CI<-c(106,86,100,100,99,103,97,113,113,110)
horastv<-c(7,0,28,50,28,28,20,12,7,17)
cor.test(CI, horastv, method="spearman","two.sided")
cors<-cor(CI, horastv, method = "spearman")

# ejemplo kendall
np<-c(84,80,78,76,70,64,62,50,47)
ip<-c(60,64,71,61,58,57,54,55,52)
cor.test(np, ip, method="kendall","two.sided")
cork<-cor(np, ip, method = c("kendall"))

# ejemplo iris
data("iris")

# Correlaciones entre las variables de "iris" excepto la nº 5
p <- cor(iris[-5], method="spearman") # Correlación de Spearman
cat("Correlación de Spearman\n")
print(p)

prueba <- cor.test(iris[,1],iris[,2], method="spearman") # Correlación de Spearman para la variable columna 1 y 2

cat("Correlación de Spearman\n")
print(prueba)



# coeficiente Kappa ejemplo tumores
concor=matrix(c(25,2,1,0,4,17,3,2,1,3,15,1,0,1,2,13),nrow = 4,ncol = 4,byrow = TRUE)
colnames(concor)=c("0","1","2","3")
rownames(concor)=c("0","1","2","3")
concor

addmargins(concor)  #agregar la suma por fila y columna


library("vcd")
# Calcular kappa ponderado
k <- Kappa(concor)
k


# Intervalos de confianza
confint(k)


# EJEMPLO COVID
concor=matrix(c(519,13,2,443),nrow = 2,ncol = 2,byrow = TRUE)
colnames(concor)=c("positivo","negativo")
rownames(concor)=c("positivo","negativo")
concor

k <- Kappa(concor)
k         # ASE error estándar asintótico del valor Kappa 

#Kappa ponderado

#El índice kappa ponderado debe emplearse cuando el resultado de la prueba analizada puede adoptar más de dos categorías, entre las que existe cierto orden jerárquico (resultados discretos ordinales).



concor=matrix(c(135,39,2,75,171,28,17,121,132),nrow = 3,ncol = 3,byrow = TRUE)
colnames(concor)=c("Bajo","Medio","Alto")
rownames(concor)=c("Bajo","Medio","Alto")
concor


library("vcd")
# Calcular kappa ponderado
k <- Kappa(concor)
k
concor

# Intervalos de confianza
confint(k) #estimador+- ASE coeficiente de confianza de la normal



diagnostico <- as.table(rbind(
  c(7, 1, 2, 3, 0), c(0, 8, 1, 1, 0),
  c(0, 0, 2, 0, 0), c(0, 0, 0, 1, 0),
  c(0, 0, 0, 0, 4)
  ))
categories <- c("Depression", "Personality Disorder",
                "Schizophrenia", "Neurosis", "Other")
dimnames(diagnostico) <- list(Doctor1 = categories, Doctor2 = categories)
diagnostico


library(irr)
data("diagnostico", package = "irr")

head(diagnostico[, 1:3], 4)

# Calculando el Kappa de Light entre los primeras 3 categorías
kappam.light(diagnostico[, 1:3])

# concordancia de Kendall

kend=matrix(c(5,3,1,2,4,4,3,2,1,5,5,2,1,3,4,5,3,2,1,4),5, 4)
colnames(kend)=c("EXPERTO1","EXPERTO2","EXPERTO3","EXPERTO4")
rownames(kend)=c("RANGO1","RANGO2","RANGO3","RANGO4","RANGO5")
kend

# estadístico
library(irr)
kendall(kend, TRUE)
library(DescTools)
KendallW(kend, correct = FALSE, test = FALSE, na.rm = FALSE)


