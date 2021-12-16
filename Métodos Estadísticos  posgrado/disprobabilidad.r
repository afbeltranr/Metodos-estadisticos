####################################################################################
#############                DISTRIBUCIÓN BINOMIAL               ###################
####################################################################################
dbinom(x, size, prob, log = FALSE)
pbinom(q, size, prob, lower.tail = TRUE, log.p = FALSE)
qbinom(p, size, prob, lower.tail = TRUE, log.p = FALSE)
rbinom(n, size, prob)



dbinom(k, size=n, prob=p) #Si X~Bi(n,p), para calcular P(X=k).
pbinom(k, n, p) #calcula la probabilidad acumulada P(X<=x)
dbinom(3,5,0.1)  # probabilidad en el punto P(X=3). Si X~Bi(n=5,p=0.1)
pbinom(3,5,0.1)  # Probabilidad acumulada hasta 3 (p(X<=3))

x<-rbinom(20, 10, 0.6)   # generar 20 aleatorios de una distribución binomial con n=10 p=0.6
y<-dbinom(x, 10, 0.6, log = FALSE) # distribución de probabilidad
plot(x,y)  # gráfico de la función de probabilidad

# función de masa de probabilidad
plot(x, dbinom(x, size=10, prob=0.6), xlab="Number of Successes", 
  ylab="Probability Mass", main="Binomial Distribution: Trials = 10, Probability of 
  success = 0.6", type="h")
points(x, dbinom(x, size=10, prob=0.6), pch=16)
abline(h=0, col="gray")


z<-0:100
plot(z,dbinom(z,500,0.1),type="h")
curve(dnorm(x,50,6.71),add=T) #se utiliza la opción add=T (TRUE) para indicarle a R que superponga este gráfico al anterior.


####################################################################################
#############            DISTRIBUCIÓN HIPERGEOMÉTRICA            ###################
####################################################################################
dhyper(x, m, n, k, log = FALSE)
phyper(q, m, n, k, lower.tail = TRUE, log.p = FALSE)
qhyper(p, m, n, k, lower.tail = TRUE, log.p = FALSE)
rhyper(nn, m, n, k)



dhyper(x, m, n, k)  # P(X=x) m elementos de interés y n que no. Se extraen k elementos del conjunto
phyper(x, m, n, k)  # calcula P(X<=x)
dhyper(2, 7, 3, 2)  # probabilidad en X=2
phyper(1, 7, 3, 2)  # probabilidad acumulada hasta 1

dhyper(1, 50, 40, 5) # población de interés m=50, no éxito n=40, muestra k=5 probabilidad de X=1
phyper(1, 50, 40, 5) # probabilidad acumulada hasta 1
####################################################################################
#############            DISTRIBUCIÓN BINOMIAL NEGATIVA          ###################
####################################################################################

dnbinom(x, size=r, prob=p) # Si X~BN(r,p), calcula la probabilidad de obtener x fracasos para lograr r éxitos
pnbinom(x, size=r, prob=p) # calcula probabilidad de a lo sumo x fracasos

dnbinom(2, size=5, prob=0.2)
pnbinom(3, size=5, prob=0.2)

dnbinom(3, size=2, prob=0.25) # ejemplo diapositivas
####################################################################################
#############            DISTRIBUCIÓN GEOMÉTRICA                 ###################
####################################################################################

dgeom(x, prob=p) # Si X~G(p). calcula P(X=x)
pgeom(q, prob=p) # calcula P(X<=x)

dgeom(4, prob=0.4)
pgeom(6, prob=0.4)

dgeom(5, prob=0.01)  # ejemplo diapositivas

####################################################################################
#############            DISTRIBUCIÓN POISSON                    ###################
####################################################################################


dpois(x, lambda=l ) calcula P(X=x) # Si X~P(lambda), calcula P(X=x).
ppois(x, lambda=l ) calcula P(X<=x) # calcula P(X<=x)

dpois(x, lambda=2 )

p(x>2)=1-ppois(2, lambda=6 ) 
####################################################################################
#############            DISTRIBUCIÓN NORMAL                     ###################
####################################################################################

dnorm(x, mean = 0, sd = 1, log = FALSE)
pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
rnorm(n, mean = 0, sd = 1)


qnorm(c(.9772), mean = 0, sd = 1, lower.tail = FALSE)# cuantiles con q, en una N(0, 1), deja en la cola izquierda una probabilidad de 0,0228
# y en la derecha 0.9772 con lower.tail=FALSE
qnorm(c(.9772), mean = 0, sd = 1, lower.tail = TRUE) # cuantile con un área de 0.9772 en la cola izquierda
# Nota.
# lower.tail = TRUE usa la cola de la izquierda, mientras que lower.tail = FALSE usa la
# derecha. Los parámetros lower.tail = TRUE, mean = 0 y sd = 1 pueden ser omitidos,
# pues son los valores por defecto en esta función.

x <- rnorm(50,mean=3,sd=3)  #genera 50 números aleatorios de una d normal con media=3 y sd=3

Z<-dnorm(x, mean = 0, sd = 1, log = FALSE)  # calcula la función de densidad de porbabilidad a partir de x
plot(x,Z)  # gráfico de la fdp
distr<-dnorm(2, mean = 0, sd = 1, log = TRUE)
qnorm(c(0.95), mean = 0, sd = 1, lower.tail = TRUE) #cuantil del 95%

pnorm(2, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)  # probabilidad acumulada hasta 2 (p(X<=2))
y <- pnorm(x, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
plot(x,y)    #gráfico función acumulada
z<- 0:100
plot(z,pnorm(z,50,6.71),type="l")
#log, log.p logical; if TRUE, probabilities p are given as log(p).

#lower.tail logical; if TRUE (default), probabilities are P[X <= x] otherwise, P[X > x].


####################################################################################
#############            DISTRIBUCIÓN UNIFORME                   ###################
####################################################################################
y<- punif(c(55), min=0, max=90, lower.tail=TRUE)
z<- punif(c(15), min=0, max=90, lower.tail=TRUE)

y<-runif(10, min=0, max=90)  #genera 10 números aleatorios de una d nuniforme (0,90)

####################################################################################
#############            DISTRIBUCIÓN EXPONENCIAL                ###################
####################################################################################
dexp(x, rate = 1, log = FALSE)
pexp(q, rate = 1, lower.tail = TRUE, log.p = FALSE)
qexp(p, rate = 1, lower.tail = TRUE, log.p = FALSE)
rexp(n, rate = 1)

pexp(c(5), rate=0.1428, lower.tail=FALSE)   # almenos 5 (p(X>=5)  Lambda=1/7=0.1428
pexp(c(3), rate=0.1428, lower.tail=TRUE)    # menos de 3 (p(X<3))
p<-rexp(20, rate =3)    #generar 20 aleatorios de una exponecial, lambda=3
qexp(0.80, rate =3, lower.tail = TRUE, log.p = FALSE) # cuantil del 80% acumulado con lambda=3

r<-dexp(p, rate =3, log = FALSE)  # distribución exponencial
plot(p,r)

####################################################################################
#############            DISTRIBUCIÓN T-STUDENT                  ###################
####################################################################################
dt(x, df, ncp, log = FALSE)
pt(q, df, ncp, lower.tail = TRUE, log.p = FALSE)
qt(p, df, ncp, lower.tail = TRUE, log.p = FALSE)
rt(n, df, ncp)

qt(c(0.5), df=16, lower.tail=TRUE)  #cuantil 50% con 16 grados de libertad
qt(c(0.85), df=16, lower.tail=TRUE) #percentil 85% con 16 grados de libertad
pt(c(-1), df=16, lower.tail=TRUE)   # probabilidad de X<=-1
pt(c(-1), df=16, lower.tail=FALSE)   # probabilidad de X>=-1
rt(n, df)     # generar número aleatorios de una t-student con ciertos gl
p<-rt(50, 12) # generar  50 aleatorios de una t-student 12 gl
pt(c(p), df=12, lower.tail=TRUE)
r<-dt(p, 12, log = FALSE)  # distribución de t-student
plot(p,r)   # gráfico de la fdp


####################################################################################
#############            DISTRIBUCIÓN CHI-CUADRADO               ###################
####################################################################################
dchisq(x, df, ncp=0, log = FALSE)
pchisq(q, df, ncp=0, lower.tail = TRUE, log.p = FALSE)
qchisq(p, df, ncp=0, lower.tail = TRUE, log.p = FALSE)
rchisq(n, df, ncp=0)


pchisq(c(7.5), df=28, lower.tail=FALSE)  #probabilidad de que X>7.5 con 28 gl.
l<-rchisq(20, 10, ncp=0) # generar 20 aleatorios con 10 gl
o<-dchisq(l, 10, ncp=0, log = FALSE)
plot(l,o)   # gráfico de la fdp

qchisq(0.5, 10, ncp=0, lower.tail = TRUE, log.p = FALSE)# cuantil 50% acumulado


####################################################################################
#############            DISTRIBUCIÓN F  SNEDECOR                ###################
####################################################################################

df(x, df1, df2, ncp, log = FALSE)
pf(q, df1, df2, ncp, lower.tail = TRUE, log.p = FALSE)
qf(p, df1, df2, ncp, lower.tail = TRUE, log.p = FALSE)
rf(n, df1, df2, ncp)


pf(2.5, 4, 5, ncp=0, lower.tail = TRUE, log.p = FALSE)  # probabilidad de una F<=2.5 gln=4, gld=5 , ncp=0 F central
pf(2.5, 4, 5, ncp=0, lower.tail = FALSE, log.p = FALSE)  # probabilidad de una F>=2.5 gln=4, gld=5 , ncp=0 F central
x<-rf(20, 4, 5, ncp=0) # generar 20 aleatorios de una F con gln=4 y gld=5

qf(0.95, 4, 5, ncp=0, lower.tail = TRUE, log.p = FALSE)    #cuantil del 95% acumulado con gln=4, gld=5 , ncp=0 F central
qf(0.95, 4, 5, ncp=0, lower.tail = FALSE, log.p =FALSE)    #cuantil del 95% en cola derecha con gln=4, gld=5 , ncp=0 F central

####################################################################################
#############            DISTRIBUCIÓN MULTINOMIAL                ###################
####################################################################################

rmultinom(n, size, prob)
dmultinom(x, size = NULL, prob, log = FALSE)

rmultinom(10, size = 12, prob = c(0.1,0.2,0.8))  # generar 10 aleatorios con n=12 y probabilidades P=0.1, p=0.2, p=0.8
