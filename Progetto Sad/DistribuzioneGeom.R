#FUNZIONE DI PROBABILITA' GEOMETRICA
y <- 0:50
par ( mfrow =c (2 ,2) )
plot (y , dgeom (y , prob =0.25) , xlab = "y" , ylab = "P(Y = y)" ,
      type ="h" , main = "p = 0.25")
plot (y , dgeom (y , prob =0.5) , xlab = "y" , ylab = "P(Y = y)" , 
      type ="h" , main = "p = 0.5")
plot (y , dgeom (y , prob =0.75) , xlab = "y" , ylab = "P(Y = y)" , 
      type ="h" , main = "p = 0.75")
plot (y , dgeom (y , prob =1) ,xlab = "y" , ylab = "P(Y = y)" , 
      type ="h" ,main = "p = 1")

#FUNZIONE DI DISTRIBUZIONE GEOMETRICA
y <- 0:100
par ( mfrow =c (2 ,2) )
plot (y , pgeom (y , prob =0.25) ,xlab = "y" , ylab = expression (P(Y <= y)) , ylim = c(0 ,1) , 
      type="s" , main = "p = 0.25")
plot (y , pgeom (y , prob =0.5) ,xlab = "y" , ylab = expression (P(Y <= y)) , ylim = c(0 ,1) , 
      type="s" , main = "p = 0.5")
plot (y , pgeom (y , prob =0.75) ,xlab = "y" , ylab = expression (P(Y <= y)) , ylim = c(0 ,1) , 
      type="s" , main = "p = 0.75")
plot (y , pgeom (y , prob =1) ,xlab = "y" , ylab = expression (P(Y <= y)) , ylim = c(0 ,1) , 
      type="s" , main = "p = 1")

#MEDIA, VARIANZA, DEVIAZIONE STANDARD, COEFFICIENTE DI VARIAZIONE
p <- 0.60
E <- (1-p)/p
V <- (1-p)/(p*p)
D <- sqrt(V)
CV <- D/E

c(E,V,D,CV)

#QUANTILI
z <-c (0 ,0.25 ,0.5 ,0.75 ,1)
qgeom (z , prob = 0.2)

#SIMULAZIONE VARIABILE GEOMETRICA
sim <- rgeom (40 , prob =0.2)
sim
table ( sim )
table(sim)/length ( sim )

#METODO DEI MOMENTI
campgeom <-c (7 , 2, 4, 0, 1, 1, 0, 2, 5, 3, 1, 0, 8, 1, 0,
              10 , 11, 1, 6, 8, 0, 4, 3, 7 ,15 , 9, 1, 0, 14, 3)

stimap <-1/ (1+ mean ( campgeom ))
stimap

#INTERVALLI DI CONFIDENZA 
alpha <- 1 - 0.9
qnorm (1 - alpha /2, mean = 0, sd = 1)
zalpha <- qnorm (1 - alpha /2, mean = 0, sd = 1)
n <- 150
medcamp <- 47

a2 <-n * medcamp ^2
a1 <- -(2 *n * medcamp - zalpha ^2)

a0 <-n - zalpha ^2
polyroot (c(a0 ,a1 ,a2))

#OPPURE
(1 / medcamp ) -( zalpha / medcamp ^2) * sqrt ( medcamp *( medcamp -1) /n)
(1 / medcamp ) +( zalpha / medcamp ^2) * sqrt ( medcamp *( medcamp -1) /n)


#VERIFICA DELLE IPOTESI
campgeom <-c (2 , 2, 4, 0, 1, 1, 0, 2, 5, 3, 1, 0, 8, 1, 0,
              5 , 0, 1, 6, 8, 0, 4, 3, 7 ,4 , 9, 1, 0, 10, 3)

p0<-0.3

m<-mean(sim)

alpha<- 0.20

qnorm(1-alpha,mean=0,sd=1)

((m-((1-p0)/p0))/(sqrt((1-p0)/(n*p*p))))



p0<-0.8

m<-mean(sim)

alpha<- 0.20

qnorm(alpha,mean=0,sd=1)

u <-(1-p0)/p0
((m-((1-p0)/p0))/(sqrt((1-p0)/(n*p*p))))




