##### IMPORT DATA #####

##  loveya<3

library(readxl)
library("RColorBrewer")

library(plotrix)

DatasetStrada <- as.data.frame(read_excel("C:/Users/FauxL/Desktop/DatasetStrada.xlsx", 
                                          skip = 3))

##### CALCOLO FREQUENZE #####
frequenze_assolute <- array(dim=c(1,8))
frequenze_cumulative <- array(dim=c(1,8))

labelss <- c("Difficoltà di parcheggio", "Difficoltà di collegamento con mezzi pubblici", 
             "Traffico", "Scarsa illuminazione stradale", "Cattive condizioni stradali", "Inquinamento dell'aria",
             "Rumore", "Rischio di criminalità")

Regioni <- as.character(unlist(DatasetStrada[,1]))
valori

colnames(frequenze_assolute) <-labelss
for ( i	in	1:8) {
  frequenze_assolute[1,i] <- sum(DatasetStrada [ , i+1 ]) }
frequenze_assolute

frequenze_cumulative <- array(dim=c(1,8))
frequenze_cumulative <- cumsum(frequenze_assolute[1,1:8])
frequenze_cumulative

View(frequenze_assolute)  
View(frequenze_cumulative)  

frequenze_assolute[1,1:8]/length(frequenze_assolute)

frequenze_relative <- prop.table(frequenze_assolute[1,1:8])
frequenze_relative

frequenze_relative_cumulate <- cumsum(prop.table(frequenze_assolute[1,1:8]))
frequenze_relative_cumulate

##### GRAFICI A BARRE #####

## FREQUENZA ASSOLUTA ITALIA PER DISTRIBUZIONE
barpos <- barplot(frequenze_assolute[,1:8], legend =labelss, args.legend=list(
  x=13, bty = "n"
), col =1:8,axisnames=FALSE)
axis(1, at=barpos[4], labels=("Frequenza assoluta dissesti stradali in Italia"))


## GRAFICI A BARRE
BarStreetPlot <- function() {
  
  j <- 1
  pdf ("GraficiABarreRegioni.pdf" )
  repeat {
    if ( j == 9 ) {
      dev.off ()
      break
    }
    valori <- as.numeric(unlist(DatasetStrada[,j+1]))
    x <- barplot ( valori , ylim = c (0,2500) ,cex.axis=10, main = labelss[j] , 
                   col = rainbow ( 22 ) , axes = FALSE, axisnames = FALSE)
    abline (h = mean(valori) , col = "blue" , lty = 2 )
    text ( x , par("usr")[3] , labels = Regioni , srt = 35 , adj= c ( 1 , 1.1) , xpd = TRUE)
    axis (2)
    j <- j + 1
  }
  
}

BarStreetPlot()  


## GRAFICI A TORTA

PieStreetPlot <- function(){
  j <- 1
  pdf("GraficiTorteProblemi.pdf")
  repeat {
    if(j==23){
      dev.off()
      break
    }
    valori <- as.numeric(unlist(DatasetStrada[j,2:9]))
    percentage = paste0(labelss, " ", round(valori / sum(valori) * 100, 1), "%")
    x <- pie3D(valori, height = 0.05, explode = 0.05,radius = 0.45, labels = NA,
               col=brewer.pal(8,"PRGn"),theta = 1)
    title(Regioni[j], line=-0.1)
    legend(legend=percentage,bty="n","top", fill =brewer.pal(8,"PRGn"))
    j <- j + 1
  }
}
PieStreetPlot()

## DIAGRAMMA DI PARETO

Dati<- rowSums(DatasetStrada[,labelss])

FreqRel <- prop.table(Dati)
ParetoTable<-data.frame(Regioni,Dati,FreqRel,FreqRelCum)
ParetoTable<- ParetoTable[rev(order(ParetoTable[,2])),]
FreqRelCum <-  paste (format ( cumsum ( unlist(ParetoTable[,3]) ) * 100 , digits = 2) , "%" )
ParetoTable[,4] <- (FreqRelCum)

x <- barplot ( unlist(ParetoTable[,3]), ylim = c (0 , 1.05) , main = " Diagramma di Pareto " , col =1:22)
lines (x, cumsum (unlist(ParetoTable[,3])) , type = "b" , pch = 16)
text ( x - 0.2 , cumsum ( unlist(ParetoTable[,3]) ) + 0.03 , paste ( format ( cumsum ( unlist(ParetoTable[,3]) 
) * 100 , digits = 2) , "%" ))
text ( x , par("usr")[3] , labels = ParetoTable[,1] , srt = 40 , adj= c ( 1 , 1.1) , xpd = TRUE)


## ISTOGRAMMA
InstaPlot <- function() {
  j <- 1
  pdf ("IstogrammiFreqAss.pdf" )
  repeat {
    if ( j == 9 ) {
      dev.off ()
      break
    }
    istogramma <- hist (as.numeric(unlist(DatasetStrada[,j+1])), freq = TRUE, main = labelss[j], 
                        ylab = "Frequenza assoluta delle classi", xlab = "Classi")
    j <- j + 1
  }
}

InstaPlot()

istogramma <- hist (unlist(DatasetStrada[,2]), freq = TRUE, main = "Difficoltà di collegamento con i mezzi pubblici", ylab = "Frequenza assoluta delle classi", xlab = "classi radio")

str(istogramma)

quantile(unlist(DatasetStrada[,3]))
boxplot ( unlist(DatasetStrada[,2]), unlist(DatasetStrada[,3]), 
          unlist(DatasetStrada[,4]), unlist(DatasetStrada[,5]), 
          unlist(DatasetStrada[,6]), unlist(DatasetStrada[,7]), 
          unlist(DatasetStrada[,8]), unlist(DatasetStrada[,9]),
          main =" Boxplot Problemi Stradali" , col = brewer.pal(8,"PRGn"), names=labelss)

summary(unlist(DatasetStrada[,2]))
pairs(DatasetStrada[,2:9], main = "Scatterplot")
is.numeric(DatasetStrada[,2])

## STASTICA DESCRITTIVA UNIVARIATA

round (cumsum( table(DatasetStrada[,2]) /length(DatasetStrada[,2])) , 3 )

plot(ecdf(DatasetStrada[,2]), main = "Funzione di distribuzione empirica discreta per difficoltà di parcheggio", 
     verticals="False", col="red")

classi <- c(-100,14,100,400,700,1200,1590,2000)

FreqRelativeCumulate <- cumsum(table(cut(DatasetStrada[,2], breaks=classi,
                                         right = FALSE)))/length(DatasetStrada[,2])

FreqRelativeCumulate <- c(0, FreqRelativeCumulate)
FreqRelativeCumulate 

plot (classi, FreqRelativeCumulate, type = "b", axes = FALSE, main = "Funzione di Distribuzione Empirica Continua", col = "red")
axis (1 , classi )
axis (2 , format (FreqRelativeCumulate , digits = 2) )
box ()

DatasetStrada <-`rownames<-`(DatasetStrada, Regioni)
DatasetStrada <- DatasetStrada[,-1]

## Indici di Sintesi

mean(DatasetStrada[,1])
median(DatasetStrada[,5])

View (as.matrix( apply ( DatasetStrada , 2 , median) )) 
View ( as.matrix ( apply ( DatasetStrada , 2 , mean) ) )

## Quartili, Percentili e Quantili

quantile(DatasetStrada[,1], c(0,0.2,0.4,0.6,0.8,1), type=2)

j <-  1
repeat{
  summary(DatasetStrada[,j])
  if(j==8)
    break
  j <- j+1
}

cv <- function(x) {sd(x)/abs(mean(x))}

var(DatasetStrada[,1])

sd(DatasetStrada[,1])

cv(DatasetStrada[,1])


## Tabella Valori

j <-1
TabInd <- data.frame(matrix (ncol = 8,nrow =4,dimnames = list(c("Media", "Varianza","Deviazione Standard", "Coefficiente di Variazione"),labelss)))
repeat{
  
  TabInd[1,j] <- mean(DatasetStrada[,j])
  TabInd[2,j] <- var(DatasetStrada[,j])
  TabInd[3,j] <-sd(DatasetStrada[,j])
  str <-  cv(DatasetStrada[,j])
  TabInd[4,j] <- str
  
  if(j==8){
    break
  }
  j <- j+1
  
}

## Curtosi e Skewness

skw <- function ( x){
  n <- length (x )
  m2 <-(n -1) * var ( x)/ n
  m3 <- ( sum ( (x - mean (x ))^3) ) /n
  m3 / ( m2^1.5)
}

Skewness <- array(dim =8, dimnames = list(labelss))

j<-1
repeat{
  
  Skewness[j] <-  skw(DatasetStrada[,j])
  if(j==8){
    break
  }
  j <- j+1
}

View(Skewness)

curt <- function (x ){
  n <- length ( x)
  m2 <-(n -1) * var (x) /n
  m4 <- ( sum ( (x - mean ( x)) ^4) )/n
  m4 /( m2 ^2) -3
}

Curtosi <-  array(dim =8, dimnames = list(labelss))

j<-1
repeat{
  
  Curtosi[j] <-  curt(DatasetStrada[,j])
  if(j==8){
    break
  }
  j <- j+1
}

View(Curtosi)


## Statistica descrittiva bivariata

plot (  DatasetStrada$`Cattive condizioni stradali`,DatasetStrada$Traffico , main =" Traffico in funzione
di Cattive condizioni stradali" , xlab = "Cattive condizioni Stradali " , ylab ="Traffico" , col =" red ")
abline ( v= median(DatasetStrada$`Cattive condizioni stradali` ) , lty =1 , col =" magenta ")
abline ( v= mean(DatasetStrada$`Cattive condizioni stradali` ) , lty =2 , col =" blue ")
abline ( h= median(DatasetStrada$Traffico ) , lty =1 , col =" magenta ")
abline ( h= mean(DatasetStrada$Traffico ) , lty =2 , col =" blue ")
legend(0,1200,c("Mediana", "Media"),pch=0, col=c("magenta","blue"), cex=0.8)


## Covarianza e Correlazione Campionaria

cov(DatasetStrada[,5], DatasetStrada[,3])
cor(DatasetStrada[,5], DatasetStrada[,3])

plot(DatasetStrada$`Cattive condizioni stradali` , DatasetStrada$Traffico , main =" Retta di regressione " ,
     xlab = "Cattive condizioni Stradali" , ylab = "Traffico" , col =" red " )
abline ( lm(DatasetStrada$Traffico~ DatasetStrada$`Cattive condizioni stradali`) , col =" blue ")


View(round(cov(DatasetStrada), digits = 1 )) 
View(round(cor(DatasetStrada), digits = 3 ))

## Coefficiente angolare e intercetta
CoefficienteAngolare <- ( sd ( DatasetStrada$Traffico ) / sd ( DatasetStrada$`Cattive condizioni stradali` ) ) *
  cor( DatasetStrada$Traffico , DatasetStrada$`Cattive condizioni stradali` )

Intercetta <- mean( DatasetStrada$Traffico ) - CoefficienteAngolare *mean(DatasetStrada$`Cattive condizioni stradali`)

c (CoefficienteAngolare, Intercetta)

lm(DatasetStrada$Traffico~ DatasetStrada$`Cattive condizioni stradali`)

## Residui

stime <- array(fitted( lm(DatasetStrada$Traffico~ DatasetStrada$`Cattive condizioni stradali`)))
`row.names<-`(stime,regioni)


residui <- array(resid( lm(DatasetStrada$Traffico~ DatasetStrada$`Cattive condizioni stradali`)))
`row.names<-`(residui,regioni)

linearModel <-  lm(DatasetStrada$Traffico~ DatasetStrada$`Cattive condizioni stradali`)
median(linearModel$residuals)
var(linearModel$residuals)
sd(linearModel$residuals)

plot(DatasetStrada$`Cattive condizioni stradali` , DatasetStrada$Traffico , main =" Retta di regressione con residui " ,
     xlab = "Cattive condizioni Stradali" , ylab = "Traffico" , col =" red " )
abline ( lm(DatasetStrada$Traffico~ DatasetStrada$`Cattive condizioni stradali`) , col =" blue ")
segments(DatasetStrada$`Cattive condizioni stradali`, stime,DatasetStrada$`Cattive condizioni stradali`, 
         DatasetStrada$Traffico, col="magenta" )

plot(DatasetStrada$Traffico, residui, main = "Diagramma dei residui", xlab="Cattive condizioni stradali",
     ylab="Traffico", pch=9, col="red")
abline(h=0, col = "blue", lty=2)

residuiStandard <- array(residui/sd(residui))
`row.names<-`(residuiStandard,regioni)


plot(stime, residui, main="Residui standard rispetto ai valori stimati", xlab = "Valori Stimati",
     ylab = "Residui standard", pch=5, col="red")
abline(h=0, col="blue", lty=2)

multilm <- lm(DatasetStrada[,3]~DatasetStrada[,1]+DatasetStrada[,2]+DatasetStrada[,4]+DatasetStrada[,5]+DatasetStrada[,6]
   +DatasetStrada[,7]+DatasetStrada[,8])

stimemulti <- array(fitted(multilm))
`row.names<-`(stimemulti,regioni)

residuimulti <- array(resid(multilm))
`row.names<-`(residuimulti,regioni)

median(multilm$residuals)
var(multilm$residuals)
sd(multilm$residuals)

residuimultistd <- array(residuimulti/sd(residuimulti))
`row.names<-`(residuimultistd,regioni)

plot(stimemulti, residuimultistd, main="Residui standard rispetto ai valori stimati",
  xlab="Valori stimati", ylab="Residui standard", pch=5, col ="red")
abline(h=0, col="blue", lty=2)

## Regressione Polinomiale

pol <- lm(DatasetStrada[,3]~ DatasetStrada[,5]+ I(DatasetStrada[,5]^2))
pol
  
  
alpha <- pol$coefficients[[1]]
beta <- pol$coefficients[[2]]
gamma <- pol$coefficients[[3]]

c(alpha,beta,gamma)

plot(DatasetStrada$Traffico, DatasetStrada$`Cattive condizioni stradali`, col="red", 
     main="Scatterplot e curva stimata",xlab="Traffico",ylab="Cattive condizioni Stradali")
curve(alpha+beta*x +gamma*x^2, add=TRUE)

### Analisi del cluster

DatasetScaled <- data.frame(scale(DatasetStrada))

matriceDistanza <- dist(DatasetScaled, method = "euclidean", diag=TRUE, upper = TRUE)
hls <- hclust(matriceDistanza, method = "single")
str(hls)

hls$merge
hls$height

plot(hls, hang=-1, xlab = "Metodo gerarchico agglomerativo", sub="del legame singolo")
axis(side = 4, at=round(c(0,hls$height),2))

hlsc <- hclust(matriceDistanza, method = "complete")
str(hlsc)

hlsc$merge
hlsc$height

plot(hlsc, hang=-1, xlab = "Metodo gerarchico agglomerativo", sub="del legame completo")
axis(side = 4, at=round(c(0,hlsc$height),2))


hlsa <- hclust(matriceDistanza, method = "average")
str(hlsa)

hlsa$merge
hlsa$height


plot(hlsa, hang=-1, xlab = "Metodo gerarchico agglomerativo", sub="del legame medio")
axis(side = 4, at=round(c(0,hlsa$height),2))

## Centroide

matriceDistanza2 <- matriceDistanza^2
hc <- hclust(matriceDistanza2, method = "centroid")


hc$merge
hc$height


plot(hc, hang=-1, xlab = "Metodo gerarchico agglomerativo", sub="del centroide")
axis(side = 4, at=round(c(0,hc$height),2))


hcm <- hclust(matriceDistanza2, method = "median")

hcm$merge
hcm$height


plot(hcm, hang=-1, xlab = "Metodo gerarchico agglomerativo", sub="della mediana")
axis(side = 4, at=round(c(0,hcm$height),2))

## Analisi del Dendrogramma

plot(hls, hang =-1, xlab="Metodo gerarchico agglomerativo", sub="del legame singolo")
rect.hclust(hls, k=2, border ="red")

rect.hclust(hls, k=3, border ="green")

table(cutree(hls, k=4, h=NULL))

taglio <- list(cutree(hls, k=4))

aggregate(DatasetScaled,taglio, mean)

aggregate(DatasetStrada,taglio, var)

aggregate(DatasetStrada,taglio, sd)


## Non omogeneità statistica

tree <- hclust(matriceDistanza2, method = "centroid")
taglio <- cutree(tree, k=4, h=null)
num <- table(taglio)
tagliolist <- list(taglio)
agvar <- aggregate(DatasetScaled, tagliolist, var)[,-1]
trh1 <- (num[[1]] -1) * sum(agvar[1,])
trh2 <- (num[[2]] -1) * sum(agvar[2,])
trh3 <- (num[[3]] -1) * sum(agvar[3,])
trh4 <- (num[[4]] -1) * sum(agvar[4,])

trh1
trh2
trh3
trh4

n <- nrow (DatasetScaled )
trHI <- (n-1)*sum( apply (DatasetScaled, 2 , var ))

## Metodi non gerarchici (k-means)

kmeans(DatasetStrada, center = 3, iter.max = 10, nstart=1)

tree <- hclust(matriceDistanza2, method = "centroid")
taglio <- list(cutree(tree, k=4, h=null))
centroidiIni <- aggregate(DatasetStrada,taglio,mean) [,-1]
kmeans(DatasetStrada, center = centroidiIni, iter.max = 10, nstart=1)

kmeans(DatasetScaled, center=5, iter.max = 10, nstart=1)


