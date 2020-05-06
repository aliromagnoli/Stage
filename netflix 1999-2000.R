###netflix 1999-2000

##comandi preliminari

setwd("C:/Users/alice/Desktop/stage/netflix 1999-2000")

##pacchetti e librerie utilizzati

library(reshape2) #per acast()
library(randomForest) #per randomForest()
library(rpart) #per rpart()

##importazione del dataset

load("Netflix_1999_2000.RData")
ls() #selected.data
df <- selected.data
rm(selected.data)
df$id <- 1:nrow(df)
df$customer <- as.factor(df$customer)
colnames(df)[5] <- "rate"
options(max.print=10000000)
#####

#1
offset <- max(df[df$day < "2000-05-01",]$id) #348104: id dell'ultima valutazione considerata per il training

#2
if(offset <= dim(df)[1]){ #ci sono dati disponibili, continuo

#3
  record <- df[df$id == offset+1,] #considero la valutazione immediatamente successiva ad offset

#4
  #nascondo il rate di record all'interno di df
  df[df$id == record$id, ]$rate <- NA 
  #I: insieme delle valutazioni effettuate finora dall'utente u
  I <- df[df$day < record$day & df$customer == record$customer,]

#5
  n <- 5
  if(nrow(I)>=n) { #l'utente u ha valutato abbastanza film 
    
#6 
    #h = 3 mesi
    day1 <- seq(record$day, length = 2, by = "-3 months")[2] #estremo inferiore
    day2 <- record$day #estremo superiore
    
    #L: insieme delle valutazioni effettuate sui film di I nel periodo h
    L <- df[(df$movie %in% I$movie | df$movie == record$movie) & df$day >= day1 & df$day < day2, ] 
    
    #aggiungo record ad L 
    L <- rbind(L, df[df$id == record$id,])
    
    M <- data.frame()
    v <- 4
    
    #M: insieme delle valutazioni effettuate sui film di I nel periodo h dagli utenti con almeno v film valutati in I e che hanno valutato m
    for(i in unique(L$customer)) {
      if(is.element(record$movie, L[L$customer==i,]$movie) & nrow(L[L$customer==i,]) > v) {
        M <- rbind(M, L[L$customer==i,])
      }
    }
    
    #D: matrice che ha per colonne i film di I e per righe gli utenti che hanno valutato almeno v film di I
    D <- acast(M, customer ~ movie , value.var='rate', dimnames=list(utenti=unique(M$customer), film=unique(M$movie)))
    #conversione di D in un dataframe
    D <- as.data.frame(D)
    #conversione da int a factor degli elementi di D
    D[sapply(D, is.integer)] <- lapply(D[sapply(D, is.integer)], as.factor)

#7
    #riscrivo i nomi delle colonne in un formato accettabile
    names(D) <- make.names(names(D)) #X + numero identificativo del film
    record$movie #10013
    #chiamo m il nome della colonna relativa al film m 
    names(D)[names(D) == paste("X", record$movie, sep="")] <- 'm'
    
    set.seed(1)
    forest <- randomForest(m ~ ., data=D, na.action = na.omit)
    
    #forest2 <- randomForest(rate ~ ., data=M, importance=TRUE, proximity=TRUE)
    #OOB estimate of error rate: ~64% per entrambi
  
#8
    #trovo la riga corrispondente a u
    index <- which(is.na(D$m)) #85
    #predico il rate di u per m
    predict(forest, D[index,], type="prob") 
    rate.predetto <- predict(forest, D[index,], type="response")
    rate.predetto #rate predetto: 3
    record$rate #rate effettivo: 2
    
#9 
    #differenza rate predetto e rate effettivo
    diff <- as.numeric((rate.predetto)[[1]]) - as.numeric(record$rate)
    diff #differiscono di 1
    
#10: tentativo con decision tree
    decisionTree <- rpart(m ~ ., data=D, method="class")
    print(decisionTree)
    predict(decisionTree, D[index,], type="class") #rate predetto: 3
    
    #rinomino la colonna m con l'identificativo del film corrispondente
    names(D)[names(D) == 'm'] <- paste("X", as.numeric(as.character(record$movie)), sep="")
    
    #rendo nuovamente visibile il rate di record all'interno di df
    df[df$id == record$id, ]$rate <- record$rate
         
  } else { #l'utente u non ha valutato abbastanza film 
    print("cold start", quote=FALSE)
  }
  
} else { #non ci sono dati disponibili
  print("nessun dato disponibile", quote=FALSE)
}
