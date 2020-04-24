###netflix 1999-2000

##comandi preliminari

setwd("C:/Users/alice/Desktop/stage/netflix 1999-2000")

##pacchetti e librerie utilizzati

#library(reshape2) #per acast()
library(randomForest) #per randomForest()
library(rpart) #per rpart()

##importazione del dataset

load("Netflix_1999_2000.RData")
ls() #selected.data
df <- selected.data
rm(selected.data)
df$id <- 1:nrow(df)
df$customer <- as.numeric(df$customer)
df$movie <- as.numeric(df$movie)
colnames(df)[5] <- "rate"
df$rate <- factor(df$rate)
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
  #aggiungo record (senza rate) ad I
  #I <- rbind(I, df[df$id == record$id, ])

#5
  n <- 5
  if(nrow(I)>=n) { #l'utente u ha valutato abbastanza film 
    
#6 
    #h = 3 mesi
    day1 <- seq(record$day, length = 2, by = "-3 months")[2] #estremo inferiore
    day2 <- record$day #estremo superiore
    
    #L: insieme delle valutazioni effettuate sui film di I nel periodo h
    L <- df[(df$movie %in% I$movie | df$movie == record$movie) & df$day >= day1 & df$day < day2, ] 
    
    M <- data.frame()
    v <- 4
    
    #M: insieme delle valutazioni effettuate sui film di I nel periodo h dagli utenti con almeno v film valutati in I e che hanno valutato m
    for(i in unique(L$customer)) {
      if(is.element(record$movie, L[L$customer==i,]$movie) & nrow(L[L$customer==i,]) > v) {
        M <- rbind(M, L[L$customer==i,])
      }
    }
    
    #D: matrice che ha per colonne i film di I e per righe gli utenti che hanno valutato almeno v film di I
    #D <- acast(M, customer ~ movie , value.var='rate', dimnames=list(utenti=unique(M$customer), film=(M$movie)))

#7
    M <- M[,3:5] #mantengo solo le colonne relative a customer, movie e rate
    forest1 <- randomForest(rate ~ ., data=M) 
    forest2 <- randomForest(rate ~ ., data=M, importance=TRUE, proximity=TRUE)
    #OOB estimate of error rate: ~64% per entrambi
  
#8
    a <- df[df$id == record$id, 3:5]
    predict(forest1, a, type="prob") 
    rate.predetto <- predict(forest1, a, type="response")
    rate.predetto #rate predetto: 3
    record$rate #rate effettivo: 2
    
#9 
    #differenza rate predetto e rate effettivo
    diff <- as.numeric((rate.predetto)[[1]]) - as.numeric(record$rate)
    diff #differiscono di 1
    
#10: tentativo con decision tree
    decisionTree <- rpart(rate ~ ., data=M, method="class")
    print(decisionTree)
    predict(decisionTree, a, type="class") #rate predetto: 3
     
    #rendo nuovamente visibile il rate di record all'interno di df
    df[df$id == record$id, ]$rate <- record$rate
         
  } else { #l'utente u non ha valutato abbastanza film 
    print("cold start", quote=FALSE)
  }
  
} else { #non ci sono dati disponibili
  print("nessun dato disponibile", quote=FALSE)
}