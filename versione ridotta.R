###netflix 1999-2000

##comandi preliminari

setwd("../netflix 1999-2000")

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

#####

#1
# trovo l'id dell'ultima valutazione considerata per il training
offset <- max(df[df$day < "2000-05-01",]$id) #348104

#errori: dataframe in cui tener traccia degli errori di previsione
l <- nrow(df)-offset
errori <- data.frame(customer=integer(l), movie=integer(l), rateReale=integer(l), ratePredetto=integer(l))
k <- 1

giorno.prima <- df[df$id == offset, ]$day
  
#2
while(offset <= dim(df)[1]){ #ci sono dati disponibili
  
#3
  record <- df[df$id == offset+1,] #considero la valutazione immediatamente successiva ad offset
  
  if(record$day != giorno.prima)
    print(record)
  
#4
  #I: insieme delle valutazioni effettuate finora dall'utente u
  I <- df[df$day < record$day & df$customer == record$customer, ]
  
#5
  n <- 5
  if(nrow(I)>=n) { #l'utente u ha valutato abbastanza film 
    
#6 
    #h = 3 mesi
    day1 <- seq(record$day, length = 2, by = "-3 months")[2] #estremo inferiore
    day2 <- record$day #estremo superiore
    
    #I : insieme delle valutazioni effettuate dall'utente u nel periodo h
    I <-  I[I$day >= day1, ]
    #L: insieme delle valutazioni effettuate sui film di I nel periodo h
    L <- df[(df$movie %in% I$movie | df$movie == record$movie) & df$day >= day1 & df$day < day2, ] 
    
    #aggiungo record ad L 
    L <- rbind(L, df[df$id == record$id,])
    
    L$customer <- as.numeric(levels(L$customer))[L$customer]
    L$movie <- as.numeric(levels(L$movie))[L$movie]
    
    #creo un dataframe vuoto M
    m <- nrow(df[df$day >= day1 & df$day <=day2, ])
    M <- data.frame(id=integer(m), day=NA, customer=integer(m), movie=integer(m), rate=integer(m))
    M$day <- as.Date(M$day)
    v <- 4
    
    #M: insieme delle valutazioni effettuate sui film di I nel periodo h dagli utenti con almeno v film valutati in I e che hanno valutato m
    j <- 1
    for(i in unique(L$customer)) {
      if(is.element(record$movie, L[L$customer==i,]$movie) & nrow(L[L$customer==i,]) > v) {
        M[j:(j+nrow(L[L$customer==i,])-1),] <- L[L$customer==i,]
        j <- j + nrow(L[L$customer==i,])
      }
    }
    
    M <- na.omit(M) #tolgo le righe in più di M
    
    if(nrow(M) != 0) { #M non è vuoto
      
      M$customer <- factor(M$customer)
      M$movie <- factor(M$movie)
      
      #nascondo il rate di record all'interno di M    
      M[M$id == record$id, ]$rate <- NA
      
      #D: matrice che ha per colonne i film di I e per righe gli utenti che hanno valutato almeno v film di I
      D <- acast(M, customer ~ movie , value.var='rate', dimnames=list(utenti=unique(M$customer), film=unique(M$movie)))
      #conversione di D in un dataframe
      D <- as.data.frame(D)
      
      if(nrow(D) > 2*ncol(D)){ #il numero di righe di D è più del doppio del numero di colonne
        
#7
        #riscrivo i nomi delle colonne in un formato accettabile
        names(D) <- make.names(names(D)) #X + numero identificativo del film
        
        #chiamo m il nome della colonna relativa al film m 
        names(D)[names(D) == paste("X", record$movie, sep="")] <- 'm'
        
        #assegno a uRate i rates di u 
        uRate <- D[which(is.na(D$m)),]
        
        #tolgo la riga relativa a u da D
        D <- D[which(!is.na(D$m)),]
        
        #conversione da NA a -999 in D
        D[which(is.na(D), arr.ind = TRUE)] <- -999
        
        #conversione a factor della colonna corrispondente a m (in modo da effettuare una classificazione)
        ix <- which(names(D)=="m")
        D[,ix] <- as.factor(D[,ix])
        
        #creazione della random forest
        forest <- randomForest(x = D[, -ix], y=D$m)
        
#8
        #conversione da NA a -999 in uRate
        uRate[which(is.na(uRate), arr.ind = TRUE)] <- -999
        #predico il rate di u per m
        rate.predetto <- predict(forest, uRate[, -ix], type="response") 

#9      
        #aggiorno il dataframe errori
        errori[k,] <- c(as.numeric(levels(record$customer))[record$customer], as.numeric(levels(record$movie))[record$movie], record$rate, rate.predetto)
        k <- k+1
        
        #rinomino la colonna m con l'identificativo del film corrispondente
        names(D)[names(D) == 'm'] <- paste("X", as.numeric(as.character(record$movie)), sep="")
        
      }
    }
  }
  
  giorno.prima <- record$day
  offset <- offset + 1 #considero la valutazione successiva
}
