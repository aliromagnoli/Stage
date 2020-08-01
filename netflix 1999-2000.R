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

#accuratezza

#totali: numero di predizioni effettuate
  #tot: numero totale di predizioni
  #tot1 ... tot5: numero totale di predizioni del rate 1...5
totali <- data.frame(tot=0, tot1=0, tot2=0, tot3=0, tot4=0, tot5=0)

#corrette: numero di predizioni effettuate correttamente
#corr: numero totale di predizioni corrette
#corr1 ... corr5: numero totale di predizioni corrette del rate 1...5
corrette <- data.frame(corr=0, corr1=0, corr2=0, corr3=0, corr4=0, corr5=0)

accuratezza <- data.frame(day=as.Date(integer(l), origin = "1970-01-01"), totale=numeric(l), acc1=numeric(l), acc2=numeric(l), acc3=numeric(l), acc4=numeric(l), acc5=numeric(l))

#2
while(offset <= dim(df)[1]){ #ci sono dati disponibili

#3
  record <- df[df$id == offset+1,] #considero la valutazione immediatamente successiva ad offset
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
        print(forest)
      
#8
        #conversione da NA a -999 in uRate
        uRate[which(is.na(uRate), arr.ind = TRUE)] <- -999
        #predico il rate di u per m
        rate.predetto <- predict(forest, uRate[, -ix], type="response") 
        
        print(paste("rate predetto:", rate.predetto)) 
        print(paste("rate effettivo:", record$rate)) 

#9
        #aggiorno il dataframe errori
        errori[k,] <- c(as.numeric(levels(record$customer))[record$customer], as.numeric(levels(record$movie))[record$movie], record$rate, rate.predetto)
        
        #aggiorno l'accuratezza
        totali$tot <- totali$tot + 1
        if(rate.predetto == record$rate)
          corrette$corr <- corrette$corr + 1
        switch(record$rate,
               "1" = {switch(record$rate==rate.predetto, corrette$corr1 <- corrette$corr1 + 1, )
                 totali$tot1 <- totali$tot1 + 1}, 
               "2" = {switch(record$rate==rate.predetto, corrette$corr2 <- corrette$corr2 + 1, )
                 totali$tot2 <- totali$tot2 + 1}, 
               "3" = {switch(record$rate==rate.predetto, corrette$corr3 <- corrette$corr3 + 1, )
                 totali$tot3 <- totali$tot3 + 1},
               "4" = {switch(record$rate==rate.predetto, corrette$corr4 <- corrette$corr4 + 1, )
                 totali$tot4 <- totali$tot4 + 1}, 
               "5" = {switch(record$rate==rate.predetto, corrette$corr5 <- corrette$corr5 + 1, )
                 totali$tot5 <- totali$tot5 + 1}
              )
        
        accuratezza[k, ] <- c(day=paste(as.Date(record$day,origin='1970-01-01')), totale=corrette$corr/totali$tot, acc1=0, acc2=0, acc3=0, acc4=0, acc5=0)
        switch(totali$tot1!=0, accuratezza[k, ]$acc1 <- corrette$corr1/totali$tot1, 0)
        switch(totali$tot2!=0, accuratezza[k, ]$acc2 <- corrette$corr2/totali$tot2, 0)
        switch(totali$tot3!=0, accuratezza[k, ]$acc3 <- corrette$corr3/totali$tot3, 0)
        switch(totali$tot4!=0, accuratezza[k, ]$acc4 <- corrette$corr4/totali$tot4, 0)
        switch(totali$tot5!=0, accuratezza[k, ]$acc5 <- corrette$corr5/totali$tot5, 0)
        
        k <- k+1
        
        #rinomino la colonna m con l'identificativo del film corrispondente
        names(D)[names(D) == 'm'] <- paste("X", as.numeric(as.character(record$movie)), sep="")
        
        
      } else { # il numero di righe di D è minore o uguale al doppio delle colonne di D
        print(paste("D contiene troppi pochi utenti rispetto al numero di film"), quote=FALSE)
      }
      
    } else { #m non è stato valutato o gli utenti hanno visto pochi film tra quelli visti da u
      print(paste("il film", record$movie, "non ha ricevuto abbastanza valutazioni o gli utenti hanno visto pochi film tra quelli visti da", record$customer), quote=FALSE)
    }
         
  } else { #l'utente u non ha valutato abbastanza film 
    print(paste("l'utente", record$customer, "non ha valutato abbastanza film"), quote=FALSE)
  }
  
  offset <- offset + 1 #considero la valutazione successiva
}
