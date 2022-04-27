#############################################################################
# Die Projektarbeit , Mahmoud Atia Ead
#############################################################################
library(foreign)
df<-read.spss("ALLBUS_2018 (1).sav") #Wo ist die Datei


# Ein neue Dataframe
# Variablen: WAHLBETEILIGUNG,GESCHLECHT, GESCHLECHT ,ALTER, WIRTSCHAFTSLAGE IN DER BRD HEUTE,LINKS-RECHTS-SELBSTEINSTUFUNG, BEFR.

wahl<- data.frame(df$pv03,df$sex,df$age,df$ep01,df$pa01 )

#############################################################################
# SML die Analyse des Einflusses von GESCHLECHT ,ALTER, WIRTSCHAFTSLAGE IN DER BRD HEUTE,LINKS-RECHTS-SELBSTEINSTUFUNG auf WAHLBETEILIGUNG
#############################################################################

#######################################################################
# Vorbereitung der variablen
########################################################################
head(wahl)

sapply(wahl, FUN=class)
#die alle Variablen sind "factor"

#######################################################################
#1- WAHLBETEILIGUNG

table(wahl$df.pv03)
library(ggplot2)
ggplot(wahl, aes(df.pv03))+ geom_bar()
names(wahl)[1]<-"wb"
head(wahl)


#Recode als 1,0, NA

wahl$wb<- ifelse(wahl$wb=="JA",1,ifelse(wahl$wb=="KEINE ANGABE",NA,0))
table(wahl$wb)
table(is.na(wahl$wb))
ggplot(wahl, aes(wb))+geom_bar()

#######################################################################

#2-GESCHLECHT

table(wahl$df.sex)
names(wahl)[2]<-"sex"
wahl$sex<- ifelse(wahl$sex=="MANN",1,0)
ggplot(wahl,aes(sex) )+ geom_bar()

#######################################################################

#3-ALTER
table(wahl$df.age)
names(wahl)[3]<-"age"

ggplot(wahl, aes(age))+ geom_bar()
#######################################################################
#4- WIRTSCHAFTSLAGE IN DER BRD HEUTE
names(wahl)[4]<-"wirt"
ggplot(wahl, aes(wirt))+ geom_bar()
wahl$wirt<- ifelse(wahl$wirt=="SEHR GUT",1,ifelse(wahl$wirt=="GUT",2,ifelse(wahl$wirt=="TEILS TEILS",3,ifelse(wahl$wirt=="SCHLECHT",4,ifelse(wahl$wirt=="SEHR SCHLECHT",5,NA)))))
table(wahl$wirt)
table(is.na(wahl$wirt))
ggplot(wahl, aes(wirt))+ geom_bar()




#######################################################################

#5-LINKS-RECHTS-SELBSTEINSTUFUNG
table(wahl$df.pa01)

#recode Link-richt
names(wahl)[5]<-"linksrechts"
library(dplyr)
wahl$linksrechts<-recode(wahl$linksrechts,"F - LINKS"=1, "A"=2, "M"=3,"O"=4,"G"=5,"Z"=6,"E"=7,"Y"=8,"I"=9,"P - RECHTS"=10,"KEINE ANGABE"= 99)

#NA und 99 löschen 
 wahl2<- na.omit(wahl)
 
 wahl2<-subset(wahl2,linksrechts!=99 )
 nrow(wahl)-nrow(wahl2)
 table(wahl2$linksrechts)
 sapply(wahl2, FUN= class)
 #change factor to numeric, numeric to factor 
  wahl2$age<-as.numeric(wahl2$age)
  wahl2$sex<-factor(wahl2$sex)
  wahl2$wb<-factor(wahl2$wb)
  sapply(wahl2, FUN= class)
  
  #Grafik Darstellungen
  
  
  ggplot(wahl2, aes(age,color= sex ))+geom_histogram()+facet_grid(wb~.)
  ggplot(wahl2, aes(linksrechts,color= sex ))+geom_histogram()+facet_grid(wb~.)
  ggplot(wahl2, aes(wirt,color= wb ))+geom_histogram()
  
  #######################################################################
  
  #Spilt train und test
  
  library(lattice)
  library(caret)
  library(MASS)
  library(klaR)

  #Data Partition
  set.seed(404)
   p<- createDataPartition(wahl2$wb, p=0.6,list = F )
   test<-wahl2[-p,] 
   train<-wahl2[p,] 
   nrow(train)
   nrow(test)
   
   #######################################################################
   
   #train model
   #GLM
   
   GLM<-glm(wb~., train, family=binomial(link = "logit"))
   summary(GLM)
   #Odds Ratio 
   exp(coef(GLM))
   #Der Alter hat der starksten Einfluss 
   #Der Effekt von Alter ist positiv (b= 0.020990) . Das chanceverhältnis zwischen [Wahlbeteilgung
    # Y=1 im gegensatz Nicht-Wahlbeteilgung Y=0] steigt um das 1.0275436 (OR) fache zugunsten
    #von Y=1 ,pro skaleneinheit des Alters.
    varImp(GLM)
    # Wirtschaftlage ist auch stark
    
    #######################################################################
    
    #Accuracy
    
    fit <- predict(GLM,test,type='response')
     fit <- ifelse(fit > 0.5, 1, 0)
     Ac <- mean(fit != test$wb)
     print(paste('Logistic Regression Accuracy', + (1 - Ac) ))
   
    # Logistic Regression Accuracy 0.87
     
     #######################################################################
     # Ctree Model
      library(grid)
      library(libcoin)
      library(mvtnorm)
      library(partykit)
     
     m.tree<-ctree(wb~.,data = train)
     m.tree
     #Grafik Darstellung
     plot(m.tree,type = "simple", pval=F, id=F)
     #der selbe Anmerkungen ,Der Alter hat der starksten Einfluss 
     #Wirtschaftlage ist die zweite stärkste Variable
     #Accuracy
     fit2 <- predict(m.tree,test,type='response')
     #Confusion Matrix
     print("Confusion Matrix "); table(test$wb, fit2 )
     
     #######################################################################
     #Compairing more Models
     
   library(e1071)
   
   # Naive Bayes
   M.nb<- train(wb~., data=train, method = "nb" )
   #Knn
   m.knn<- train(wb~., train, method= "knn")
   #Glm
   m.glm<- train(wb~., train, method= "glm")
   #Ctree
   m.ctree<- train(wb~., train, method= "ctree")
   
     densityplot(M.nb, pch = "|")
     densityplot(m.glm, pch = "|")
     densityplot(m.knn, pch = "|")
     densityplot(m.ctree, pch = "|")
    
     #der Accuracy hat ziemlich gleiche Werte