---
title: "Projet"
author: "Thibault CANAVAGGIO "
date: "27/03/2022"
output: html_document
editor_options: 
  chunk_output_type: inline
---

---
# Sommaire
## I Initialisation de l'espace de Travail
### 1) Librairies utilisées
### 2) Chargement des données
## II Exploration des données
### 1) Aperçu
### 2) Recodage
### 3) pré-traitement des données
## III Classifieur
---

------------------------------------------------------------------------

# I Initialisation de l'espace de Travail

```{r}
setwd("C:/Users/visit/OneDrive/Documents/Notes")
# repertoire de travail
```

## 1) Librairies utilisées

```{r}
install.packages("kknn")
install.packages("rpart")
install.packages("C50")
install.packages("randomForest")
install.packages("random")
install.packages("nnet")
install.packages("naivebayes")
install.packages("dplyr")
install.packages("ROCR")
install.packages("cluster",DEPENDANCIES=T)
install.packages("ggplot2",DEPENDANCIES=T)
install.packages("random",DEPENDANCIES=T)
install.packages("dplyr",DEPENDANCIES=T)
install.packages("plotly",DEPENDANCIES=T)
install.packages("caret",DEPENDANCIES=T)
install.packages("e1071",DEPENDANCIES=T)
install.packages("tree",DEPENDANCIES=T)
install.packages("nycflights13")
install.packages("tidyverse")
install.packages("dbscan")
install.packages("htmltools")

```

```{r}
library(cluster)
library(ggplot2)
library(random)
library(rpart)
library(dplyr)
library(plotly)
library(caret)
library(e1071)
library(tree)
library(nycflights13)
library(tidyverse)
library(dbscan)
library(plotly)
library(rmarkdown)
library(Rcpp)
library(htmltools)
library(readr)
library(C50)
library(tree)
library(randomForest)
library(kknn)
library(naivebayes)
library(nnet)
library(dplyr)
library(ROCR)
```

## 2) Chargement des données

```{r}
assessments<-read.csv("C:/Users/visit/OneDrive/Documents/Notes/assessments.csv")
courses<-read.csv("C:/Users/visit/OneDrive/Documents/Notes/courses.csv")
studentAssessment<-read.csv("C:/Users/visit/OneDrive/Documents/Notes/studentAssessment.csv")
studentinfo<-read.csv("C:/Users/visit/OneDrive/Documents/Notes/studentInfo.csv")
studentRegistration<-read.csv("C:/Users/visit/OneDrive/Documents/Notes/studentRegistration.csv")
studentVle<-read.csv("C:/Users/visit/OneDrive/Documents/Notes/studentVle.csv")
Vle<-read.csv("C:/Users/visit/OneDrive/Documents/Notes/vle.csv")
```

###Travail sur courses

```{r}
courses$code_module<-factor(as.factor(courses$code_module),ordered = FALSE)
courses$code_presentation<-factor(as.factor(courses$code_presentation),ordered = FALSE)
g= ggplot(courses,aes(module_presentation_length,fill=code_module))+ geom_bar()
ggplotly(g)
```

### Travail sur studentregisration

```{r}
studentRegistration$code_module <-factor(as.factor(studentRegistration$code_module), ordered=FALSE)
studentRegistration$code_presentation<-factor(as.factor(studentRegistration$code_presentation), ordered=FALSE)
```

### Travail sur studentinfo

```{r}
studentinfo$code_module<-factor(as.factor(studentinfo$code_module), ordered=FALSE)
studentinfo$code_presentation<-factor(as.factor(studentinfo$code_presentation), ordered=FALSE)
studentinfo$gender<-factor(as.factor(studentinfo$gender), ordered=FALSE)
studentinfo$region<-factor(as.factor(studentinfo$region), ordered=FALSE)
studentinfo$highest_education<-factor(as.factor(studentinfo$highest_education), ordered=FALSE)
studentinfo$imd_band<-factor(as.factor(studentinfo$imd_band), ordered=FALSE)
studentinfo$age_band<-factor(as.factor(studentinfo$age_band), ordered=FALSE)
studentinfo$disability<-factor(as.factor(studentinfo$disability), ordered=FALSE)
studentinfo$final_result<-factor(as.factor(studentinfo$final_result), ordered=FALSE)

```

### Travail sur Assessments

```{r}
assessments$code_module <- factor(as.factor(assessments$code_module), ordered=FALSE)
assessments$code_presentation <- factor(as.factor(assessments$code_presentation), ordered=FALSE)
assessments$assessment_type <- factor(as.factor(assessments$assessment_type), ordered=FALSE)
```

###Travail sur studentVle

```{r}
studentVle$code_module<-factor(as.factor(studentVle$code_module),ordered=FALSE)
studentVle$code_presentation<-factor(as.factor(studentVle$code_presentation),ordered=FALSE)
```

###Travail sur vle

```{r}
Vle$code_module<-factor(as.factor(Vle$code_module),ordered=FALSE)
Vle$code_presentation<-factor(as.factor(Vle$code_presentation),ordered=FALSE)
Vle$activity_type<-factor(as.factor(Vle$activity_type),ordered=FALSE)
```

### Tableau avec la moyenne de chaque étudiant

```{r}
data_studentAssessment<-studentAssessment %>% group_by(id_student) %>% summarise_at(vars(score),list(moyenne=mean))
```

###Création d'un tableau prédiction pour l'abandon

```{r}
datAbandon <- merge(studentRegistration,studentinfo)
```

### transformation de final_result en abandon ou continue

```{r}
datAbandon$"Giveup_or_no"=ifelse(datAbandon$final_result=="Withdrawn", "Yes", "No")
```

### Suppression de final_result

```{r}
datAbandon <- datAbandon[,-14]
```

### Visualisation des données

```{r}
gA= ggplot(datAbandon,aes(Giveup_or_no,fill=code_module))+ geom_bar()
ggplotly(gA)
```

### Visualisation des données 2

```{r}
gA= ggplot(datAbandon,aes(Giveup_or_no,fill=gender))+ geom_bar()
ggplotly(gA)
```

### Visualisation des données 3

```{r}
gA= ggplot(datAbandon,aes(Giveup_or_no,fill=region))+ geom_bar()
ggplotly(gA)
```

### Visualisation des données 4

```{r}
gA= ggplot(datAbandon,aes(Giveup_or_no,fill=highest_education))+ geom_bar()
ggplotly(gA)
```

### Visualisation des données 5

```{r}
gA= ggplot(datAbandon,aes(Giveup_or_no,fill=imd_band))+ geom_bar()
ggplotly(gA)
```

### Visualisation des données 6

```{r}
gA= ggplot(datAbandon,aes(Giveup_or_no,fill=age_band))+ geom_bar()
ggplotly(gA)
```

### Visualisation des données 7

```{r}
gA= ggplot(datAbandon,aes(Giveup_or_no,fill=disability))+ geom_bar()
ggplotly(gA)
```

### Visualisation des données 7

```{r}
gA= ggplot(datAbandon,aes(Giveup_or_no,fill=code_presentation))+ geom_bar()
ggplotly(gA)
```

### Visualisation des données 8

```{r}
datAbandon$Giveup_or_no <-factor(as.factor(datAbandon$Giveup_or_no), ordered=FALSE)
gA= ggplot(datAbandon,aes(x=Giveup_or_no,studied_credits , fill=num_of_prev_attempts))+ geom_boxplot()
ggplotly(gA)
```

### suppression de la colonne "date_unregistration"

```{r}
datAbandon <- datAbandon[,-5]
```

### création data_click

```{r}
data_click<- merge(studentVle,Vle)
data_click<-data_click %>% group_by(id_student,activity_type)   %>% summarise_at(vars(sum_click),list(somme_des_clicks=sum))
```

### inversion de tableau

```{r}
data_click <- pivot_wider(data_click, names_from = "activity_type", values_from = "somme_des_clicks")
```

###transformation des NA en 0

```{r}
data_click <- mutate_all(data_click,~replace(.,is.na(.),0))
```

### Regroupement de variables en catégories
```{r}
data_click <- data_click %>%
  mutate(Collaboration=forumng+oucollaborate+ouelluminate,
   Course_structure=glossary+homepage+dataplus,
   Course_content=resource+url+oucontent,subpage,
   Evaluation=quiz)
```
### suppression des variables de construction
```{r}
data_click <- data_click[,-(2:13)]
```

### Normalisation des données

```{r}
dummy_variables = dummyVars(~.,data=datAbandon)
dummy_variables_data = predict(dummy_variables,newdata=datAbandon)
```



### Création du databandon_Click

```{r}
datAbandon_Click <- merge(datAbandon,data_click)

```

### datarecap

```{r}
datarecap <- merge(data_studentAssessment,datAbandon_Click)

summary(datarecap)
```

### Création de jeu de test

```{r}
set.seed(3033)
datarecap <- datarecap[,-1]
training_size=floor(0.7*nrow(datarecap))
indices=sample(seq_len(nrow(datarecap)),size=training_size)
data_b.train=datarecap[indices,]
data_b.test=datarecap[-indices,]

```

### Normalisation des données

```{r}
data_preprocess_value=preProcess(data_b.train,method=c("center","scale"))
data_b.train.scaled=predict(data_preprocess_value,data_b.train)
data_b.test.scaled=predict(data_preprocess_value, data_b.test)
```

#equilibrage

```{r}
table(data_b.train.scaled[,"Giveup_or_no"])
```

### creation d'un test

```{r}
data_b.train.scaled
```

### 

```{r}
'%ni%'= Negate("%in%")
```

#downsample

```{r}
data_b.train.scaled.downsample=downSample(x=data_b.train.scaled[,colnames(data_b.train.scaled) %ni%"Giveup_or_no"],y=as.factor(data_b.train.scaled$"Giveup_or_no"))
names(data_b.train.scaled.downsample)[names(data_b.train.scaled.downsample)=="Class"]="Giveup_or_no"
table(data_b.train.scaled.downsample[,"Giveup_or_no"])
 
```

#upsample

```{r}
data_b.train.scaled.upsample=upSample(x=data_b.train.scaled[,colnames(data_b.train.scaled) %ni%"Giveup_or_no"],y=as.factor(data_b.train.scaled$"Giveup_or_no"))
names(data_b.train.scaled.upsample)[names(data_b.train.scaled.upsample)=="Class"]="Giveup_or_no"
table(data_b.train.scaled.upsample[,"Giveup_or_no"])

```

### change na to 0

```{r}
data_b.test.scaled <- data_b.test.scaled[!is.na(data_b.test.scaled$moyenne),]
data_b.train.scaled <- data_b.train.scaled[!is.na(data_b.test.scaled$moyenne),]
data_b.train.scaled.downsample <- data_b.train.scaled.downsample[!is.na(data_b.train.scaled.downsample$moyenne),]
```

### créateur d'un classificateur en ne prenant en compte que son inscription

```{r}
studentInscritpions <- studentinfo
```

### Pré-traitement de studentinfo

```{r}
set.seed(3033)
studentInscritpions<- studentInscritpions[,-3]
```

### Spécification du tableau à l'abandon ou non du cursus

```{r}
studentInscritpions$"Giveup_or_no"=ifelse(studentInscritpions$final_result=="Withdrawn", "Yes", "No")
studentInscritpions$Giveup_or_no <- as.factor(studentInscritpions$Giveup_or_no)
```

### Suppression de la colonne final_result

```{r}
studentInscritpions <- studentInscritpions[,-11]
```

### creation test et train

```{r}
dataechantillon <-sample_n(studentInscritpions,size=3000, replace =FALSE)
training_size=floor(0.7*nrow(dataechantillon))
indices=sample(seq_len(nrow(dataechantillon)),size=training_size)
studentInscritpions.train=dataechantillon[indices,]
studentInscritpions.test=dataechantillon[-indices,]
```

### Normalisation des données

```{r}
data_preprocess_value=preProcess(studentInscritpions.train,method=c("center","scale"))
studentInscritpions.train.scaled=predict(data_preprocess_value,studentInscritpions.train)
studentInscritpions.test.scaled=predict(data_preprocess_value, studentInscritpions.test)
```


### création de la matrice de distance

```{r}
dataechantillon$Giveup_or_no<-factor(as.factor(dataechantillon$Giveup_or_no),ordered = FALSE)
dmatrix <- daisy(dataechantillon)
summary(dmatrix)
```

# Clustering par fonction dbscan()

```{r}
dbs <- dbscan(dmatrix, eps=0.125, minPts = 3)
```

# Répartition des classes Produit=Oui/Non par cluster

```{r}
table(dbs$cluster,  dataechantillon$Giveup_or_no)
```
# Histogramme des effectifs des clusters avec la classe en couleur

```{r}
qplot(km4$cluster, data=datacluster, fill=Giveup_or_no)
```

# Nuages de points avec classe en couleur

```{r}
qplot(gender, as.factor(km4$cluster), data=datacluster, color=Giveup_or_no) + geom_jitter(width = 0.3, height = 0.3)
qplot(imd_band, as.factor(km4$cluster), data=datacluster, color=Giveup_or_no) + geom_jitter(height = 0.3)
qplot(age_band, as.factor(km4$cluster), data=datacluster, color=Giveup_or_no) + geom_jitter(width = 0.3, height = 0.3)
```

# Ajout de la colonne du numéro de cluster

```{r}
Giveup_or_no_km4 <- data.frame(datacluster, km4$cluster)
View(produit_km4)
```

# Boule for() faisant varier K

```{r}
for (k in 4:20 ){
  km <- kmeans(dmatrix, k)
  print(table(km$cluster,datacluster$Giveup_or_no))
  print(qplot(km$cluster, data=datacluster, fill=Giveup_or_no))
}  
```


###Création d'un tableau prédiction pour Distinction

```{r}
datAbandon <- merge(studentRegistration,studentinfo)
```

### transformation de final_result en abandon ou continue

```{r}
datAbandon$"Giveup_or_no"=ifelse(datAbandon$final_result=="Withdrawn", "Yes", "No")
```
###
```{r}
data_b.test.scaled$Giveup_or_no <- as.factor(data_b.test.scaled$Giveup_or_no)
data_b.train.scaled$Giveup_or_no <- as.factor(data_b.train.scaled$Giveup_or_no)
```

## Arbre de décision (rpart)
```{r echo=TRUE, warning=FALSE,message=FALSE, results="hold"}
test1 <- function(arg1,arg2,arg3,arg4){
  classifieur_1 <- rpart(Giveup_or_no ~ ., data_b.train.scaled,parms = list(split = arg1), control = rpart.control(minbucket = arg2))
  test_classifieur1 <- predict(classifieur_1,data_b.test.scaled, type="class")
  test_classifieur1_prob <- predict(classifieur_1,data_b.test.scaled, type="prob")
  mc_tree1 <- table(data_b.test.scaled$Giveup_or_no,test_classifieur1)
  roc_pred1 <- prediction(test_classifieur1_prob[,2], data_b.test.scaled$Giveup_or_no)
  roc_perf1 <- performance(roc_pred1,"tnr","fnr")
  plot(roc_perf1, col = arg3,add=arg4, main = "Courbes AUC du classifieur Rpart")
  auc_tree1 <- performance(roc_pred1, "auc")
  str(auc_tree1)
  cat("AUC = ",as.character(attr(auc_tree1, "y.values")))
  print(mc_tree1)
}

test1("gini",10,"red",FALSE)
test1("gini",5,"blue",TRUE)
test1("information",10,"yellow",TRUE)
test1("information",5,"black",TRUE)

```
```{r}
data_b.train.scaled <- data_b.train.scaled %>% drop_na(moyenne)
```

# RandomForest
```{r}

test2 <- function(arg1,arg2,arg3,arg4){
  class2 <- randomForest(Giveup_or_no~., data_b.train.scaled,ntree=arg1,mtry=arg2)
  test_class2 <- predict(class2,data_b.test.scaled, type="class")
  test_class2_prob <- predict(class2,data_b.test.scaled, type="prob")
  mc_tree2 <- table(data_b.test.scaled$Giveup_or_no,test_class2)
  roc_pred2 <- prediction(test_class2_prob[,2], data_b.test.scaled$Giveup_or_no)
  roc_perf2 <- performance(roc_pred2,"tnr","fnr")
  plot(roc_perf2, col = arg3,add=arg4, main = "AUC")
  auc_tree2 <- performance(roc_pred2, "auc")
  cat("AUC =",as.character(attr(auc_tree2, "y.values")))
  print(mc_tree2)
}

test2(500,5,"black",FALSE)
test2(500,3,"red",TRUE)
test2(300,5,"blue",TRUE)
test2(300,3,"grey",TRUE)


```



# Analogie

```{r}
test_knn <- function(arg1, arg2, arg3, arg4){
  knn <- kknn(Giveup_or_no~., data_b.train.scaled,data_b.test.scaled, k = arg1, distance = arg2)
  print(table(data_b.test.scaled$Giveup_or_no, knn$fitted.values))
  knn_pred <- prediction(knn$prob[,2], data_b.test.scaled$Giveup_or_no)
  knn_perf <- performance(knn_pred,"tpr","fpr")
  plot(knn_perf, main = "Classifeurs par analogie", add = arg3, col = arg4)
  knn_auc <- performance(knn_pred, "auc")
  cat("AUC = ", as.character(attr(knn_auc, "y.values")))
  invisible()
}

test_knn(10, 1, FALSE, "black")
test_knn(10, 2, TRUE, "red")
test_knn(20, 1, TRUE, "blue")
test_knn(20, 2, TRUE, "grey")

```

# SUpport Vector Machines

```{r}
test_svm <- function(arg1, arg2, arg3){
  svm <- svm(Giveup_or_no~., data_b.train.scaled, probability=TRUE, kernel = arg1)
  svm_class <- predict(svm, data_b.test.scaled, type="response")
  print(table(data_b.test.scaled$Giveup_or_no, svm_class))
  svm_prob <- predict(svm, data_b.test.scaled, probability=TRUE) 
  svm_prob <- attr(svm_prob, "probabilities") 
  svm_pred <- prediction(svm_prob[,1], data_b.test.scaled$Giveup_or_no)
  svm_perf <- performance(svm_pred,"tnr","fnr")
  plot(svm_perf, main = "Support vector machines", add = arg2, col = arg3)
  svm_auc <- performance(svm_pred, "auc")
  cat("AUC = ", as.character(attr(svm_auc, "y.values")))
  invisible()
}

test_svm("linear", FALSE, "black") 
test_svm("polynomial", TRUE, "red") 
test_svm("radial", TRUE, "blue") 
test_svm("sigmoid", TRUE, "grey")
```

# méthode baysienne
```{r}
test_nb <- function(arg1, arg2, arg3, arg4){
  nb <- naive_bayes(Giveup_or_no~., data_b.train.scaled, laplace = arg1, usekernel = arg2)
  nb_class <- predict(nb, data_b.test.scaled, type="class") 
  print(table(data_b.test.scaled$Giveup_or_no, nb_class))
  nb_prob <- predict(nb, data_b.test.scaled, type="prob") 
  nb_pred <- prediction(nb_prob[,2], data_b.test.scaled$Giveup_or_no)
  nb_perf <- performance(nb_pred,"tnr","fnr")
  plot(nb_perf, main = "Classifieurs bayesiens", add = arg3, col
       = arg4)
  nb_auc <- performance(nb_pred, "auc")
  cat("AUC = ", as.character(attr(nb_auc, "y.values")))
  invisible()
}

test_nb(0, FALSE, FALSE, "black") 
test_nb(20, FALSE, TRUE, "red") 
test_nb(0, TRUE, TRUE, "blue") 
test_nb(20, TRUE, TRUE, "grey")



```

## réseau de neurone
```{r}
test_nnet <- function(arg1, arg2, arg3, arg4, arg5){
  nn <- nnet(Giveup_or_no~., data_b.train.scaled, size = arg1, decay = arg2, maxit=arg3)
  nn_class <- predict(nn, data_b.test.scaled, type="class")
  print(table(data_b.test.scaled$Giveup_or_no, nn_class))
  nn_prob <- predict(nn, data_b.test.scaled, type="raw")
  nn_pred <- prediction(nn_prob[,1], data_b.test.scaled$Giveup_or_no)
  nn_perf <- performance(nn_pred,"tnr","fnr")
  plot(nn_perf, main = "Reseaux de neurones nnet()", add = arg4, col = arg5) 
  nn_auc <- performance(nn_pred, "auc")
  cat("AUC = ", as.character(attr(nn_auc, "y.values")))
  invisible()
}

test_nnet(15, 0.01, 100, F, "black") 
test_nnet(10, 0.01, 300, T, "red") 
test_nnet(10, 0.01, 100, T, "blue") 
test_nnet(10, 0.01, 300, T, "grey") 
test_nnet(15, 0.001, 100, T, "orange") 
test_nnet(10, 0.001, 300, TRUE, "pink") 
test_nnet(10, 0.001, 100, TRUE, "green") 
test_nnet(10, 0.001, 300, TRUE, "brown")



```


## Arbre de décision (rpart)
```{r echo=TRUE, warning=FALSE,message=FALSE, results="hold"}
test1 <- function(arg1,arg2,arg3,arg4){
  classifieur_1 <- rpart(Giveup_or_no ~ ., studentInscritpions.train.scaled,parms = list(split = arg1), control = rpart.control(minbucket = arg2))
  test_classifieur1 <- predict(classifieur_1,studentInscritpions.test.scaled, type="class")
  test_classifieur1_prob <- predict(classifieur_1,studentInscritpions.test.scaled, type="prob")
  mc_tree1 <- table(studentInscritpions.test.scaled$Giveup_or_no,test_classifieur1)
  roc_pred1 <- prediction(test_classifieur1_prob[,2], studentInscritpions.test.scaled$Giveup_or_no)
  roc_perf1 <- performance(roc_pred1,"tnr","fnr")
  plot(roc_perf1, col = arg3,add=arg4, main = "Courbes AUC du classifieur Rpart")
  auc_tree1 <- performance(roc_pred1, "auc")
  str(auc_tree1)
  cat("AUC = ",as.character(attr(auc_tree1, "y.values")))
  print(mc_tree1)
}

test1("gini",10,"red",FALSE)
test1("gini",5,"blue",TRUE)
test1("information",10,"yellow",TRUE)
test1("information",5,"black",TRUE)

```
```{r}
data_b.train.scaled <- data_b.train.scaled %>% drop_na(moyenne)
```

# RandomForest
```{r}

test2 <- function(arg1,arg2,arg3,arg4){
  class2 <- randomForest(Giveup_or_no~., studentInscritpions.train.scaled,ntree=arg1,mtry=arg2)
  test_class2 <- predict(class2,studentInscritpions.test.scaled, type="class")
  test_class2_prob <- predict(class2,studentInscritpions.test.scaled, type="prob")
  mc_tree2 <- table(studentInscritpions.test.scaled$Giveup_or_no,test_class2)
  roc_pred2 <- prediction(test_class2_prob[,2], studentInscritpions.test.scaled$Giveup_or_no)
  roc_perf2 <- performance(roc_pred2,"tnr","fnr")
  plot(roc_perf2, col = arg3,add=arg4, main = "AUC")
  auc_tree2 <- performance(roc_pred2, "auc")
  cat("AUC =",as.character(attr(auc_tree2, "y.values")))
  print(mc_tree2)
}

test2(500,5,"black",FALSE)
test2(500,3,"red",TRUE)
test2(300,5,"blue",TRUE)
test2(300,3,"grey",TRUE)


```



# Analogie

```{r}
test_knn <- function(arg1, arg2, arg3, arg4){
  knn <- kknn(Giveup_or_no~., studentInscritpions.train.scaled,studentInscritpions.test.scaled, k = arg1, distance = arg2)
  print(table(studentInscritpions.test.scaled$Giveup_or_no, knn$fitted.values))
  knn_pred <- prediction(knn$prob[,2], studentInscritpions.test.scaled$Giveup_or_no)
  knn_perf <- performance(knn_pred,"tpr","fpr")
  plot(knn_perf, main = "Classifeurs par analogie", add = arg3, col = arg4)
  knn_auc <- performance(knn_pred, "auc")
  cat("AUC = ", as.character(attr(knn_auc, "y.values")))
  invisible()
}

test_knn(10, 1, FALSE, "black")
test_knn(10, 2, TRUE, "red")
test_knn(20, 1, TRUE, "blue")
test_knn(20, 2, TRUE, "grey")

```

# SUpport Vector Machines

```{r}
test_svm <- function(arg1, arg2, arg3){
  svm <- svm(Giveup_or_no~., studentInscritpions.train.scaled, probability=TRUE, kernel = arg1)
  svm_class <- predict(svm, studentInscritpions.test.scaled, type="response")
  print(table(studentInscritpions.test.scaled$Giveup_or_no, svm_class))
  svm_prob <- predict(svm, studentInscritpions.test.scaled, probability=TRUE) 
  svm_prob <- attr(svm_prob, "probabilities") 
  svm_pred <- prediction(svm_prob[,1], studentInscritpions.test.scaled$Giveup_or_no)
  svm_perf <- performance(svm_pred,"tnr","fnr")
  plot(svm_perf, main = "Support vector machines", add = arg2, col = arg3)
  svm_auc <- performance(svm_pred, "auc")
  cat("AUC = ", as.character(attr(svm_auc, "y.values")))
  invisible()
}

test_svm("linear", FALSE, "black") 
test_svm("polynomial", TRUE, "red") 
test_svm("radial", TRUE, "blue") 
test_svm("sigmoid", TRUE, "grey")
```

# méthode baysienne
```{r}
test_nb <- function(arg1, arg2, arg3, arg4){
  nb <- naive_bayes(Giveup_or_no~., data_b.train.scaled, laplace = arg1, usekernel = arg2)
  nb_class <- predict(nb,studentInscritpions.test.scaled, type="class") 
  print(table(studentInscritpions.test.scaled$Giveup_or_no, nb_class))
  nb_prob <- predict(nb, studentInscritpions.test.scaled, type="prob") 
  nb_pred <- prediction(nb_prob[,2], studentInscritpions.test.scaled$Giveup_or_no)
  nb_perf <- performance(nb_pred,"tnr","fnr")
  plot(nb_perf, main = "Classifieurs bayesiens", add = arg3, col
       = arg4)
  nb_auc <- performance(nb_pred, "auc")
  cat("AUC = ", as.character(attr(nb_auc, "y.values")))
  invisible()
}

test_nb(0, FALSE, FALSE, "black") 
test_nb(20, FALSE, TRUE, "red") 
test_nb(0, TRUE, TRUE, "blue") 
test_nb(20, TRUE, TRUE, "grey")



```

## réseau de neurone
```{r}
test_nnet <- function(arg1, arg2, arg3, arg4, arg5){
  nn <- nnet(Giveup_or_no~., studentInscritpions.train.scaled, size = arg1, decay = arg2, maxit=arg3)
  nn_class <- predict(nn, studentInscritpions.test.scaled, type="class")
  print(table(studentInscritpions.test.scaled$Giveup_or_no, nn_class))
  nn_prob <- predict(nn, studentInscritpions.test.scaled, type="raw")
  nn_pred <- prediction(nn_prob[,1], studentInscritpions.test.scaled$Giveup_or_no)
  nn_perf <- performance(nn_pred,"tnr","fnr")
  plot(nn_perf, main = "Reseaux de neurones nnet()", add = arg4, col = arg5) 
  nn_auc <- performance(nn_pred, "auc")
  cat("AUC = ", as.character(attr(nn_auc, "y.values")))
  invisible()
}

test_nnet(15, 0.01, 100, F, "black") 
test_nnet(10, 0.01, 300, T, "red") 
test_nnet(10, 0.01, 100, T, "blue") 
test_nnet(10, 0.01, 300, T, "grey") 
test_nnet(15, 0.001, 100, T, "orange") 
test_nnet(10, 0.001, 300, TRUE, "pink") 
test_nnet(10, 0.001, 100, TRUE, "green") 
test_nnet(10, 0.001, 300, TRUE, "brown")



```



























#créateur d'un data frame sans étudiants qui abandonnent
```{r}
datadistinction <- merge(studentinfo,data_studentAssessment)
```
### suppression des abandons
```{r}
datadistinction <- datadistinction %>%
  group_by(id_student,final_result) %>%
  filter(final_result !="Withdrawn")
datadistinction$"Pass_or_no"=ifelse(datadistinction$final_result=="Fail", "Fail", "Pass")
  
```
### Fusion avec data-click
```{r}
datadistinction<-datadistinction[,-12]
datadistinction <- merge(datadistinction,data_click)

```

###Suppression des valeurs manquantes
```{r}

datadistinction <- datadistinction %>% drop_na(moyenne)
datadistinction$Pass_or_no <-as.factor(as.character(datadistinction$Pass_or_no))
```
### Création de jeu de test

```{r}
set.seed(300)
datadistinction <- datadistinction[,-1]
training_size=floor(0.7*nrow(datadistinction))
indices=sample(seq_len(nrow(datadistinction)),size=training_size)
datadistinction.train=datadistinction[indices,]
datadistinction.test=datadistinction[-indices,]

```

### Normalisation des données

```{r}
data_preprocess_value=preProcess(datadistinction.train,method=c("center","scale"))
datadistinction.train.scaled=predict(data_preprocess_value,datadistinction.train)
datadistinction.test.scaled=predict(data_preprocess_value, datadistinction.test)
```
###Suppression des valeurs manquantes
```{r}

datadistinction.test.scaled <-datadistinction.test%>% drop_na(moyenne)
datadistinction.train.scaled<- datadistinction.train %>% drop_na(moyenne) 
```

## Arbre de décision (rpart)
```{r echo=TRUE, warning=FALSE,message=FALSE, results="hold"}
test1 <- function(arg1,arg2,arg3,arg4){
  classifieur_1 <- rpart(Pass_or_no ~ ., datadistinction.train.scaled,parms = list(split = arg1), control = rpart.control(minbucket = arg2))
  test_classifieur1 <- predict(classifieur_1,datadistinction.test.scaled, type="class")
  test_classifieur1_prob <- predict(classifieur_1,datadistinction.test.scaled, type="prob")
  mc_tree1 <- table(datadistinction.test.scaled$Pass_or_no,test_classifieur1)
  roc_pred1 <- prediction(test_classifieur1_prob[,2], datadistinction.test.scaled$Pass_or_no)
  roc_perf1 <- performance(roc_pred1,"tnr","fnr")
  plot(roc_perf1, col = arg3,add=arg4, main = "Courbes AUC du classifieur Rpart")
  auc_tree1 <- performance(roc_pred1, "auc")
  str(auc_tree1)
  cat("AUC = ",as.character(attr(auc_tree1, "y.values")))
  print(mc_tree1)
}

test1("gini",10,"red",FALSE)
test1("gini",5,"blue",TRUE)
test1("information",10,"yellow",TRUE)
test1("information",5,"black",TRUE)

```

# RandomForest
```{r}

test2 <- function(arg1,arg2,arg3,arg4){
  class2 <- randomForest(Pass_or_no~., datadistinction.train.scaled,ntree=arg1,mtry=arg2)
  test_class2 <- predict(class2,datadistinction.test.scaled, type="class")
  test_class2_prob <- predict(class2,datadistinction.test.scaled, type="prob")
  mc_tree2 <- table(datadistinction.test.scaled$Pass_or_no,test_class2)
  roc_pred2 <- prediction(test_class2_prob[,2], datadistinction.test.scaled$Pass_or_no)
  roc_perf2 <- performance(roc_pred2,"tnr","fnr")
  plot(roc_perf2, col = arg3,add=arg4, main = "AUC")
  auc_tree2 <- performance(roc_pred2, "auc")
  cat("AUC =",as.character(attr(auc_tree2, "y.values")))
  print(mc_tree2)
}

test2(500,5,"black",FALSE)
test2(500,3,"red",TRUE)
test2(300,5,"blue",TRUE)
test2(300,3,"grey",TRUE)


```
# Analogie

```{r}
test_knn <- function(arg1, arg2, arg3, arg4){
  knn <- kknn(Pass_or_no~., datadistinction.train.scaled,datadistinction.test.scaled, k = arg1, distance = arg2)
  print(table(datadistinction.test.scaled$Pass_or_no, knn$fitted.values))
  knn_pred <- prediction(knn$prob[,2], datadistinction.test.scaled$Pass_or_no)
  knn_perf <- performance(knn_pred,"tpr","fpr")
  plot(knn_perf, main = "Classifeurs par analogie", add = arg3, col = arg4)
  knn_auc <- performance(knn_pred, "auc")
  cat("AUC = ", as.character(attr(knn_auc, "y.values")))
  invisible()
}

test_knn(10, 1, FALSE, "black")
test_knn(10, 2, TRUE, "red")
test_knn(20, 1, TRUE, "blue")
test_knn(20, 2, TRUE, "grey")

```
# SUpport Vector Machines

```{r}
test_svm <- function(arg1, arg2, arg3){
  svm <- svm(Pass_or_no~., datadistinction.train.scaled, probability=TRUE, kernel = arg1)
  svm_class <- predict(svm, datadistinction.test.scaled, type="response")
  print(table(datadistinction.test.scaled$Pass_or_no, svm_class))
  svm_prob <- predict(svm, datadistinction.test.scaled, probability=TRUE) 
  svm_prob <- attr(svm_prob, "probabilities") 
  svm_pred <- prediction(svm_prob[,1], datadistinction.test.scaled$Pass_or_no)
  svm_perf <- performance(svm_pred,"tnr","fnr")
  plot(svm_perf, main = "Support vector machines", add = arg2, col = arg3)
  svm_auc <- performance(svm_pred, "auc")
  cat("AUC = ", as.character(attr(svm_auc, "y.values")))
  invisible()
}

test_svm("linear", FALSE, "black") 
test_svm("polynomial", TRUE, "red") 
test_svm("radial", TRUE, "blue") 
test_svm("sigmoid", TRUE, "grey")
```
# méthode baysienne
```{r}
test_nb <- function(arg1, arg2, arg3, arg4){
  nb <- naive_bayes(Pass_or_no~., datadistinction.train.scaled, laplace = arg1, usekernel = arg2)
  nb_class <- predict(nb, datadistinction.test.scaled, type="class") 
  print(table(datadistinction.test.scaled$Pass_or_no, nb_class))
  nb_prob <- predict(nb, datadistinction.test.scaled, type="prob") 
  nb_pred <- prediction(nb_prob[,2], datadistinction.test.scaled$Pass_or_no)
  nb_perf <- performance(nb_pred,"tnr","fnr")
  plot(nb_perf, main = "Classifieurs bayesiens", add = arg3, col
       = arg4)
  nb_auc <- performance(nb_pred, "auc")
  cat("AUC = ", as.character(attr(nb_auc, "y.values")))
  invisible()
}

test_nb(0, FALSE, FALSE, "black") 
test_nb(20, FALSE, TRUE, "red") 
test_nb(0, TRUE, TRUE, "blue") 
test_nb(20, TRUE, TRUE, "grey")



```
## réseau de neurone
```{r}
test_nnet <- function(arg1, arg2, arg3, arg4, arg5){
  nn <- nnet(Pass_or_no~., datadistinction.train.scaled, size = arg1, decay = arg2, maxit=arg3)
  nn_class <- predict(nn, datadistinction.test.scaled, type="class")
  print(table(datadistinction.test.scaled$Pass_or_no, nn_class))
  nn_prob <- predict(nn, datadistinction.test.scaled, type="raw")
  nn_pred <- prediction(nn_prob[,1], datadistinction.test.scaled$Pass_or_no)
  nn_perf <- performance(nn_pred,"tnr","fnr")
  plot(nn_perf, main = "Reseaux de neurones nnet()", add = arg4, col = arg5) 
  nn_auc <- performance(nn_pred, "auc")
  cat("AUC = ", as.character(attr(nn_auc, "y.values")))
  invisible()
}

test_nnet(15, 0.01, 100, F, "black") 
test_nnet(10, 0.01, 300, T, "red") 
test_nnet(10, 0.01, 100, T, "blue") 
test_nnet(10, 0.01, 300, T, "grey") 
test_nnet(15, 0.001, 100, T, "orange") 
test_nnet(10, 0.001, 300, TRUE, "pink") 
test_nnet(10, 0.001, 100, TRUE, "green") 
test_nnet(10, 0.001, 300, TRUE, "brown")



```
#créateur d'un data frame sans étudiants qui abandonnent
```{r}
datamention <- merge(studentinfo,data_studentAssessment)
```
### suppression des abandons
```{r}
datamention <- datamention %>%
  group_by(id_student,final_result) %>%
  filter(final_result !="Withdrawn")
datamention <- datamention %>%
  group_by(id_student,final_result) %>%
  filter(final_result !="Fail")
datamention$"Distinction_or_no"=ifelse(datamention$final_result=="Distinction", "Distinction", "Pass")
  
```
### Fusion avec data-click
```{r}
datamention<-datamention[,-12]
datamention <- merge(datamention,data_click)

```

###Suppression des valeurs manquantes
```{r}

datamention <-datamention %>% drop_na(moyenne)
datamention$Distinction_or_no <-as.factor(as.character(datamention$Distinction_or_no))
```
### Création de jeu de test

```{r}
set.seed(300)
datamention <- datamention[,-1]
training_size=floor(0.7*nrow(datamention))
indices=sample(seq_len(nrow(datamention)),size=training_size)
datamention.train=datamention[indices,]
datamention.test=datamention[-indices,]

```

### Normalisation des données

```{r}
data_preprocess_value=preProcess(datamention.train,method=c("center","scale"))
datamention.train.scaled=predict(data_preprocess_value,datamention.train)
datamention.test.scaled=predict(data_preprocess_value, datamention.test)
```
###Suppression des valeurs manquantes
```{r}

datamention.test.scaled <-datamention.test%>% drop_na(moyenne)
datamention.train.scaled<- datamention.train %>% drop_na(moyenne) 
```

## Arbre de décision (rpart)
```{r echo=TRUE, warning=FALSE,message=FALSE, results="hold"}
test1 <- function(arg1,arg2,arg3,arg4){
  classifieur_1 <- rpart(Distinction_or_no ~ ., datamention.train.scaled,parms = list(split = arg1), control = rpart.control(minbucket = arg2))
  test_classifieur1 <- predict(classifieur_1,datamention.test.scaled, type="class")
  test_classifieur1_prob <- predict(classifieur_1,datamention.test.scaled, type="prob")
  mc_tree1 <- table(datamention.test.scaled$Distinction_or_no,test_classifieur1)
  roc_pred1 <- prediction(test_classifieur1_prob[,2], datamention.test.scaled$Distinction_or_no)
  roc_perf1 <- performance(roc_pred1,"tnr","fnr")
  plot(roc_perf1, col = arg3,add=arg4, main = "Courbes AUC du classifieur Rpart")
  auc_tree1 <- performance(roc_pred1, "auc")
  str(auc_tree1)
  cat("AUC = ",as.character(attr(auc_tree1, "y.values")))
  print(mc_tree1)
}

test1("gini",10,"red",FALSE)
test1("gini",5,"blue",TRUE)
test1("information",10,"yellow",TRUE)
test1("information",5,"black",TRUE)

```
# RandomForest
```{r}

test2 <- function(arg1,arg2,arg3,arg4){
  class2 <- randomForest(Distinction_or_no~., datamention.train.scaled,ntree=arg1,mtry=arg2)
  test_class2 <- predict(class2,datamention.test.scaled, type="class")
  test_class2_prob <- predict(class2,datamention.test.scaled, type="prob")
  mc_tree2 <- table(datamention.test.scaled$Distinction_or_no,test_class2)
  roc_pred2 <- prediction(test_class2_prob[,2], datamention.test.scaled$Distinction_or_no)
  roc_perf2 <- performance(roc_pred2,"tnr","fnr")
  plot(roc_perf2, col = arg3,add=arg4, main = "AUC")
  auc_tree2 <- performance(roc_pred2, "auc")
  cat("AUC =",as.character(attr(auc_tree2, "y.values")))
  print(mc_tree2)
}

test2(500,5,"black",FALSE)
test2(500,3,"red",TRUE)
test2(300,5,"blue",TRUE)
test2(300,3,"grey",TRUE)


```
# SUpport Vector Machines

```{r}
test_svm <- function(arg1, arg2, arg3){
  svm <- svm(Distinction_or_no~., datamention.train.scaled, probability=TRUE, kernel = arg1)
  svm_class <- predict(svm, datamention.test.scaled, type="response")
  print(table(datamention.test.scaled$Distinction_or_no, svm_class))
  svm_prob <- predict(svm, datamention.test.scaled, probability=TRUE) 
  svm_prob <- attr(svm_prob, "probabilities") 
  svm_pred <- prediction(svm_prob[,1], datamention.test.scaled$Distinction_or_no)
  svm_perf <- performance(svm_pred,"tnr","fnr")
  plot(svm_perf, main = "Support vector machines", add = arg2, col = arg3)
  svm_auc <- performance(svm_pred, "auc")
  cat("AUC = ", as.character(attr(svm_auc, "y.values")))
  invisible()
}

test_svm("linear", FALSE, "black") 
test_svm("polynomial", TRUE, "red") 
test_svm("radial", TRUE, "blue") 
test_svm("sigmoid", TRUE, "grey")
```
# méthode baysienne
```{r}
test_nb <- function(arg1, arg2, arg3, arg4){
  nb <- naive_bayes(Distinction_or_no~., datamention.train.scaled, laplace = arg1, usekernel = arg2)
  nb_class <- predict(nb, datamention.test.scaled, type="class") 
  print(table(datamention.test.scaled$Distinction_or_no, nb_class))
  nb_prob <- predict(nb, datamention.test.scaled, type="prob") 
  nb_pred <- prediction(nb_prob[,2], datamention.test.scaled$Distinction_or_no)
  nb_perf <- performance(nb_pred,"tnr","fnr")
  plot(nb_perf, main = "Classifieurs bayesiens", add = arg3, col
       = arg4)
  nb_auc <- performance(nb_pred, "auc")
  cat("AUC = ", as.character(attr(nb_auc, "y.values")))
  invisible()
}

test_nb(0, FALSE, FALSE, "black") 
test_nb(20, FALSE, TRUE, "red") 
test_nb(0, TRUE, TRUE, "blue") 
test_nb(20, TRUE, TRUE, "grey")



```
## réseau de neurone
```{r}
test_nnet <- function(arg1, arg2, arg3, arg4, arg5){
  nn <- nnet(Distinction_or_no~., datamention.train.scaled, size = arg1, decay = arg2, maxit=arg3)
  nn_class <- predict(nn, datamention.test.scaled, type="class")
  print(table(datamention.test.scaled$Distinction_or_no, nn_class))
  nn_prob <- predict(nn, datamention.test.scaled, type="raw")
  nn_pred <- prediction(nn_prob[,1], datamention.test.scaled$Distinction_or_no)
  nn_perf <- performance(nn_pred,"tnr","fnr")
  plot(nn_perf, main = "Reseaux de neurones nnet()", add = arg4, col = arg5) 
  nn_auc <- performance(nn_pred, "auc")
  cat("AUC = ", as.character(attr(nn_auc, "y.values")))
  invisible()
}

test_nnet(15, 0.01, 100, F, "black") 
test_nnet(10, 0.01, 300, T, "red") 
test_nnet(10, 0.01, 100, T, "blue") 
test_nnet(10, 0.01, 300, T, "grey") 
test_nnet(15, 0.001, 100, T, "orange") 
test_nnet(10, 0.001, 300, TRUE, "pink") 
test_nnet(10, 0.001, 100, TRUE, "green") 
test_nnet(10, 0.001, 300, TRUE, "brown")



```
```{r}
datadistinction$Pass_or_no<-factor(as.factor(datadistinction$Pass_or_no),ordered = FALSE)
dmatrix <- daisy(datadistinction)
summary(dmatrix)
```

# Clustering par fonction dbscan()

```{r}
dbs <- dbscan(dmatrix, eps=0.125, minPts = 3)
```

# Répartition des classes Produit=Oui/Non par cluster

```{r}
table(dbs$cluster,  datadistinction$Pass_or_no)
```

# Histogramme des effectifs des clusters avec la classe en couleur

```{r}
qplot(as.factor(dbs$cluster), data=datadistinction, fill=Pass_or_no)
```
# K-means pour K = 4

```{r}
km4 <- kmeans(dmatrix, 4)
```

# Répartition des classes Produit=Oui/Non par cluster

```{r}
table(km4$cluster, datadistinction$Pass_or_no)
```

# Histogramme des effectifs des clusters avec la classe en couleur

```{r}
qplot(km4$cluster, data=datadistinction, fill=Pass_or_no)
```
# Nuages de points avec classe en couleur

```{r}
qplot(gender, as.factor(km4$cluster), data=datadistinction, color=Pass_or_no) + geom_jitter(width = 0.3, height = 0.3)
qplot(imd_band, as.factor(km4$cluster), data=datadistinction, color=Pass_or_no) + geom_jitter(height = 0.3)
qplot(age_band, as.factor(km4$cluster), data=datadistinction, color=Pass_or_no) + geom_jitter(width = 0.3, height = 0.3)
```



# Ajout de la colonne du numéro de cluster

```{r}
Giveup_or_no_km4 <- data.frame(datadistinction, km4$cluster)
View(Giveup_or_no_km4)
```

# Boule for() faisant varier K

```{r}
for (k in 4:10 ){
  km <- kmeans(dmatrix, k)
  print(table(km$cluster,datadistinction$Pass_or_no))
  print(qplot(km$cluster, data=datadistinction, fill=Pass_or_no))
}  
### K=6 ou 5
```
# Nuages de points avec classe en couleur

```{r}
qplot(Course_content, as.factor(km$cluster), data=datadistinction, color=Pass_or_no) + geom_jitter(width = 0.3, height = 0.3)
qplot(Evaluation, as.factor(km$cluster), data=datadistinction, color=Pass_or_no) + geom_jitter(height = 0.3)
qplot(Collaboration, as.factor(km$cluster), data=datadistinction, color=Pass_or_no) + geom_jitter(width = 0.3, height = 0.3)
qplot(Course_structure, as.factor(km$cluster), data=datadistinction, color=Pass_or_no) + geom_jitter(width = 0.3, height = 0.3)
```


### création de la matrice de distace

```{r}
datamention$Distinction_or_no<-factor(as.factor(datamention$Distinction_or_no),ordered = FALSE)
dmatrix <- daisy(datamention)
summary(dmatrix)
```
# Clustering par fonction dbscan()

```{r}
dbs <- dbscan(dmatrix, eps=0.125, minPts = 3)
```

# Répartition des classes Produit=Oui/Non par cluster

```{r}
table(dbs$cluster,  datamention$Distinction_or_no)
```

# Histogramme des effectifs des clusters avec la classe en couleur

```{r}
qplot(as.factor(dbs$cluster), data=datamention, fill=Distinction_or_no)
```
# K-means pour K = 4

```{r}
km4 <- kmeans(dmatrix, 4)
```

# Répartition des classes Produit=Oui/Non par cluster

```{r}
table(km4$cluster, datamention$Distinction_or_no)
```

# Histogramme des effectifs des clusters avec la classe en couleur

```{r}
qplot(km4$cluster, data=datamention, fill=Distinction_or_no)
```
# Nuages de points avec classe en couleur

```{r}
qplot(gender, as.factor(km4$cluster), data=datamention, color=Distinction_or_no) + geom_jitter(width = 0.3, height = 0.3)
qplot(imd_band, as.factor(km4$cluster), data=datamention, color=Distinction_or_no) + geom_jitter(height = 0.3)
qplot(age_band, as.factor(km4$cluster), data=datamention, color=Distinction_or_no) + geom_jitter(width = 0.3, height = 0.3)
```



# Ajout de la colonne du numéro de cluster

```{r}
Giveup_or_no_km4 <- data.frame(datamention, km4$cluster)
View(Giveup_or_no_km4)
```

# Boule for() faisant varier K

```{r}
for (k in 4:10 ){
  km <- kmeans(dmatrix, k)
  print(table(km$cluster,datamention$Distinction_or_no))
  print(qplot(km$cluster, data=datamention, fill=Distinction_or_no))
}  
### K=8 est plutôt interessante

```
