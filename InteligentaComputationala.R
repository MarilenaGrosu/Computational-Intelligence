library(moments)
library(mnormt)
library(psych)
library(sp)
library(raster)
library(corrplot)
install.packages("mnormt")

a<-read.table(file="BasicMaterials-IC.csv",header=TRUE,sep=',')
a
fix(a)
attach(a)
View(a)
str(a)
head(a)
tail(a)

#I.Statistici descriptive
summary(a)
hist(a$Price, col="coral", main="PRICE")
plot(density(a$Price))
table(a$Industry)
barplot(table(a$Industry))
pie(table(a$Industry))

industries<-c("Petroleum","Oil & Gas","Mining","Metallurgy","Manufacturing","Chemicals","Steel")
f<-c(20,9,5,2,2,1,1)
pie(f,labels=industries,col=rainbow(7),main="PIE")

df<-data.frame(industries,f)
df
df$proportie<-df$f
df$proportie
procente<-100*(f/sum(f))
procente
barplot(f,names.arg=industries,col=rainbow(7),xlab="Industries",ylab="Frequencies",cex.names=0.8,main="Distributie")

cov<-cov(a[,2:9])
cov
cor<-cor(a[,2:9])
cor
sd<-apply(a, 2, sd)
sd


skewness(a[,2:9])
kurtosis(a[,2:9])
#Verificarea existentei outlierilor
par(mfrow=c(4,4))
boxplot(Price,main="Price",col="red")
boxplot(Change,main="Change",col="green")
boxplot(Price.Sales,main="Price/Sales",col="blue")
boxplot(Price.Book,main="Price/Book",col="blue4")
hist(Price,main="Price",col="red")
hist(Change,main="Change",col="green")
hist(Price.Sales,main="Price/Sales",col="blue")
hist(Price.Book,main="Price/Book",col="blue4")

par(mfrow=c(4,4))
boxplot(Revenue,main="Revenue",col="pink")
boxplot(MkCap,main="MkCap",col="brown")
boxplot(ROA,main="ROA",col="cyan")
boxplot(ROE,main="ROE",col="chocolate")
hist(Revenue,main="Revenue",col="pink")
hist(MkCap,main="MkCap",col="brown")
hist(ROA,main="ROA",col="cyan")
hist(ROE,main="ROE",col="chocolate")

model<-lm(Revenue~Price)
model
plot(Revenue, Price, col="coral2")
abline(lm(Price~Revenue), col="purple")

# II. ANALIZA COMPONENTELOR PRINCIPALE
# Standardizarea datelor: mean=0, sd=1
acp<-a[,2:9]
acp
date_std=scale(acp,scale=TRUE)
date_std
head(date_std)

#Componente principale
pc=princomp(date_std,cor=TRUE)
sd=pc$sd 
valpr=sd*sd
procentA= valpr*100/8
procentC=cumsum(procentA) 
v=zapsmall(data.frame(valpr,procentA,procentC))
v
summary(pc)

scree_plot=prcomp(date_std)
plot(scree_plot,type="l",main="Scree plot")
plot(pc,type="barplot")
biplot(pc)

#Vectori proprii si valori proprii
ev<-eigen(cov(date_std))
ev

loadings(pc)
#vectpr=zapsmall(pc$loadings)
#vectpr

scoruriprinc=zapsmall(pc$scores)
scoruriprinc

#Matricea corelatiilor
c=zapsmall(pc$scores)
corFact=zapsmall(cor(date_std,c))
corFact
corrplot(cor(date_std,c),method="circle")


#############################################
#Algoritmi de clusterizare
View(a)
require (cluster)
require(factoextra)

#Creare dateframe cu variabile numerice
b <- a[,2:9]
b
rownames(b, do.NULL = TRUE, prefix = "row")
rownames(b)<- a$Symbol #etichetarea randurilor cu numele tarilor
View(b)
#standardizarea observatiilor in vederea aplicarii analizei cluster
standardizare <- function(x) {(x - mean(x))/sd(x)} #standardizarea observatiilor
datestd <-apply(b,2,standardizare)
datestd

#calcularea distantelor dintre obiecte
distance <- dist(as.matrix(datestd))

# analiza cluster metoda Ward
hc.w <- hclust(distance, method="ward.D2")
plot(hc.w, labels = b$Symbol, hang=-1, col="coral2")
rect.hclust(hc.w, k = 3, border = 2:5)
member.w <- cutree(hc.w,k=3)
member.w

install.packages("dbscan")
library(dbscan)
install.packages("fpc")
library(fpc)

#K-MEANS
rezultat.kmeans<-kmeans(datestd,3)
rezultat.kmeans
table(a$Industry, rezultat.kmeans$cluster)

kNNdistplot(datestd,k=3) #kNN-k nearest neighbours
abline(h=1.8,col="red")
db<-dbscan(datestd,eps=1.8,MinPts=3)
db
fviz_cluster(db,datestd,ellipse=TRUE,geom="points")
table(a$Industry,db$cluster)
plotcluster(datestd,db$cluster)

db_vector<-db[['cluster']]
db_vector
dist<-dist(datestd)
dist
silueta<-silhouette(db_vector,dist)
silueta
fviz_silhouette(silueta)


#Fuzzy C-MEANS
library(factoextra)
library(cluster)
library(dbscan)
library(e1071)
rezultat<-cmeans(datestd, 3, 100, 2, method="cmeans")
rezultat
# 3=nr de clustere, 100= nr de iteratii, 2=parametrii de fuzzificare
rezultat$centers
rezultat$membership
rezultat$cluster
#Reprezentarea grafica a punctelor
plot(datestd, col=rezultat$cluster)
points(rezultat$centers[,c(1,2)], col=1:3, pch="*", cex=3)


##########################################
#Arbori de decizie
df1<-data.frame(datestd)
df1
df2<-data.frame(a[,10])
df2
df<-cbind(df1, df2)
df
colnames(df)[colnames(df)=="a...10."] <- "Industry"
df

ind<-sample(2,nrow(df),replace=TRUE,prob=c(0.7,0.3)) //Extragerea a 2 esantioane din setul de date
ind #Extragere cu revenire - Apartenenta la cele 2 esantioane
traindata<-df[ind==1,]  
traindata
testdata<-df[ind==2,]
testdata
formula<-Industry~.
formula
ctree<-ctree(formula, data=traindata)
ctree
table(predict(ctree),traindata$Industry)
plot(ctree)

print(ictree)
plot(ctree, type="simple")  

predictie<-predict(ctree,traindata,type="response")
predictie #predictie etichete
confuzie<-table(traindata$Industry,predictie)
confuzie #arata ce s-a previzionat corect
classAgreement(confuzie)
#diag=0.97->97% de date corect etichetate
#kappa=0.95->95% acord f bun intre etichetele reale si cele previzionate
mean(predictie !=traindata$Industry)

predictie1<-predict(ctree,testdata,type="response")
predictie1
confuzie1<-table(testdata$Industry,predictie1)
confuzie1
classAgreement(confuzie1)
mean(predictie1 !=testdata$Industry)

library(tree)
library(ISLR)
#Pruning the tree
set.seed(3)
cv.tree<-cv.tree(ctree,FUN=prune.misclass)
cv.tree
names(cv.tree) #size-marime arbore  si dev-indicator pt puritatea nodului
plot(cv.tree$size,cv.tree$dev,type="b")



install.packages("pROC")
library(pROC)
install.packages("rpart")
library(rpart)

#Curba ROC
df1<-data.frame(datestd)
df1
df2<-data.frame(a[,10])
df2
df<-cbind(df1, df2)
df
colnames(df)[colnames(df)=="a...10."] <- "Industry"
df
fix(df)
attach(df)
VenituriMari<-ifelse(Revenue>=0.1,"Yes","No")
VenituriMari
df=data.frame(df, VenituriMari)
df=df[ ,-5]
df
names(df)
set.seed(123)
antrenare<-sample(1:nrow(df),nrow(df)/2)
antrenare
testare=-antrenare
setantrenare<-df[antrenare,]
setantrenare
settestare<-df[testare,]
settestare

arbore<-rpart(as.factor(VenituriMari)~.,data=setantrenare,method="class")
arbore
plot(arbore,uniform=TRUE)#uniform -spatiere verticala a nodurilor
text(arbore,use.n=TRUE,all=TRUE,cex=0.8)
print(arbore)
#loss-obiecte incorect clasificate
#yval-clasa majoritara a acelui nod
#yprob-vectorul de probabilitati
#root 200 79 no (0.6050000 0.3950000)  
predictie<-predict(arbore,settestare,type="class")
predictie
matriceconfuzie<-table(settestare$VenituriMari,predictie)
matriceconfuzie
#94 si 59 sunt obs corect previzionate
(94+59)/(94+21+26+59)
#0.76 76% din date sunt corect previzionate
prob<-predict(arbore,settestare,type="prob")
head(prob)
curbaROC<-roc(settestare$VenituriMari,prob[,"Yes"])
curbaROC
plot(curbaROC)
auc(curbaROC) #area under curve
printcp(arbore) #complex parameter-cant cu care divizarea nodului imbunatateste eroarea relativa de clasificare
#nsplit=nr de noduri terminale
#rel error=eroare relativa
#x error=eroare de validare incrucisata 
#xstd=abaterea standard
#criteriul de alegere: xerror sa fie minim 
plotcp(arbore,col="red")
arborecuratat<-prune(arbore,cp=arbore$cptable[which.min(arbore$cptable[ ,"xerror"]),"CP"])
arborecuratat
plot(arborecuratat,uniform=TRUE)
text(arborecuratat,use.n=TRUE,all=TRUE,cex=0.8)
predictie1<-predict(arborecuratat,settestare,type="class")
predictie1
matriceconfuzie1<-table(settestare$VenituriMari,predictie1)
matriceconfuzie1



#Arbori de regresie
install.packages("tree")
library(tree)
install.packages("MASS")
library(MASS)
set.seed(234)
antrenare<-sample(1:nrow(df),nrow(df)/2)
antrenare
arbore<-tree(ROE~.,df,subset=antrenare)
arbore
plot(arbore)
text(arbore,pretty=0)
cv.tree<-cv.tree(arbore, FUN=prune.misclass)
cv.tree


#SVM
install.packages('e1071',dependencies=TRUE)
install.packages("dplyr")
library(dplyr)
library(e1071)
library(MASS)
df
df<-df %>% select(7,8,9)
df
index <- 1:nrow(df)    
index

testindex<- sample(index, trunc(length(index)/3))   
testindex
settestare<- df[testindex,]  
settestare
setantrenare<- df[-testindex,]  
setantrenare
model<-svm(Industry~.,data = setantrenare)
model 
plot(model,df)
prediction <- predict(model, settestare[,-3])
prediction
tab <- table(pred = prediction, true = settestare[,3])
tab
classAgreement(tab)
datenoi<-data.frame(ROA=c(-0.235665,0.120007),ROE=c(0.735665,-0.140607))
datenoi
predict(model,datenoi)
predict(model,datenoi,prob=TRUE)
predict


######################
#Retele neuronale
install.packages("neuralnet")
library(neuralnet)
setantrenare<- df[sample(1:40, 20),]
setantrenare
setantrenare$petroleum <- c(setantrenare$Industry == "petroleum")
setantrenare$oilgas <- c(setantrenare$Industry == "oil&gas")
setantrenare$mining <- c(setantrenare$Industry == "mining")
setantrenare$manufacturing <- c(setantrenare$Industry == "manufacturing")
setantrenare$metallurgy <- c(setantrenare$Industry == "metallurgy")
setantrenare$chemicals <- c(setantrenare$Industry == "chemicals")
setantrenare$steel <- c(setantrenare$Industry == "steel")
setantrenare
settestare$Industry <- NULL
#Se antrenează reţeaua neuronală care conţine 3 noduri în stratul ascuns. 
retea<-neuralnet(petroleum+oilgas+mining+manufacturing+metallurgy+chemicals+steel~Price+Change+Price.Sales+Price.Book+Revenue+MkCap+ROA+ROE, setantrenare, hidden=7, lifesign="full")
retea
plot(retea, rep="best", intercept=FALSE)

#Incarcare date analiza

a<-read.table(file="BasicMaterials-IC.csv",header=TRUE,sep=',')
a
b <- a[,2:9]
b
rownames(b, do.NULL = TRUE, prefix = "row")
rownames(b)<- a$Symbol #etichetarea randurilor cu numele tarilor
View(b)
#standardizarea observatiilor in vederea aplicarii analizei cluster
standardizare <- function(x) {(x - mean(x))/sd(x)} #standardizarea observatiilor
datestd <-apply(b,2,standardizare)
datestd
df1<-data.frame(datestd)
df1
df2<-data.frame(a[,10])
df2
df<-cbind(df1, df2)
df
colnames(df)[colnames(df)=="a...10."] <- "Industry"
df

################################
#Regresia logistica multinomiala
install.packages("MASS")
library(MASS)
install.packages("nnet")
library(nnet)
df$Industry.f<-factor(df$Industry)
df$Industry.f
df$ref<-relevel(df$Industry.f, ref="petroleum")
df$ref
model<-multinom(ref~Revenue+ROA+ROE, data=df, traice=FALSE)
model
summary(model)
predict(model, df)
predict(model, df, type="prob")
predict(model, df[c(3,7,17),], type="prob")
confuzie<-table(df$Industry[1:40], predict(model, df[1:40, ]))
confuzie
mean(df$Industry[1:40]==predict(model, df[1:40,]))

######################
#Retele neuronale
#install.packages("neuralnet")
library(neuralnet)

setantrenare<- df[sample(1:40, 20),]
setantrenare
setantrenare$petroleum <- c(setantrenare$Industry == "petroleum")
setantrenare$oilgas <- c(setantrenare$Industry == "oil&gas")
setantrenare$mining <- c(setantrenare$Industry == "mining")
setantrenare$manufacturing <- c(setantrenare$Industry == "manufacturing")
setantrenare$metallurgy <- c(setantrenare$Industry == "metallurgy")
setantrenare$chemicals <- c(setantrenare$Industry == "chemicals")
setantrenare$steel <- c(setantrenare$Industry == "steel")
setantrenare
setantrenare$Industry <- NULL
#Se antrenează reţeaua neuronală care conţine 3 noduri în stratul ascuns. 
retea<-neuralnet(petroleum+oilgas+mining+manufacturing+metallurgy+chemicals+steel~Price+Change+Price.Sales+Price.Book+Revenue+MkCap+ROA+ROE, setantrenare, hidden=7, lifesign="full")
retea
plot(retea, rep="best", intercept=FALSE)
predictie<-compute(retea,df[-8])$net.result
predictie