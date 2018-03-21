#SOME LIBRARIES
rm(list=ls(all=TRUE))
require(plotrix)
require(VIM)
require(MASS) 
require(graphics)
require(chemometrics)
require(gclus)
require(FactoMineR)
require(MASS)
require(cluster)
#GET THE IMPUTED CONTINUOUS DATA IN X
russet<-read.table("russet.txt", header=TRUE, sep="\t") #read the data set
data<-kNN(russet,variable=c("Rent","ecks"),k=6) #realize the imputation
CountryStatus<-data[,10] #get the contry status of the demo variable
X<-data[,2:9]#get the continuous data
X<-data.matrix(X, rownames.force = NA) #put X to matrix
data
#DEFINE THE WEIGHTS
weights<-c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
#CALL THE FUNCTION
source("PCAPolSerra.R")
PCAPolSerra(X,weights)

