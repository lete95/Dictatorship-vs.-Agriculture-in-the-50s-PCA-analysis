PCAPolSerra<-function(X,weights) {
    
    #FUNCTION REGARDING HOMEWORK 2 OF MVA
    
    #X must be a data set of continuous numeric variables
    #weights must be a vector, with length equal to the observations (nrows) of X
    
    # (A,B,C) PROCEDURES UNTILL GETTING THE CORRELATION MATRIX AND ITS EIGEN(VECTOR/VALUES) (A,B,C,D)
    
    XCentered<-matrix(0,nrow(X),ncol(X)); XStandardized<-matrix(0,nrow(X),ncol(X)) #declare centered and standardized matrix
    
    N<-diag(weights/sum(weights),nrow(X),nrow(X)); XN<-N%*%X; G<-apply(XN, 2, sum) #get N, the weighted matrix, and the centroid G
    
    XCentered<-X-matrix(rep(t(G), nrow(X)), ncol=ncol(X), byrow=T);#get the centered matrix through the centroid G
    
    S<-t(XCentered)%*%N%*%XCentered#get the covariance matrix
    
    sds<-c(); for (i in 1:ncol(X)) {sds<-c(sds,1/sd(X[,i]))}#get vector of standard deviations
    
    SDS<-diag(sds,ncol(X),ncol(X)); XStandardized<-XCentered%*%SDS;colnames(XStandardized)<-colnames(X)#get the standardized matrix
    
    R<-t(XStandardized)%*%N%*%XStandardized;eigenva<-eigen(R)$values;eigenve<-eigen(R)$vectors#get the correlation matrix and eigen()
    
    #(E) DO THE SCREEPLOT OF EIGENVALUES AND DEFINE SIGNIFICANT DIMENSIONS. RETAINED INFO? (E)
    
    cumulative=matrix(0,length(eigenva),1);cumulative[1]<-100*eigenva[1]/sum(eigenva)#define cum. contributions of eigenvalues
    
    for(j in (2:length(eigenva))){cumulative[j]<-cumulative[j-1]+(100*eigenva[j]/sum(eigenva))}#get the cumulative contrib.
    
    plot(seq(1:length(eigenva)),eigenva,col="black",type="b",pch=4,cex=1,xlab="Number of eigenvalues",ylab="Eigenvalues")#plot
    
    text(x=1:length(eigenva), y=eigenva+0.2, labels=as.character(round(cumulative)),cex=1,pos=1,col="black")#cumulative contrib.
    
    text(7,mean(eigenva)+0.3,labels="Mean of eigenvalues",cex=1,col="black")
    
    grid();abline(h=mean(eigenva),col="red",lwd=2)#introduce meanline and grid
    
    #(F,H) COMPUTE AND PLOT THE INDIVIDUALS ON THE FIRST FACTORIAL PLANE OF R^p (F,H)
    
    Psi<-XStandardized%*%eigenve; #compute the projections
    
    Demos=as.factor(CountryStatus)#get the demo variable to act as color
    
    plot(Psi[,1],Psi[,2],xlab="PC1",ylab="PC2",pch=19,col=Demos,ylim=range(-3.5:2.5),xlim=range(-4.5:3))#plot the projections
    
    text(Psi[,1],Psi[,2]-0.1, labels=as.character(data[,1]),cex=0.5,pos=3,col="black")#Give names to countries
    
    #(G,I) COMPUTE AND PLOT THE VARIABLES IN THE FIRST FACTORIAL PLANE OF R^n (G,I)
    
    variablesProj<-sqrt(N)%*%XStandardized%*%t(XStandardized)%*%sqrt(N) #projection of the variables in the firsp f.plance
    
    variablesEigenve<-eigen(variablesProj)$vectors #variablesEigenve (R^n)
    
    arrowVariables<-(t(XStandardized))%*%sqrt(N)%*%variablesEigenve#compute the arrow varialbes
    
    arrows(0,0,arrowVariables[,1]*2.5,-arrowVariables[,2]*2.5,col="black",length=0.1)#plot the arrows
    
    text(arrowVariables[,1]*2.5,-arrowVariables[,2]*2.5+0.3, labels=colnames(X),cex=0.9,col="red")#add names of variables
    
    abline(h=0,col="black",lwd=2);abline(v=0,col="black",lwd=2);grid()#introduce axis and grid
}