
mylm<-function(X,y){
	n<-nrow(X)
	p<-ncol(X)
	model<-lm.fit(X,y)
  	R <- chol2inv(qr.R(model$qr))
    se <- sqrt(diag(R) * sum(model$residuals^2)/(n-p))
    list(beta=model$coefficients, se=se)
}

one.snp = function(snp, perm, df){
	X<-cbind(1,df$ismale,snp[perm])
    y<-df$sbp
    model<-mylm(X,y)
    model$beta[3]/model$se[3]
}

one.gene<-function(snps, perm, df){
    p<-ncol(snps)
    zs<-sapply(1:p, function(i) one.snp(snps[,i], perm, df))
    max(abs(zs))
}

one.perm<-function(snps, df){
    n<-nrow(snps)
    one.gene(snps,perm=sample(1:n), df)
}

genedata<-na.omit(read.csv("https://raw.githubusercontent.com/tslumley/AKLfaster/master/examplebp.csv"))

snps<-genedata[,4:14]
for(i in 1:ncol(snps)) 
   snps[[i]]<-as.numeric(snps[[i]])-1  #0/1/2 coding
phenotype<-genedata[,2:3]
phenotype$ismale <-as.integer(phenotype$sex=="MALE")

many.perm <- replicate(1000, one.perm(snps,  phenotype))

real.max.Z <- one.gene(snps,1:nrow(snps),phenotype)
mean(many.perm < real.max.Z)