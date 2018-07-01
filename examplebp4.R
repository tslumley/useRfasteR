

## If we didn't adjust for sex we could permute y instead

all.perm<-function(snps, df,LOTS=1000){
	ystar<-matrix(nrow=nrow(snps),ncol=LOTS)
	for(i in 1:LOTS) 
	   ystar[,i]<-sample(df$sbp)
	p<-ncol(snps)
	allZ<-matrix(nrow=LOTS,ncol=p)
	for(i in 1:p){
		models<-coef(summary(lm(ystar~snps[,i],data=df)))
		allZ[,i]<-sapply(models, function(model) model[2,3])
	}
	apply(abs(allZ),1,max)
}

genedata<-na.omit(read.csv("https://raw.githubusercontent.com/tslumley/AKLfaster/master/examplebp.csv"))

snps<-genedata[,4:14]
for(i in 1:ncol(snps)) 
   snps[[i]]<-as.numeric(snps[[i]])-1  #0/1/2 coding
phenotype<-genedata[,2:3]
phenotype$ismale <-as.integer(phenotype$sex=="MALE")


many.perm <- all.perm(snps,phenotype,LOTS=1000)


## Or if adjust SBP for sex first

all.permadj<-function(snps, df,LOTS=1000){
	y<-resid(lm(sbp~ismale,data=df))
	ystar<-matrix(nrow=nrow(snps),ncol=LOTS)
	for(i in 1:LOTS) 
	   ystar[,i]<-sample(y)
	p<-ncol(snps)
	allZ<-matrix(nrow=LOTS,ncol=p)
	for(i in 1:p){
		models<-coef(summary(lm(ystar~snps[,i],data=df)))
		allZ[,i]<-sapply(models, function(model) model[2,3])
	}
	apply(abs(allZ),1,max)
}
