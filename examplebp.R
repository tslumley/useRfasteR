one.snp = function(snp, perm, df){
    coef(summary(lm(sbp~as.numeric(snp)[perm]+sex,data=df)))[2,3]
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

## start with no missing data
genedata<-na.omit(read.csv("https://raw.githubusercontent.com/tslumley/AKLfaster/master/examplebp.csv"))

snps<-genedata[,4:14]
phenotype<-genedata[,2:3]

many.perm <- replicate(1000, one.perm(snps,  phenotype))

real.max.Z <- one.gene(snps,1:nrow(snps),phenotype)
mean(many.perm < real.max.Z)