load("sequence.rda")
# devtools::install_github("tslumley/bigQF")
library(bigQF)

system.time(s1<-svd(sequence,nu=10,nv=10))
system.time(s2<-ssvd(sequence, n=10,U=TRUE,V=TRUE))
spseq<-sparse.matrixfree(Matrix(sequence))
system.time(s3<-ssvd(spseq, n=10,U=TRUE,V=TRUE))

plot(s1$u[,1:2],pch=19)
points(s2$u[,1:2],col="red")

plot(s1$u[,1:2],pch=19)
points(s3$v[,1],-s3$v[,2],col="green")
