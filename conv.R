conv1<-function(x,y){
m <- length(x)
n <- length(y)
z <- numeric(m+n-1)
for(j in 1:m){
	for (k in 1:n){
		z[j+k-1] <- z[j+k-1] + x[j]*y[k]
		}
	}
	z
}	

conv2<-function(x,y){
m <- length(x)
n <- length(y)
z <- numeric(m+n-1)
for(j in 1:m){
		z[j+(1:n)-1] <- z[j+(1:n)-1] + x[j]*y[(1:n)]
	}
	z
}

conv3<-function(x,y){
  m <- length(x)
  n <- length(y)

  xy<-outer(x,y,"*")
  xyshift<-matrix(rbind(xy,matrix(0,n,n))[1:((m+n-1)*n)],ncol=n)
  rowSums(xyshift)
}