## simple code
system.time({
my.sims <- replicate(10000, {
    mydiff <- rnorm(100, 2, 7)
    t.test(mydiff)$p.value
})
})

	
## strip it down
system.time({
my.sims <- replicate(10000, {
    mydiff <- rnorm(100, 2, 7)
    t <- mean(mydiff)/sd(mydiff)*sqrt(100)
	2*pt(-abs(t), df=99)
})
})


## only call qt() once
crit.t <- qt(0.975, df=99)

system.time({
my.sims <- replicate(10000, {
    mydiff <- rnorm(100, 2, 7)
    t <- mean(mydiff)/sd(mydiff)*sqrt(100)
	abs(t)>crit.t
})
})


## Vectorise
system.time({
mydiffs <- matrix(rnorm(100*100000,2,7),nrow=100)
m       <- colMeans(mydiffs)
s       <- sqrt(colMeans(mydiffs*mydiffs)-m*m)
m*sqrt(100)/s > crit.t
})


## Strip out square root
crit2<-crit.t^2/100
system.time({
mydiffs <- matrix(rnorm(100*100000,2,7),nrow=100)
m2      <- colMeans(mydiffs)^2
s2      <- colMeans(mydiffs*mydiffs)-m2
m2/s2 > crit2
})
