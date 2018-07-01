system.time({
 touch<-function(z) {force(z); NULL}
 x<-integer(1e5)
 y<-integer(1e5)
 for(i in 1:1e5){ 
 touch(x)
 x[i]<-i^2
}
})

#also try profvis, tracemem, Rprofmem
# with compiler:::enableJIT(0)
