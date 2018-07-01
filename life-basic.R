## basic life
get.nbhs  =  function(alive, nrows=dim(alive)[1]-2, ncols=dim(alive)[2]-2){
   nbhs  =  matrix(0, nrows+2, ncols+2)
      for(i in 2:(nrows+1)){
      for(j in 2:(ncols+1)){
         nbhs[i,j]  =  alive[i-1,j-1] + 
                       alive[i-1,j  ] + 
                       alive[i-1,j+1] + 
                       alive[i  ,j-1] + 
                       alive[i  ,j+1] + 
                       alive[i+1,j-1] + 
                       alive[i+1,j  ] + 
                       alive[i+1,j+1] # adding over the 8 neighbors
         } # close j loop
      }    # close i loop
   nbhs}

do.basic.plot  =  function(nrows, ncols){
   plot(0,0, type="n", xlab="", ylab="", axes=F,  
     xlim=c(0.5,nrows+0.5), ylim=c(0.5,ncols+0.5), asp=1)
  invisible()
}

update.plot  =  function(alive, nrows=dim(alive)[1]-2, ncols=dim(alive)[2]-2){
   for(i in 1:nrows){
      for(j in 1:ncols){
         rect(j-0.5,i-0.5,j+0.5,i+0.5, 
               col=alive[i+1,j+1]*6 + 1, border="blue")
      }   # NB cols here are 1 or 7 - black or yellow
   }
  invisible()
}



update.alive  =  function(alive, nbhs, nrows=dim(alive)[1]-2, ncols=dim(alive)[2]-2){
   alive.new  =  matrix(0, nrows+2, ncols+2) # note full of zeros
   for(i in 2:(nrows+1)){
     for(j in 2:(ncols+1)){
       if(alive[i,j]==1 & nbhs[i,j]<2     ){ alive.new[i,j]  =  0 }
       if(alive[i,j]==1 & nbhs[i,j]%in%2:3){ alive.new[i,j]  =  1 }
       if(alive[i,j]==1 & nbhs[i,j]>3     ){ alive.new[i,j]  =  0 }
       if(alive[i,j]==0 & nbhs[i,j]==3    ){ alive.new[i,j]  =  1 }
     }
   }
   alive  =  alive.new
   alive
}

nrows  =  40
ncols  =  40
n.updates  =  100 

set.seed(4)
alive  =  matrix(rbinom((nrows+2)*(ncols*2),1, 0.3), nrows+2, ncols+2) # "+2" is adding the gray border

do.basic.plot(nrows, ncols)
update.plot(alive)
for(k in 1:n.updates){
  nbhs  =  get.nbhs(alive)
  alive  =  update.alive(alive, nbhs)
  update.plot(alive)
}