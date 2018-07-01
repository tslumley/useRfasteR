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

###
# faster: vectorise the plotting
###

update.plot2  =  function(alive, nrows=dim(alive)[1]-2, ncols=dim(alive)[2]-2){
	i = rep(1:nrows,ncols)
	j = rep(1:ncols,each=nrows)
   rect(j-0.5,i-0.5,j+0.5,i+0.5, 
               col=alive[2:(nrows+1),2:(ncols+1)]*6 + 1, border="blue")
      
  invisible()
}



set.seed(4)
alive  =  matrix(rbinom((nrows+2)*(ncols*2),1, 0.3), nrows+2, ncols+2) # "+2" is adding the gray border

do.basic.plot(nrows, ncols)
update.plot2(alive)
for(k in 1:n.updates){
  nbhs  =  get.nbhs(alive)
  alive  =  update.alive(alive, nbhs)
  update.plot2(alive)
}

## bigger example

nrows  =  100
ncols  =  100
n.updates  =  100 
set.seed(4)
alive  =  matrix(rbinom((nrows+2)*(ncols*2),1, 0.3), nrows+2, ncols+2) # "+2" is adding the gray border

do.basic.plot(nrows, ncols)
update.plot2(alive)
for(k in 1:n.updates){
  nbhs  =  get.nbhs(alive)
  alive  =  update.alive(alive, nbhs)
  update.plot2(alive)
}

##
## vectorise the neighbour computations
##

get.nbhs2  =  function(alive, nrows=dim(alive)[1]-2, ncols=dim(alive)[2]-2){
   nbhs  =  matrix(0, nrows+2, ncols+2)
   nbhs[2:(nrows+1),2:(ncols+1)] = alive[-c(nrows+1,nrows+2),-c(ncols+1,ncols+2)] + 
                       alive[-c(nrows+1,nrows+2),-c(1,ncols+2)] + 
                       alive[-c(nrows+1,nrows+2),-c(1,2)] + 
                       alive[-c(1,nrows+2),-c(ncols+1,ncols+2)] + 
                       alive[-c(1,nrows+2)  ,-c(1,2)] + 
                       alive[-c(1,2),-c(ncols+1,ncols+2)] + 
                       alive[-c(1,2),-c(1,ncols+2)  ] + 
                       alive[-c(1,2),-c(1,2)]
      
   nbhs
   }


update.alive2  =  function(alive, nbhs, nrows=dim(alive)[1]-2, ncols=dim(alive)[2]-2){   
   alive.new = (nbhs==3) | (alive & (nbhs %in% 2:3))
   alive.new[1,] = 0
   alive.new[,1] = 0
   alive.new[nrows+2,] = 0
   alive.new[,ncols+2] = 0
   alive.new
 }

update.plot3  =  function(alive, nrows=dim(alive)[1]-2, ncols=dim(alive)[2]-2){
	i = rep(1:nrows,ncols)
	j = rep(1:ncols,each=nrows)
   rect(j-0.5,i-0.5,j+0.5,i+0.5, 
               col=alive[2:(nrows+1),2:(ncols+1)]*6 + 1, border=alive[2:(nrows+1),2:(ncols+1)]*6 + 1)
      
  invisible()
}

##
##
##

nrows  =  200
ncols  =  200
n.updates  =  200 
set.seed(4)
alive  =  matrix(rbinom((nrows+2)*(ncols*2),1, 0.3), nrows+2, ncols+2) # "+2" is adding the gray border

do.basic.plot(nrows, ncols)
update.plot3(alive)
for(k in 1:n.updates){
  nbhs  =  get.nbhs2(alive)
  alive  =  update.alive2(alive, nbhs)
  update.plot3(alive)
}


##glider-gun
gun<-as.matrix(read.table("AKLfaster/glidergun.txt"))

nrows=100
ncols=100
 alive=matrix(0,nrows+2,ncols+2)
 alive[5+(1:9),5+(1:36)]=as.matrix(gun)
 n.updates  =  400 
 
 do.basic.plot(nrows, ncols)
 update.plot3(alive)
 for(k in 1:n.updates){
   nbhs  =  get.nbhs2(alive)
   alive  =  update.alive2(alive, nbhs)
   update.plot3(alive)
 }

nrows=150
ncols=150
 alive=matrix(0,nrows+2,ncols+1)
 alive[5+(1:9),5+(1:36)]=as.matrix(gun)
 alive[50+(1:9),50+(1:36)]=as.matrix(gun)
 n.updates  =  1000 
 
 do.basic.plot(nrows, ncols)
 update.plot3(alive)
 for(k in 1:n.updates){
   nbhs  =  get.nbhs2(alive)
   alive  =  update.alive2(alive, nbhs)
   update.plot3(alive)
 }


###
# fast-forward: don't plot every iteration
###

nrows=150
ncols=150
 alive=matrix(0,nrows+2,ncols+2)
 alive[5+(1:9),5+(1:36)]=as.matrix(gun)
 alive[50+(1:9),50+(1:36)]=as.matrix(gun)
 n.updates  =  1000 
 
 do.basic.plot(nrows, ncols)
 update.plot3(alive)
 for(k in 1:n.updates){
  for(j in 1:5){
    nbhs  =  get.nbhs2(alive)
    alive  =  update.alive2(alive, nbhs)
   }
   update.plot3(alive)
 }
 
 
 ###
 #  Draw only the changes
 ###

update.plot4  =  function(alive, wasalive, nrows=dim(alive)[1]-2, ncols=dim(alive)[2]-2){
	changed= as.vector((alive!=wasalive)[2:(nrows+1),2:(ncols+1)])
	i = rep(1:nrows,ncols)[changed]
	j = rep(1:ncols,each=nrows)[changed]
   rect(j-0.5,i-0.5,j+0.5,i+0.5, 
               col=as.vector(alive[2:(nrows+1),2:(ncols+1)])[changed]*6 + 1,
               border=as.vector(alive[2:(nrows+1),2:(ncols+1)])[changed]*6 + 1)
      
  invisible()
}
 
nrows=250
ncols=250
 alive=matrix(0,nrows+2,ncols+2)
 alive[5+(1:9),5+(1:36)]=as.matrix(gun)
 alive[205+(9:1),210+(36:1)]=as.matrix(gun)
 alive[100+(1:9),100+(1:36)]=as.matrix(gun)
 n.updates  =  4000 
 
 do.basic.plot(nrows, ncols)
 update.plot3(alive)
 for(k in 1:n.updates){
  wasalive=alive
    nbhs  =  get.nbhs2(alive)
    alive  =  update.alive2(alive, nbhs)
    update.plot4(alive,wasalive)
 }
