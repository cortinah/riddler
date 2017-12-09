### 538 Riddler Classic 11/8/2017

polar2cart<-function(dist,bearing){
  newx<-dist*sin(bearing)  ##X
  newy<-dist*cos(bearing)  ##Y
  return(list("x"=newx,"y"=newy)) }



mc <- function(reps=20000) {
  results <- vector(length=reps)
  
  for(loops in 1:reps) {
  
  trial<-as.data.frame(matrix(NA,nrow=5,ncol=2))
  
  i <- 1
  repeat{
r <- runif(1,0,1)
t <- runif(1,0,2*pi)

trial[i,] <- polar2cart(r,t)

if(any(dist(trial)<1,na.rm=T)==TRUE) break
i <- i+1 }


results[loops]<-sum(complete.cases(trial)) 
  }
  results-1
  }


dart<-mc(1000000)
max(dart)
sum(dart>1)/length(dart)
mean(dart)

