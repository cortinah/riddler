simpill <- function() {
  pillbox <- rep(1.0, 15)
  
  takepill <- function(pillbox) {
    
    pillcount <- length(pillbox)
    take <- sample(pillcount, 3)
    pilla <- pillbox[take][1]
    pillb <- pillbox[take][2]
    
    if (pilla==1 & pillb==1) {pillbox[take][1]<-0; pillbox[take][2]<-0.5}
    if (pilla==1 & pillb==0.5) {pillbox[take][1]<-0; pillbox[take][2]<-0}
    if (pilla==0.5 & pillb==1) {pillbox[take][1]<-0; pillbox[take][2]<-0}
    if (pilla==0.5 & pillb==0.5) {pillbox[take][1]<-0; pillbox[take][2]<-0;
    pillc <- pillbox[take][3]; 
    if (pillc==0.5) pillbox[take][3]<-0
    if (pillc==1) pillbox[take][3]<-0.5}
    
    pillbox <- pillbox[pillbox!=0]
    return(pillbox)
  }
  
  takepilllastday <- function(pillbox) {
    
    pillcount <- length(pillbox)
    take <- sample(pillcount, 2)
    pilla <- pillbox[take][1]
    pillb <- pillbox[take][2]
    
    if (pilla==1 & pillb==1) {pillbox[take][1]<-0; pillbox[take][2]<-0.5}
    if (pilla==1 & pillb==0.5) {pillbox[take][1]<-0; pillbox[take][2]<-0}
    if (pilla==0.5 & pillb==1) {pillbox[take][1]<-0; pillbox[take][2]<-0}
    if (pilla==0.5 & pillb==0.5) {pillbox[take][1]<-0; pillbox[take][2]<-0}
    
    pillbox <- pillbox[pillbox!=0]
    return(pillbox)
  }
  
  days <- 9
  repeat{
   # print(pillbox)
    pillbox <- takepill(pillbox)
    days <- days-1
    if (days==0) break()
  }
  pillbox <- takepilllastday(pillbox)
  return(pillbox)
}

results <- as.numeric(replicate(1000000, simpill()))
sum(!is.na(results))/length(results)
#21.0365%