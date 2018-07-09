library(e1071)
library(reshape2)
library(ggplot2)
library(RColorBrewer)

grid <- matrix(NA,nrow=1, ncol=2)

up <- function(n) {
  v <- matrix(NA, nrow=1,ncol=2)

  for (i in n:(n+59)) {
    
    v <- rbind(v,cbind(i,i+1))
  }
  v[complete.cases(v),]
}

cols <- function() {
  columns <- seq(1,by = 61,length.out = 21)
  
  for (i in columns) {
    grid <- rbind(grid, up(i))
  }
  grid[complete.cases(grid),] 
}

side <- function(n) {
  v <- matrix(NA, nrow=1,ncol=2)
  
  for (i in n:(n+60)) {
    
    v <- rbind(v,cbind(i,i+61))
  }
  v[complete.cases(v),]
}


rows <- function() {
  columns <- seq(1,by = 61,length.out = 20)
  
  for (i in columns) {
    grid <- rbind(grid, side(i))
  }
  grid[complete.cases(grid),] 
}

g1<-cols()
g2<-rows()
grid<-rbind(g1,g2)
rm(g1,g2,cols,rows,side,up)

grid<-as.data.frame(grid)
grid$d<-0.1/20
colnames(grid)<-c('from','to','d')
grid[1201:1260,'d'] <- 0.1/200


distance<-matrix(NA,nrow=1281,ncol=1281)
diag(distance) <- 0

fill <- function(distance) {
  for (i in 1:nrow(grid)) {
    distance[grid[i,1],grid[i,2]]<-grid[i,3]
    distance[grid[i,2],grid[i,1]]<-grid[i,3]
  }
  distance
}

distance<-fill(distance)
rm(grid,fill)

paths <- allShortestPaths(distance)
rm(distance)
#20th and F (6)
start <- (61*5) + 20


ultra <- function(paths) {
  nodes <- vector("integer",length=0)
  for (i in 1:1281) {
    if (any(extractPath(paths,start,i) %in% 1220:1281)) nodes<-c(nodes,i)
  }
  nodes
}

nodes<-ultra(paths)
rm(paths,ultra,start)
nodes<-as.data.frame(nodes)

nodes$x <- ((nodes$nodes-1) %/% 61) +1
nodes$y <- (nodes$nodes %% 61)
nodes$y <- ifelse(nodes$y==0,61,nodes$y)
nodes$nodes<-NULL

heat <- matrix(0,nrow=61,ncol=21)

fill <- function(heat) {
  for (i in 1:nrow(nodes)) {
      heat[nodes[i,2],nodes[i,1]]<-1
  }
  heat
}

heat<-fill(heat)
heat[20,6]<-2
heatmap(heat,Colv=NA,Rowv=NA,scale="none")
rm(fill,nodes)


heatm<-melt(heat)

ggplot(heatm, aes(x = Var2, y = Var1, fill = as.factor(value))) + geom_tile() +
  theme_bw() +labs(title='Riddler Express: July 6, 2018',subtitle='How Fast Can You Deliver PB&Js?',x='Avenue',y='Street') +coord_equal() +
 guides(fill=guide_legend(title='Optimal path to destination')) +scale_fill_manual(values=c(c("#377EB8","#E41A1C","#4DAF4A")),breaks=c(2,0,1), labels=c('Origin', 'Skip Ultra Trafficway', 'Use Ultra Trafficway')) +
  scale_y_continuous(breaks=c(1,11,21,31,41,51,61)) + scale_x_continuous(breaks=c(1,6,11,16,21),labels=LETTERS[c(1,6,11,16,21)])

       