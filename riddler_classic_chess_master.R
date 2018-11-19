chess <- function(games) {
  
  if ((games %% 2) != 0) stop('Must be even integer')
  
  min_points <- (games/2)+0.5
  
  i <- min_points
  points <- vector(mode='numeric', length=1)
  vec_index <- 1
  
  while (games >= i) {
    points[vec_index] <- i
    i <- i+0.5
    vec_index <- vec_index+1
      }
  
  numrows <- length(points); numcols <- floor(min_points)
  combos <- matrix(NA, nrow=numrows, ncol=numcols*3)
  rownames(combos) <- rev(points)
  
  i <- 1; draw <- 1; wins <- numcols
  while (i <= numcols*3) {
    
    combos[numrows, i] <- wins; wins <- wins-1; i <- i+1
    combos[numrows, i] <- draw; draw <- draw+2; i <- i+1
    combos[numrows, i] <- (games - combos[numrows, i-1] - combos[numrows, i-2]); i <- i+1
  }
  for (i in 1:numcols) {
    j <- numrows-1
    while (j>=1) {
      
      combos[j, i*3] <- combos[j+1, i*3]
      j <- j-1
      combos[j, i*3] <- ifelse( (combos[j+1, i*3]-1)>=0, combos[j+1, i*3]-1, NA)
      j <- j-1
    }
  }
  
  for (i in 1:numcols) {
    j <- numrows-1
    while (j>=1) {
      
      combos[j, i*3-2] <- ifelse (!is.na(combos[j, i*3]), (combos[j+1, i*3-2]+1), NA)
      j <- j-1
      combos[j, i*3-2] <- ifelse (!is.na(combos[j, i*3]), combos[j+1, i*3-2], NA)
      j <- j-1
    }
  }
  
  for (i in 1:numcols) {
    j <- numrows-1
    while (j>=1) {
      
      combos[j, i*3-1] <- games - combos[j, i*3-2] - combos[j, i*3]
      j <- j-1
      }
  }
    
  proba <- matrix(NA, nrow=numrows, ncol=numcols)
  
  for (i in 1:numcols) {
    j <- numrows
    while (j>=1) {
      
      proba[j, i] <- choose(games, combos[j, i*3-2]) * (0.2^combos[j, i*3-2]) * (0.65^combos[j, i*3-1]) * (0.15^combos[j, i*3]) *
                     choose(games-combos[j, i*3-2], combos[j, i*3-1])
      j <- j-1
    }
  }
  
  sum(proba, na.rm = T)
}