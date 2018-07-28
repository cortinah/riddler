# 538 Riddler Classic: The Perfect Doodle Puzzle
# Source script and type search() to run solver

# Function to check valid moves given a board and current position
moves <- function(board, pos) {
  pos_moves <- rep(FALSE,8)
  names(pos_moves) <- 1:8
  
  if ((pos[1]+3) < 6) (if (board[ pos[1]+3,pos[2] ]==F) pos_moves[1] <- T)
  if ((pos[1]-3) > 0) (if (board[ pos[1]-3,pos[2] ]==F) pos_moves[2] <- T)
  if ((pos[2]+3) < 6) (if (board[ pos[1],pos[2]+3 ]==F) pos_moves[3] <- T)
  if ((pos[2]-3) > 0) (if (board[ pos[1],pos[2]-3 ]==F) pos_moves[4] <- T)
  if ((pos[1]+2) < 6  & (pos[2]+2) < 6) (if (board[ pos[1]+2,pos[2]+2 ]==F) pos_moves[5] <- T)
  if ((pos[1]+2) < 6  & (pos[2]-2) > 0) (if (board[ pos[1]+2,pos[2]-2 ]==F) pos_moves[6] <- T)
  if ((pos[1]-2) > 0  & (pos[2]-2) > 0) (if (board[ pos[1]-2,pos[2]-2 ]==F) pos_moves[7] <- T)
  if ((pos[1]-2) > 0  & (pos[2]+2) < 6) (if (board[ pos[1]-2,pos[2]+2 ]==F) pos_moves[8] <- T)
    
  pos_moves
}

# Function that returns next position given current position and selected move
next_pos <- function(pos, path) {
  
  if (path==1) pos <- c(pos[1]+3, pos[2])
  if (path==2) pos <- c(pos[1]-3, pos[2])
  if (path==3) pos <- c(pos[1], pos[2]+3)
  if (path==4) pos <- c(pos[1],pos[2]-3)
  if (path==5) pos <- c(pos[1]+2,pos[2]+2)
  if (path==6) pos <- c(pos[1]+2,pos[2]-2)
  if (path==7) pos <- c(pos[1]-2,pos[2]-2)
  if (path==8) pos <- c(pos[1]-2,pos[2]+2)
    
  pos }

game <- function() {
  
# Set-up board and initial location
board <- matrix(F,ncol=5,nrow=5)
initial <- sample(1:5,2,replace=T)

board[initial[1],initial[2]] <- T
current <- initial
i <- 1
pos_hist <- vector("list", 25)
pos_hist[[i]] <- current

repeat {
  possible <- moves(board, current)
  if (all(possible==F)) {ret<-1; break}
  possible <- possible[possible==T]
  path <- sample(possible,1)
  current <- next_pos(current, as.numeric(names(path)))
  board[current[1],current[2]] <- T
  i <- i+1
  pos_hist[[i]] <- current
  if (all(board==T)) {ret<-0; break}
}

return(list(ret,pos_hist))
}


search <- function(){
  trys <- 1 
  repeat{
    t <- game()
    if (t[[1]]==0) break
    print(paste('Attempts:',trys)); trys <- trys+1
    
    }
  t[[2]]
}