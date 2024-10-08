# Tic-Tac-Toe
Input <- function(){
  if (interactive()) {
    con <- stdin()
  } else {
    con <- "stdin"
  }
  symbol <- readLines(con = con, n = 1)
  return (symbol)
}

play <- function(round_no, user_input, board, positions_of_X, positions_of_O, winner, winning_positions){
  Round_text <- sprintf('\n#######################
####### Round #%d ######
#######################',round_no)
  cat(Round_text)
  cat('\n\nCurrent board : \n\n')
  cat('~~~~~~~~~~~~~~~~~~\n\n')
  print(board)
  cat('\n')
  cat('~~~~~~~~~~~~~~~~~~\n\n')
  Sys.sleep(2)
  cat('Player \'X\' turn\n')
  Sys.sleep(1)
  if (user_input=='X'){
    answer <- 'n'
    while (answer=='n'){
      cat('What row? ')
      row_no <- as.numeric(Input())
      cat('What column? ')
      col_no <- as.numeric(Input())
      
      if (is.na(row_no) || is.na(col_no) || row_no <= 0 || col_no <= 0 || row_no > nrow(board) || col_no > ncol(board)) {
        cat('Invalid row or column. Please enter valid numbers.\n')
        next
      }
      string1 <- sprintf('Place \'X\' at row %d column %d? [y/n]: ',row_no, col_no)
      cat(string1)
      answer <- Input()
      if (answer=='n' || length(answer)==0){
        cat('Move not placed\n')
        answer <- 'n'
      } else if (answer=='y'){
        if (is.na(board[row_no,col_no])){
          board[row_no,col_no] <- 'X'
          pos <- board_position(row_no,col_no)
          positions_of_X <- c(positions_of_X, pos)
          cat('Move placed!\n')
          cat('\nCurrent board : \n\n')
          cat('~~~~~~~~~~~~~~~~~~\n\n')
          print(board)
          cat('\n')
          cat('~~~~~~~~~~~~~~~~~~\n\n')
          Sys.sleep(2)
          if (round_no>2){
            this <- winner_checker(positions_of_X,winning_positions)
            if (this){
              winner <- 'X'
              cat('\n')
              print(board)
              return (list(b=board,pX=positions_of_X,pO=positions_of_O,w=winner))
            }
          }
        } else {
          cat('Place already occupied!\n')
          answer <- 'n'
        }
      } else {
        cat('Invalid response. Please enter "y" or "n".\n')
        answer <- 'n'
      }
    }
    if (!any(is.na(board))){
      winner <- 'tie'
      cat('\n')
      print(board)
      return (list(b=board,pX=positions_of_X,pO=positions_of_O,w=winner))
    }
    cat('\nPlayer \'O\' turn\n')
    Sys.sleep(1)
    placing <- FALSE
    while (!placing){
      row_no <- sample(1:3,1)
      col_no <- sample(1:3,1)
      if (is.na(board[row_no,col_no])){
        board[row_no,col_no] <- 'O'
        pos <- board_position(row_no,col_no)
        positions_of_O <- c(positions_of_O, pos)
        cat('Computer move registered\n')
        Sys.sleep(2)
        if (round_no>2){
          this <- winner_checker(positions_of_O,winning_positions)
          if (this){
            winner <- 'O'
            cat('\n')
            print(board)
            return (list(b=board,pX=positions_of_X,pO=positions_of_O,w=winner))
          }
        }
        placing <- TRUE
      }
    }
  } else if(user_input=='O'){
    answer <- 'n'
    placing <- FALSE
    while (!placing){
      row_no <- sample(1:3,1)
      col_no <- sample(1:3,1)
      if (is.na(board[row_no,col_no])){
        board[row_no,col_no] <- 'X'
        pos <- board_position(row_no,col_no)
        positions_of_X <- c(positions_of_X, pos)
        cat('Computer move registered\n')
        Sys.sleep(2)
        cat('\nCurrent board : \n\n')
        cat('~~~~~~~~~~~~~~~~~~\n\n')
        print(board)
        cat('\n')
        cat('~~~~~~~~~~~~~~~~~~\n\n')
        Sys.sleep(2)
        if (round_no>2){
          this <- winner_checker(positions_of_X,winning_positions)
          if (this){
            winner <- 'X'
            cat('\n')
            print(board)
            return (list(b=board,pX=positions_of_X,pO=positions_of_O,w=winner))
          }
        }
        placing <- TRUE
        if (!any(is.na(board))){
          winner <- 'tie'
          cat('\n')
          print(board)
          return (list(b=board,pX=positions_of_X,pO=positions_of_O,w=winner))
        }
      }
    }
    cat('\nPlayer \'O\' turn\n')
    Sys.sleep(1)
    while (answer=='n'){
      cat('What row? ')
      row_no <- as.numeric(Input())
      cat('What column? ')
      col_no <- as.numeric(Input())
      
      if (is.na(row_no) || is.na(col_no) || row_no <= 0 || col_no <= 0 || row_no > nrow(board) || col_no > ncol(board)) {
        cat('Invalid row or column. Please enter valid numbers.\n')
        next
      }
      string1 <- sprintf('Place \'O\' at row %d column %d? [y/n]: ',row_no, col_no)
      cat(string1)
      answer <- Input()
      if (answer=='n' || length(answer)==0){
        cat('Move not placed\n')
        answer <- 'n'
      } else if (answer=='y'){
        if (is.na(board[row_no,col_no])){
          board[row_no,col_no] <- 'O'
          pos <- board_position(row_no,col_no)
          positions_of_O <- c(positions_of_O, pos)
          cat('Move placed!\n')
          if (round_no>2){
            this <- winner_checker(positions_of_O,winning_positions)
            if (this){
              winner <- 'O'
              cat('\n')
              print(board)
              return (list(b=board,pX=positions_of_X,pO=positions_of_O,w=winner))
            }
          }
        } else {
          cat('Place already occupied!\n')
          answer <- 'n'
        }
      } else {
        cat('Invalid response. Please enter "y" or "n".\n')
        answer <- 'n'
      }
    }
  }
  cat('\n')
  #print(board)
  return (list(b=board,pX=positions_of_X,pO=positions_of_O,w=winner))
}

winner_checker <- function(poses, winning_positions){
  for (each_poses in winning_positions){
    if(all(each_poses %in% poses)){
      return(TRUE)
    }
  }
  return(FALSE)
}

board_position <- function(row_no, col_no){
  if (row_no==1){
    if (col_no==1){
      pos <- 1
    } else if (col_no==2){
      pos <- 4
    } else {
      pos <- 7
    }
  } else if(row_no==2){
    if (col_no==1){
      pos <- 2
    } else if (col_no==2){
      pos <- 5
    } else {
      pos <- 8
    }
  } else {
    if (col_no==1){
      pos <- 3
    } else if (col_no==2){
      pos <- 6
    } else {
      pos <- 9
    }
  }
  return (pos)
}

winning_positions <- list(c(1,2,3), c(4,5,6), c(7,8,9), c(1,4,7), c(2,5,8), c(3,6,9), c(1,5,9), c(3,5,7))
positions_of_X <- c()
positions_of_O <- c()
winner <- NA

input_vector <- c('X','O')
cat("X or O? ")
user_input <- Input()
while (!(user_input %in% input_vector)){
  cat('Invalid response. Please enter "X" or "O". ')
  user_input <- Input()
}
someone_wins <- FALSE
round_no <- 1
board <- matrix(NA,ncol = 3,nrow = 3)

while (!someone_wins) {

  result <- play(round_no, user_input, board, positions_of_X, positions_of_O, winner, winning_positions)
  board <- result$b
  positions_of_X <- result$pX 
  positions_of_O <- result$pO
  winner <- result$w
  
  if (!is.na(winner)) {
    if (winner=='X'){
      someone_wins <- TRUE
      cat('\nX wins!!!')
    } else if (winner=='O'){
      someone_wins <- TRUE
      cat('\nO wins!!!')
    } else {
      someone_wins <- TRUE
      cat('\nIt is a tie :( ')
    }
  }
  round_no <- round_no + 1
  Sys.sleep(1)
}