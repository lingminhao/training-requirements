if (interactive()){
  con <- stdin()
} else{
  con <- "stdin"
}

count <- "Z"

player_turn <- function(){
  cat(paste("Player",player,"turn: \n"))
  while (TRUE){
    cat("What row? ")
    row.number <- readLines(con = con, n = 1)
    row.number <- strtoi(row.number)
    cat("What column? ")
    column.number <- readLines(con = con, n = 1)
    column.number <- strtoi(column.number)
    if ((row.number %in% board_size) & (column.number %in% board_size)){
      coordinate <- board[row.number, column.number]
      if (is.na(coordinate) != TRUE){
        cat("This position has been placed. Choose another position.\n\n")
        next
      }
    }else{
      cat("Invalid input. Choose 1,2, or 3 for row and column.\n\n")
      next
    }
    cat(paste("Place", player, "at row", row.number,
              ", column", column.number, "? [y/n] "))
    yes_or_no <- readLines(con = con, n = 1)
    if (yes_or_no == 'y'){
      cat("\nMove placed!\n\n")
      Sys.sleep(1)
      cat("Current board: \n\n")
      cat("~~~~~~~~~~~~~~~~~~~~~~~~\n\n")
      board[row.number,column.number] = player
      print(board)
      cat("\n~~~~~~~~~~~~~~~~~~~~~~~~\n\n")
      Sys.sleep(1)
      break
    } else if (yes_or_no == 'n'){
      cat("Move not placed.\n\n")
    } else {
      cat("Invalid input. Please input either y or n next time.\n\n")
    }
  }
  
  # Player wins because 3 in a row
  player_test1 <- board[row.number,] 
  player_test1[is.na(player_test1) == TRUE] = "a"
  if (all(player_test1 == c(player,player,player)) == TRUE){
    cat(paste(player,"wins the game!\n\n"))
    count <- "PW"
  }
  
  # Player wins because 3 in a column
  player_test2 <- board[,column.number]
  player_test2[is.na(player_test2) == TRUE] = "a"
  if (all(player_test2 == c(player,player,player)) == TRUE){
    cat(paste(player,"wins the game!\n\n"))
    count <- "PW"
  }
  
  # Player wins because 3 in a diagonal
  player_test3 <- c(board[1,1],board[2,2],board[3,3])
  player_test3[is.na(player_test3) == TRUE] = "a"
  if (all(player_test3 == c(player,player,player)) == TRUE){
    cat(paste(player,"wins the game!\n\n"))
    count <- "PW"
  }
  
  # Player wins because 3 in a diagonal 
  player_test4 <- c(board[1,3],board[2,2],board[3,1])
  player_test4[is.na(player_test4) == TRUE] = "a"
  if (all(player_test4 == c(player,player,player)) == TRUE){
    cat(paste(player,"wins the game!\n\n"))
    count <- "PW"
  }
  
  # Tie game 
  if ((TRUE %in% is.na(board)) == FALSE){
    cat(paste("No one wins the game !\n\n"))
    count <- "T"
  }
  return(list(board,count))
}

computer_turn <- function(){
  cat(paste("Player",computer,"turn: \n\n"))
  cat("Computer move registered.\n\n")
  Sys.sleep(1)
  cat("Current board: \n\n")
  cat("~~~~~~~~~~~~~~~~~~~~~~~~\n\n")
  
  rand_1 <- sample(board_size,1)
  rand_2 <- sample(board_size,1)
  while (is.na(board[rand_1,rand_2]) != TRUE){
    rand_1 <- sample(board_size,1)
    rand_2 <- sample(board_size,1) 
  }
  
  board[rand_1,rand_2] <- computer
  print(board)
  cat("\n~~~~~~~~~~~~~~~~~~~~~~~~\n\n")
  Sys.sleep(1)
  
  #Computer wins because 3 in a row
  computer_test1 <- board[rand_1,] 
  computer_test1[is.na(computer_test1) == TRUE] = "a"
  if (all(computer_test1 == c(computer,computer,computer)) == TRUE){
    cat(paste(computer,"wins the game!\n\n"))
    count <- "CW"
  }
  
  #Computer wins because 3 in a column
  computer_test2 <- board[,rand_2]
  computer_test2[is.na(computer_test2) == TRUE] = "a"
  if (all(computer_test2 == c(computer,computer, computer)) == TRUE){
    cat(paste(computer,"wins the game!\n\n"))
    count <- "CW"
  }
  
  #Computer wins because 3 in a diagonal
  computer_test3 <- c(board[1,1],board[2,2],board[3,3])
  computer_test3[is.na(computer_test3) == TRUE] = "a"
  if (all(computer_test3 == c(computer,computer,computer)) == TRUE){
    cat(paste(computer,"wins the game!\n\n"))
    count <- "CW"
  }
  
  #Computer wins because 3 in a diagonal
  computer_test4 <- c(board[1,3],board[2,2],board[3,1])
  computer_test4[is.na(computer_test4) == TRUE] = "a"
  if (all(computer_test4 == c(computer,computer,computer)) == TRUE){
    cat(paste(computer,"wins the game!\n\n"))
    count <- "CW"
  }
  
  # Tie game 
  if ((TRUE %in% is.na(board)) == FALSE){
    cat(paste("No one wins the game !\n\n"))
    count <- "T"
  }
  return(list(board,count))
}

# Choose who to go first
while (TRUE){
  cat("X or O? ")
  player <- readLines(con = con, n=1)
  if (player == 'X'){
    computer <- 'O'
    break
  } else if (player == 'O'){
    computer <- 'X'
    break
  } else{
    cat("Invalid input. Please input either X or O.\n\n")
  }
}

# Game start
board <- matrix(rep(NA, 9), 3, 3)
board_size <- c(1,2,3)
i <- 1

while (TRUE){
  cat("\n#######################\n\n")
  cat(paste("####### Round", i,  "#######\n\n"))
  cat("#######################\n\n")
  Sys.sleep(1)
  cat("Current board: \n\n")
  cat("~~~~~~~~~~~~~~~~~~~~~~~~\n\n")
  print(board)
  cat("\n~~~~~~~~~~~~~~~~~~~~~~~~\n\n")
  Sys.sleep(1)
  
  if (player == "X"){
    answer <- player_turn()
    board <- answer[[1]]
    count <- answer[[2]]
    if ((count == "PW") | (count == "T")){
      break
    }
    answer <- computer_turn()
    board <- answer[[1]]
    count <- answer[[2]]
    if ((count == "CW") | (count == "T")){
      break
    }
  }else{
    answer <- computer_turn()
    board <- answer[[1]]
    count <- answer[[2]]
    if ((count == "CW") | (count == "T")){
      break
    }
    answer <- player_turn()
    board <- answer[[1]]
    count <- answer[[2]]
    if ((count == "PW") | (count == "T")){
      break
    }
  }
  
  i <- i + 1
}


