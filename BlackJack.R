rm(list=ls())

#### Purpose: This code runs a simplified version of Blackjack with classical rules, single-player (no dealer)
#### 
#### Author: Kenneth Rios
#### Last Updated: 1/1/2019


## Initialize deck
# WARNING: RStudio has known issues handling the unicode characters for the suits when the code is compiled from Source. 
# Instead, Select All from this script and then Run the entire code.

# diamonds <- c("Ad", "2d", "3d", "4d", "5d", "6d", "7d", "8d", "9d", "10d", "Jd", "Qd", "Kd")
# clubs <- c("Ac", "2c", "3c", "4c", "5c", "6c", "7c", "8c", "9c", "10c", "Jc", "Qc", "Kc")
# hearts <- c("Ah", "2h", "3h", "4h", "5h", "6h", "7h", "8h", "9h", "10h", "Jh", "Qh", "Kh")
# spades <- c("As", "2s", "3s", "4s", "5s", "6s", "7s", "8s", "9s", "10s", "Js", "Qs", "Ks")
diamonds <- c("A♦", "2♦", "3♦", "4♦", "5♦", "6♦", "7♦", "8♦", "9♦", "10♦", "J♦", "Q♦", "K♦")
clubs <- c("A♣", "2♣", "3♣", "4♣", "5♣", "6♣", "7♣", "8♣", "9♣", "10♣", "J♣", "Q♣", "K♣")
hearts <- c("A♥", "2♥", "3♥", "4♥", "5♥", "6♥", "7♥", "8♥", "9♥", "10♥", "J♥", "Q♥", "K♥")
spades <- c("A♠", "2♠", "3♠", "4♠", "5♠", "6♠", "7♠", "8♠", "9♠", "10♠", "J♠", "Q♠", "K♠")

startingDeck <- c(diamonds, clubs, hearts, spades)
sum <- 0



# Randomly draws a card from the 52-card deck without replacement
Draw <- function(deck){
  card <- sample(deck, 1)
  deck <<- deck[-match(card, deck)]
  
  return(card)
}



# Converts a card to its numeric value
Value <- function(card, live_aces){
  # A card is a string, such as 'A♠'
  
  if(substr(card, 1, nchar(card)-1) %in% c("J", "Q", "K")){
    # Faces are worth 10
    value <- 10
    
    if((sum + value) > 21 & live_aces > 0){
      value <- value - 10
      live_aces <<- live_aces - 1
    }
  }
  
  else if(substr(card, 1, nchar(card)-1) == "A"){
    # Allows A = 1 or A = 11, whichever is better for the player
    if((sum + 11) <= 21){
      value <- 11
      live_aces <<- live_aces + 1
    }
    else{
      value <- 1
    }
    
  }
  
  else{
    value <- as.numeric(substr(card, 1, nchar(card)-1))
    
    if((sum + value) > 21 & live_aces > 0){
      value <- value - 10
      live_aces <<- live_aces - 1
    }
  }
  
  return(value)
  
}



# Play a round of Blackjack
Round <- function(){

  # Initialize the number of "live" aces that are worth 11
  live_aces <<- 0
  
  # Get dealt 2 cards
  card1 <- Draw(startingDeck)
  card2 <- Draw(deck)
  
  # Reset the previous sum
  sum <<- 0
  
  # Find the sum of the first two cards dealt while keeping track of the new sum
  sum <<- Value(card1, live_aces) 
  first_sum <- sum
  sum <<- Value(card2, live_aces)
  second_sum <- sum
  sum <<- first_sum + second_sum
  
  cards <- c(card1, card2)
  
  stop <- FALSE
  while(stop == FALSE){
    if(sum < 21){
      
      print(cards)
      continue <- readline(prompt = "Would you like another card? [y/n] ")
      
      if(continue %in% c("Y", "y", "Ye", "ye", "Yes", "yes", "Yep", "yep", "YES", "Yugoslavia")){
        card <- Draw(deck)
        cards <- c(cards, card)
        
        sum <<- sum + Value(card, live_aces)
      }

      else{
        roundScore <- 21 - sum
        gameScore <<- gameScore - roundScore
        
        cat(paste0("OK, your round score is ", as.character(sum), " and your game score is ", as.character(gameScore), ".", "\n"))
        stop <- TRUE
      }
      
    }
    
    else if(sum == 21){
      print(cards)
      
      cat(paste0("Blackjack! Your game score is still ", as.character(gameScore), ".", "\n"))
      stop <- TRUE
    }
    
    else{
      print(cards)
      roundScore <- 21
      gameScore <<- gameScore - roundScore
      
      cat(paste0("Oops! You busted! Your game score is ", as.character(gameScore), ".", "\n"))
      stop <- TRUE
    }
      
  }
}



# Game algorithm
BlackJack <- function(rounds=10){
  
  n <- rounds
  
  name <- readline(prompt = "Welcome to the BlackJack Interface. What is your name? ")
  cat(paste0("\nHi, ", name, "! You'll be playing ", as.character(n), " rounds of BlackJack.\nYou start off with ",
             as.character(n * 20), " points. Points are deducted for standing before 21 and for busting. \n",
             "Face cards are worth 10. Aces are worth either 1 or 11. All other cards are worth face-value.\n"))
  cat("\nYou're responsible for keeping track of your within-round total. 
      Good luck! \n")
  cat("\n ============================================ \n")
  
  gameScore <<- rounds * 20
  
  # Loops through n instances of Round()
  rounds <- 1:rounds
  for(round in rounds){
    cat(paste0("\n", "ROUND # ", as.character(round), "\n"))
    Round()
    cat("\n ============================================ \n")
  }
  
  cat("\nVERDICT: \n")
  
  if(gameScore >= (.7 * n * 20)){
    cat("Well done!")
  }
  if(gameScore <= (.50 * n * 20)){
    cat(paste0("You must be trying to lose on purpose."))
  }
  if(gameScore > (.50 * n * 20) & gameScore < (.7 * n * 20)){
    cat("Try better next time.")
  }
}




# Play 100 rounds of Blackjack!
BlackJack(100)

