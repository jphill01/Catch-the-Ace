### A simplified version of Catch (Chase) the Ace! ###

# Author: Jarrett D. Phillips
# Last updated: June 28, 2021

# Clear memory

rm(list = ls())

# Playing Card Alphabet

cards <- c("Ace of Clubs",
           "Ace of Diamonds",
           "Ace of Hearts",
           "Ace of Spades",
           "2 of Clubs",
           "2 of Diamonds",
           "2 of Hearts",
           "2 of Spades",
           "3 of Clubs",
           "3 of Diamonds",
           "3 of Hearts",
           "3 of Spades",
           "4 of Clubs",
           "4 of Diamonds",
           "4 of Hearts",
           "4 of Spades",
           "5 of Clubs",
           "5 of Diamonds",
           "5 of Hearts",
           "5 of Spades",
           "6 of Clubs",
           "6 of Diamonds",
           "6 of Hearts",
           "6 of Spades",
           "7 of Clubs",
           "7 of Diamonds",
           "7 of Hearts",
           "7 of Spades",
           "8 of Clubs",
           "8 of Diamonds",
           "8 of Hearts",
           "8 of Spades",
           "9 of Clubs",
           "9 of Diamonds",
           "9 of Hearts",
           "9 of Spades",
           "10 of Clubs",
           "10 of Diamonds",
           "10 of Hearts",
           "10 of Spades",
           "Jack of Clubs",
           "Jack of Diamonds",
           "Jack of Hearts",
           "Jack of Spades",
           "King of Clubs",
           "King of Diamonds",
           "King of Hearts",
           "King of Spades",
           "Queen of Clubs",
           "Queen of Diamonds",
           "Queen of Hearts",
           "Queen of Spades")


## Simulation

catch.ace <- function(ticket.price, max.num.tickets) {
  week <- 0 # initialize counter
  payoff <- 0 # initialize weekly winnings
  jackpot <- 0 # initialize progressive jackpot
  
  while(week < 52) {
    week <- week + 1 # increment counter
    tickets <- sample(max.num.tickets, size = 1, replace = FALSE) # random number of tickets
    fill.envelopes <- sample(cards, size = length(cards), replace = FALSE) # assign cards to envelopes
    pick.ticket <- sample(tickets, size = 1, replace = TRUE) # select random ticket
    pick.envelope <- sample(fill.envelopes, size = 1, replace = TRUE) # choose random envelope
    cards <- cards[!(cards == pick.envelope)] # remove selected card
    payoff <- tickets * ticket.price # ticket sales
    jackpot <- jackpot + (0.30 * payoff) # update weekly winnings; 30% of ticket sales goes into jackpot
    
    cat("\n \n Week: ", week,
        "\n Ticket number: ", pick.ticket,
        "\n Card selected: ", pick.envelope)
    
    if ("Ace of Spades" %in% pick.envelope) {
      cat("\n Outcome: Congratulations, you've selected the Ace of Spades! You've won the progressive jackpot! \n Payoff: $", jackpot,
          "\n Proceeds donated to charity: $", (0.50 * payoff)) # 50% of all ticket sales goes to charity
      break
      } else {
        cat("\n Outcome: Sorry, you didn't select the Ace of Spades! Better luck next time! \n Payoff: $", (0.20 * payoff), # 20% of ticket sales goes to winning ticket holder each week
            "\n Proceeds donated to charity: $", (0.50 * payoff))  
        }
      }
      cards <- cards # reset deck
    }


## Let's play!

ticket.price <- 5 # price per ticket
max.num.tickets <- 5000 # maximum number of tickets sold

catch.ace(ticket.price, max.num.tickets)
