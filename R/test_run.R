
error_round_finder  <-TRUE
error_round <- 6
end_game_browser <- FALSE

source("R/board_functions.R")
source("R/player_functions.R")
source("R/chance_card_functions.R")
source("R/main.R")
for(i in 1:100) {print(i); set.seed(i); main(nplayers = 3, verbose = FALSE, max_rounds = 100);}

