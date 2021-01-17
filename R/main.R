# ============ Imports ====================
library(tidyverse)
library(magrittr)
source("R/board_functions.R")
source("R/player_functions.R")

main <- function(nplayers, max_rounds, verbose){
  # ============ Set up =====================
  players <- create_players(letters[1:nplayers], 22 - (2 * nplayers))
  board <- create_board()
  current_player_id <- 0L
  rounds <- 0
  
  # ============ Actual Game ================
  while(rounds < (max_rounds * nplayers) -1){
    #if(rounds == 33){browser()}
    old_position   <- players %>% get_position(current_player_id)
    
    if(players %>% get_player_field(current_player_id, "imprisoned")){          # Attempt unprison prior to move
      players %<>%  unprison(current_player_id)
    }
    if(!(players %>% get_player_field(current_player_id, "imprisoned"))){       # Only play turn if not imprisoned, else stay...
      players %<>% make_move(current_player_id)
      new_position   <- players %>% get_position(current_player_id)
      if(new_position < old_position){ # I.e. player is on or has passed GO
        players %<>% change_balance(current_player_id, 2) 
      }
      position_type  <- board %>% get_board_field(new_position, "type")
      # ============ Lots a' logic ==============
      if(position_type == "lot"){
        position_owner <- board %>% get_board_field(new_position, "owner")
        if(is.na(position_owner)){                                              # Empty buyable lot
          lot_price <- board %>% get_board_field(new_position, "price")
          if(get_player_field(players, current_player_id, "balance") > lot_price){
            board %<>% change_lot_owner(current_player_id, new_position)
            players %<>% 
              change_balance(current_player_id, -1 * lot_price) 
          } else {                                                              # Needs balance > 1 after buying lot
            if(verbose){
              write(paste(current_player_id, "looses due to insufficient funds"),"")
            }
            break
            } 
        } else if(position_owner == current_player_id){                         # Own lot - End turn
        } else if(position_owner != current_player_id){                         # Lot owned buy someone else - pay rent!
          lot_price <- board %>% get_board_field(new_position, "price")
          double_rent <- board %>% owns_whole_lot_group(position_owner, new_position)
          players %>% transfer_balance(position_owner, current_player_id, 
                                       lot_price * (1 + double_rent))
        } else {
          stop("Lot owner not in (NA, current player or other player)!!!")      # Shouldn't happen...
        }
        
        # ============ Prison logic ===============
      } else if(position_type == "prison"){
        # just visiting, end turn
        
        # ============ Parking time ===============
      } else if(position_type == "parking"){
        # end_turn - its sole purpose...
        
        # ============ To prison ya go ============
      } else if(position_type == "to_prison"){
        players %<>% imprison(current_player_id)
        
        # ============ Back to the start ==========
      } else if(position_type == "go"){
        # end turn, payout is handled in make_move()
        
        # ============ Chance cards ===============
      } else if(position_type == "chance"){
        # TODO...
      }
    }
    # ============ Round stat update ==========
    if(any((players %>% pull(balance)) < 1)){ break }
    current_player_id <- (current_player_id +1) %% nplayers
    rounds <- rounds + 1
  }
  end_game_stats(players, board, rounds==(max_rounds*nplayers))
}

 games <- lapply(1:200, function(x){
   print(x)
   set.seed(x)
   main(nplayers = 3, verbose = FALSE, max_rounds = 100)
   })

games <- bind_rows(games)

set.seed(116)
main(nplayers = 3, verbose = FALSE, max_rounds = 100)
