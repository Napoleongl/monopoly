# ============ Imports ====================
library(tidyverse)
library(magrittr)
source("R/board_functions.R")
source("R/player_functions.R")

# ============ Set up =====================
verbose = TRUE
players <- create_players(c("p1", "p2"), 20)
board <- create_board()

# ============ Actual Game ================
current_player_id <- 0L
nplayers <- nrow(players)
while(TRUE){
  old_position   <- players %>% get_position(current_player_id)
  # TODO - attempt unprison before move!
  players %<>% make_move(current_player_id)
  new_position   <- players %>% get_position(current_player_id)
  position_type  <- board %>% get_board_field(new_position, "type")
  
  # ============ Lots a' logic ==============
  if(position_type == "lot"){
    position_owner <- board %>% get_board_field(new_position, "owner")
    if(position_owner == current_player_id){ #Own lot
      #End turn
    } else if(is.na(position_owner)){ #Empty buyable lot
      lot_price <- board %>% get_board_field(new_position, "price")
      if(get_player_field(players, current_player_id, "balance") > lot_price){
      board %<>% change_lot_owner(current_player_id, new_position)
      players %<>% 
        change_balance(current_player_id, -1 * lot_price) 
      } else { break } # Needs balance > 1 after buying lot
    } else if(position_owner != current_player_id){ #Lot owned buy someone else - pay rent!
      lot_price <- board %>% get_board_field(new_position, "price")
      double_rent <- board %>% owns_whole_lot_group(position_owner, new_position)
      players %>% transfer_balance(position_owner, current_player_id, 
                                   lot_price * (1 + double_rent))
    } else {
      stop("Lot owner not in (NA, current player or other player)!!!")
    }
 
  # ============ Prison logic ===============
  } else if(position_type == "prison"){
    # end turn
    
  # ============ Parking time ===============
  } else if(position_type == "parking"){
    #end_turn
    
  # ============ To prison ya go ============
  } else if(position_type == "to_prison"){
    player %<>% imprison(current_player_id)
    
  # ============ Back to the start ==========
  } else if(position_type == "go"){
    
  }
}

