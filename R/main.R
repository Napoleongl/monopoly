main <- function(nplayers, max_rounds, verbose = FALSE){
  # ============ Set up =====================
  players <- create_players(letters[1:nplayers], 22 - (2 * nplayers))
  board <- create_board()
  chance_deck <- create_deck()
  current_player_id <- 0L
  rounds <- 0
  # ============ Actual Game ================
  while(rounds < (max_rounds * nplayers) -1){
    if(exists("error_round_finder")) {print(rounds)}
    if(exists("error_round") && rounds == error_round){browser()}
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
        pb_list <- lot_buy_or_pay(players, board, current_player_id, new_position)
        board <- pb_list[["board"]]
        players <- pb_list[["players"]]
        # 
        if(!pb_list[["game_alive"]]){ break }
        
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
        chance_card <- pick_card(chance_deck, type = "random", player_id = current_player_id)
        pb_list <- chance_card(players, board, current_player_id)
        board <- pb_list[["board"]]
        players <- pb_list[["players"]]
        # 
        if(!pb_list[["game_alive"]]){ break }
      }
    }
    # ============ Round stat update ==========
    if(any((players %>% pull(balance)) < 1)){ break }
    current_player_id <- (current_player_id +1) %% nplayers
    rounds <- rounds + 1
  }
  end_game_stats(players, board, rounds, max_rounds*nplayers)
}
