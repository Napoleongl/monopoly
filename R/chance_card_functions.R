create_deck <- function(){
  list(
# ============ Player cards ===========
give_gift_to_player = function(.players, .board, player_id, amount = NULL){     # 0 Cards actually...
  # The player is instructed to give an amount to another (random) player.
  # Potentially game ending for the current player.
  if(is.null(amount)){ amount <- 2 }
  other_player <- filter(.players, ID != player_id) %>% 
    pull(ID) %>% 
    sample(1)
  if(verbose %in% c("all", "chance")){
    write(paste("Player", player_id, "gives", amount, "money to player", other_player), "")
  }
  .players %<>% transfer_balance(other_player, player_id, amount)
  return(list(players = .players, board = .board, game_alive = TRUE))
},

give_gift_to_bank = function(.players, .board, player_id, amount = NULL){       # 1 Card
  # The player is instructed to give an amount to the bank.
  # Potentially game ending for the current player.
  game_alive <- TRUE
  if(is.null(amount)){ amount <- 2 }
  player_balance <- .players %>% get_player_field(player_id, "balance")
  if(player_balance < amount){ 
    amount <- player_balance
    game_alive <- FALSE # Somewhat unnecessary since balance will be zeroed, but doesn't hurt...
    }
  if(verbose %in% c("all", "chance")){
    write(paste("Player", player_id, "pays", amount, "to the bank!"), "")
  }
  .players %<>% change_balance(player_id, -1 * amount)
  return(list(players = .players, board = .board, game_alive = game_alive))
},

get_gift_from_all = function(.players, .board, player_id, amount = NULL){       # 1 Card
  # The player gets a gift from all other players.
  # Potentially game ending for other player with balance <= gift amount.

  if(is.null(amount)){ amount <- 1 }
  other_players <- filter(.players, ID != player_id) %>% pull(ID) 
  for(p in other_players){ 
    .players %<>% transfer_balance(player_to = player_id, player_from = p, amount = amount)
  }
  if(verbose %in% c("all", "chance")){
    write(paste("Player", player_id, "gets",amount, "money from every player!"), "")
    }
  return(list(players = .players, board = .board, game_alive = TRUE))
},

get_gift_from_bank = function(.players, .board, player_id, amount = NULL){      # 3 Cards
  # The player gets a gift from the bank.
  if(is.null(amount)){ amount <- rpois(1, 2/3) } # Two cards with amount 2 and one with amount 1 exists
  if(verbose %in% c("all", "chance")){
    write(paste("Player", player_id, "gets a gift of", amount, "money!"), "")
  }
  .players %<>% change_balance(player_id, +2) 
  return(list(players = .players, board = .board, game_alive = TRUE))
},

get_out_of_jail = function(.players, .board, player_id){                        # 1 Card
  # Player gets a card that lets them out of jail without bail. Card is kept
  # until needed.
  if(verbose %in% c("all", "chance")){
    write(paste("Player", player_id, "gets a get out of jail card!"), "")
  }
  .players %<>% set_player_field(player_id = player_id, method = "set", 
                                 field = "get_out_of_jail_card", value = TRUE)
  return(list(players = .players, board = .board, game_alive = TRUE))
},

# ============ Board cards ============
goto_go = function(.players, .board, player_id){                          # 2 Cards
  # Player is sent to the GO-lot and gets money for it
  if(verbose %in% c("all", "chance")){
    write(paste("Player", player_id, "goes to GO!"), "")
  }
  .players %<>% 
    set_player_field(player_id = player_id, field = "position", 
                     value = 0, method = "set") %>%
    change_balance(player_id, 2) 
  return(list(players = .players, board = .board, game_alive = TRUE))
},

imprison = function(.players, .board, player_id){                         # 1 Card
  # Player is sent to prison because of reason!
  if(verbose %in% c("all", "chance")){
    write(paste("Player", player_id, "is sent to prison!"), "")
  }
  .players %<>% imprison(player_id = player_id)
  return(list(players = .players, board = .board, game_alive = TRUE))
},

move_to_and_get_or_pay = function(.players, .board, player_id, groups = NULL, lot = NULL){ # 3 cards with 2 groups, 3 cards with one group, 2 cards with lots
  # The player is instructed to go to any lot of group x or y and if it is vacant
  # get it free, else pay rent to the owner (or do nothing if self owned).
  # The only place where there is actual strategy in the game as you need to 
  # choose which lot to go to in case of multiple options!
  # Potentially game ending if the player has balance <= lot price.
  
  # TODO: figure out strategy and turn to logic:
  # Primarily select the first empty lot in a group, then the empty lot where 
  # the other is owned, then a lot owned by player, then something else...
  groups <- sample(.board %>% filter(type == "lot") %>% pull(lot_group), ngroups)
  lot_id <- sample(.board %>% filter(lot_group %in% groups) %>% pull(ID), 1)    # TODO: replace with proper logic...
  
  if(verbose %in% c("all", "chance")){
    write(paste("Player", player_id, "moves to lot", lot_id), "")
    }
  return(lot_buy_or_pay(.players, .board, player_id, lot_id))
},

move_to_and_steal = function(.players, .board, player_id, lot_id = NULL){       # 3 Cards
  # The player is instructed to move to a specific lot and buy it regardless
  # of whether it is already owned or not. 
  # Potentially game ending if the player has balance <= lot price.
  if(is.null(lot_id)){
    lot_id <- .board %>% filter(type == "lot") %>% pull(ID) %>% sample(1)
  }
  lot_price <- get_board_field(.board, lot_id, "price")
  if(get_player_field(.players, player_id, "balance") < lot_price){             # Player hasn't got enough funds
    return(list(players = .players, board = .board, game_alive = FALSE))
  }
  previous_owner <- get_board_field(.board, lot_id, "owner")
  if(verbose %in% c("all", "chance")){
    if(!is.na(previous_owner)){
      write(paste("Player", player_id, "buys lot", lot_id, "from player", previous_owner), "")
    } else {
      write(paste("Player", player_id, "buys lot", lot_id), "")
    }
  }
  if(is.na(previous_owner)){                                                    # Lot is vacant and bought from the bank
    .board %<>% change_lot_owner(player_id, lot_id)
    .players %<>% change_balance(player_id, -1 * lot_price) 
  } else {                                                                      # Lot is owned and price is transferred
    .board %<>% change_lot_owner(player_id, lot_id)
    .players %<>% transfer_balance(previous_owner, player_id, -1 * lot_price) 
  }
  return(list(players = .players, board = .board, game_alive = TRUE))
}
)}
# ============ Spare card =============
# .blank_card_template = function(.players, .board, player_id, ...){
#   #In case I find some more cards!
#   if(verbose %in% c("all", "chance")){
#     write(paste("Player", player_id, ""), "")
#     }
#   return(list(players = .players, board = .board, game_alive = TRUE))
# }

# ============ Pick a card, any card! =
pick_card <- function(card_types, player_id, type = "random", card_probs = NULL, ...){
  # Simulates picking a card from the deck of chance cards. 
  # Type is either "random" for any possible card or the name of a card function.
  # If type is specified then addition card arguments like amount or lot group
  # are supplied via "...".
  if(!(type %in% names(card_types))){
    if(is.null(card_probs)){
      card_probs <- c(give_gift_to_player    = 0,  give_gift_to_bank        = 1,  
                      get_gift_from_all      = 1,  get_gift_from_bank       = 3,  
                      get_out_of_jail        = 1,  goto_go                  = 2,
                      imprison               = 1,  move_to_and_get_or_pay   = 0, # since not yety implemented...  
                      move_to_and_steal      = 3) 
    } else {
      card_probs <- card_probs[names(card_types)] # sorting necessary since sample() doesn't honor names of prob-vector vs names(x)
    }
    type <- sample(names(card_types), 1,prob =  card_probs)
  }

  if(verbose %in% c("all", "chance")){
    write(paste("Player", player_id, "picks a", type, "card."), "")
  }

  return(card_types[[type]])
}
