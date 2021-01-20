# ============ Chance cards ===========
draw_card <- function(player_id){
  return(TRUE)
}

# ============ Card types =============
avg_payment <- 0.5

give_money_to_player <- function(.players, .board, player_id){
  #TODO: Check player balance before transfer!
  amount <- rpois(1, avg_payment) + 1
  other_player <- filter(.players, ID != player_id) %>% 
    pull(ID) %>% 
    sample(1)
  if(verbose %in% c("all", "chance")){
    write(paste("Player", player_id, "gives", amount, "money to player", other_player), "")
  }
  .players %<>% transfer_balance(other_player, player_id, +2)
  return(list(players = .players, board = .board))
}

get_money_from_all <- function(.players, .board, player_id){
  #TODO: check balances and loop transfers
  other_players <- filter(.players, ID != player_id) %>% pull(ID) 
  if(verbose %in% c("all", "chance")){
    write(paste("Player", player_id, "gets 1 money from every player!"), "")
    }
  return(list(players = .players, board = .board))
}

get_gift <- function(.players, .board, player_id){
  if(verbose %in% c("all", "chance")){
    write(paste("Player", player_id, "gets a gift of 2 money!"), "")
  }
  .players %<>% change_balance(player_id, +2) 
  return(list(players = .players, board = .board))
}

move_to_and_buy_or_pay <- function(.players, .board, player_id){
  #TODO: figure out strategy and turn to logic...
  #Primarily select the first empty lot in a group, then the empty lot where 
  #the other is owned, then a lot owned by player, then something else...
  if(verbose %in% c("all", "chance")){
    write(paste("Player", player_id, ""), "")
    }
  return(list(players = .players, board = .board))
}

move_to_and_steal <- function(.players, .board, player_id){
  lot_id <- .board %>% filter(type == "lot") %>% pull(ID) %>% sample(1)
  if(verbose %in% c("all", "chance")){
    write(paste("Player", player_id, ""), "")
    }
  return(list(players = .players, board = .board))
}

get_out_of_jail_card <- function(.players, .board, player_id){
  if(verbose %in% c("all", "chance")){
    write(paste("Player", player_id, "gets a get out of jail card!"), "")
  }
  .players %<>% set_player_field(player_id = player_id, method = "set", 
                                 field = "get_out_of_jail_card", value = TRUE)
  return(list(players = .players, board = .board))
}

.blank_card_template <- function(.players, .board, player_id){
  #In case I find some more cards!
  if(verbose %in% c("all", "chance")){
    write(paste("Player", player_id, ""), "")
    }
  return(list(players = .players, board = .board))
}

# ============ Deck definition ========
create_chance_deck <- function(){
  return(TRUE)
}