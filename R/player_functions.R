# ============ Player basics ========
create_players <- function(player_names = paste0("player_", c(1L, 2L)), 
                           starting_balance = 20L){
  players <- tibble(
    ID = seq.int(0, length(player_names)-1), #zero-based index to allow mod-calcs
    name = player_names, 
    balance = starting_balance,
    position = 0L,
    alive = TRUE,
    imprisoned = FALSE,
    get_out_of_jail_card = FALSE
    )
  if(verbose %in% c("all", "creation", "players")){print(players)}
  return(players)
}
testthat::test_that("Player creation", {
  verbose <<- FALSE
  players2 <- create_players()
  testthat::expect_equal(dim(players2), c(2,7))
  players3 <- create_players(c("p1", "p2", "p3"),10)
  testthat::expect_length(players3$ID, 3)
  testthat::expect_equal(sum(players3 %>% pull(balance)), 30)
})

# ============ Getters and setters ====
set_player_field <- function(.players, player_id, field, value, method = "add", mod = 0L){
  stopifnot(player_id %in% (.players %>% pull(ID)))
  stopifnot(field %in% colnames(.players))
  # Sometimes dplyr syntax just complicates things... 
  # mutate(replace... freaks out on += type operations
  if(method == "add") {
    .players[.players$ID == player_id, field] <- .players[.players$ID == player_id, field] + value
  } else if(method == "mod"){
    stopifnot(mod > 1L)
    .players[.players$ID == player_id, field] <- (.players[.players$ID == player_id, field] + value) %% mod
  } else if(method == "set"){
    .players[.players$ID == player_id, field] <- value
  } else {
    stop("Invalid method passed")
  }
  .players
}

get_player_field <- function(.players, player_id, field){
  stopifnot(player_id %in% (.players %>% pull("ID")))
  stopifnot(field %in% colnames(.players))
  .players %>% filter(ID == player_id) %>% pull(field)
}

testthat::test_that("Player getters and setters", {
  verbose <<- FALSE
  players2 <- create_players(player_names = c("a", "b"), starting_balance = 10)
  testthat::expect_equal(players2 %>% get_player_field(0,"name") , "a")
  testthat::expect_equal(players2 %>% get_player_field(1,"balance") , 10)
  testthat::expect_true(players2 %>% get_player_field(1,"alive"))
  testthat::expect_false(players2 %>% get_player_field(1,"get_out_of_jail_card"))
  testthat::expect_error(players2 %>% get_player_field(2, "balance"))
  testthat::expect_error(players2 %>% get_player_field(1, "balanc"))
  testthat::expect_false(players2 %>% 
                           set_player_field(1, "alive", FALSE, method = "set") %>% 
                           get_player_field(1,"alive"))
  testthat::expect_equal(players2 %>% 
                           set_player_field(0, "balance", 5, method = "add") %>% 
                           get_player_field(0, "balance"), 15)
  testthat::expect_error(players2 %>% 
                           set_player_field(0, "balance", 5, method = "mod"))
  testthat::expect_equal(players2 %>% 
                           set_player_field(0, "position", 50, method = "mod", mod = 24) %>% 
                           get_player_field(0, "position"), 2)
  testthat::expect_error(players2 %>% 
                           set_player_field(0, "balance", 5, method = "fail"))
  players4 <- create_players(letters[1:5])
  testthat::expect_equal(players4 %>% get_player_field(3, "name"), "d")
})
# ============ Balance handlers =======
change_balance <- function(.players, player_id, amount){
  .players %>% set_player_field(player_id = player_id, field = "balance", 
                                   value = amount, method = "add")
}
testthat::test_that("Change of balances",{
  players <- create_players()
  testthat::expect_equal(players %>% 
                           change_balance(1,5) %>% 
                           pull(balance), 
                         c(20,25))
  testthat::expect_equal(players %>% 
                           change_balance(0,10) %>% 
                           change_balance(1,-10) %>% 
                           pull(balance), 
                         c(30,10))
})

transfer_balance <- function(.players, player_to, player_from, amount){
  # Moves funds between players, no check for correctness!
  .players %>% 
    change_balance(player_to, amount) %>% 
    change_balance(player_from, -1 * amount)
}

testthat::test_that("Transfers",{
  players <- create_players()
  testthat::expect_equal(players %>% 
                           transfer_balance(0,1,5) %>% 
                           pull(balance), 
                         c(25,15))
  testthat::expect_equal(players %>% 
                           transfer_balance(1,0,10) %>% 
                           transfer_balance(1,0,10) %>% 
                           pull(balance), 
                         c(0,40))
  })


# ============ Board movement =========
make_move <- function(.players, player_id){
  stopifnot(player_id %in% (.players %>% pull(ID)))
  .players %>% set_player_field(player_id = player_id, field = "position", 
                                   value = sample(1:6, 1), method = "mod", 
                                   mod = 24) 
}
testthat::test_that("Player move correctly", {
set.seed(1234)
players <- create_players()
testthat::expect_equal(players %>% make_move(0)%>% pull(position), c(4,0))
testthat::expect_equal(players %>% make_move(0) %>% make_move(0) %>% 
                         make_move(0) %>% make_move(0) %>% make_move(0) %>% 
                         make_move(0) %>% make_move(0) %>% make_move(0) %>% 
                         make_move(1) %>% make_move(1) %>% make_move(1) %>% 
                         make_move(1) %>% pull(position),
                       c(9,16))
testthat::expect_error(players %>% make_move(2))
testthat::expect_error(players %>% make_move("p1"))
})

get_position <- function(.players, player_id){
  .players %>% get_player_field(player_id = player_id, field = "position")
}

# ============ Prison system ==========
imprison <- function(.players, player_id, jail_position = 6){
  if(verbose %in% c("all", "players")){write(paste0("Player ", player_id, " goes to prison!"),"")}
  .players %>% 
    set_player_field(player_id, method = "set", field = "imprisoned", value = TRUE) %>% 
    set_player_field(player_id, method = "set", field = "position", value = jail_position)
}

unprison <- function(.players, player_id){
  if(.players %>% get_player_field(player_id, "get_out_of_jail_card")){ # Use card
    if(verbose %in% c("all", "players")){write(paste0("Releasing player ", player_id, " using card!"),"")}
    .players %<>% 
      set_player_field(player_id, method = "set", field = "imprisoned", value = FALSE) %>%
      set_player_field(player_id, method = "set", field = "get_out_of_jail_card", value = FALSE) 
  } else if((.players %>% get_player_field(player_id, "balance")) > 1){ # Bail your way out...
    if(verbose %in% c("all", "players")){write(paste0("Bailing player ", player_id, " out of prison!"),"")}
    .players %<>% 
      set_player_field(player_id, method = "set", field = "imprisoned", value = FALSE) %>%
      change_balance(player_id, amount = -1L)
  } else {
    if(verbose %in% c("all", "players")){write(paste0("Can't unprison player ", player_id, ", not enough money!"),"")}
  }
  # If player only has 1 money and no card will remain in jail, hoping for rent money!
  # May result in sit. where player remains forever since no lots are owned but this
  # isn't detailed in the rules and rather unlikely...
  return(.players)
}

testthat::test_that("Prison system", {
  verbose <<- FALSE
  players3 <- create_players(letters[1:3], 2) %>% 
    imprison(0) %>% imprison(1) %>% imprison(2) %>% 
    set_player_field(0, method = "set", field = "get_out_of_jail_card", value = TRUE) %>% 
    change_balance(2, -1) %>% 
    unprison(0) %>% unprison(1) %>% unprison(2)
  testthat::expect_true(players3 %>% get_player_field(2, "imprisoned"))
  testthat::expect_equal(players3 %>% get_position(0), 6)
  testthat::expect_false(players3 %>% get_player_field(0, "imprisoned"))
  testthat::expect_false(players3 %>% get_player_field(0, "get_out_of_jail_card"))
  testthat::expect_false(players3 %>% get_player_field(1, "imprisoned"))
  testthat::expect_equal(players3 %>% get_player_field(1, "balance"), 1)
  testthat::expect_true(players3 %>% get_player_field(2, "imprisoned"))
  testthat::expect_equal(players3 %>% get_player_field(2, "balance"), 1)
})



# ============ End game  ==============
end_game_stats <- function(.players, .board, premature_end){
  # ============ Winner calculations ====
  if(exists("end_game_browser")) {browser()}
  winning_balance <- .players %>% pull(balance) %>% max()
  winners <- .players %>% filter(balance == winning_balance) 
  if(nrow(winners) == 1){                                                       # Only one player with max balance
    win_type <- "Balance"
    winner <- winners %>% pull(ID) 
  } else {
    winners_lots <- .board %>% 
      filter(owner %in% (winners %>% pull(ID))) %>% 
      group_by(owner) %>% 
      summarise(lot_count = n(), lot_value = sum(price))
    if(length(which(winners_lots$lot_count==max(winners_lots$lot_count))) == 1){# Player with most lots win
      win_type <- "Count"
      winner <- winners_lots %>% 
        filter(lot_count == max(lot_count)) %>% 
        pull(owner) 
    } else {
      most_valued_owners <- which(winners_lots$lot_value==max(winners_lots$lot_value))
      if(length(most_valued_owners) == 1){                                      # One player with the highest lot value
        win_type <- "Value"
        winner <- winners_lots %>% 
          filter(lot_value == max(lot_value)) %>% 
          pull(owner)
      } else {                                                                  # Worst case sample a winner
        win_type <- "Chance"
        winner <- sample(winners$ID[most_valued_owners], 1)   
      }
    }
  }
  # ============ Looser calculations ====
  loosing_balance <- .players %>% pull(balance) %>% min()
  loosers <- .players %>% filter(balance == loosing_balance)
  if(length(loosers %>% pull(ID)) == 1){                                        # Either loosing on balance...
    looser <- loosers %>% pull(ID)
  } else {                                                                      # or by chance
    looser <- sample(loosers %>% pull(ID), 1)
  }
  # ============ Misc ===================
  all_imprisoned <- .players %>% pull(imprisoned) %>% all() %>% isTRUE()
  # ============ Board calculations =====
  winning_lots <- .board %>% filter(owner == winner) %>% pull(ID)
  loosing_lots <- .board %>% filter(owner %in% looser) %>% pull(ID)
  # ============ Printing ===============
  if(verbose %in% c("all", "players", "end_game")){
    if(premature_end){
      write(paste("Round ended in defeat for player", looser, "!"), "")
    }
    if(all_imprisoned){
      write("All players in prison without bail funds.")
    }
    write(paste("Player", winner, "won with balance",
                winning_balance, "and", length(winning_lots), "owned lots.",
                "\nPlayer", looser, "lost with balance", 
                loosing_balance, "."), "")
  }
  # ============ Return =================
  tibble(winner          = winner,
         winning_balance = winning_balance, 
         winning_lots    = list(winning_lots),
         win_type        = win_type,
         looser          = looser,
         loosing_balance = loosing_balance, 
         loosing_lots    = list(loosing_lots),
         premature_end   = premature_end,
         all_imprisoned  = all_imprisoned
         )
}

testthat::test_that("End game is correct", {
  verbose <<- FALSE
  t_players <- create_players(letters[1:5])
  board <- create_board()
  # this is gonna take a while....
})