# ============ Player basics ========
create_players <- function(player_names = paste0("player_", c(1L, 2L)), 
                           starting_balance = 20L){
  tibble(ID = seq.int(0, length(player_names)-1),
         name = player_names, 
         balance = starting_balance,
         position = 0L,
         )
}
testthat::test_that("Player creation", {
  players2 <- create_players()
  testthat::expect_equal(dim(players2), c(2,4))
  players3 <- create_players(c("p1", "p2", "p3"),10)
  testthat::expect_length(players3$ID, 3)
  testthat::expect_equal(sum(players3 %>% pull(balance)), 30)
})

set_player_field <- function(.players, player_id, field, value, method = "add", mod = 0L){
  # Sometimes dplyr syntax just complicates things...
  if(method == "add") {
    .players[.players$ID == player_id, field] <- .players[.players$ID == player_id, field] + value
  } else if(method == "mod"){
    .players[.players$ID == player_id, field] <- (.players[.players$ID == player_id, field] + value) %% mod
  } else if(method == "set"){
    .players[.players$ID == player_id, field] <- value
  }
  .players
}

get_player_field <- function(.players, player_id, field){
  .players %>% filter(ID == player_id) %>% pull(field)
}

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

# ============ Chance cards ===========
chance_card <- function(player_id){
  return()
}

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

imprison <- function(.players, player_id){
  # TODO
  return()
}
