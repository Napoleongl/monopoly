# ============ Player Creation ========
create_players <- function(player_names = paste0("player_", c(1L, 2L)), 
                           starting_balance = 20){
  tibble(ID = seq.int(0, length(player_names)-1),
         name = player_names, 
         balance = starting_balance,
         position = 0,
         )
}
testthat::test_that("Player creation", {
  players2 <- create_players()
  testthat::expect_equal(dim(players2), c(2,4))
  players3 <- create_players(c("p1", "p2", "p3"),10)
  testthat::expect_length(players3$ID, 3)
  testthat::expect_equal(sum(players3 %>% pull(balance)), 30)
})

# ============ Balance handlers =======
change_balance <- function(.players, player, amount){
  # Sometimes dplyr syntax just complicates things...
  .players[.players$ID == player, "balance"] <- .players[.players$ID == player, "balance"] + amount
  .players
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
chance_card <- function(player){
  return()
}
