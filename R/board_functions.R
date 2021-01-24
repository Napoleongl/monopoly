create_board <- function(){
  tribble(
# Defines the board the game is played on
  ~ID,       ~type,       ~name,~lot_group,   ~price,  ~owner,   ~side,   ~x,  ~y,
#====|============|=SIDE 1=====|==========|=========|=========|========|=====|====|  
   0L,        "go",        "go", "special",       NA,       NA,      1L,   1L,  1L,   
   1L,       "lot",    "lot_a1",       "a",        1,       NA,      1L,   1L,  2L,   
   2L,       "lot",    "lot_a2",       "a",        1,       NA,      1L,   1L,  3L,   
   3L,    "chance",   "chance1",  "chance",       NA,       NA,      1L,   1L,  4L,   
   4L,       "lot",    "lot_b1",       "b",        1,       NA,      1L,   1L,  5L,   
   5L,       "lot",    "lot_b2",       "b",        1,       NA,      1L,   1L,  6L,   
#====|===========|=SIDE 2======|==========|=========|=========|========|=====|====|
   6L,    "prison",    "prison", "special",       NA,       NA,      2L,   1L,  7L,
   7L,       "lot",    "lot_c1",       "c",        2,       NA,      2L,   2L,  7L,
   8L,       "lot",    "lot_c2",       "c",        2,       NA,      2L,   3L,  7L,
   9L,    "chance",   "chance2",  "chance",       NA,       NA,      2L,   4L,  7L,
  10L,       "lot",    "lot_d1",       "d",        2,       NA,      2L,   5L,  7L,
  11L,       "lot",    "lot_d2",       "d",        2,       NA,      2L,   6L,  7L,
#====|===========|=SIDE 3======|==========|=========|=========|========|=====|====|
  12L,      "park",   "parking", "special",       NA,       NA,      3L,   7L,  7L,
  13L,       "lot",    "lot_e1",       "e",        3,       NA,      3L,   7L,  6L,
  14L,       "lot",    "lot_e2",       "e",        3,       NA,      3L,   7L,  5L,
  15L,    "chance",   "chance3",  "chance",       NA,       NA,      3L,   7L,  4L,
  16L,       "lot",    "lot_f1",       "f",        3,       NA,      3L,   7L,  3L,
  17L,       "lot",    "lot_f2",       "f",        3,       NA,      3L,   7L,  2L,
#====|===========|=SIDE 4======|==========|=========|=========|========|=====|====|
  18L, "to_prison", "to_prison", "special",       NA,       NA,      4L,   7L,  1L,
  19L,       "lot",    "lot_g1",       "g",        4,       NA,      4L,   6L,  1L,
  20L,       "lot",    "lot_g2",       "g",        4,       NA,      4L,   5L,  1L,
  21L,    "chance",   "chance4",  "chance",       NA,       NA,      4L,   4L,  1L,
  22L,       "lot",    "lot_h1",       "h",        5,       NA,      4L,   3L,  1L,
  23L,       "lot",    "lot_h2",       "h",        5,       NA,      4L,   2L,  1L
#====|============|============|==========|=========|=========|========|=====|====|
  )
}

lot_colours <- setNames(c("paleturquoise1", "paleturquoise3",
                          "yellow2", "yellow3",
                          "darkseagreen1", "darkseagreen4", 
                          "mediumorchid1", "darkorchid3",
                          "grey15", "grey95"),
                        c(letters[1:8], "special", "chance"))

plot_board <- function(board){
  ggplot(board) +
    aes(xmin =x, ymin=y,, ymax =y+1, xmax=x+1, x=x, y=y+1,fill = lot_group, label = name) +
    geom_rect(color = "grey20") +
    geom_label(fill = alpha("grey95",.75), vjust = "top", hjust = "left") +
    scale_fill_manual(values = lot_colours) +
    theme_void() +
    theme(legend.position = "none")
}

change_lot_owner <- function(.board, player_id, lot_id){
  stopifnot(.board %>% filter(ID == lot_id) %>% pull(type) == "lot")
  stopifnot(is.numeric(lot_id) & is.numeric(player_id))
  if(verbose %in% c("all", "board")){
    write(paste("Lot", lot_id, "now owned by player", player_id),"")
  }
  .board %>% mutate(owner = replace(owner, ID == lot_id, player_id))
}
testthat::test_that("Change lot owner", {
  verbose <<- FALSE
  board <- create_board()
  testthat::expect_equal(board %>% 
                           change_lot_owner(1, 1) %>% 
                           pull(owner), 
                         c(  NA, 1, rep(  NA,22)))
  testthat::expect_error(board %>% 
                           change_lot_owner(1, 3))
  testthat::expect_error(board %>% 
                           change_lot_owner("p1", 3))
  testthat::expect_error(board %>% 
                           change_lot_owner(1, "lot_a1"))
})

get_board_field <- function(.board, piece_id, field){
  .board %>% filter(ID == piece_id) %>% pull(field)
}

owns_whole_lot_group <- function(.board, pieces){
  sapply(pieces, function(piece_id){
  current_group <- .board %>% get_board_field(piece_id, "lot_group")
  owners_in_group <- .board %>% filter(lot_group == current_group) %>% pull(owner)
  isTRUE(length(unique(owners_in_group)) == 1)
  })
}
testthat::test_that("Owns entire group", {
  board <- create_board()
  testthat::expect_true(board %>% 
                           change_lot_owner(1, 1) %>% 
                           change_lot_owner(1, 2) %>% 
                           owns_whole_lot_group(1))
  testthat::expect_false(board %>% 
                           change_lot_owner(1, 1) %>% 
                           change_lot_owner(2, 2) %>% 
                           owns_whole_lot_group(1))
  testthat::expect_false(board %>% 
                           change_lot_owner(1, 1) %>% 
                           change_lot_owner(2, 2) %>% 
                           owns_whole_lot_group(1))
  testthat::expect_equal(board %>% 
                           change_lot_owner(1, 1) %>% 
                           change_lot_owner(1, 2) %>% 
                           change_lot_owner(1, 7) %>% 
                           change_lot_owner(2, 4) %>% 
                           change_lot_owner(2, 5) %>% 
                           change_lot_owner(2, 8) %>% 
                           owns_whole_lot_group(c(1,4,7)), 
                         c(TRUE, TRUE, FALSE))
})

lot_buy_or_pay <- function(.players, .board, player_id, lot_id, lot_price = NULL){
  position_owner <- .board %>% get_board_field(lot_id, "owner")
  game_alive <- TRUE
  if(is.na(position_owner)){                                                    # Empty buyable lot
    if(is.null(lot_price)){
      lot_price <- .board %>% get_board_field(lot_id, "price")
    } 
    if(get_player_field(.players, player_id, "balance") > lot_price){
      .board %<>% change_lot_owner(player_id, lot_id)
      .players %<>% change_balance(player_id, -1 * lot_price) 
    } else {                                                                    # Needs balance > 1 after buying lot
      if(verbose %in% c("all", "game")){
        write(paste(player_id, "looses due to insufficient funds"),"")
      }
      game_alive <- FALSE
    } 
  } else if(position_owner == player_id){                                       # Own lot - End turn
  } else if(position_owner != player_id){                                       # Lot owned buy someone else - pay rent!
    double_rent <- .board %>% owns_whole_lot_group(lot_id)
    lot_rent <- .board %>% get_board_field(lot_id, "price")* (1 + double_rent)
    if(get_player_field(.players, player_id, "balance") > lot_rent){
      .players %<>% transfer_balance(position_owner, player_id, lot_rent)
    } else {                                                                    # Needs balance > 1 after paying rent
      if(verbose %in% c("all", "game")){
        write(paste(player_id, "looses due to insufficient funds"),"")
      }
      game_alive <- FALSE
    }
  } else {
    stop("Lot owner not in (NA, current player or other player)!!!")            # Shouldn't happen...
  }
  return(list(players = .players, board = .board, game_alive = game_alive))
}
