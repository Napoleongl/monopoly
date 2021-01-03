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

plot_board <- function(board){
  board_colours <- c(a = "paleturquoise1", b = "green2", c = "firebrick2",
                     d = "gold", e = "palevioletred4", f = "magenta2", 
                     g = "gold3", h ="slateblue2", special = "grey15", chance = "grey95")
  ggplot(board) +
    aes(xmin =x, ymin=y,, ymax =y+1, xmax=x+1, x=x, y=y+1,fill = lot_group, label = name) +
    geom_rect(color = "grey20") +
    geom_label(fill = alpha("grey95",.75), vjust = "top", hjust = "left") +
    scale_fill_manual(values = board_colours) +
    theme_void() +
    theme(legend.position = "none")
}

change_lot_owner <- function(.board, player_id, lot_id){
  stopifnot(.board %>% filter(ID == lot_id) %>% pull(type) == "lot")
  stopifnot(is.numeric(lot_id) & is.numeric(player_id))
  .board %>% mutate(owner = replace(owner, ID == lot_id, player_id))
}
testthat::test_that("Change lot owner", {
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

owns_whole_lot_group <- function(.board, player_id, piece_id){
  current_group <- .board %>% get_board_field(piece_id, "lot_group")
  owners_in_group <- .board %>% filter(lot_group == current_group) %>% pull(owner)
  isTRUE(length(unique(owners_in_group)) == 1)
}
testthat::test_that("Owns entire group", {
  board <- create_board()
  testthat::expect_true(board %>% 
                           change_lot_owner(1, 1) %>% 
                           change_lot_owner(1, 2) %>% 
                           owns_whole_lot_group(1, 1))
  testthat::expect_false(board %>% 
                           change_lot_owner(1, 1) %>% 
                           change_lot_owner(2, 2) %>% 
                           owns_whole_lot_group(1, 1))
  testthat::expect_false(board %>% 
                           change_lot_owner(1, 1) %>% 
                           change_lot_owner(2, 2) %>% 
                           owns_whole_lot_group(2, 1))
})
