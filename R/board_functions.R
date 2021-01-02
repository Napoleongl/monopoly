create_board <- function(){
  tribble(
# Defines the board the game is played on
  ~ID,       ~type,       ~name,~lot_group,     ~price,     ~owner,   ~side,   ~x,  ~y,
# =============== SIDE 1 ===============  
   0L,        "go",        "go", "special",      NULL,        NULL,      1L,   1L,  1L,   
   1L,       "lot",    "lot_a1",       "a",          1,       NULL,      1L,   1L,  2L,   
   2L,       "lot",    "lot_a2",       "a",          1,       NULL,      1L,   1L,  3L,   
   3L,    "chance",   "chance1",  "chance",       NULL,       NULL,      1L,   1L,  4L,   
   4L,       "lot",    "lot_b1",       "b",          1,       NULL,      1L,   1L,  5L,   
   5L,       "lot",    "lot_b2",       "b",          1,       NULL,      1L,   1L,  6L,   
# =============== SIDE 2 ===============         
   6L,    "prison",    "prison", "special",       NULL,       NULL,      2L,   1L,  7L,
   7L,       "lot",    "lot_c1",       "c",          2,       NULL,      2L,   2L,  7L,
   8L,       "lot",    "lot_c2",       "c",          2,       NULL,      2L,   3L,  7L,
   9L,    "chance",   "chance2",  "chance",       NULL,       NULL,      2L,   4L,  7L,
  10L,       "lot",    "lot_d1",       "d",          2,       NULL,      2L,   5L,  7L,
  11L,       "lot",    "lot_d2",       "d",          2,       NULL,      2L,   6L,  7L,
# =============== SIDE 3 ===============       
  12L,      "park",   "parking", "special",       NULL,       NULL,      3L,   7L,  7L,
  13L,       "lot",    "lot_e1",       "e",          3,       NULL,      3L,   7L,  6L,
  14L,       "lot",    "lot_e2",       "e",          3,       NULL,      3L,   7L,  5L,
  15L,    "chance",   "chance3",  "chance",       NULL,       NULL,      3L,   7L,  4L,
  16L,       "lot",    "lot_f1",       "f",          3,       NULL,      3L,   7L,  3L,
  17L,       "lot",    "lot_f2",       "f",          3,       NULL,      3L,   7L,  2L,
# =============== SIDE 4 ===============       
  18L, "to_prison", "to_prison", "special",       NULL,       NULL,      4L,   7L,  1L,
  19L,       "lot",    "lot_g1",       "g",          4,       NULL,      4L,   6L,  1L,
  20L,       "lot",    "lot_g2",       "g",          4,       NULL,      4L,   5L,  1L,
  21L,    "chance",   "chance4",  "chance",       NULL,       NULL,      4L,   4L,  1L,
  22L,       "lot",    "lot_h1",       "h",          5,       NULL,      4L,   3L,  1L,
  23L,       "lot",    "lot_h2",       "h",          5,       NULL,      4L,   2L,  1L
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

buy_lot <- function(player, lot){
  
}
