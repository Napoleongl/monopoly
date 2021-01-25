# ============ Script args ================
suppressPackageStartupMessages(library("optparse"))

option_list <- list( 
  make_option(c("-i", "--input_dir"), type = "character", default = "data",
              help = "Path to load simulation result from [default %default]",
              dest = "input_dir"),
  make_option(c("-o", "--output_dir"), type = "character", default = "analysis",
              help = "Path to store analysis graphs in [default %default]",
              dest = "output_dir"),
  make_option(c("-p", "--players"), type = "integer", default = 4,
              help = "Number of players in each game [default %default]",
              dest = "nplayers"),
  make_option(c("-g", "--games"), type = "integer", default = 4500,
              help = "Number of games played per core [default %default]",
              dest = "ngames")
)


opt_parser <- OptionParser(option_list=option_list);
opts       <- parse_args(opt_parser);

# ============ Error handling =============
if(!dir.exists(opts$input_dir)){ stop("Invalid input directory")}
data_files <- dir(opts$input_dir, pattern = "games_data[:print:]*",full.names = TRUE)
if(length(data_files) <1){stop("No games_data-files found in input directory")}

save_file <- paste0(opts$input_dir,"/games_data_p",opts$nplayer, "_g", opts$ngames, ".Rdata")
if(!(save_file %in% data_files)){
  stop(paste0("Invalid data requested.\nValid files are:\n  -",
              paste0(data_files,collapse="\n  -")))
  }

if(!dir.exists(opts$output_dir)){dir.create(opts$output_dir)}

# ============ Imports & set up ===========
suppressPackageStartupMessages(library(tidyverse, warn.conflicts = FALSE)) 
suppressPackageStartupMessages(library(magrittr))

options(dplyr.summarise.inform = FALSE)
theme_set(theme_minimal() + 
            theme(axis.title.y = element_text(angle = 0, vjust = 1)))
source("R/board_functions.R")

# ============ Data import ================
load(save_file)
board <- create_board()

players <- games_data %>% 
  select(player = winner) %>%
  union(games_data %>% 
          select(player = looser)) %>% 
  arrange(player) %>% 
  pull(player)
nplayers <- length(players)

player_colours <- setNames(viridisLite::cividis(n = 4, 
                                                begin = 0.01, 
                                                end=0.9, 
                                                direction = 1), 
                           0:3) 
lot_colours <- lot_colours
one_colour <- player_colours[1]
two_colour <- player_colours[1]

# ============ Game length ================
ggplot(games_data) + 
  aes(x = rounds_played) + 
  geom_histogram(fill = one_colour, bins = (max(games_data$rounds_played)-3)) +
  labs(x = "Rounds", y = "No. of\ngames",
       title = "Distribution of game length")


table(games_data$rounds_played)
# ============ Win-loose stats ============
win_lose_stats <- games_data %>% 
  select(winner, looser) %>% 
  pivot_longer(cols=c("winner", "looser"),
               names_to = "outcome", 
               values_to = "player") %>% 
  group_by(outcome, player) %>% 
  summarise(proportion=n()/opts$ngames) %>% 
  mutate(player = factor(player, levels = (nplayers -1):0)) %>% 
  arrange(player)

ggplot(win_lose_stats) + 
  aes(y = outcome, x = proportion, group = player, fill = player, label = round(proportion,2)) +
  geom_col() +
  scale_fill_manual("Player", values = player_colours) +
  geom_label(fill = alpha("grey95",.6), colour = "grey15",position = position_stack(vjust = 0.5)) +
  labs(x = "", y = "", title = "Proportion of outcomes by player")+
  theme(panel.grid.major.y = element_blank())
  
# ============ Win-loose lots =============
loosing_lots <- do.call(c,  games_data$loosing_lots) %>% 
  table() %>% #as.vector %>% 
  enframe(name = "lot_id", value = "lost") %>% 
  mutate(lost = as.double(lost/opts$ngames), lot_id = as.integer(lot_id))
winning_lots <- do.call(c,  games_data$winning_lots) %>% 
  table() %>% #as.vector%>% 
  enframe(name = "lot_id", value = "won") %>% 
  mutate(won = as.double(won/opts$ngames), lot_id = as.integer(lot_id))

lot_stats <- inner_join(loosing_lots, winning_lots, by = "lot_id") %>% 
  inner_join(board %>% select(ID, name, lot_group, x, y), by = c("lot_id" = "ID")) %>% 
  pivot_longer(cols = c("won", "lost"), names_to = "outcome", values_to = "proportion") %>% 
  mutate(outcome = factor(outcome, levels = c("won", "lost")))

ggplot(lot_stats) + 
  aes(x=lot_id, y = proportion, fill = lot_group) +
  geom_col(colour = "grey15", size = rel(0.25)) + 
  facet_grid(cols = vars((outcome))) +
  scale_fill_manual(values = lot_colours)+ 
  #scale_fill_brewer(type = "qual", palette = "Set2")  +
  theme(legend.position = "bottom") +
  labs(title = "Lot ownership by outcome", 
       y = "Proportion\n of games", x = "Lot ID")
  

