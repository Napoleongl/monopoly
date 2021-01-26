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

if(!dir.exists(opts$output_dir)){dir.create(opts$output_dir)}

# ============ Set up =====================
suppressPackageStartupMessages(library(tidyverse, warn.conflicts = FALSE)) 
suppressPackageStartupMessages(library(magrittr))

options(dplyr.summarise.inform = FALSE)

source("R/board_functions.R")
board <- create_board()

player_colours <- setNames(viridisLite::cividis(n = 4, 
                                                begin = 0.01, 
                                                end=0.9, 
                                                direction = 1), 
                           0:3) 
lot_colours <- lot_colours
one_colour <- player_colours[1]
two_colour <- player_colours[c(1,4)]

theme_set(theme_bw(base_family = "serif") + 
            theme(text = element_text(colour = two_colour[1]),
              axis.title.y = element_text(angle = 0, vjust = 1.05),
              strip.background = element_rect(fill = two_colour[2]),
              panel.grid.minor = element_blank()
              ))
plot_width <- 10
# ============ Data import ================
games <- lapply(2:4, function(nplayers){
  save_file <- paste0(opts$input_dir,"/games_data_p",nplayers, "_g", opts$ngames, ".Rdata")
  load(save_file)
  games_data %>% mutate(players = nplayers)
}) %>% bind_rows() %>% 
  mutate(winner = factor(winner, levels = 3:0),
         looser = factor(looser, levels = 3:0))

# ============ Game length ================
ggsave("images/rounds_played.png", width = plot_width, height = 5, 
       plot = games %>% 
         group_by(players, rounds_played) %>% 
         summarize(count = n()) %>% 
         ggplot() + 
         aes(x = rounds_played, y = count) + 
         geom_col(width  = .5, fill = one_colour) +  
         facet_grid(cols = vars(players)) +
         labs(x = "Rounds", y = "No. of\ngames",
              title = "Distribution of game length by number of players") +
         scale_x_continuous(limits = c(0,100)) 
)

# ============ Win-loose stats ============
win_lose_stats <- games %>% 
  select(players, winner, looser) %>% 
  pivot_longer(cols=c("winner", "looser"),
               names_to = "outcome", 
               values_to = "player") %>% 
  group_by(players, outcome, player) %>% 
  summarise(proportion=n()/opts$ngames) %>% 
  arrange(player)

ggsave("images/player_stats.png", width = plot_width, height = 4, 
       plot = ggplot(win_lose_stats) + 
         aes(y = outcome, x = proportion, group = player, fill = player,
             label = round(proportion,2)) +
         geom_col() +
         scale_fill_manual("Player", values = player_colours) +
         geom_label(fill = alpha("grey95",.6), colour = "grey15",
                    position = position_stack(vjust = 0.5)) +
         labs(x = "", y = "", title = "Proportion of outcomes by player and number of players")+
         theme(panel.grid.major.y = element_blank()) +
         facet_grid(cols = vars(players)) 
)
  
# ============ Win-loose lots =============
lot_stats <- lapply(2:4, function(nplayers){
  games_data <- filter(games, players == nplayers) 
loosing_lots <- do.call(c,  games_data$loosing_lots) %>% 
  table() %>% #as.vector %>% 
  enframe(name = "lot_id", value = "lost") %>% 
  mutate(lost = as.double(lost/opts$ngames), lot_id = as.integer(lot_id))
winning_lots <- do.call(c,  games_data$winning_lots) %>% 
  table() %>% #as.vector%>% 
  enframe(name = "lot_id", value = "won") %>% 
  mutate(won = as.double(won/opts$ngames), lot_id = as.integer(lot_id))

inner_join(loosing_lots, winning_lots, by = "lot_id") %>% 
  inner_join(board %>% select(ID, name, lot_group, x, y), by = c("lot_id" = "ID")) %>% 
  pivot_longer(cols = c("won", "lost"), names_to = "outcome", values_to = "proportion") %>% 
  mutate(outcome = factor(outcome, levels = c("won", "lost")),
         players = nplayers)
}) %>% bind_rows()

ggsave("images/lot_ownership.png", width = plot_width, height = 6,  
       plot = ggplot(lot_stats) + 
         aes(x=lot_id, y = proportion, fill = lot_group) +
         geom_col(colour = "grey15", size = rel(0.25)) + 
         facet_grid(rows = vars(outcome), cols = vars(players), switch = "y") +
         scale_fill_manual("Lot group", values = lot_colours) + 
         scale_y_continuous(limits = c(0,0.5), breaks = seq(0,0.4,0.2), expand = c(0,0)) +
         theme(legend.position = "bottom") +
         labs(title = "Lot ownership by outcome", 
              y = "Proportion\n of games", x = "Lot ID")+ 
         guides(fill=guide_legend(nrow=1,byrow=TRUE))
)

ggsave("images/winning_lot_count.png", width = plot_width, height = 6, 
       plot = games %>% rowwise() %>% 
  mutate(win_lot_count = length(winning_lots)) %>% 
  select(win_lot_count, players, winner) %>% 
  group_by(players, winner, win_lot_count) %>% 
  summarize(count = n()/opts$ngames) %>% 
  ggplot() + 
  aes(x = win_lot_count, y = count) + 
  geom_col(width  = .5, fill = one_colour) +  
  facet_grid(rows = vars(winner), cols = vars(players),switch = "y") +
  labs(x = "No. of lots owned", y = "Player\nID",
       title = "Distribution of number of lots owned by winning player by number of players") +
  scale_x_continuous(limits = c(0,12), breaks = seq(3,12,3), expand = c(0,0)) +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
)

ggsave("images/loosing_lot_count.png", width = plot_width, height = 6,
       plot = games %>% rowwise() %>% 
  mutate(loose_lot_count = length(loosing_lots)) %>%
  select(loose_lot_count, players, looser) %>% 
  group_by(players, looser, loose_lot_count) %>% 
  summarize(count = n()/opts$ngames) %>% 
  ggplot() + 
  aes(x = loose_lot_count, y = count) + 
  geom_col(width  = .5, fill = one_colour) +  
  facet_grid(rows = vars(looser), cols = vars(players),switch = "y") +
  labs(x = "No. of lots owned", y = "Player\nID",
       title = "Distribution of number of lots owned by loosing player by number of players") +
  scale_x_continuous(limits = c(0,12), breaks = seq(3,12,3), expand = c(0,0)) +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
)


