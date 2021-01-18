# ============ Script args ================
suppressPackageStartupMessages(library("optparse"))

option_list <- list( 
  make_option(c("-c", "--cores"), type = "integer", default = parallel::detectCores()-1,
              help = "Number of cores to use [default %default]",
              dest = "ncores"),
  make_option(c("-", "--rounds"), type = "integer", default = 100,
              help = "Maximum turns for each player before calling it quits. [default %default]",
              dest = "rounds"),
  make_option(c("-p", "--players"), type = "integer", default = 2,
              help = "Number of players in each game [default %default]",
              dest = "nplayers"),
  make_option(c("-g", "--games"), type = "integer", default = 100,
              help = "Number of games played per core [default %default]",
              dest = "ngames")
)

opt_parser <- OptionParser(option_list=option_list);
opts       <- parse_args(opt_parser);
# ============ Set up =====================
library(parallel)

cl <- makeCluster(opts$num_cores)

clusterEvalQ(cl = cl, library(tidyverse))
clusterEvalQ(cl = cl, library(magrittr))
clusterEvalQ(cl = cl, source("R/board_functions.R"))
clusterEvalQ(cl = cl, source("R/player_functions.R"))
clusterEvalQ(cl = cl, source("R/chance_card_functions.R"))
clusterEvalQ(cl = cl, source("R/main.R"))
clusterExport(cl = cl, c("opts"))

# ============ Simulation =================
games <- parLapply(cl = cl, 1L:(opts$ncores * opts$ngames), function(x){
  set.seed(x)
  main(nplayers = opts$nplayers, max_rounds = opts$rounds)
})

games_data <- bind_rows(games)

save(games_data, "data/games_data.rdata")

# error_round_finder  <-FALSE
# error_round <- 999
# end_game_browser <- FALSE
# set.seed(156)
# main(nplayers = 3, verbose = FALSE, max_rounds = 100)
