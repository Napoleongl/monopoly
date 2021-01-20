# ============ Script args ================
suppressPackageStartupMessages(library("optparse"))

option_list <- list( 
  make_option(c("-c", "--cores"), type = "integer", default = parallel::detectCores()-1,
              help = "Number of cores to use [default %default]",
              dest = "ncores"),
  make_option(c("-r", "--rounds"), type = "integer", default = 100,
              help = "Maximum turns for each player before calling it quits. [default %default]",
              dest = "rounds"),
  make_option(c("-p", "--players"), type = "integer", default = 2,
              help = "Number of players in each game [default %default]",
              dest = "nplayers"),
  make_option(c("-g", "--games"), type = "integer", default = 10,
              help = "Number of games played per core [default %default]",
              dest = "ngames"),
  make_option(c("-s", "--savedir"), type = "character", default = "data",
              help = "Path to store simulation result [default %default]",
              dest = "save_dir")
)


opt_parser <- OptionParser(option_list=option_list);
opts       <- parse_args(opt_parser);

# ============ Set up =====================
suppressPackageStartupMessages(library(pbapply))
suppressPackageStartupMessages(library(tidyverse, warn.conflicts = FALSE))
suppressPackageStartupMessages(library(magrittr))

options(dplyr.summarise.inform = FALSE)
pbo = pboptions(type="timer", char = "=", label = "Simulation", style = 3)

source("R/board_functions.R")
source("R/player_functions.R")
source("R/chance_card_functions.R")
source("R/main.R")

# ============ Simulation =================
total_games <- opts$ncores * opts$ngames
write(paste("Simulating", total_games, "games with", opts$nplayers, 
            "players using", opts$ncores, "threads."),
      "")

games <- pblapply(X = 1L:total_games, cl = opts$ncores, FUN = function(x){
                    set.seed(x)
                    main(nplayers = opts$nplayers, max_rounds = opts$rounds)
                  })

games_data <- bind_rows(games)

if(!dir.exists(opts$save_dir)){dir.create(opts$save_dir)}
save_file <- paste0(opts$save_dir,"/games_data_p",opts$nplayer, "_g", total_games, ".Rdata")
save_opts <- opts # as to not overwrite analysis opts when re-loaded
save(games_data, save_opts, file = save_file)
write(paste0("Simulation done, results saved to ", getwd(), "/", save_file),
      "")
# ============ Finished ===================
