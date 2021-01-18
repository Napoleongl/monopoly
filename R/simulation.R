# ============ Imports ====================
library(parallel)

error_round_finder  <-FALSE
error_round <- 999
end_game_browser <- FALSE
verbose <- FALSE

cl <- makeCluster(getOption("cl.cores", 4) - 1)

clusterEvalQ(cl = cl, library(tidyverse))
clusterEvalQ(cl = cl, library(magrittr))
clusterEvalQ(cl = cl, source("R/board_functions.R"))
clusterEvalQ(cl = cl, source("R/player_functions.R"))
clusterEvalQ(cl = cl, source("R/chance_card_functions.R"))
clusterEvalQ(cl = cl, source("R/main.R"))
clusterExport(cl = cl, c("error_round_finder", "error_round", "end_game_browser", "verbose"))


games <- parLapply(cl = cl, 1:300, function(x){
  print(x)
  set.seed(x)
  main(nplayers = 3, verbose = FALSE, max_rounds = 100)
})

games_data <- bind_rows(games)

# set.seed(156)
# main(nplayers = 3, verbose = FALSE, max_rounds = 100)
