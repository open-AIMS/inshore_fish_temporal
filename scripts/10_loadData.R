source('../scripts/functions.R')

## ---- loadData
fish <- read.csv(paste0(DATA_PATH, "primary/Selected fish benthic physical sitelevel 2021.csv"),
                 strip.white = TRUE)
## ----end

## ---- glimpseData
fish %>% glimpse()
save(fish, file = paste0(DATA_PATH, "primary/fish.RData"))
## ----end
