source('../scripts/functions.R')

## ---- loadData
fish <- read.csv(paste0(DATA_PATH, "primary/Selected fish benthic physical sitelevel 2021.csv"),
                 strip.white = TRUE)
pc <- read.csv(paste0(DATA_PATH, "primary/Inshore fish temporal PCA scores.csv"),
                 strip.white = TRUE)
## ----end

## ---- glimpseData
fish %>% glimpse()
## ----end

## ---- joinFishPC
pc <- pc %>%
    rename(PCO1 = PC1, PCO2 = PC2, PCO3 = PC3, PCO4 = PC4, PCO5 = PC5,
           REGION = Region) %>%
    extract(YearSite, into = c('YEAR','SITE'), regex = "([0-9]{4})(.*)") %>%
    mutate(YEAR = as.integer(as.character(YEAR)))

fish <- fish %>%
    full_join(pc)
## ----end

## ---- savePrimary
save(fish, file = paste0(DATA_PATH, "primary/fish.RData"))
## ----end

