source('../scripts/functions.R')

SAVE_PATHS_ONLY <<- TRUE


load(file = paste0(DATA_PATH, "primary/var.lookup.RData"))



## ---- centred versions
load(file = paste0(DATA_PATH, "modelled/fish.analysis.responses_VS.RData"))
fish.analysis.temporal <-
    fish.analysis.responses %>%
    mutate(data = map2(.x = data, .y = Variables,
                                .f = ~ temporal_centering(data = .x, Variables = .y)
                                ),
           Form = map2(.x = Form, .y = Variables,
                                .f = ~ temporal_form(form = .x, Variables = .y)
                                ))
## ----end

## data <- fish.analysis.temporal[1,'data'][[1]][[1]]
## ## ## Variables <- fish.analysis.responses[1,'Variables'][[1]][[1]]
## ## form <- fish.analysis.temporal[1,'Form'][[1]][[1]]

## data <- fish.analysis.temporal[50,'data'][[1]][[1]]
## ## Variables <- fish.analysis.responses[1,'Variables'][[1]][[1]]
## form <- fish.analysis.temporal[50,'Form'][[1]][[1]]
## Response <- fish.analysis.temporal[50,'Response'][[1]]
## Model <- fish.analysis.temporal[50,'Model'][[1]]

## fish.analysis.temporal %>% pull(Response)
## data <- fish.analysis.temporal[61,'data'][[1]][[1]]
## ## Variables <- fish.analysis.responses[1,'Variables'][[1]][[1]]
## form <- fish.analysis.temporal[61,'Form'][[1]][[1]]
## Response <- fish.analysis.temporal[61,'Response'][[1]]
## Model <- fish.analysis.temporal[61,'Model'][[1]]
## R = 2
## i = 1

## ---- fitGBM1_temporal
fish.analysis.temporal <-
    fish.analysis.temporal %>%
    mutate(GBM = map2(.x = data, .y = Form,
                      .f = ~ fitGBM(data = .x, form = .y,
                                    Response, Model, var.lookup, R=2,
                                    prefix = "temporal.")
                      ))
save(fish.analysis.temporal, file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.RData"))
## ----end



partial_plots <- function(preds, data, var.lookup) {
    nms <- names(preds)
    
    preds[[1]] %>%
        filter(Iteration == 'Pred1') %>%
        ggplot(aes(y = Preds, x = SSTMEAN, group = Iteration, colour = REGION)) +
        geom_line(alpha = 0.5)
    }

## partial plots
## ---- partial_plots
load(file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.RData"))
fish.analysis.temporal <-
    fish.analysis.temporal %>%
    `[`(1,) %>%
    mutate(PartialPredictions = pmap(.l = list(x = GBM, y=data, z = Groupings),
                                     .f = ~ partial_preds(..1,..2,..3, var.lookup, len = 10)))
    
    mutate(PartialPlot = map2(.x = PartialPredictions, .y = data,
                      .f = partial_plots(.x, .y, var.lookup = var.lookup)
                      ))


## ----end

mods <- fish.analysis.temporal[1,'GBM'][[1]][[1]]
data <- fish.analysis.temporal[1,'data'][[1]][[1]]
Form <- fish.analysis.temporal[1,'Form'][[1]][[1]]
groupings <- fish.analysis.temporal[1,'Groupings'][[1]][[1]]

preds <- fish.analysis.temporal[1,'PartialPredictions'][[1]][[1]]
