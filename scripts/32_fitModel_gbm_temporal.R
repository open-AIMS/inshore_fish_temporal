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



## partial plots
## ---- partial_plots
load(file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.RData"))
num_ticks <- fish.analysis.temporal %>%
    dplyr::select(Response, Model) %>%
    distinct() %>%
    nrow() %>%
    `*`(2)
pb <- progress::progress_bar$new(format = "[:bar] :current/:total (:percent) elapsed :elapsed eta :eta",
                                 total = num_ticks)
fish.analysis.temporal.plots <-
    fish.analysis.temporal %>%
    mutate(PartialPredictions = pmap(.l = list(x = GBM, y=data, z = Groupings),
                                     .f = ~ partial_preds(..1,..2,..3, var.lookup, len = 100))) %>%
    mutate(PartialPlot = pmap(.l = list(x = PartialPredictions, y = data, z = Groupings),
                              .f = ~ partial_plots(..1, ..2, ..3, var.lookup = var.lookup)
                              ))


save(fish.analysis.temporal.plots, file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.plots.RData"))
## ----end

fish.analysis.temporal.plots[1,'PartialPlot'][[1]][[1]][[1]]
fish.analysis.temporal.plots[1,'PartialPlot'][[1]][[1]][[2]]
fish.analysis.temporal.plots[1,'PartialPlot'][[1]][[1]][1:6] %>% wrap_plots()
fish.analysis.temporal.plots[1,'PartialPlot'][[1]][[1]][[7]]
fish.analysis.temporal.plots[1,'PartialPlot'][[1]][[1]] %>% length()

fish.analysis.temporal.plots[2,'PartialPlot'][[1]][[1]] %>% length()
fish.analysis.temporal.plots[2,'PartialPlot'][[1]][[1]][1:4] %>% wrap_plots()

fish.analysis.temporal.plots[3,'PartialPlot'][[1]][[1]] %>% length()
fish.analysis.temporal.plots[3,'PartialPlot'][[1]][[1]][1:8] %>% wrap_plots()

## fish.analysis.temporal[3,'data'][[1]][[1]] %>% as.data.frame %>%  head
## fish.analysis.temporal[3,'Groupings'][[1]][[1]] %>% as.data.frame %>%  head
## fish.analysis.temporal[3,'Form'][[1]][[1]] %>% as.data.frame %>%  head


mods <- fish.analysis.temporal[1,'GBM'][[1]][[1]]
data <- fish.analysis.temporal[1,'data'][[1]][[1]]
Form <- fish.analysis.temporal[1,'Form'][[1]][[1]]
groupings <- fish.analysis.temporal[1,'Groupings'][[1]][[1]]

preds <- fish.analysis.temporal[1,'PartialPredictions'][[1]][[1]]
