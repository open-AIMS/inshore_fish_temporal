source('../scripts/functions.R')

SAVE_PATHS_ONLY <<- TRUE


load(file = paste0(DATA_PATH, "primary/var.lookup.RData"))



## ---- centred versions
load(file = paste0(DATA_PATH, "modelled/fish.analysis.responses_VS.RData"))
fish.analysis.temporal <-
    fish.analysis.responses %>%
    mutate(data = map2(.x = data, .y = Variables,
                                .f = ~ temporal_centering(data = .x, Variables = .y)
                                )#,
           ## Form = map2(.x = Form, .y = Variables,
           ##                      .f = ~ temporal_form(form = .x, Variables = .y))
           )
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
                                    Response, Model, var.lookup, R=100,
                                    prefix = "temporal.")
                      ))
save(fish.analysis.temporal, file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.RData"))
## ----end

## ---- fitBBM1_temporal Rel Inf
load(file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.RData"))
fish.analysis.temporal <-
  fish.analysis.temporal %>%
  ## filter(Response == 'PL', Model == 'Magnetic') %>% 
  mutate(
      Rel.inf = map(
          .x = GBM,
          ## .f = ~ rel.inf(mods = .x)
          .f = ~ {print(head(.x,1));rel.inf(mods = .x); }
      ),
      Rel.inf.sum = map(.x = Rel.inf,
                        .f = ~ rel.inf.sum(l = .x))
    )
save(fish.analysis.temporal, file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.relinf.RData"))
## ----end

## ---- fitGBM1_temporal Rel Inf plot
load(file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.relinf.RData"))
fish.analysis.temporal.rel.inf <-
  fish.analysis.temporal %>%
  mutate(Rel.inf.plot = map(
    .x = Rel.inf.sum,
    .f = ~ rel.inf.plot(Rel.inf = .x, var.lookup = var.lookup)
  )) %>%
  dplyr::select(Rel.inf, Rel.inf.plot)
save(fish.analysis.temporal.rel.inf, file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.rel.inf.RData"))
## ----end

## Partial plots
## ---- partial_predictions
load(file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.RData"))
num_ticks <- fish.analysis.temporal %>%
    dplyr::select(Response, Model) %>%
    distinct() %>%
    nrow()
    
pb <- progress::progress_bar$new(format = "[:bar] :current/:total (:percent) elapsed :elapsed eta :eta",
                                 total = num_ticks)
fish.analysis.temporal.predictions <-
    fish.analysis.temporal %>%
    mutate(PartialPredictions = pmap(.l = list(x = GBM, y=data, z = Groupings),
                                     .f = ~ partial_preds(..1,..2,..3, var.lookup, len = 100)))
save(fish.analysis.temporal.predictions,
     file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.predictions.RData"))
## ----end


## ---- partial_plots
load(file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.predictions.RData"))
num_ticks <- fish.analysis.temporal %>%
    dplyr::select(Response, Model) %>%
    distinct() %>%
    nrow()
    
pb <- progress::progress_bar$new(format = "[:bar] :current/:total (:percent) elapsed :elapsed eta :eta",
                                 total = num_ticks)
fish.analysis.temporal.plots <- 
    fish.analysis.temporal.predictions %>%
    mutate(PartialPlot = pmap(.l = list(x = PartialPredictions, y = data, z = Groupings),
                              .f = ~ partial_plots(..1, ..2, ..3, var.lookup = var.lookup)
                              ))
save(fish.analysis.temporal.plots, file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.plots.RData"))
## ----end

## ---- R2
load(file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.RData"))
num_ticks <- fish.analysis.temporal %>%
    dplyr::select(Response, Model) %>%
    distinct() %>%
    nrow()
pb <- progress::progress_bar$new(format = "[:bar] :current/:total (:percent) elapsed :elapsed eta :eta",
                                 total = num_ticks)
## In the below:
## Preds = covariates get NA values
## Preds2 = covariates get mean values
## Preds3 = covariates get observed values 
fish.analysis.temporal.R2 <-
    fish.analysis.temporal %>%
    mutate(PartialFits = pmap(.l = list(x = GBM, y=data),
                                     .f = ~ partial_fit(..1,..2, var.lookup)))

## method == 1: covariates with NA and 1 - (var(P - O)/var(O))
## method == 2: covariates with mean values and 1 - (var(P - O)/var(O))
## method == 2: covariates with observed values and 1 - (var(P - O)/var(O)) then multiply by rel.inf
R2 <- function(dat, method = 1) {
    switch(method,
           d <- dat %>% map(.f = ~.x %>% group_by(Iteration, DV) %>%
                               summarise(R2 = 1 - (var(Preds - Resp)/var(Resp))) %>%
                               mutate(Method = 1)
                           ) %>%
               suppressMessages(),
           d <- dat %>% map(.f = ~.x %>% group_by(Iteration, DV) %>%
                               summarise(R2 = 1 - (var(Preds2 - Resp)/var(Resp))) %>%
                               mutate(Method = 2)
                           ) %>% suppressMessages(),
           d <- dat %>% map(.f = ~.x %>% group_by(Iteration, DV) %>%
                               summarise(R2 = 1 - (var(Preds3 - Resp)/var(Resp))) %>%
                               mutate(Method = 3)
                           ) %>% suppressMessages()
           )
           
    ## case_when(
    ##     method == 1 ~ 
    ##         dat %>% map(.f = ~.x %>% group_by(Iteration, DV) %>%
    ##                         summarise(R2 = 1 - (var(Preds - Resp)/var(Resp))) %>%
    ##                         mutate(Method = 1)
    ##                     ) %>%
    ##             suppressMessages()
    ##     ,
    ##     method == 2 ~ {
    ##         dat %>% map(.f = ~.x %>% group_by(Iteration, DV) %>%
    ##                         summarise(R2 = 1 - (var(Preds2 - Resp)/var(Resp))) %>%
    ##                         mutate(Method = 2)
    ##                     ) %>% suppressMessages()
    ##         },
    ##     method == 3 ~ {
    ##         dat %>% map(.f = ~.x %>% group_by(Iteration, DV) %>%
    ##                         summarise(R2 = 1 - (var(Preds3 - Resp)/var(Resp))) %>%
    ##                         mutate(Method = 3)
    ##                     ) %>% suppressMessages()
    ##         }
    ## )
    return(d)
}

fish.analysis.temporal.R2 <- fish.analysis.temporal.R2 %>%
    mutate(
        R2 = map(.x = PartialFits,
                 .f = ~ R2(.x, method = 1)),
        R2.2 = map(.x = PartialFits,
                 .f = ~ R2(.x, method = 2)),
        R2.3 = map(.x = PartialFits,
                 .f = ~ R2(.x, method = 3)),
        R2tab = map(.x = R2,
                    .f = ~ .x %>%
                        map2_df(.y = names(.x), .f = ~ .x %>%
                                ungroup() %>%
                                    summarise(across(R2, list(!!!quantile_map), .names = "{.fn}")) %>%
                                    mutate(DV = .y))),
        R2tab.2 = map(.x = R2.2,
                    .f = ~ .x %>%
                        map2_df(.y = names(.x), .f = ~ .x %>%
                                ungroup() %>%
                                    summarise(across(R2, list(!!!quantile_map), .names = "{.fn}")) %>%
                                    mutate(DV = .y))),
        R2tab.3 = map(.x = R2.3,
                    .f = ~ .x %>%
                        map2_df(.y = names(.x), .f = ~ .x %>%
                                ungroup() %>%
                                    summarise(across(R2, list(!!!quantile_map), .names = "{.fn}")) %>%
                                    mutate(DV = .y)))
    )
save(fish.analysis.temporal.R2, file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.R2.RData"))
## ----end

## ---- compilation plots
load(file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.plots.RData"))
load(file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.rel.inf.RData"))

pmap(.l = list(paste0(OUTPUT_PATH, "figures/partial_plots_",
                 fish.analysis.temporal.plots$Response, "_", fish.analysis.temporal.plots$Model, ".pdf"),
               fish.analysis.temporal.plots$PartialPlot,
               fish.analysis.temporal.rel.inf$Rel.inf.plot),
     .f = ~ partial_plot_compilations(path=..1, g=..2, r=..3, ncol = 3)
     )

pmap(.l = list(paste0(OUTPUT_PATH, "figures/partial_plots_",
                 fish.analysis.temporal.plots$Response, "_", fish.analysis.temporal.plots$Model, ".png"),
               fish.analysis.temporal.plots$PartialPlot,
               fish.analysis.temporal.rel.inf$Rel.inf.plot),
     .f = ~ partial_plot_compilations(path=..1, g=..2, r=..3, ncol = 3, dpi = 75)
     )
## map2(.x = paste0(OUTPUT_PATH, "figures/partial_plots_",
##                  fish.analysis.temporal.plots$Response, "_", fish.analysis.temporal.plots$Model, ".pdf"),
##      .y = fish.analysis.temporal.plots$PartialPlot,
##      ~ partial_plot_compilations(path=.x, g=.y, ncol = 3)
##      )

## ----end


## fish.analysis.temporal
## a <- fish.analysis.temporal.plots[1,'PartialPlot'][[1]][[1]] %>%
##     ## lapply(function(x) x + guides(fill = 'none')) %>%
##     wrap_plots()  + guide_area() + plot_layout(guides = 'collect', ncol = 3) 
## a$patches

## a <- fish.analysis.temporal.plots[1,'PartialPlot'][[1]][[1]]
## fish.analysis.temporal.plots[1,'PartialPlot'][[1]][[1]][[1]]
## fish.analysis.temporal.plots[1,'PartialPlot'][[1]][[1]][[2]]
## fish.analysis.temporal.plots[1,'PartialPlot'][[1]][[1]] %>%
##     ## lapply(function(x) x + guides(fill = 'none')) %>%
##     wrap_plots() + guide_area() + plot_layout(guides = 'collect', ncol = 3) &
##     guides(
##         fill = "none",
##         colour = guide_legend(override.aes = list(shape = NA, size = 0.7)))
## dev.off()
## fish.analysis.temporal.plots[1,'PartialPlot'][[1]][[1]][[7]]
## fish.analysis.temporal.plots[1,'PartialPlot'][[1]][[1]] %>% length()

## fish.analysis.temporal.plots[2,'PartialPlot'][[1]][[1]] %>% length()
## fish.analysis.temporal.plots[2,'PartialPlot'][[1]][[1]][1:4] %>% wrap_plots()

## fish.analysis.temporal.plots[3,'PartialPlot'][[1]][[1]] %>% length()
## fish.analysis.temporal.plots[3,'PartialPlot'][[1]][[1]][1:8] %>% wrap_plots()

## ## fish.analysis.temporal[3,'data'][[1]][[1]] %>% as.data.frame %>%  head
## ## fish.analysis.temporal[3,'Groupings'][[1]][[1]] %>% as.data.frame %>%  head
## ## fish.analysis.temporal[3,'Form'][[1]][[1]] %>% as.data.frame %>%  head


## mods <- fish.analysis.temporal[1,'GBM'][[1]][[1]]
## data <- fish.analysis.temporal[1,'data'][[1]][[1]]
## Form <- fish.analysis.temporal[1,'Form'][[1]][[1]]
## groupings <- fish.analysis.temporal[1,'Groupings'][[1]][[1]]

## preds <- fish.analysis.temporal[1,'PartialPredictions'][[1]][[1]]



## fish.analysis.temporal.plots[1,'PartialPlot'][[1]][[1]] %>%
##     wrap_plots() +
##     guide_area() + plot_layout(guides = 'collect') &
##     guides(
##         fill = "none",
##         colour = guide_legend(override.aes = list(shape = NA, size = 0.7)))
## dev.off()
