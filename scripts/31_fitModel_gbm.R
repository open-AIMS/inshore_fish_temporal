source('../scripts/functions.R')

SAVE_PATHS_ONLY <<- TRUE

## ---- formulas
formulas = list(
    all = ~REGION + NTR.Pooled + LHC + SC + MA + TURF + UC + BR + CMD + SLOPE + RUG + SCI + CHL + KD490 + SSTMEAN + SSTANOM + WAVE + DEPTH + DHW + CYCLONE + EXP,
    all1 = ~NTR.Pooled + LHC + SC + MA + TURF + UC + BR + CMD + SLOPE + RUG + SCI + CHL + KD490 + SSTMEAN + SSTANOM + WAVE + DEPTH + DHW + CYCLONE + EXP,
    Palm = ~ NTR.Pooled + LHC + SC + MA + TURF + UC + BR + CMD + SLOPE + RUG + SCI + CHL + KD490 + SSTMEAN + SSTANOM + WAVE + DEPTH + DHW + CYCLONE + EXP,
    Magnetic = ~ NTR.Pooled + LHC + SC + MA + TURF + UC + BR + CMD + SLOPE + RUG + SCI + CHL + KD490 + SSTMEAN + SSTANOM + WAVE + DEPTH + DHW + CYCLONE + EXP,
    Whitsunday = ~ NTR.Pooled + LHC + SC + MA + TURF + UC + BR + CMD + SLOPE + RUG + SCI + CHL + KD490 + SSTMEAN + SSTANOM + WAVE + DEPTH + DHW + CYCLONE + EXP,
    Keppel = ~ NTR.Pooled + LHC + SC + MA + TURF + UC + BR + CMD + SLOPE + RUG + SCI + CHL + KD490 + SSTMEAN + SSTANOM + WAVE + DEPTH + DHW + CYCLONE + EXP
)

## ----end

## In addition, there is a all1 model that is a full model (all
## regions), yet does not include a Region predictor.  The reason for
## this is that within the gbm, continuous predictors that vary
## between regions will likely be dominated by the Region predictor -
## that is the model will learn regional pattern from the Region
## predictor (as it is a factor and factors tend to be 'win').
## However, the purpose of the analyses is to explore the drivers of
## regional differences and therefore we are less interested in
## indicating that the regions are different than we are in indicating
## the possible drivers of these differences


## ---- prepareDataFit1 part 1
load(file = paste0(DATA_PATH, "processed/fish.RData"))
load(file = paste0(DATA_PATH, "primary/var.lookup.RData"))

responses <- var.lookup %>%
    filter(Type == 'Response') %>%
    pull(Field.name) %>%
    unique()

predictors <- var.lookup %>%
    filter(Type == 'Predictor') %>%
    pull(Field.name) %>%
    unique()

fish.analysis.responses <- fish %>%
    dplyr::select(!!!responses, !!!predictors, YEAR, SITE) %>%
    pivot_longer(cols = c(!!!responses),
                 names_to = "Response",
                 values_to = "Value") %>%
    mutate(Response = factor(Response, levels = unique(Response))) %>%
    group_by(Response) %>%
    nest() %>%
    left_join(var.lookup,
              by = c('Response' = 'Field.name')) 

## ----end

## fish.analysis.responses[3, 'data'][[1]][[1]]

## ---- prepareDataFit1 part 2 
fish.analysis.responses <-
    fish.analysis.responses %>%
    crossing(Model = names(formulas)) %>%
    group_by(Response, Abbreviation, Model) %>%   # group again to gain access to Responses variable within nest
    mutate(data = map2(.x = data, .y = Abbreviation, ~ change_colname_value(.x, .y))) %>% # replace field 'Value' with focal Response
    mutate(data = map(.x = data, ~ replace_colnames_abbrev(.x, var.lookup))) %>% 
    mutate(Form = map(.x = data,
                      .f = ~ applyFormula(Response, Model)
                      )) %>%
    mutate(Groupings = map(.x = data,
                           .f = ~ applyGroupings(x=.x, Response, Model))) %>%
    mutate(Response = factor(Abbreviation, levels = unique(Abbreviation)))
## ----end

## Ideally, it would be nice to have purrr store the result of all models,
## However, even with just a small number of iterations, the resulting
## object is enormous.  The compromise is to have purrr run the models and save the individual models as it goes and only return a path to the model.
## ---- fitGBM1
fish.analysis.responses <-
    fish.analysis.responses %>%
    `[`(1,) %>%
    mutate(GBM = map2(.x = data, .y = Form,
                      .f = ~ fitGBM(data = .x, form = .y,
                                    Response, Model, var.lookup, R=5)
                      ))
save(fish.analysis.responses, file=paste0(DATA_PATH, "modelled/fish.analysis.responses.RData"))
## ----end

## fish.analysis.responses %>% pull(Response) %>% unique
## a <- fish.analysis.responses %>% filter(Response == 'BE')
## data <- a[1,'data'][[1]][[1]]
## form <- a[1,'Form'][[1]][[1]]
## Model = 'all'
## Response = 'BE'

## fish.analysis.responses1[1, ]

## fish.analysis.responses1[1, 'GBM'][[1]][[1]][[1]] %>% summary()
## fish.analysis.responses[1, 'GBM'][[1]][[1]]
## fish.analysis.responses1[1, 'GBM'][[1]][[1]][[2]] %>% summary()

## ---- Rel.infGMB1
load(file=paste0(DATA_PATH, "modelled/fish.analysis.responses.RData"))
fish.analysis.responses <-
    fish.analysis.responses %>%
    mutate(Rel.inf = map(.x = GBM,
                         .f = ~ rel.inf(mods = .x)
                         ))
save(fish.analysis.responses, file = paste0(DATA_PATH, "modelled/fish.analysis.responses_1.RData"))
## ----end

## fish.analysis.responses2[1, ]
## fish.analysis.responses2[1, 'Rel.inf'][[1]][[1]]  

## Rel.inf <- fish.analysis.responses2[1, 'Rel.inf'][[1]][[1]]  

## ---- Rel.inf.PlotGMB1
load(file = paste0(DATA_PATH, "modelled/fish.analysis.responses_1.RData"))
fish.analysis.responses.rel.inf <-
    fish.analysis.responses %>%
    mutate(Rel.inf.plot = map(.x = Rel.inf,
                         .f = ~ rel.inf.plot(Rel.inf = .x, var.lookup = var.lookup)
                         )) %>%
    dplyr::select(Rel.inf, Rel.inf.plot)
save(fish.analysis.responses.rel.inf, file = paste0(DATA_PATH, "modelled/fish.analysis.responses.rel.inf.RData"))

map2(paste0(OUTPUT_PATH, "figures/rel.infl_",fish.analysis.responses.rel.inf$Response,"_",fish.analysis.responses.rel.inf$Model,".pdf"),
     fish.analysis.responses.rel.inf$Rel.inf.plot, ggsave, width = 7, height = 5)
map2(paste0(OUTPUT_PATH, "figures/rel.infl_",fish.analysis.responses.rel.inf$Response,"_",fish.analysis.responses.rel.inf$Model,".png"),
     fish.analysis.responses.rel.inf$Rel.inf.plot, ggsave, width = 7, height = 5, dpi = 72)
map2(paste0(OUTPUT_PATH, "figures/rel.infl_",fish.analysis.responses.rel.inf$Response,"_",fish.analysis.responses.rel.inf$Model,"_large.png"),
     fish.analysis.responses.rel.inf$Rel.inf.plot, ggsave, width = 7, height = 5, dpi = 300)
## ----end

## ---- Variable selection
load(file = paste0(DATA_PATH, "modelled/fish.analysis.responses_1.RData"))
fish.analysis.responses <-
    fish.analysis.responses %>%
    mutate(Variables = map(.x = Rel.inf,
                           .f = ~ variable_selection(Rel.inf = .x)
                           ))
## save variable selection version of the data
save(fish.analysis.responses, file = paste0(DATA_PATH, "modelled/fish.analysis.responses_VS.RData"))
## ----end

