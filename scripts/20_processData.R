source('../scripts/functions.R')


load(file = paste0(DATA_PATH, "primary/fish.RData"))

## In the fish spatial data, there was a field called
## Exposure.to.primary.weeks In the current data, this field is
## absent, and in its place is a field called Exposure.to.turbidity.  I will assume that they are the same and thus alter the name of Exposure.to.turbidity to Exposure.to.primary.weeks

## There appears to be approx 100 additional empty rows at the bottom
## of the data.  I suspect that this is a spreadsheet issue.  I will
## strip them out.
## ---- standardiseFieldNames
fish <- fish %>%
    rename(Exposure.to.primary.weeks = Exposure.to.turbidity) %>%
    dplyr::select(!matches('^X$|^X\\.[0-9]$')) %>%
    filter(!is.na(YEAR)) %>%
    droplevels()
## ----end


## ---- nameLookup
var.lookup = rbind(
    data.frame(pretty.name='Total density', Field.name='Total.fish.density', Abbreviation='TFD', Family='gaussian', Type='Response', Transform='log', Groupby=''),
    data.frame(pretty.name='Total species richness', Field.name='Total.fish.species.richness', Abbreviation='TFSR', Family='gaussian', Type='Response', Transform='I', Groupby=''),
    data.frame(pretty.name='Benthic invertivores', Field.name='BE', Abbreviation='BE', Family='gaussian', Type='Response', Transform='log', Groupby=''),
    data.frame(pretty.name='Grazers', Field.name='GRAZ', Abbreviation='GRAZ', Family='gaussian', Type='Response', Transform='log', Groupby=''),
    data.frame(pretty.name='Grazers2', Field.name='GRAZ2', Abbreviation='GRAZ2', Family='gaussian', Type='Response', Transform='log', Groupby=''),
    data.frame(pretty.name='Parrot', Field.name='Parrot', Abbreviation='PA', Family='gaussian', Type='Response', Transform='log', Groupby=''),
    data.frame(pretty.name='Corallivores', Field.name='COR', Abbreviation='COR', Family='gaussian', Type='Response', Transform='log', Groupby=''),
    data.frame(pretty.name='Omnivores', Field.name='OM', Abbreviation='OM', Family='gaussian', Type='Response', Transform='log', Groupby=''),
    data.frame(pretty.name='Planktivores', Field.name='PL', Abbreviation='PL', Family='gaussian', Type='Response', Transform='log', Groupby=''),
    data.frame(pretty.name='Carnivores', Field.name='CA', Abbreviation='CA', Family='gaussian', Type='Response', Transform='log', Groupby=''),
    data.frame(pretty.name='Piscivores', Field.name='PI', Abbreviation='PI', Family='gaussian', Type='Response', Transform='log', Groupby=''),
    data.frame(pretty.name='Farmers', Field.name='FA', Abbreviation='FA', Family='gaussian', Type='Response', Transform='log', Groupby=''),
    data.frame(pretty.name='Plectropomus total density', Field.name='Plectropomus.total.density', Abbreviation='PTD', Family='gaussian', Type='Response', Transform='log', Groupby=''),
    data.frame(pretty.name='Plectropomus total biomass', Field.name='Plectropomus.total.biomass', Abbreviation='PTB', Family='gaussian', Type='Response', Transform='log', Groupby=''),
    data.frame(pretty.name='Plectropomus legal density', Field.name='Plectropomus.legal.density', Abbreviation='PLD', Family='gaussian', Type='Response', Transform='log', Groupby=''),
    data.frame(pretty.name='Plectropomus legal biomass', Field.name='Plectropomus.legal.biomass', Abbreviation='PLB', Family='gaussian', Type='Response', Transform='log', Groupby=''),
  data.frame(pretty.name='PCO1',  Field.name='PCO1', Abbreviation='PCO1', Family='gaussian', Type='Response', Transform='I', Groupby=''), 

  data.frame(pretty.name='Region', Field.name='REGION', Abbreviation='REGION', Family=NA, Type='Predictor', Transform='I', Groupby=''),
    data.frame(pretty.name='Zone', Field.name='NTR.Pooled', Abbreviation='NTR.Pooled', Family=NA, Type='Predictor', Transform='I', Groupby='Region'),
    data.frame(pretty.name='% hard coral', Field.name='LHC_.', Abbreviation='LHC', Family=NA, Type='Predictor', Transform='I', Groupby='Region'),
    data.frame(pretty.name='% soft coral', Field.name='SC_.', Abbreviation='SC', Family=NA, Type='Predictor', Transform='I', Groupby='Region'),
    data.frame(pretty.name='% macroalgae', Field.name='MAC_.', Abbreviation='MA', Family=NA, Type='Predictor', Transform='I', Groupby='Region'),
    data.frame(pretty.name='% turf', Field.name='Turf_.', Abbreviation='TURF', Family=NA, Type='Predictor', Transform='I', Groupby='Region'),
    data.frame(pretty.name='% unconsolidated', Field.name='Unconsolidated_.', Abbreviation='UC', Family=NA, Type='Predictor', Transform='I', Groupby='Region'),
    data.frame(pretty.name='Benthic richness', Field.name='Benthic.richness', Abbreviation='BR', Family=NA, Type='Predictor', Transform='I', Groupby='Region'),
    data.frame(pretty.name='Coral morphological diversity', Field.name='Coral_Morph.Diversity', Abbreviation='CMD', Family=NA, Type='Predictor', Transform='I', Groupby='Region'),
    data.frame(pretty.name='Slope', Field.name='slope', Abbreviation='SLOPE', Family=NA, Type='Predictor', Transform='I', Groupby='Region'),
    data.frame(pretty.name='Rugosity', Field.name='rugosity', Abbreviation='RUG', Family=NA, Type='Predictor', Transform='I', Groupby='Region'),
    data.frame(pretty.name='SCI (structural complexity)', Field.name='SCI', Abbreviation='SCI', Family=NA, Type='Predictor', Transform='I', Groupby='Region'),
    data.frame(pretty.name='Chla', Field.name='ChlA', Abbreviation='CHL', Family=NA, Type='Predictor', Transform='I', Groupby='Region'),
    data.frame(pretty.name='Kd490', Field.name='kd490', Abbreviation='KD490', Family=NA, Type='Predictor', Transform='I', Groupby='Region'),
    data.frame(pretty.name='SST mean', Field.name='SSTmean', Abbreviation='SSTMEAN', Family=NA, Type='Predictor', Transform='I', Groupby='Region'),
    data.frame(pretty.name='SST anom', Field.name='SSTanom', Abbreviation='SSTANOM', Family=NA, Type='Predictor', Transform='I', Groupby='Region'),
    data.frame(pretty.name='Wave exposure', Field.name='wave.exposure.index', Abbreviation='WAVE', Family=NA, Type='Predictor', Transform='I', Groupby='Region'),
    data.frame(pretty.name='Corrected depth', Field.name='Corrected.depth', Abbreviation='DEPTH', Family=NA, Type='Predictor', Transform='I', Groupby='Region'),
    data.frame(pretty.name='Max DHW', Field.name='maxDHW', Abbreviation='DHW', Family=NA, Type='Predictor', Transform='I', Groupby='Region'),
    data.frame(pretty.name='Cyclone', Field.name='Cyclone', Abbreviation='CYCLONE', Family=NA, Type='Predictor', Transform='I', Groupby='Region'),
    data.frame(pretty.name='High turbidity exposure', Field.name='Exposure.to.primary.weeks', Abbreviation='EXP', Family=NA, Type='Predictor', Transform='I', Groupby='Region'),
    data.frame(pretty.name='Prey density', Field.name='Prey.density', Abbreviation='PREY.DENSITY', Family=NA, Type='Predictor', Transform='I', Groupby='Region'),
  data.frame(pretty.name='Prey biomass', Field.name='Prey.biomass', Abbreviation='PREY.BIOMASS', Family=NA, Type='Predictor', Transform='I', Groupby='Region')
)
save(var.lookup, file=paste0(DATA_PATH, "primary/var.lookup.RData")) 
names = with(var.lookup, setNames(as.character(Field.name), Abbreviation))
## exclude those whose names equal their values otherwise there will be duplicate fields created in the fish data
names = names[names(names)!=names]
## ----end

## ---- removePCO1
var.lookup <- var.lookup %>%
    filter(Field.name != 'PCO1') %>%
    droplevels()
save(var.lookup, file=paste0(DATA_PATH, "primary/var.lookup.RData")) 
names = with(var.lookup, setNames(as.character(Field.name), Abbreviation))
## exclude those whose names equal their values otherwise there will be duplicate fields created in the fish data
names = names[names(names)!=names]
## ----end


## ---- AbbreviatedNames
## Create duplicates of the fields that are not already abbreviated 
fish <- fish %>%
    mutate(SSTmean = ifelse(as.character(SSTmean)=='#N/A',
                            NA,
                            as.numeric(as.character(SSTmean)))) %>%
    mutate_at(as.character(var.lookup$Field.name), list(A=~I(.))) %>%
    rename(!!! gsub('(.*)','\\1_A',names)) %>%
    mutate(REGION=factor(REGION, levels=c('Palm','Magnetic','Whitsunday','Keppel')))
save(fish, file=paste0(DATA_PATH, 'processed/fish.RData'))
## ----end

## ---- EDA responsesprocessData
responses <- var.lookup %>%
    filter(Type == 'Response') %>%
    pull(Field.name) %>%
    unique()

fish.eda.responses <- fish %>%
    dplyr::select(!!!responses, REGION) %>%
    pivot_longer(cols = c(everything(),-REGION),
                 names_to = "Response",
                 values_to = "Value") %>%
    full_join(var.lookup %>% filter(Field.name %in% c(responses, "REGION")),
              by = c('Response' = 'Field.name')) %>%
    group_by(Response) %>%
    filter(Response != 'REGION') %>%
    droplevels() %>%
    nest() %>%
    mutate(EDA1 = map(.x = data,
                      .f = EDA_histogram),
           EDA2 = map(.x = data,
                      .f = EDA_density),
           EDA3 = map2(.x = EDA1, .y = EDA2,
                       .f = function(x,y) x/y)
           )
save(fish.eda.responses, file = paste0(DATA_PATH, "processed/fish.eda.responses.RData"))
map2(paste0(OUTPUT_PATH, "figures/eda_",fish.eda.responses$Response,"_.pdf"),
     fish.eda.responses$EDA3, ggsave, width = 10, height = 6)
map2(paste0(OUTPUT_PATH, "figures/eda_",fish.eda.responses$Response,"_.png"),
     fish.eda.responses$EDA3, ggsave, dpi = 72, width = 10, height = 6)
map2(paste0(OUTPUT_PATH, "figures/eda_",fish.eda.responses$Response,"_large_.png"),
     fish.eda.responses$EDA3, ggsave, dpi = 300, width = 10, height =6)
## ----end

## dat <- fish.eda.responses[4,'data'][[1]][[1]] 
## fish.eda.responses[4,'data'][[1]][[1]] %>% pull(Value) 
## fish.eda.responses[4,'data'][[1]][[1]] %>% filter(is.na(Value))
## fish.eda.responses[4,'data'][[1]][[1]] %>% EDA_histogram()
## fish.eda.responses[4,'data'][[1]][[1]] %>% EDA_density()
## fish.eda.responses[1,'EDA3'][[1]][[1]]
## fish.eda.responses[1,'EDA2'][[1]][[1]]

## ---- EDA predictorsprocessData
predictors <- var.lookup %>%
    filter(Type == 'Predictor') %>%
    pull(Field.name) %>%
    unique()

## Start with continuous predictors
fish.eda.predictors <- fish %>%
    dplyr::select(!!!predictors) %>%
    dplyr::select(where(is.numeric), REGION) %>%
    pivot_longer(cols = c(everything(), -REGION),
                 names_to = "Predictor",
                 values_to = "Value") %>%
    left_join(var.lookup %>% filter(Field.name %in% c(predictors, "REGION")),
              by = c('Predictor' = 'Field.name')) %>%
    group_by(Predictor) %>%
    nest() %>%
    mutate(EDA1 = map(.x = data,
                      .f = EDA_histogram),
           EDA2 = map(.x = data,
                      .f = EDA_density),
           EDA3 = map2(.x = EDA1, .y = EDA2,
                       .f = function(x,y) x/y)
           )


## Now for the categorical predictors
fish.eda.predictors1 <- fish %>%
    dplyr::select(!!!predictors) %>%
    dplyr::select(where(is.character), REGION) %>%
    mutate(`REGION ` = REGION) %>%
    pivot_longer(cols = c(everything(), -REGION),
                 names_to = "Predictor",
                 values_to = "Value") %>%
    left_join(var.lookup %>%
              mutate(Field.name = ifelse(Field.name == 'REGION', 'REGION ', Field.name)) %>%
              filter(Field.name %in% c(predictors, "REGION ")),
              by = c('Predictor' = 'Field.name')) %>%
    group_by(Predictor) %>%
    nest() %>%
    mutate(EDA1 = map(.x = data,
                      .f = EDA_bar),
           EDA2 = map(.x = data,
                      .f = EDA_bar1),
           EDA3 = map2(.x = EDA1, .y = EDA2,
                       .f = function(x,y) x/y)
           )
fish.eda.predictors <- fish.eda.predictors %>%
    bind_rows(fish.eda.predictors1)

save(fish.eda.predictors, file = paste0(DATA_PATH, "processed/fish.eda.predictors.RData"))
map2(paste0(OUTPUT_PATH, "figures/eda_",fish.eda.predictors$Predictor,"_.pdf"),
     fish.eda.predictors$EDA3, ggsave, width = 10, height = 6)
map2(paste0(OUTPUT_PATH, "figures/eda_",fish.eda.predictors$Predictor,"_.png"),
     fish.eda.predictors$EDA3, ggsave, dpi = 72, width = 10, height = 6)
map2(paste0(OUTPUT_PATH, "figures/eda_",fish.eda.predictors$Predictor,"_large_.png"),
     fish.eda.predictors$EDA3, ggsave, dpi = 300, width = 10, height =6)
## ----end

## dat <- fish.eda.predictors1[1,'data'][[1]][[1]] 
## fish.eda.predictors1[1,'data'][[1]][[1]] %>% pull(Value) 
## fish.eda.predictors[4,'data'][[1]][[1]] %>% filter(is.na(Value))
## fish.eda.predictors1[1,'data'][[1]][[1]] %>% EDA_bar()
## fish.eda.predictors1[1,'data'][[1]][[1]] %>% EDA_bar1()
## fish.eda.predictors1[2,'data'][[1]][[1]] %>% EDA_bar()
## fish.eda.predictors1[2,'data'][[1]][[1]] %>% EDA_bar1()
## fish.eda.predictors[4,'data'][[1]][[1]] %>% EDA_density()
## fish.eda.predictors[1,'EDA3'][[1]][[1]]
## fish.eda.predictors[1,'EDA2'][[1]][[1]]
