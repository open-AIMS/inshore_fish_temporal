source('../scripts/functions.R')
library(furrr)
library(foreach)
library(parallel)
library(doParallel)
library(abt)

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


if (Attempt == 'Old way') {
## ---- abt models
library(abt)
fish.analysis.temporal.abt <-
    fish.analysis.temporal %>%
    ungroup() %>%
    mutate(
        N = 1:n(),
        TotalN = max(N)) %>%
    group_by(Response, Abbreviation, Model) %>%
    mutate(
        data = map(.x = data,
                   .f = ~ .x %>% mutate(NTR.Pooled = factor(NTR.Pooled))
                  ),
        data = map2(.x = data, .y = Form,
                    .f = ~ {
                        ## DV <- get_response(.y)
                        DV <- all.vars(.y)[1]
                        .x %>% mutate(across(DV, ~replace(., .==0, min(.[.>0])/2)))
                    }
                    ),
        GBM = pmap(.l = list(Form, data, Family, N, TotalN),
                      .f = ~ {
                          Form <- ..1
                          data <- ..2
                          Family <- ..3
                          N <- ..4
                          TotalN <- ..5
                          print(paste0(N,"/",TotalN))
                          
                          mod <- abt(Form, data = data, distribution = Family, cv.folds = 100,
                                 interaction.depth = 10, n.trees = 10000,
                                 shrinkage = 0.001, n.minobsinnode = 2, mcores = FALSE)
                          nm <- paste0(DATA_PATH, "modelled/abt_",N,".RData")
                          save(mod, file=nm)
                          nm
                          }
                      ))
save(fish.analysis.temporal.abt, file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.abt.RData"))

## ----end

## ---- abt model stats
load(file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.abt.RData"))
fish.analysis.temporal.abt.stats <-
    fish.analysis.temporal.abt %>%
    mutate(p = pmap(.l = list(GBM, Groupby, N, TotalN),
                    .f = ~ {
                        N <- ..3
                        TotalN <- ..4
                        print(paste0(N,"/",TotalN))
                        mod <- get(load(file = ..1))
                        p = plot.abts(mod, var.lookup, center = FALSE,
                                  type = "response", return.grid = TRUE,
                                  groupby = ..2)
                        nm <- paste0(DATA_PATH, "modelled/abt_stats_",N,".RData")
                        save(p, file=nm)
                        nm
                    }
                    ))
save(fish.analysis.temporal.abt.stats,
     file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.abt.stats.RData"))

## ----end
p <- get(load(fish.analysis.temporal.abt.stats[1,'p'][[1]][[1]]))


## ---- abt model thresholds
load(file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.abt.stats.RData"))
fish.analysis.temporal.abt.stats.thresholds <-
    fish.analysis.temporal.abt.stats %>%
    mutate(p = map2(.x = p,
                    .y = N,
                    .f = ~ {
                        print(N)
                        p <- get(load(file = .x))
                        thresholds=p$thresholds
                        ## nm <- paste0(DATA_PATH, "modelled/thresholds_",N,".RData")
                        ## save(p, file=nm)
                        thresholds
                   }
                   ))
save(fish.analysis.temporal.abt.stats.thresholds,
     file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.abt.stats.thresholds.RData"))
load(file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.abt.stats.thresholds.RData"))
## ----end

## ---- abt model plots
load(file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.abt.stats.RData"))
fish.analysis.temporal.abt.stats.plots <-
    fish.analysis.temporal.abt.stats %>%
    mutate(p = pmap(.l = list(p, N, data, Model),
                    .f = ~ {
                        p <- ..1
                        N <- ..2
                        data <- ..3
                        mod.name <- ..4
                        print(N)
                        p <- get(load(file = p))
                        ps <- p[['ps']]
                        p <- p[['p']]
                        if ((length(levels(data$REGION))>1 ||
                             length(levels(data$NTR.Pooled))>1) &
                            mod.name!='all1') {
                            p = common_legend(p)
                            ps = common_legend(ps)
                        }
 
                        ## nm <- paste0(DATA_PATH, "modelled/thresholds_",N,".RData")
                        ## save(p, file=nm)
                   }
                   ))
save(fish.analysis.temporal.abt.stats.thresholds,
     file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.abt.stats.thresholds.RData"))
## ----end





                                        #do.call('grid.arrange', p) ## version for the supplimentary
ggsave(filename=paste0('output/figures/data.all.abt.',mod.name,'_',resp,'_ABT.png'), do.call('grid.arrange', p), width=15, height=10, dpi=300)
ggsave(filename=paste0('output/figures/data.all.abt.',mod.name,'_',resp,'_ABT.pdf'), do.call('grid.arrange', p), width=15, height=10)

}



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
plan(multisession, workers = 30)
fish.analysis.temporal.relinf <-
  fish.analysis.temporal %>%
    ungroup() %>%
    mutate(
        N = 1:n(),
        TotalN = max(N)) %>%
  ## filter(Response == 'PL', Model == 'Magnetic') %>% 
  mutate(
      Rel.inf = future_map2(
          .x = GBM,
          .y = N,
          ## .f = ~ rel.inf(mods = .x)
          .f = ~{
              print(.y)
              print(head(.x,1))
              rel.inf(mods = .x)
          }
      ),
      Rel.inf.sum = map(.x = Rel.inf,
                        .f = ~ rel.inf.sum(l = .x))
    )
save(fish.analysis.temporal.relinf, file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.relinf.RData"))
## check to see the Rel.inf.sum - SSTMean should be highest
## ----end

## ---- fitGBM1_temporal Rel Inf plot
load(file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.relinf.RData"))
plan(multisession, workers = 30)
fish.analysis.temporal.rel.inf <-
  fish.analysis.temporal %>%
  mutate(Rel.inf.plot = future_map(
    .x = Rel.inf.sum,
    .f = ~ rel.inf.plot(Rel.inf = .x, var.lookup = var.lookup)
  )) %>%
  dplyr::select(Rel.inf, Rel.inf.plot)
save(fish.analysis.temporal.rel.inf, file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.rel.inf.RData"))
## ----end

## Partial plots
## ---- partial_predictions
load(file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.RData"))
## plan(multisession, workers = 50)
num_ticks <- fish.analysis.temporal %>%
    dplyr::select(Response, Model) %>%
    distinct() %>%
    nrow()
    
## pb <- progress::progress_bar$new(format = "[:bar] :current/:total (:percent) elapsed :elapsed eta :eta",
##                                  total = num_ticks)
registerDoParallel(cl <- makeCluster(50))
PP <- foreach(i = 1:nrow(fish.analysis.temporal), .packages = c('tidyverse','gbm')) %dopar% {
    N <- i
    GBM <- fish.analysis.temporal[i,'GBM'][[1]][[1]]
    data <- fish.analysis.temporal[i,'data'][[1]][[1]]
    Groupings <- fish.analysis.temporal[i,'Groupings'][[1]][[1]]
    print(i)
    partialPredictions <- partial_preds(GBM,data,Groupings,
                                        var.lookup, len = 100,
                                        progress = FALSE)
    nm <- paste0(DATA_PATH, "partials/partialPredictions",
                 N,".RData")
    save(partialPredictions, file=nm)
    nm
}
stopCluster(cl)
fish.analysis.temporal.predictions <-
    fish.analysis.temporal %>%
    bind_cols(PartialPredictions = unlist(PP))

## fish.analysis.temporal.predictions <-
##     fish.analysis.temporal %>%
##     ungroup() %>%
##     mutate(
##         N = 1:n(),
##         TotalN = max(N)) %>%
##     mutate(PartialPredictions = future_pmap(.l = list(GBM, data, Groupings, N),
##                                             .f = ~ {
##                                                 GBM <- ..1
##                                                 data <- ..2
##                                                 Groupings <- ..3
##                                                 N <- ..4
##                                                 print(N)
##                                                 partialPredictions <- partial_preds(GBM,data,Groupings,
##                                                                           var.lookup, len = 100,
##                                                                           progress = FALSE)
##                                                 nm <- paste0(DATA_PATH, "partials/partialPredictions",
##                                                              N,".RData")
##                                                 save(partialPredictions, file=nm)
##                                                 nm
##                                             }
##                                             ))
save(fish.analysis.temporal.predictions,
     file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.predictions.RData"))
## ----end


## ---- partial_plots
load(file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.predictions.RData"))
num_ticks <- fish.analysis.temporal.predictions %>%
    dplyr::select(Response, Model) %>%
    distinct() %>%
    nrow()
    
## pb <- progress::progress_bar$new(format = "[:bar] :current/:total (:percent) elapsed :elapsed eta :eta",
##                                  total = num_ticks)

registerDoParallel(cl <- makeCluster(50))
PP <- foreach(i = 1:nrow(fish.analysis.temporal.predictions), .packages = c('tidyverse','gbm')) %dopar% {
    N <- i
    partialPredictions <- fish.analysis.temporal.predictions[i,'PartialPredictions'][[1]][[1]]
    data <- fish.analysis.temporal.predictions[i,'data'][[1]][[1]]
    Groupings <- fish.analysis.temporal.predictions[i,'Groupings'][[1]][[1]]
    print(i)
    partialPredictions <- get(load(partialPredictions))
    partialPlot <- partial_plots(partialPredictions,data,Groupings,
                                        var.lookup, progress = FALSE)
    nm <- paste0(DATA_PATH, "partials/partialPlots",
                 N,".RData")
    save(partialPlot, file=nm)
    nm
}
stopCluster(cl)
fish.analysis.temporal.plots <-
    fish.analysis.temporal.predictions %>%
    bind_cols(PartialPlot = unlist(PP))



## fish.analysis.temporal.plots <- 
##     fish.analysis.temporal.predictions %>%
##     mutate(PartialPlot = pmap(.l = list(x = PartialPredictions, y = data, z = Groupings),
##                               .f = ~ partial_plots(..1, ..2, ..3, var.lookup = var.lookup)
##                               ))
save(fish.analysis.temporal.plots, file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.plots.RData"))
## ----end

## ---- polarity
load(file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.predictions.RData"))
registerDoParallel(cl <- makeCluster(50))
PP <- foreach(i = 1:nrow(fish.analysis.temporal.predictions), .packages = c('tidyverse','gbm')) %dopar% {
    N <- i
    partialPredictions <- fish.analysis.temporal.predictions[i,'PartialPredictions'][[1]][[1]]
    partialPredictions <- get(load(partialPredictions))
    p <- map2(.x = names(partialPredictions),
              .y = partialPredictions,
              .f = ~ {
                  .y %>%
                      group_by(!!sym(.x), .add = TRUE) %>%
                      summarise(Median = median(Preds)) %>%
                      summarise(Polarity = ifelse(first(Median) > last(Median),
                                                  'Decrease', 'Increase')) %>%
                      ungroup() %>%
                      count(Polarity) %>%
                      arrange(desc(n)) %>%
                      slice(1) %>%
                      pull(Polarity)
                      
              }
              )
    data.frame(DV = names(partialPredictions),
               Polarity = unlist(p))
             
        
}
stopCluster(cl)
fish.analysis.temporal.polarity <-
    fish.analysis.temporal.predictions %>%
    add_column(Polarity = PP)


save(fish.analysis.temporal.polarity,
     file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.polarity.RData"))
## ----


## ---- Thresholds
load(file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.predictions.RData"))

findThreshold = function(x, y, tol=0.5, deriv=1, type=c('low','high','max','average')) {
    if (length(unique(y))==1) return(sample(x, 1))
  ffun=splinefun(x, y)
  fval=ffun(x, deriv=deriv)
  (rr = range(fval))
  wch=which(abs(rr)==max(abs(rr)))
  if (wch==1) {
    wch=which(fval<rr[1]*tol)
  } else {
    wch=which(fval>rr[2]*tol)
  }
  if (type=='high') {
    wch2 =which(y[wch]==max(y[wch]))
    return(x[wch][wch2])
  } else if (type=='low'){
    wch2 =which(y[wch]==min(y[wch]))
    return(x[wch][wch2])
  } else if (type=='max') {
    wch2 =which(fval[wch]==max(fval[wch]))
    return(x[wch][wch2])
  } else {
    return(mean(x[wch]))
  }
}

ungroup_by <- function(x,...){
  group_by_(x, .dots = group_vars(x)[!group_vars(x) %in% ...])
}


registerDoParallel(cl <- makeCluster(50))
PP <- foreach(i = 1:nrow(fish.analysis.temporal.predictions), .packages = c('tidyverse','gbm')) %dopar% {
    N <- i
    partialPredictions <- fish.analysis.temporal.predictions[i,'PartialPredictions'][[1]][[1]]
    partialPredictions <- get(load(partialPredictions))
    data <- fish.analysis.temporal.predictions[i,'data'][[1]][[1]]
    Groupings <- fish.analysis.temporal.predictions[i,'Groupings'][[1]][[1]]
    print(i)
    thresholds <- vector('list', length(partialPredictions))
    names(thresholds) <- names(partialPredictions)
    for (j in 1:length(partialPredictions)) {
        pred <- names(partialPredictions[j])
        if (class(partialPredictions[[j]][[pred]]) %in% c('factor','character')) {
            thresholds[[j]] <- partialPredictions[[j]] %>%
                group_by(DV, .add =TRUE) %>%
                mutate(Threshold.low = NA,
                       Threshold.high = NA,
                       Threshold.max = NA,
                       Threshold.av = NA) %>%
                pivot_longer(cols = starts_with("Threshold"), names_to = "Threshold", values_to = "Values") %>%
                mutate(Median = NA, Lower = NA, Upper = NA)
                
        } else {
            thresholds[[j]] <- partialPredictions[[j]] %>%
                group_by(DV, Iteration, .add =TRUE) %>%
                summarise(Threshold.low = findThreshold(x = !!sym(pred), y = Preds, deriv = 1, type = 'low'),
                          Threshold.high = findThreshold(x = !!sym(pred), y = Preds, deriv = 1, type = 'high'),
                          Threshold.max = findThreshold(x = !!sym(pred), y = Preds, deriv = 1, type = 'max'),
                          Threshold.av = findThreshold(x = !!sym(pred), y = Preds, deriv = 1, type = 'av')
                          ) %>%
                ungroup_by("Iteration") %>%
                pivot_longer(cols = starts_with("Threshold"), names_to = "Threshold", values_to = "Values") %>%
                group_by(Threshold, .add = TRUE) %>%
                summarise(Median = median(Values),
                          Lower = quantile(Values, p = 0.025),
                          Upper = quantile(Values, p = 0.975))
        }
            
    }
    nm <- paste0(DATA_PATH, "partials/thresholds",
                 N,".RData")
    save(thresholds, file=nm)
    nm
}
stopCluster(cl)
fish.analysis.temporal.thresholds <-
    fish.analysis.temporal.predictions %>%
    bind_cols(Thresholds = unlist(PP))
save(fish.analysis.temporal.thresholds,
     file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.thresholds.RData"))
load(file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.thresholds.RData"))
## ----end


## ---- R2
load(file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.relinf.RData"))
## load(file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.RData"))
num_ticks <- fish.analysis.temporal.relinf %>%
    dplyr::select(Response, Model) %>%
    distinct() %>%
    nrow()
## pb <- progress::progress_bar$new(format = "[:bar] :current/:total (:percent) elapsed :elapsed eta :eta",
##                                  total = num_ticks)

## method == 1: covariates with NA and cor(P, O)^2
## method == 2: covariates with NA and 1 - (var(P - O)/var(O))
## method == 3: covariates with mean values and 1 - (var(P - O)/var(O))
## method == 4: covariates with observed values and 1 - (var(P - O)/var(O)) then multiply by rel.inf
R2 <- function(dat, method = 1, rel.inf = NULL) {
    switch(method,
           d <- dat %>% map(.f = ~.x %>% group_by(Iteration, DV) %>%
                                summarise(R2 = cor(Preds, Resp)^2) %>%
                                mutate(Method = 1)),
           d <- dat %>% map(.f = ~.x %>% group_by(Iteration, DV) %>%
                               summarise(R2 = 1 - (var(Preds - Resp)/var(Resp))) %>%
                               mutate(Method = 2)
                           ) %>%
               suppressMessages(),
           d <- dat %>% map(.f = ~.x %>% group_by(Iteration, DV) %>%
                               summarise(R2 = 1 - (var(Preds2 - Resp)/var(Resp))) %>%
                               mutate(Method = 3)
                           ) %>% suppressMessages(),
           d <- dat %>%
               map2(.y = names(dat), .f = ~ {
                   r <- rel.inf %>% filter(var == .y) %>% pull(Median)
                   .x %>% 
                       group_by(Iteration, DV) %>%
                       summarise(R2 = (1 - (var(Preds3 - Resp)/var(Resp))) * r/100) %>%
                       mutate(Method = 4)
                   } %>% suppressMessages()
                   )
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


## In the below:
## Preds = covariates get NA values
## Preds2 = covariates get mean values
## Preds3 = covariates get observed values 
## fish.analysis.temporal.R2 <-
##     fish.analysis.temporal %>%
##     mutate(PartialFits = pmap(.l = list(x = GBM, y=data),
##                                      .f = ~ partial_fit(..1,..2, var.lookup)))
registerDoParallel(cl <- makeCluster(50))
PP <- foreach(i = 1:nrow(fish.analysis.temporal.relinf), .packages = c('tidyverse','gbm')) %dopar% {
    N <- i
    GBM <- fish.analysis.temporal.relinf[i,'GBM'][[1]][[1]]
    data <- fish.analysis.temporal.relinf[i,'data'][[1]][[1]]
    Rel.inf.sum <- fish.analysis.temporal.relinf[i,'Rel.inf.sum'][[1]][[1]]
    print(i)
    PartialFits <- partial_fit(GBM, data, var.lookup, progress = FALSE)
    nm <- paste0(DATA_PATH, "partials/PartialFits",
                 N,".RData")
    save(PartialFits, file=nm)

    R2.1 <- R2(PartialFits, method = 1)
    nm.1 <- paste0(DATA_PATH, "partials/R2.1_",
                 N,".RData")
    save(R2.1, file=nm.1)
    R2tab.1 <- map2_df(.x = R2.1, .y = names(R2.1),
                       .f = ~ .x %>%
                           filter(!is.na(R2)) %>%
                           ungroup() %>%
                           summarise(across(R2, list(!!!quantile_map), na.rm = TRUE, .names = "{.fn}")) %>%
                           mutate(DV = .y))
    nmt.1 <- paste0(DATA_PATH, "partials/R2tab.1_",
                 N,".RData")
    save(R2tab.1, file=nmt.1)

    R2.2 <- R2(PartialFits, method = 2)
    nm.2 <- paste0(DATA_PATH, "partials/R2.2_",
                 N,".RData")
    save(R2.2, file=nm.2)
    R2tab.2 <- map2_df(.x = R2.2, .y = names(R2.2),
                       .f = ~ .x %>%
                           ungroup() %>%
                           summarise(across(R2, list(!!!quantile_map), .names = "{.fn}")) %>%
                           mutate(DV = .y))
    nmt.2 <- paste0(DATA_PATH, "partials/R2tab.2_",
                 N,".RData")
    save(R2tab.2, file=nmt.2)

    R2.3 <- R2(PartialFits, method = 3)
    nm.3 <- paste0(DATA_PATH, "partials/R2.3_",
                 N,".RData")
    save(R2.3, file=nm.3)
    R2tab.3 <- map2_df(.x = R2.3, .y = names(R2.3),
                       .f = ~ .x %>%
                           ungroup() %>%
                           summarise(across(R2, list(!!!quantile_map), .names = "{.fn}")) %>%
                           mutate(DV = .y))
    nmt.3 <- paste0(DATA_PATH, "partials/R2tab.3_",
                 N,".RData")
    save(R2tab.3, file=nmt.3)

    R2.4 <- R2(PartialFits, method = 4, rel.inf = Rel.inf.sum)
    nm.4 <- paste0(DATA_PATH, "partials/R2.4_",
                 N,".RData")
    save(R2.4, file=nm.4)
    R2tab.4 <- map2_df(.x = R2.4, .y = names(R2.4),
                       .f = ~ .x %>%
                           ungroup() %>%
                           summarise(across(R2, list(!!!quantile_map), .names = "{.fn}")) %>%
                           mutate(DV = .y))
    nmt.4 <- paste0(DATA_PATH, "partials/R2tab.4_",
                 N,".RData")
    save(R2tab.4, file=nmt.4)

    cbind(nm, nm.1, nmt.1, nm.2, nmt.2, nm.3, nmt.3, nm.4, nmt.4)
}
stopCluster(cl)
PPP <- lapply(PP, function(x) as.data.frame(x))
PPP <- do.call('rbind', PPP)
colnames(PPP) <- c('PartialFits',
                   'R2.1','R2tab.1',
                   'R2.2','R2tab.2',
                   'R2.3','R2tab.3',
                   'R2.4','R2tab.4')
fish.analysis.temporal.R2 <-
    fish.analysis.temporal.relinf %>%
    bind_cols(PPP)
save(fish.analysis.temporal.R2,
     file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.R2.RData"))


## fish.analysis.temporal.R2 <- fish.analysis.temporal.R2 %>%
##     mutate(
##         R2 = map(.x = PartialFits,
##                  .f = ~ R2(.x, method = 1)),
##         R2.2 = map(.x = PartialFits,
##                  .f = ~ R2(.x, method = 2)),
##         R2.3 = map2(.x = PartialFits, .y = Rel.inf.sum,
##                  .f = ~ R2(.x, method = 3, rel.inf = .y)),
##         R2tab = map(.x = R2,
##                     .f = ~ .x %>%
##                         map2_df(.y = names(.x), .f = ~ .x %>%
##                                 ungroup() %>%
##                                     summarise(across(R2, list(!!!quantile_map), .names = "{.fn}")) %>%
##                                     mutate(DV = .y))),
##         R2tab.2 = map(.x = R2.2,
##                     .f = ~ .x %>%
##                         map2_df(.y = names(.x), .f = ~ .x %>%
##                                 ungroup() %>%
##                                     summarise(across(R2, list(!!!quantile_map), .names = "{.fn}")) %>%
##                                     mutate(DV = .y))),
##         R2tab.3 = map(.x = R2.3,
##                     .f = ~ .x %>%
##                         map2_df(.y = names(.x), .f = ~ .x %>%
##                                 ungroup() %>%
##                                     summarise(across(R2, list(!!!quantile_map), .names = "{.fn}")) %>%
##                                     mutate(DV = .y)))
##     )
## save(fish.analysis.temporal.R2, file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.R2.RData"))
## ----end


## ---- Optimum
load(file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.predictions.RData"))
## load(file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.RData"))
num_ticks <- fish.analysis.temporal.relinf %>%
    dplyr::select(Response, Model) %>%
    distinct() %>%
    nrow()

registerDoParallel(cl <- makeCluster(50))
PP <- foreach(i = 1:nrow(fish.analysis.temporal.predictions), .packages = c('tidyverse','gbm')) %dopar% {
    N <- i
    partialPredictions <- fish.analysis.temporal.predictions[i,'PartialPredictions'][[1]][[1]]
    partialPredictions <- get(load(partialPredictions))
    print(i)
    optimum <- vector('list', length(partialPredictions))
    names(optimum) <- names(partialPredictions)
    for (j in 1:length(partialPredictions)) {
        pred <- names(partialPredictions[j])
        optimum[[j]] <- partialPredictions[[j]] %>% group_by(Iteration) %>%
            mutate(Var = !!sym(pred)) %>%
            summarise(Optimum = unique(Var[which.max(Preds)]))
        if (class(partialPredictions[[j]][[pred]]) %in% c('factor','character')) {
            optimum[[j]] <- optimum[[j]] %>%
                count(Optimum) %>% slice(which.max(n)) %>%
                pull(Optimum) %>%
                as.character()
        } else {
            optimum[[j]] <- optimum[[j]] %>%
                summarise(Lower = quantile(Optimum, p = 0.025),
                          Upper = quantile(Optimum, p = 0.975),
                          Optimum = median(Optimum)) %>%
                mutate(Optimum = sprintf("%0.2f (%0.2f-%0.2f)", Optimum, Lower, Upper)) %>%
                dplyr::select(Optimum)
        }
        
    }
    nm <- paste0(DATA_PATH, "partials/optimums",
                 N,".RData")
    save(optimum, file=nm)
    nm
}
stopCluster(cl)
fish.analysis.temporal.optimum <-
    fish.analysis.temporal.predictions %>%
    bind_cols(Optimum = unlist(PP))
save(fish.analysis.temporal.optimum,
     file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.optimum.RData"))
## ----end



## ---- compilation plots
load(file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.plots.RData"))
load(file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.rel.inf.RData"))
load(file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.R2.RData"))
## load(file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.relinf.RData"))

## Specific figures for the paper
source("functions.R")
source("functions_abt.R")
paper_figure1()

## All other figures
pmap(.l = list(paste0(OUTPUT_PATH, "figures/partial_plots_paper_R2.4_",
                 fish.analysis.temporal.plots$Response, "_", fish.analysis.temporal.plots$Model, ".pdf"),
               fish.analysis.temporal.plots$PartialPlot,
               fish.analysis.temporal.rel.inf$Rel.inf.plot,
               fish.analysis.temporal.R2$R2tab.4,
               fish.analysis.temporal.R2$Rel.inf.sum
               ),
     .f = ~ partial_plot_compilations_paper(path=..1, g=..2, r=..3, r2 = ..4, rel.inf = ..5, ncol = 1)
     )
pmap(.l = list(paste0(OUTPUT_PATH, "figures/partial_plots_paper_R2.4_",
                 fish.analysis.temporal.plots$Response, "_", fish.analysis.temporal.plots$Model, ".png"),
               fish.analysis.temporal.plots$PartialPlot,
               fish.analysis.temporal.rel.inf$Rel.inf.plot,
               fish.analysis.temporal.R2$R2tab.4,
               fish.analysis.temporal.R2$Rel.inf.sum
               ),
     .f = ~ partial_plot_compilations_paper(path=..1, g=..2, r=..3, r2 = ..4, rel.inf = ..5, ncol = 1, dpi = 300)
     )
pmap(.l = list(paste0(OUTPUT_PATH, "figures/partial_plots_paper_R2.1_",
                 fish.analysis.temporal.plots$Response, "_", fish.analysis.temporal.plots$Model, ".pdf"),
               fish.analysis.temporal.plots$PartialPlot,
               fish.analysis.temporal.rel.inf$Rel.inf.plot,
               fish.analysis.temporal.R2$R2tab.1,
               fish.analysis.temporal.R2$Rel.inf.sum
               ),
     .f = ~ partial_plot_compilations_paper(path=..1, g=..2, r=..3, r2 = ..4, rel.inf = ..5, ncol = 1)
     )

pmap(.l = list(paste0(OUTPUT_PATH, "figures/partial_plots_paper_R2.1_",
                 fish.analysis.temporal.plots$Response, "_", fish.analysis.temporal.plots$Model, ".png"),
               fish.analysis.temporal.plots$PartialPlot,
               fish.analysis.temporal.rel.inf$Rel.inf.plot,
               fish.analysis.temporal.R2$R2tab.1,
               fish.analysis.temporal.R2$Rel.inf.sum
               ),
     .f = ~ partial_plot_compilations_paper(path=..1, g=..2, r=..3, r2 = ..4, rel.inf = ..5, ncol = 1, dpi = 300)
     )

pmap(.l = list(paste0(OUTPUT_PATH, "figures/partial_plots_R2.4_",
                 fish.analysis.temporal.plots$Response, "_", fish.analysis.temporal.plots$Model, ".pdf"),
               fish.analysis.temporal.plots$PartialPlot,
               fish.analysis.temporal.rel.inf$Rel.inf.plot,
               fish.analysis.temporal.R2$R2tab.4,
               fish.analysis.temporal.R2$Rel.inf.sum
               ),
     .f = ~ partial_plot_compilations(path=..1, g=..2, r=..3, r2 = ..4, rel.inf = ..5, ncol = 3)
     )
pmap(.l = list(paste0(OUTPUT_PATH, "figures/partial_plots_R2.4_",
                 fish.analysis.temporal.plots$Response, "_", fish.analysis.temporal.plots$Model, ".png"),
               fish.analysis.temporal.plots$PartialPlot,
               fish.analysis.temporal.rel.inf$Rel.inf.plot,
               fish.analysis.temporal.R2$R2tab.4,
               fish.analysis.temporal.R2$Rel.inf.sum
               ),
     .f = ~ partial_plot_compilations(path=..1, g=..2, r=..3, r2 = ..4, rel.inf = ..5, ncol = 3)
     )
pmap(.l = list(paste0(OUTPUT_PATH, "figures/partial_plots_R2.1_",
                 fish.analysis.temporal.plots$Response, "_", fish.analysis.temporal.plots$Model, ".pdf"),
               fish.analysis.temporal.plots$PartialPlot,
               fish.analysis.temporal.rel.inf$Rel.inf.plot,
               fish.analysis.temporal.R2$R2tab.1,
               fish.analysis.temporal.R2$Rel.inf.sum
               ),
     .f = ~ partial_plot_compilations(path=..1, g=..2, r=..3, r2 = ..4, rel.inf = ..5, ncol = 3)
     )
pmap(.l = list(paste0(OUTPUT_PATH, "figures/partial_plots_R2.1_",
                 fish.analysis.temporal.plots$Response, "_", fish.analysis.temporal.plots$Model, ".png"),
               fish.analysis.temporal.plots$PartialPlot,
               fish.analysis.temporal.rel.inf$Rel.inf.plot,
               fish.analysis.temporal.R2$R2tab.1,
               fish.analysis.temporal.R2$Rel.inf.sum
               ),
     .f = ~ partial_plot_compilations(path=..1, g=..2, r=..3, r2 = ..4, rel.inf = ..5, ncol = 3)
     )

## map2(.x = paste0(OUTPUT_PATH, "figures/partial_plots_",
##                  fish.analysis.temporal.plots$Response, "_", fish.analysis.temporal.plots$Model, ".pdf"),
##      .y = fish.analysis.temporal.plots$PartialPlot,
##      ~ partial_plot_compilations(path=.x, g=.y, ncol = 3)
##      )

## ----end

## ---- R2tables
load(file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.R2.RData"))
load(file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.optimum.RData"))
load(file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.thresholds.RData"))
fish.analysis.temporal.stats <- fish.analysis.temporal.R2 %>%
    bind_cols(Optimum = fish.analysis.temporal.optimum$Optimum) %>%
    bind_cols(Thresholds = fish.analysis.temporal.thresholds$Thresholds) %>%
    dplyr::select(Response, Abbreviation, pretty.name, Model, Rel.inf.sum,
                  R2tab.1, R2tab.4, Optimum, Thresholds) %>%
    mutate(R2.1 = map(.x = R2tab.1,
                      .f = ~ get(load(.x)) %>% dplyr::select(DV, Median, Lower, Upper) %>%
                          mutate(R2 = sprintf("%0.2f (%0.3f-%0.2f)", Median*100, Lower*100, Upper*100)) %>%
                          dplyr::select(-Median, -Lower, -Upper)
                    )) %>%
    mutate(R2.4 = map(.x = R2tab.4,
                    .f = ~ get(load(.x)) %>% dplyr::select(DV, Median, Lower, Upper) %>%
                          mutate(R2 = sprintf("%0.2f (%0.3f-%0.2f)", Median*100, Lower*100, Upper*100)) %>%
                          dplyr::select(-Median, -Lower, -Upper)
                    )) %>%
    mutate(Optimum = map(.x = Optimum,
                         .f = ~ {
                             x <- get(load(.x))
                             data.frame(DV = names(x), do.call('rbind',x))
                             }
                         )) %>%
    mutate(Thresholds = map(.x = Thresholds,
                            .f = ~ {
                                x <- get(load(.x))
                                xx <- bind_rows(x, .id = "Covariate") %>%
                                    filter(Threshold %in% c("Threshold.high", "Threshold.low")) %>%
                                    dplyr::rename(Response = DV) %>%
                                    dplyr::rename(DV = Covariate) %>%
                                    group_by(Response, DV, Threshold) %>%
                                    summarise(Median = median(Median),
                                              Lower = min(Lower),
                                              Upper = max(Upper)) %>%
                                    mutate(Thresh = sprintf("%0.2f (%0.2f-%0.2f)", Median, Lower, Upper)) %>%
                                    dplyr::select(-Median, -Lower, -Upper) %>%
                                    pivot_wider(id_cols = everything(), names_from = Threshold, values_from = Thresh)
                                xx
                            }
                            )) %>%
    mutate(Rel.inf = map(.x = Rel.inf.sum,
                         .f = ~ .x %>%
                             dplyr::select(DV = var, Median, Lower = Lower.90,
                                           Upper = Upper.90, Flag.median) %>%
                             mutate(Rel.inf = sprintf("%0.2f (%0.3f-%0.2f)",
                                                 Median, Lower, Upper)) %>%
                             dplyr::select(-Median, -Lower, -Upper)
                         )) %>%
    mutate(Stats = pmap(.l = list(R2.1, R2.4, Optimum, Rel.inf, Thresholds),
                        .f = ~ {
                            ..4 %>% full_join(..2, by = "DV") %>%
                                full_join(..3, by = "DV") %>%
                                full_join(..1, by = "DV") %>%
                                full_join(..5, by = "DV") %>%
                                filter(Flag.median) %>%
                                droplevels()
                            }))
fish.analysis.temporal.stats[1, 'R2.1'][[1]][[1]]
fish.analysis.temporal.stats[1, 'R2.4'][[1]][[1]]
fish.analysis.temporal.stats[1, 'Optimum'][[1]][[1]]
fish.analysis.temporal.stats[1, 'Rel.inf'][[1]][[1]]
fish.analysis.temporal.stats[1, 'Stats'][[1]][[1]]



## fish.analysis.temporal.R2.2 <- fish.analysis.temporal.R2 %>%
##     ungroup() %>%
##     mutate(Influential = map(.x = Rel.inf.sum,
##                              .f = ~ .x %>%
##                                  filter(Flag.median) %>%
##                                  pull(var)
##                              )) %>%
##     mutate(R2 = map(.x = R2tab.4,
##                     .f = ~ get(load(.x)) %>% dplyr::select(DV, Median, Lower, Upper)
##                     )) %>%
##     mutate(R2.inf = map2(.x = R2,
##                          .y = Influential,
##                          .f = ~ .x %>% filter(DV %in% .y) 
##                              ## mutate(DV = factor(DV, levels = .y)) %>%
##                              ## arrange(DV)
##                          )) %>%
##     dplyr::select(Response, pretty.name, Model, R2.inf, Rel.inf.sum) %>%
##     unnest(c(R2.inf,Rel.inf.sum)) %>%
##     mutate(R2 = sprintf("%0.3f (%0.3f - %0.3f)", Median, Lower, Upper)) %>%
##     dplyr::select(-Median, -Lower, -Upper, -pretty.name) %>%
##     pivot_wider(id_cols = everything(), names_from = Response, values_from = R2) 
## fish.analysis.temporal.R2.2
save(fish.analysis.temporal.stats,
     file = paste0(DATA_PATH, "modelled/fish.analysis.temporal.stats.RData"))

## Now pair them back to just the substantial influence predictors

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

## ---- heatmap
load(file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.stats.RData"))
load(file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.polarity.RData"))
stats <- fish.analysis.temporal.stats %>%
    ## dplyr::select(Response, pretty.name, Model, Stats) %>%
    dplyr::select(pretty.name, Model, Stats) %>%
    unnest(c(Stats)) %>%
    dplyr::select(-Flag.median)
polarity <- fish.analysis.temporal.polarity %>%
    dplyr::select(Response, pretty.name, Model, Polarity) %>%
    unnest(c(Polarity)) 
stats <- stats %>%
    left_join(polarity)
for (model in unique(stats$Model)){
    var.lookup.dv <- var.lookup %>% filter(Type == "Predictor") %>%
        dplyr::select(DV = Abbreviation, VAR = pretty.name) 
    stats.1 <- 
        stats %>%
        filter(Model == model) %>%
        droplevels() %>% 
        filter(!Response %in% c('PTB','PLD','PTD','PTB','PLB','GRAZ')) %>%
        droplevels() %>%
        mutate(Resp=forcats::fct_relevel(Response, 'TFD', 'TFSR', 'PCO1')) %>%
        dplyr::select(-Response, -Model, -Optimum, -Rel.inf, -R2.x) %>%
        mutate(R2 = as.numeric(str_replace(R2.y, "(.*)\\ .*","\\1"))) %>%
        left_join(var.lookup.dv) %>% 
        group_by(VAR) %>%
        mutate(R2.total = sum(R2)) %>%
        ungroup() %>%
        arrange(R2.total) %>%
        mutate(VAR = factor(VAR, unique(VAR))) %>%
        mutate(pretty.name = ifelse(pretty.name == "Parrot", "Parrotfish", pretty.name)) %>%
        mutate(pretty.name = ifelse(pretty.name == "Grazers2", "Grazers", pretty.name)) %>%
        mutate(pretty.name = str_wrap(pretty.name, 10)) %>%
        mutate(pretty.name = forcats::fct_reorder(pretty.name, as.numeric(Resp)))
    segs <- expand.grid(y=0:length(unique(stats.1$VAR)),
                        x=0:length(unique(stats.1$pretty.name)))
    g1 <- stats.1 %>%
        ggplot(aes(y = VAR, x = pretty.name, fill = Polarity, alpha = R2/100)) +
        geom_tile() +
        geom_segment(data=segs %>% arrange(x,y), aes(y=y+0.5, yend=lag(y)+0.5, x=x+0.5, xend=x+0.5),
                     inherit.aes = FALSE)+
        geom_segment(data=segs %>% arrange(y,x), aes(y=y+0.5, yend=y+0.5, x=x+0.5, xend=lag(x)+0.5),
                     inherit.aes = FALSE) +
        scale_fill_manual('Polarity',values=c('red','green')) +
        scale_alpha_continuous('R-squared', limits=c(0,1)) +
        scale_x_discrete('Response variables',position='top') +
        scale_y_discrete('Predictor variables') +
        coord_cartesian(expand=FALSE) +
        theme_bw() +
        theme(
            text = element_text(size = 18),
            axis.title=element_text(size=rel(1.5)),
            panel.grid.major = element_blank(),
            legend.position = 'bottom',
            legend.title.align = 0.5,
            legend.margin = ggplot2::margin(t=0,b=0,l=20,r=20, unit='pt')) 
    ggsave(paste0("../output/figures/heatmap.",model,".png"),
           g1,
           width = 20, height = 10)
}
## ----end

## ---- heatmaps for the paper
load(file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.stats.RData"))
load(file=paste0(DATA_PATH, "modelled/fish.analysis.temporal.polarity.RData"))
stats <- fish.analysis.temporal.stats %>%
    dplyr::select(pretty.name, Model, Stats) %>%
    unnest(c(Stats)) %>%
    dplyr::select(-Flag.median)
polarity <- fish.analysis.temporal.polarity %>%
    dplyr::select(Response, pretty.name, Model, Polarity) %>%
    unnest(c(Polarity)) 
stats <- stats %>%
    left_join(polarity)
gs <- vector('list', 4)
names(gs) <- c('Palm', 'Magnetic', 'Whitsunday', 'Keppel')
i <- 1
for (model in c('Palm', 'Magnetic', 'Whitsunday', 'Keppel')){
    var.lookup.dv <- var.lookup %>% filter(Type == "Predictor") %>%
        dplyr::select(DV = Abbreviation, VAR = pretty.name) 
    stats.1 <- 
        stats %>%
        filter(Model == model) %>%
        droplevels() %>% 
        filter(!Response %in% c('PTB','PLD','PTD','PTB','PLB','GRAZ')) %>%
        droplevels() %>%
        mutate(Resp=forcats::fct_relevel(Response, 'TFD', 'TFSR', 'PCO1')) %>%
        dplyr::select(-Response, -Model, -Optimum, -Rel.inf, -R2.x) %>%
        mutate(R2 = as.numeric(str_replace(R2.y, "(.*)\\ .*","\\1"))) %>%
        left_join(var.lookup.dv) %>% 
        group_by(VAR) %>%
        mutate(R2.total = sum(R2)) %>%
        ungroup() %>%
        arrange(R2.total) %>%
        mutate(VAR = factor(VAR, unique(VAR))) %>%
        mutate(pretty.name = ifelse(pretty.name == "Parrot", "Parrotfish", pretty.name)) %>%
        mutate(pretty.name = ifelse(pretty.name == "Grazers2", "Grazers", pretty.name)) %>%
        mutate(pretty.name = str_wrap(pretty.name, 10)) %>%
        mutate(pretty.name = forcats::fct_reorder(pretty.name, as.numeric(Resp)))
    segs <- expand.grid(y=0:length(unique(stats.1$VAR)),
                        x=0:length(unique(stats.1$pretty.name)))
    gs[[model]] <- stats.1 %>%
        ggplot(aes(y = VAR, x = pretty.name, fill = Polarity, alpha = R2/100)) +
        geom_tile() +
        geom_segment(data=segs %>% arrange(x,y), aes(y=y+0.5, yend=lag(y)+0.5, x=x+0.5, xend=x+0.5),
                     inherit.aes = FALSE)+
        geom_segment(data=segs %>% arrange(y,x), aes(y=y+0.5, yend=y+0.5, x=x+0.5, xend=lag(x)+0.5),
                     inherit.aes = FALSE) +
        scale_fill_manual('Polarity',values=c('red','green')) +
        scale_alpha_continuous('R-squared', limits=c(0,1)) +
        scale_x_discrete('Response variables',position='top') +
        scale_y_discrete('Predictor variables', label = capitalise_some) +
        coord_cartesian(expand=FALSE) +
        theme_bw() +
        theme(
            text = element_text(size = 18),
            axis.title=element_text(size=rel(1)),
            panel.grid.major = element_blank(),
            legend.position = 'bottom',
            legend.title.align = 0.5,
            legend.margin = ggplot2::margin(t=0,b=0,l=20,r=20, unit='pt'),
            title = element_text(size = 25)) +
        ggtitle(label = paste0(letters[i], ") ", model))
    i <- i + 1
}

ggsave(paste0("../output/figures/heatmaps.png"),
       ## (gs[[1]] + plot_annotation(title = "Palm", tag_levels = "a")) +
       ## (gs[[2]] + plot_annotation(title = "Magnetic", tag_levels = "a")),
       wrap_plots(gs) & theme(plot.margin = margin(t=0, b = 50, l = 20, r=20, unit="pt")),
       width = 37, height = 15, dpi = 300)
ggsave(paste0("../output/figures/heatmaps1.png"),
       ## (gs[[1]] + plot_annotation(title = "Palm", tag_levels = "a")) +
       ## (gs[[2]] + plot_annotation(title = "Magnetic", tag_levels = "a")),
       wrap_plots(gs, ncol = 1) & theme(plot.margin = margin(t=0, b = 50, l = 20, r=20, unit="pt")),
       width = 20, height = 30, dpi = 300)
## ----end
