## ---- loadPackages
library(knitr)
library(tidyverse)
library(gbm)
library(gratia)
library(patchwork)
## ----end

## ---- preparePaths
DATA_PATH <<- "../data/"
OUTPUT_PATH <<- "../output/"
FIGS_PATH <<- paste0(OUTPUT_PATH, "figures")

if (!dir.exists(DATA_PATH)) dir.create(DATA_PATH)
if (!dir.exists(paste0(DATA_PATH,"primary"))) dir.create(paste0(DATA_PATH, "primary"))
if (!dir.exists(paste0(DATA_PATH,"processed"))) dir.create(paste0(DATA_PATH, "processed"))
if (!dir.exists(paste0(DATA_PATH,"modelled"))) dir.create(paste0(DATA_PATH, "modelled"))
if (!dir.exists(paste0(DATA_PATH,"summarised"))) dir.create(paste0(DATA_PATH, "summarised"))

if (!dir.exists(OUTPUT_PATH)) dir.create(OUTPUT_PATH)
if (!dir.exists(FIGS_PATH)) dir.create(FIGS_PATH)
## ----end

## ---- functionEDA_histogram
EDA_histogram <- function(dat) {
    g1 <- dat %>% ggplot() +
        geom_histogram(aes(x = Value)) +
        scale_x_continuous(unique(dat$pretty.name)) +
        scale_y_continuous(expand = c(0,0)) +
        theme_classic()
    g2 <- dat %>% ggplot() +
        geom_histogram(aes(x = Value)) +
        scale_x_continuous(unique(dat$pretty.name),
                           trans = scales::pseudo_log_trans(sigma=1, base=10),
                           ## breaks = scales::log_breaks()) +
                           breaks = as.vector(c(1,2,5) %o% 10^(-1:4))) +
        scale_y_continuous(expand = c(0,0)) +
        theme_classic()
    g1 + g2
}
## ----end

## ---- functionEDA_density
EDA_density <- function(dat) {
    g1 <- dat %>% ggplot() +
        geom_density(aes(x = Value, fill = REGION), alpha = 0.5) +
        scale_x_continuous(unique(dat$pretty.name)) +
        scale_y_continuous(expand = c(0,0)) +
        theme_classic() +
        theme(legend.position = "bottom", legend.direction = "horizontal")
    g2 <- dat %>% ggplot() +
        geom_density(aes(x = Value+0.01, fill = REGION), alpha = 0.5) +
        scale_x_continuous(unique(dat$pretty.name),
                           trans = scales::pseudo_log_trans(sigma=1, base=10),
                           ## breaks = scales::log_breaks()) +
                           breaks = as.vector(c(1,2,5) %o% 10^(-1:4))) +
        scale_y_continuous(expand = c(0,0)) +
        theme_classic() +
        theme(legend.position = "bottom", legend.direction = "horizontal")
    g1 + g2 + plot_layout(guides = 'collect') & theme(legend.position = "bottom")
}
## ----end


## ---- functionEDA_bar
EDA_bar <- function(dat) {
    dat %>% ggplot() +
        geom_bar(aes(x = Value), colour = 'black') +
        scale_x_discrete(unique(dat$pretty.name)) +
        scale_y_continuous(expand = c(0,0)) +
        theme_classic()
}
## ----end

## ---- functionEDA_bar1
EDA_bar1 <- function(dat) {
    dat %>% ggplot() +
        geom_bar(aes(x = Value, fill = REGION), alpha = 0.5, colour = 'black') +
        scale_x_discrete(unique(dat$pretty.name)) +
        scale_y_continuous(expand = c(0,0)) +
        theme_classic() +
        theme(legend.position = "bottom", legend.direction = "horizontal")
}
## ----end

## ---- function_change_colname_value
change_colname_value <- function(df, new_colname){
  df %>% rename(!!new_colname := Value)
}
## ----end

## ---- function_replace_colnames_abbrev
replace_colnames_abbrev <- function(df, var.lookup) {
    conv <- var.lookup %>% dplyr::filter(Field.name %in% predictors) %>%
        dplyr::select(Field.name, Abbreviation) %>%
        filter(Field.name != Abbreviation) 
    df %>%
        dplyr::rename_with(~conv$Abbreviation, all_of(conv$Field.name))
}
## ----end


## ---- function_applyFormula
applyFormula <- function(Response, Model) {
    f <- formulas[[unique(Model)]]
    v <- var.lookup %>% filter(Field.name == Response)
    f <- update(f, paste0(v$Transform,'(',v$Abbreviation,') ~.'))
    if (Response %in% c('PI','PTD','PLD'))
        f <- update(f, .~.+PREY.DENSITY)
    if (Response %in% c('PTB','PLB'))
        f <- update(f, .~.+PREY.BIOMASS)
    f 
}
## ----end

## ---- function_applyGroupings
applyGroupings <- function(x, Response, Model) {
    nms <- data.frame(Abbreviation = colnames(x)) %>%
        left_join(var.lookup) %>%
        pull(Abbreviation) %>%
        suppressWarnings() %>%
        suppressMessages()
    
    if (Model == 'all')
        g <- rep('REGION', length(nms))
    else if (Model == 'all1')
        g <- rep(NA, length(nms))
    else 
        g <- rep('NTR.Pooled', length(nms))
    names(g) <- nms
    g[nms==g] <- NA
    g
}
## ----end

## ---- function_AssignMonotone
assignMonotone <- function(data,formula) {
    MF <- model.frame(formula, data)
    dataClasses <- attr(terms(MF),'dataClasses')[-1]
    MF <- MF %>% mutate_if(is.character, as.factor)    
    MF <- MF %>% mutate_if(is.factor, as.numeric)
                                        #MM <- model.matrix(formula,data)
    VAR_MONOTONE <- cor(MF[, 1], MF[, -1], method = 'spearman') / abs(cor(MF[, 1], MF[, -1], method = 'spearman'))
    VAR_MONOTONE[dataClasses=='factor']  <-  0
    VAR_MONOTONE[is.na(VAR_MONOTONE)] <- 0
    as.vector(VAR_MONOTONE)
}
## ----end

## ---- function_fitGBM
fitGBM <- function(data, form, Response, Model, var.lookup, R = 200, prefix = "") {
    Response <- as.character(Response)
    MONOTONE <- assignMonotone(data, form)
    fish.sub <- data
    if (Model %in% c('all', 'all1')) {
        fish.sub <- data
    } else {
        fish.sub <- data %>% filter(REGION == Model)
    }
    set.seed(123)
    fish.sub <- fish.sub %>% mutate_if(is.character,  as.factor)
    family <- var.lookup %>% filter(Abbreviation == Response) %>%
        pull(Family)
    mod <- vector('list', length = R)
    for (i in 1:R) {
        print(paste0('Response = ', Response, ', Model = ', Model, ', i = ', i))
        fish.sub.boot <- fish.sub %>% sample_n(n(), replace = TRUE)
        ## if response is log transformed, make sure there are no zero values
        if (all.vars(form, functions = TRUE)[2] == 'log') {
            fish.sub.boot <-
                fish.sub.boot %>%
                filter(!is.na(!!sym(Response)),
                       !is.infinite(!!sym(Response))) %>%
                mutate(Response1 = !!sym(Response),
                       Response2 = replace(Response1, Response1 == 0, min(Response1[Response1>0], na.rm = TRUE)/2),
                       Response2 = ifelse(is.infinite(Response2), 0.01, Response2),
                       Response3 = sum(Response2),
                       !!sym(Response) := ifelse(Response3 == 0, 0.01, Response2))# handle cases when all zeros
        }
        if (length(all.vars(form)) > 2) {
            mod[[i]] <- gbm(form, data=fish.sub.boot, distribution=family,
                            cv.folds=10,interaction.depth=10,n.trees=10000, shrinkage=0.001, n.minobsinnode=2,
                            var.monotone=as.vector(MONOTONE))
        } else { # if there is only one predictor (no cv folds)
            mod[[i]] <- gbm(form, data=fish.sub.boot, distribution=family,
                            interaction.depth=10,n.trees=10000, shrinkage=0.001, n.minobsinnode=2,
                            var.monotone=as.vector(MONOTONE))
        }
        
        if (SAVE_PATHS_ONLY) {
            m <- mod[[i]]
            save(m, file = paste0(DATA_PATH, "modelled/",prefix,"mod_",Response,"__",Model, "___", i, ".RData"))
        }
    }
    if (SAVE_PATHS_ONLY) {
        return(paste0(DATA_PATH, "modelled/",prefix,"mod_",Response,"__",Model, "___", 1:R, ".RData"))
    } else {
        return(mod)
    }
}
## ----end

## ---- function_quantile_map
quantile_map <- map(c(0.025, 0.05, 0.25, 0.5, 0.75, 0.90, 0.975), ~ partial(quantile, probs = .x)) %>%
    set_names(nm = c("Lower", "Lower.90", "Lower.50", "Median", "Upper.50", "Upper.90", "Upper"))
## ----end

## ---- function_rel.inf
rel.inf <- function(mods) {
    if (SAVE_PATHS_ONLY) {
       mods <- lapply(mods, function(x) get(load(x))) 
    }
    if (length(mods[[1]]$var.names) > 1) {  ### more than one predictor
        best.iter <- sapply(mods, gbm.perf, plot.it = FALSE, method = "cv")
    } else { ## when there was only a single predictor
        best.iter <- sapply(mods, gbm.perf, plot.it = FALSE, method = "OOB")
    }
    R <- length(mods)
    l <- lapply(1:R, function(x) data.frame(Boot = x, summary(mods[[x]],
                                                              n.trees = best.iter[x],
                                                              plotit = FALSE)))
    reference.infl <- 100/nrow(l[[1]])
    do.call('rbind', l) %>%
        as.data.frame() %>%
        group_by(var) %>%
        ## summarise(quants = list(quantile(rel.inf, probs = c(0.5, 0.025, 0.975)))) %>% unnest_wider(quants,)
        summarise(across(rel.inf, list(!!!quantile_map), .names = "{.fn}")) %>%
        arrange(desc(Median)) %>%
        mutate(Flag = ifelse(Lower.90 > reference.infl, TRUE, FALSE),
               Flag.median = ifelse(Median > reference.infl, TRUE, FALSE))
}
## ----end

## ---- function_rel.inf.plot 
rel.inf.plot <- function(Rel.inf, var.lookup) {
    R <- 100/nrow(Rel.inf)
    Rel.inf %>%
        left_join(var.lookup, by = c("var" = "Abbreviation")) %>%
        mutate(pretty.name = fct_reorder(pretty.name, Median)) %>%
        ggplot() +
        geom_vline(xintercept = R, linetype = 'dashed') +
        geom_pointrange(aes(x = Median, xmin = Lower, xmax = Upper, y = pretty.name, colour = Flag), show.legend = FALSE) +
        geom_linerange(aes(xmin = Lower.90, xmax = Upper.90, y = pretty.name, colour = Flag), size = 1, show.legend = FALSE) +
        geom_linerange(aes(xmin = Lower.50, xmax = Upper.50, y = pretty.name, colour = Flag), size = 1.5, show.legend = FALSE) +
        scale_colour_manual(breaks = c(FALSE,TRUE), values = c('grey', 'black')) +
        scale_x_continuous('Relative influence') +
        theme_classic() +
        theme(axis.title.y = element_blank())
}
## ----end

## ---- function_variable_selection
variable_selection <- function(Rel.inf) {
    Rel.inf %>%
        filter(Flag.median) %>%
        pull(var) 
}
## ----end

## ---- function_VS_check_var
VS_check_var <- function(Variables, data) {
    wch.numerics <- data %>%
        group_by(SITE, across(where(is.factor))) %>%
        pivot_longer(cols = where(is.numeric)) %>%
        filter(name %in% Variables) %>%
        group_by(name, .add = TRUE) %>%
        summarise(var = var(value)) %>%
        filter(var > 0) %>%
        ungroup() %>%
        dplyr::select(-SITE, -var) %>%
        pull(name) %>% unique()
    wch.factors <-
        data %>%
        dplyr::select(any_of(Variables)) %>%
        dplyr::select(where(is.factor)) %>%
        colnames()
    c(wch.factors, wch.numerics)

}
## ----end

## ---- function_VS_factors
VS_factors <- function(Variables, data, Form) {
    all_predictors <- all.vars(Form)[-1]
    classes <- data %>% dplyr::select(any_of(all_predictors)) %>%
        sapply(class)
    c(Variables, names(classes)[classes %in% c("factor", "character")]) 
    }
## ----end

## ---- function_temporal_centering
temporal_centering <- function(data, Variables) {
    data %>%
        group_by(SITE) %>% 
        mutate(across(!!Variables & where(is.numeric), ~ mean(.x), .names = "{col}.mean")) %>%
        mutate(across(!!Variables & where(is.numeric), ~ .x - mean(.x))) %>%
        arrange(YEAR) %>%
        ungroup()
}
## ----end
## ---- function_temporal_form
temporal_form <- function(form, Variables) {
    update(as.formula(form), as.formula(paste0('. ~',paste0(Variables, collapse=' + '))))
}
## ----end



## ---- function_a_seq
a_seq <- function(x, len = 100) {
    seq(min(x, na.rm = TRUE),
        max(x, na.rm = TRUE),
        len = len)
    }
## ----end

## ---- function_partial_preds
partial_preds <- function(mods, data, groupings, var.lookup, len) {
    if (SAVE_PATHS_ONLY) {
       mods <- lapply(mods, function(x) get(load(x))) 
    } 
    ## if (length(all.vars(Form))>2) {  
    if (length(mods[[1]]$var.names)>1) {  
        best.iter <- sapply(mods, gbm.perf, plot.it = FALSE, method = "cv")
    } else { ## when there was only a single predictor
        best.iter <- sapply(mods, gbm.perf, plot.it = FALSE, method = "OOB")
    }
    DV <- all.vars(mods[[1]]$Terms)[1]
    terms <- attr(mods[[1]]$Terms, 'dataClasses')[-1]
    fit <- vector('list', length(terms)) 
    for (i in 1:length(terms)) {
        ## make newdata
        IV <- names(terms)[i]
        grouping <- groupings[IV]
        sgrouping <- as.character(str_replace_na(grouping, ''))
        IV.sym <- sym(IV)
        otherIV <- names(terms)[-i]  ## names of predictors
        if (!is.na(grouping)) otherIV <- otherIV[otherIV != grouping]
        ## grouping <- str_replace_na(grouping, '')
        otherIV.sym <- syms(otherIV)
        if (terms[i] == 'numeric') {
            newdata <- data %>%
                ungroup() %>%
                group_by(!!sym(sgrouping)) %>% 
                mutate(across(all_of(otherIV), ~ NA)) %>%
                expand(!!IV.sym := a_seq(!!IV.sym, len), !!!otherIV.sym) 
        } else {
            newdata <- data %>%
                ungroup() %>%
                group_by(!!sym(sgrouping)) %>% 
                mutate(across(all_of(otherIV), ~ NA)) %>%
                expand(!!IV.sym, !!!otherIV.sym) 
        }
        ## if (!is.na(grouping))
        ##     newdata <- newdata %>%
        ##         ## dplyr::select(-sym(grouping)) %>%
        ##         crossing(unique(data[,grouping]))
        ## predict in lapply
        R <- length(mods)
        preds <- sapply(1:R, function(r) predict(mods[[r]],
                                                 newdata = newdata,
                                                 n.trees = best.iter[r]))
        colnames(preds) <- paste0('Pred',1:R)
        newdata <- newdata %>% cbind(preds) %>%
            dplyr::select(-all_of(otherIV)) %>%
            pivot_longer(cols = matches('^Pred[0-9]+$'),
                         names_to = 'Iteration',
                         values_to = 'Pred')
        ## get centered values

        if (terms[i] == 'numeric') {
            IV.mean.sym <- sym(paste0(IV, '.mean'))
            centered_means <- data %>%
                ungroup() %>% 
                group_by(select(.,any_of(as.character(str_replace_na(grouping, ''))))) %>%
                summarise(!!IV.mean.sym := mean(!!IV.mean.sym, na.rm = TRUE))
            newdata <- newdata %>%
                {if (!is.na(grouping)) full_join(.,centered_means) %>% suppressMessages() %>% suppressWarnings()
                 else full_join(.,centered_means, by = character()) %>% suppressMessages() %>% suppressWarnings()
                 } %>%
                mutate(!!sym(paste0(IV, ".fit")) := !!IV.sym) %>%
                mutate(!!IV.sym := !!IV.sym + !!IV.mean.sym)
        }
        FUN <- all.vars(mods[[1]]$Terms, functions = TRUE)[2]
        inv.fun <- function(FUN) {
            switch(FUN,
                   "I" = I,
                   "log" = exp)
        }
        newdata <-
            newdata %>% mutate(Preds = inv.fun(FUN)(Pred))
        fit[[i]] <- newdata %>% mutate(DV = DV) 
    }
    pb$tick()
    return(setNames(fit, names(terms))) 
}


## ----end

## ---- function_partial_plots
partial_plots <- function(preds, data, groupings, var.lookup, spaghetti = FALSE) {
    nms <- names(preds)
    plots <- setNames(vector('list', length(nms)), nms)
    for (i in 1:length(nms)) {
        focal_predictor <- var.lookup %>%
            filter(Abbreviation == nms[i])
        IV <- nms[i]
        IV.pretty <- focal_predictor %>% pull(pretty.name)
        IV.type <- preds[[i]] %>% pull(!!sym(IV)) %>% class()
        DV <- preds[[i]]$DV %>% unique() 
        focal_response <- var.lookup %>%
            filter(Abbreviation == DV)
        DV.pretty <- focal_response %>% pull(pretty.name)
        GROUP <- groupings[nms[i]]
        ## GROUP <- GROUP %>% str_to_upper()
        GROUP <- ifelse(GROUP == 'Region', 'REGION', GROUP)
        sGROUP <- as.character(str_replace_na(GROUP,''))
        if(sGROUP=='') nGROUP <- NULL else nGROUP <- sym(sGROUP)
        preds[[i]] <-
            preds[[i]] %>%
            {if(!is.na(GROUP) & GROUP !="") mutate(., GROUP = paste(Iteration, !!sym(GROUP)))
             else mutate(., GROUP = paste(Iteration))
             }
        
        if (spaghetti & IV.type != "factor") { 
            g <- preds[[i]] %>%
                ggplot(aes(y = Preds, x = !!sym(IV),
                           group = GROUP, colour = !!sym(GROUP))) +
                geom_line(alpha = 0.5) +
                scale_x_continuous(IV.pretty)
        } else {
            preds.sum <- preds[[i]] %>%
                ungroup() %>% 
                group_by(!!sym(IV), !!sym(sGROUP)) %>%
                summarise(across(Preds, list(!!!quantile_map), .names = "{.fn}")) %>%
                suppressMessages() %>%
                suppressWarnings()
            if (IV.type != "factor") {
                g <- preds.sum %>%
                    ggplot(aes(y = Median, x = !!sym(IV),
                               ## colour = !!sym(sGROUP), fill = !!sym(sGROUP))) +
                               colour = !!nGROUP, fill = !!nGROUP)) +
                    geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.3, colour = NA) +
                    geom_ribbon(aes(ymin = Lower.90, ymax = Upper.90), alpha = 0.3, colour = NA) +
                    geom_ribbon(aes(ymin = Lower.50, ymax = Upper.50), alpha = 0.3, colour = NA) +
                    geom_line() +
                    scale_x_continuous(IV.pretty)
            } else {
                if (sGROUP!='') {
                    g <- preds.sum %>%
                        ggplot(aes(y = Median, x = !!sym(IV),
                                   colour = !!nGROUP)) 
                } else {
                    g <- preds.sum %>%
                    ggplot(aes(y = Median, x = !!sym(IV),
                               colour = !!sym(IV))) 
                }
                g <- g + 
                    geom_pointrange(aes(ymin = Lower, ymax = Upper), size =0.8) +
                    geom_linerange(aes(ymin = Lower.90, ymax = Upper.90), size = 1) +
                    geom_linerange(aes(ymin = Lower.50, ymax = Upper.50), size = 1.2) +
                    scale_x_discrete(IV.pretty)
            }
            
        }
        plots[[nms[i]]] <- g + theme_classic() +
            scale_y_continuous(DV.pretty) +
            scale_colour_discrete('') +
            scale_fill_discrete('')
    }
    pb$tick()
    plots
}
## ----end
