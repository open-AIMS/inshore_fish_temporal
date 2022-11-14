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
    nms <- data.frame(Field.name = colnames(x)) %>%
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
fitGBM <- function(data, form, Response, Model, var.lookup, R = 200) {
    MONOTONE <- assignMonotone(data, form)
    fish.sub <- data
    if (Model %in% c('all', 'all1')) fish.sub <- data
    else fish.sub <- data %>% filter(REGION == Model)
    
    set.seed(123)
    fish.sub <- fish.sub %>% mutate_if(is.character,  as.factor)
    family <- var.lookup %>% filter(Field.name == Response) %>%
        pull(Family)
    mod <- vector('list', R)
    for (i in 1:R) {
        print(paste0('i = ', i))
        fish.sub.boot <- fish.sub %>% sample_n(n(), replace = TRUE)
        mod[[i]] <- gbm(form, data=fish.sub.boot, distribution=family,
                   cv.folds=10,interaction.depth=10,n.trees=10000, shrinkage=0.001, n.minobsinnode=2,
                   var.monotone=as.vector(MONOTONE)) 
    }
    mod
}
## ----end

## ---- function_quantile_map
quantile_map <- map(c(0.025, 0.05, 0.25, 0.5, 0.75, 0.90, 0.975), ~ partial(quantile, probs = .x)) %>%
    set_names(nm = c("Lower", "Lower.90", "Lower.50", "Median", "Upper.50", "Upper.90", "Upper"))
## ----end

## ---- function_rel.inf
rel.inf <- function(mods) {
    best.iter <- sapply(mods, gbm.perf, plot.it = FALSE, method = "cv")
    R <- length(mods)
    l <- lapply(1:R, function(x) data.frame(Boot = x, summary(mods[[x]], best.iter[x])))
    reference.infl <- 100/nrow(l[[1]])
    do.call('rbind', l) %>%
        as.data.frame() %>%
        group_by(var) %>%
        ## summarise(quants = list(quantile(rel.inf, probs = c(0.5, 0.025, 0.975)))) %>% unnest_wider(quants,)
        summarise(across(rel.inf, list(!!!quantile_map), .names = "{.fn}")) %>%
        arrange(desc(Median)) %>%
        mutate(Flag = ifelse(Lower.90 > reference.infl, TRUE, FALSE))
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
