## ---- stats.abt

## fitMethod=1: peffects.abt on newdata with NA values
## fitMethod=2; predict.abt on newdata
## fitMethod=3; predict.abt on data then averaged
stats.abt = function(mod, fitMethod=1, analysis, mins=NULL, maxs=NULL) {
    N=length(mod)
    mdata=mod[[1]]$mdata
    pdata=mod[[1]]$mdata[,-1]
    preds = colnames(pdata)
    obs = mdata[[1]]
    fit = vector('list',N); names(fit) <- paste0('Boot',1:N);
    rel.imp = vector('list',N); names(rel.imp) <- paste0('Boot',1:N);
    R2.value = vector('list',N); names(R2.value) <- paste0('Boot',1:N);
    optim = vector('list',N); names(optim) <- paste0('Boot',1:N);

    m_gaussian <- function(y, f) sum((y - f)^2)
    m_binomial <- function(y, f) sum(2 * (y * log(ifelse(y == 0, 
        1, y/f)) + (1 - y) * log(ifelse(y == 0, 1, (1 - y)/(1 - 
        f)))))
    m_poisson <- function(y, f) sum(2 * (y * log(ifelse(y == 0, 
        1, y/f)) - (y - f)))
    m_laplace <- function(y, f) sum(abs(y - f))
    invlogit <- function(y) 1/(1 + exp(-y))
    dist <- mod[[1]]$distribution
    if (dist == "gaussian") {
        dev.fun <- m_gaussian
        fun <- I
        fun2=I
    } else if (dist == "bernoulli") {
        dev.fun <- m_binomial
        fun <- invlogit
        fun2<-binomial()$linkfun
    } else if (dist == "poisson") { # | names(mdata)[1]=='log(y)') {
        dev.fun <- m_poisson
        fun <- exp
        fun2<-log
    } else if (dist == "laplace") {
        dev.fun <- m_laplace
        fun <- I
        fun2<-I
    }
    
    for (i in 1:N) {
        #print(i)
        ri = relative.influence(mod[[i]])
        rel.imp[[i]] =  bind_rows(100*ri/sum(ri)) %>% gather(key=var, value=rel.inf) %>% as.data.frame
        fl=vector('list', length(preds)); names(fl) <- preds;
        pl=vector('list', length(preds)); names(pl) <- preds;
        a=cbind(pdata, y=predict.abt(mod[[i]], newdata=pdata)) #%>% mutate(y=binomial()$linkinv(y))
        ## Get the REGION averages on the response scale
        aa=a %>% mutate(y=fun(y))%>% lapply(FUN=function(x,y=.$y) data.frame(X1=x,y))
        for (j in preds) {
            #print(j)
            ## Make some prediction data.  Note, this will include means of non-focal predictors,
            ## Yet for some of the methods below, they will be replaced with NA values before predictions.
            #if (!exists('mod[[1]]$mf')) mod[[1]]$mf = mod[[1]]$m
            newdata = prediction.data(Var=j, formula(mod[[i]]$Terms), data=mod[[1]]$mf, cyclic=NULL, groups=analysis[[j]], cat_levels=list(NTR.Pooled='NTR'))
            pred=j#preds[i]
            #xlab = xlabs[[pred]]
            ii = which(preds==pred)
            ## predicted values
            if (fitMethod==6) { #straight from plot.abt
                g=unique(c(j, ifelse(is.null(grps[[j]]), j, grps[[j]])))
                ad = newdata[[j]] %>% mutate_at(vars(-one_of(c(j,grps[[j]]))), funs(replace(.,TRUE,NA))) #%>%
                                        #dplyr::select_(.dots=g)
                ad = ad[names(ad) %in% g]
                fl[[j]] = peffects.abt(mod[[i]], data=ad, return.grid=TRUE, center=FALSE)
                }
            if (fitMethod==4) {
                g=ifelse(is.null(grps[[j]]), j, grps[[j]])
                group.means = a %>% mutate(y=binomial()$linkinv(y)) %>% group_by_(.dots=g) %>% summarize(ym=mean(y)) %>% ungroup %>% mutate(ym=binomial()$linkfun(ym))
                ad = newdata[[j]] %>% mutate_at(vars(-one_of(c(j,grps[[j]]))), funs(replace(.,TRUE,NA))) %>%
                    dplyr::select_(.dots=preds)
                fl[[j]] = peffects.abt(mod[[i]], data=ad, return.grid=TRUE, center=FALSE)  #%>% mutate(y=binomial()$linkinv(y))
                fl[[j]] = fl[[j]] %>% group_by_(.dots=g) %>% mutate(mu=mean(y)) %>% ungroup
                fl[[j]] = fl[[j]] %>% left_join(group.means) %>% mutate(y=y+(ym-mu))
            }
            if (fitMethod==2) fl[[j]] = cbind(newdata[[j]], y=predict.abt(mod[[i]], newdata=newdata[[j]])) #%>% mutate(y=binomial()$linkinv(y))
            if (fitMethod==1) {
                ad = newdata %>% mutate_at(vars(-one_of(c(j,analysis[[j]]))), funs(replace(.,TRUE,NA))) %>%
                    dplyr::select_(.dots=preds)
                fl[[j]] = peffects.abt(mod[[i]], data=ad, return.grid=TRUE, center=FALSE,n.trees=mod[[i]]$best) #%>% mutate(y=binomial()$linkinv(y))
            }
            if (fitMethod==5) {
                g=ifelse(is.null(grps[[j]]), j, grps[[j]])
                group.means = a %>% mutate(y=binomial()$linkinv(y)) %>% group_by_(.dots=g) %>% summarize(ym=mean(y)) %>% ungroup %>% mutate(ym=binomial()$linkfun(ym))
                ad = pdata %>% mutate_at(vars(-one_of(c(j,grps[[j]]))), funs(replace(.,TRUE,NA))) %>%
                    dplyr::select_(.dots=preds)
                fl[[j]] = peffects.abt(mod[[i]], data=ad, return.grid=TRUE, center=FALSE) #%>% mutate(y=binomial()$linkinv(y))
                fl[[j]] = fl[[j]] %>% group_by_(.dots=g) %>% mutate(mu=mean(y)) %>% ungroup
                fl[[j]] = fl[[j]] %>% left_join(group.means) %>% mutate(y=y+(ym-mu))
            }      
            if (fitMethod==3) fl[[j]] =a %>% group_by_(.dots=c(j,grps[[j]])) %>% summarize(y=mean(y))
            ## Generate data that is the obs for the focal predictor and means for all the rest
            dd = (mdata[,colnames(mdata)!=j] %>%
                mutate_all(.funs=function(x) ifelse(is.factor(x) | is.character(x), rev(sort(names(table(x))))[1] ,mean(x,na.rm=TRUE))) %>%
                #cbind(mdata[j]) %>% dplyr::select(-y)
                cbind(mdata[j]))[,-1]

            #dd = dd %>% mutate_at(vars(-one_of(c(j,analysis[[j]]))), funs(replace(.,TRUE,NA))) %>%
            #        dplyr::select_(.dots=preds)
       

            
            ## Create a data frame of predicted (y) and observed (y) on link scale for R2 calculations
            #pl[[j]]=data.frame(dd, y=predict.abt(object=mod[[i]], newdata=dd, n.trees=mod[[i]]$best), obs=fun2(mdata[[1]])) %>%
            #    dplyr::select(y,obs)
            pl[[j]]=data.frame(dd, y=fun(predict.abt(object=mod[[i]], newdata=dd, n.trees=mod[[i]]$best)), obs=(mdata[[1]])) %>%
                dplyr::select(y,obs)
            ## fl[[j]] = fl[[j]] %>% mutate_(.dots=setNames(pred,'X1'))
        }
        fit[[paste0('Boot',i)]] = fl
        R2.value[[paste0('Boot',i)]] = data.frame(lapply(pl,R2.gbm,method=1)) %>%
            mutate_all(.funs = function(x) ifelse(x<0,0,x))
       
        #optim[[paste0('Boot',i)]]=as.data.frame(lapply(aa ,getMax))
    }
    optim=abt.get.optim(mod, mins=mins, maxs=maxs) #as.data.frame(lapply(fl ,getMax))
    list(rel.imp=rel.imp, fit=fit,R2.value=R2.value, optim=optim)
}

## ----end


## ---- Prediction.data

## Generate a general prediction grid for a focal predictor (and optional group)
## This is a sequence for the focal predictor (optionally conditional on the levels of the grouping variable)
## and means for all other predictors
prediction.data = function(Var=NULL, formula, data, cyclic=NULL,groups=NULL, continuous.resolution=100, cat_levels=NULL) {
    terms=terms(formula)
    Var.names = attr(terms,'term.labels')
    num_vars = Var.names[unlist(lapply(data[,Var.names], 'is.numeric'))]
    cat_vars = Var.names[unlist(lapply(data[,Var.names], 'is.factor'))]
    dat = data
    if (!is.null(cyclic)) {
        for (i in length(cyclic)) {
            ii = which(Var.names %in% names(cyclic[[i]]))
            Var.names = Var.names[-1*ii]
            Var.names = c(Var.names, names(cyclic)[i])
            num_vars = num_vars[-1*ii]
            num_vars = c(num_vars, names(cyclic)[i])
        }
    }
    if (!is.null(groups)) if (is.na(groups)) groups=NULL; groupings=NULL
    if (!is.null(groups)) groupings=groups
    if (!is.null(groups)) {
        dat = data %>% dplyr::group_by_(.dots=groups)
    } else {
        dat= data
    }
    grid.levels = dat %>% do({
        z=. #%>% droplevels
        ll = list()
        focal = as.vector(as.data.frame(z[,Var])[,1])
        if (is.numeric(focal)) {
            if (all(is.na(focal))==TRUE) {
                ll[[Var]]=NA
            } else {
                ll[[Var]]=seq(min(focal,na.rm=TRUE), max(focal,na.rm=TRUE),len=continuous.resolution)
            }
        } else {
            ll[[Var]] = levels(as.data.frame(z)[,Var])    
        }
        k=0
        ## All other numeric predictors
        for (i in num_vars[num_vars!=Var]) {#x$var.names[x$var.names!=Var]) {
            focal = as.vector(as.data.frame(z[,i])[,1])
            if (all(is.na(focal))==TRUE) {
                ll[[i]]=NA
            } else {
                ll[[i]]=mean(focal,na.rm=TRUE)
            }
        }
        ## All other categorical predictors - just use a single level
        cat_var=cat_vars[cat_vars!=Var]
        if (!is.null(groupings)) cat_var=cat_var[cat_var!=groupings]
        for (i in cat_var) {#x$var.names[x$var.names!=Var]) {
            if (!is.null(cat_levels[[i]])) {
                ll[[i]]=factor(cat_levels[[i]], levels=levels(as.data.frame(z)[,i])) #levels(as.data.frame(z)[,i])
            } else {
                ll[[i]]=levels(as.data.frame(z)[,i])
            }
        }
        ll=expand.grid(ll)
        ## incase any of these are cyclic
        if (!is.null(cyclic)) {
            for (i in length(cyclic)) {
                ll = ll %>% mutate_(.dots=cyclic[[i]])
            }
        }
        ll 
    }) %>% na.omit %>% #droplevels %>%
    as.data.frame
    grid.levels
}

## ----end


## ---- Prediction.data.old
prediction_data.old = function(formula, data, group_by_cats=TRUE,groups=NULL,continuous.resolution=100, cyclic=NULL, cat_levels=NULL) {
    terms=terms(formula)
    Var.names = attr(terms,'term.labels')
    num_vars = Var.names[unlist(lapply(data[,Var.names], 'is.numeric'))]
    cat_vars = Var.names[unlist(lapply(data[,Var.names], 'is.factor'))]
    
    if (!is.null(cyclic)) {
        for (i in length(cyclic)) {
            ii = which(Var.names %in% names(cyclic[[i]]))
            Var.names = Var.names[-1*ii]
            Var.names = c(Var.names, names(cyclic)[i])
            num_vars = num_vars[-1*ii]
            num_vars = c(num_vars, names(cyclic)[i])
        }
    }
    grid.levels = list()
    for (ii in 1:length(Var.names)) {
        Var = Var.names[ii]
        if (group_by_cats) {
            groupings = cat_vars
            dat = data
            if (!is.null(groups)) {
                groupings = groups[[Var]]
                if (!is.null(groupings)) dat = data %>% dplyr::group_by_(.dots=groupings)
            }

        } else {
            dat= data
        }
        grid.levels[[Var]] = dat %>% do({
            z=.
            ll = list() #vector('list',length(num_vars))
            b = as.vector(as.data.frame(z[,Var])[,1])
            if (is.numeric(b)) {
                if (all(is.na(b))==TRUE) {
                    ll[[Var]]=NA
                } else {
                    ll[[Var]]=seq(min(b,na.rm=TRUE), max(b,na.rm=TRUE),len=continuous.resolution)
                }
            } else {
                ll[[Var]] = levels(as.data.frame(z)[,Var])    
            }
            k=0
            ## All other numeric predictors
            for (i in num_vars[num_vars!=Var]) {#x$var.names[x$var.names!=Var]) {
                b = as.vector(as.data.frame(z[,i])[,1])
                if (all(is.na(b))==TRUE) {
                    ll[[i]]=NA
                } else {
                    ll[[i]]=mean(b,na.rm=TRUE)
                }
            }
            ## All other categorical predictors - just use a single level
            cat_var=cat_vars[cat_vars!=Var]
            if (!is.null(groupings)) cat_var=cat_var[cat_var!=groupings]
            for (i in cat_var) {#x$var.names[x$var.names!=Var]) {
                if (!is.null(cat_levels[[i]])) {
                    ll[[i]]=factor(cat_levels[[i]], levels=levels(as.data.frame(z)[,i])) #levels(as.data.frame(z)[,i])
                } else {
                    ll[[i]]=levels(as.data.frame(z)[,i])
                }
            }
            ll=expand.grid(ll)
            ## incase any of these are cyclic
            if (!is.null(cyclic)) {
                for (i in length(cyclic)) {
                    ll = ll %>% mutate_(.dots=cyclic[[i]])
                }
            }
            ll 
        }) %>% na.omit %>% #droplevels %>%
        as.data.frame
    }
    grid.levels
    
}
## ----end


## ---- R2.gbm
R2.gbm = function(x, method=1) {
    x=na.omit(x)
    y_i = x$obs
    u_i = x$y
    if (method==1) R2=cor(y_i,u_i)^2
    if (method==2) {
        r=y_i - u_i
        yadj = y_i - mean(y_i, na.rm=TRUE)
        R2=1 - sum(r^2)/sum(yadj^2)
    }
    if (method==3) {
        R2=(var(y_i, na.rm=TRUE) - mean((y_i - u_i)^2, na.rm=TRUE))/var(y_i, na.rm=TRUE)
    }
    if (method==4) {
        R2=var(u_i,na.rm=TRUE)/ (var(u_i,na.rm=TRUE) + var((y_i-u_i),na.rm=TRUE))
    }
    R2
}

## ----end


## ---- abt.get.optim
abt.get.optim = function(mod, center=FALSE, return.grid=TRUE,type='response', npts=100, mins=NULL, maxs=NULL) {
    tree.list <- mod
    nlist=length(mod)
    x <- tree.list[[1]]
    n.trees <- sapply(tree.list, "[[", "n.trees")
    n.trees <- rep(n.trees, nlist)
    N=length(mod)
    mdata=mod[[1]]$mdata
    pdata=mod[[1]]$mdata[,-1]
    preds = colnames(pdata)
    optims = list()
    for (j in 1:length(preds)) {
        pred=preds[j]
        i.var <- j[order(x$var.type[j] > 0)]
        grid.levels <- vector("list", length(i.var))
        i = 1
        if (is.numeric(x$var.levels[[i.var[i]]])) {
            grid.levels[[i]] <- seq(min(x$var.levels[[i.var[i]]]), 
                                    max(x$var.levels[[i.var[i]]]), length = npts[i])
            XX = grid.levels[[i]]
        } else {
            grid.levels[[i]] <- as.numeric(factor(x$var.levels[[i.var[i]]], 
                                                  levels = x$var.levels[[i.var[i]]])) - 1
            XX = factor(x$var.levels[[i.var[i]]], 
                        levels = x$var.levels[[i.var[i]]])
        }
        X <- expand.grid(grid.levels)
        names(X) <- paste("x", 1:length(i.var), sep = "")
        x.num <- (x$var.type == 0)[i.var]
        ytemp <- matrix(NA, nrow = nrow(X), ncol = nlist)
        for (i in 1:nlist) {
            ytemp[, i] <- .Call("gbm_plot", X = as.double(data.matrix(X)), 
                cRows = as.integer(nrow(X)), cCols = as.integer(ncol(X)), 
                n.class = as.integer(x$num.classes), i.var = as.integer(i.var - 
                  1), n.trees = as.integer(n.trees[i]), initF = as.double(tree.list[[i]]$initF), 
                trees = tree.list[[i]]$trees, c.splits = tree.list[[i]]$c.splits, 
                var.type = as.integer(x$var.type), PACKAGE = "abt")
        }
        wch=apply(ytemp, 2, which.max)
        if (!is.null(mins) & is.numeric(x$var.levels[[i.var]])) {
            XX = ((XX-min(XX))/(max(XX)-min(XX))) * (max(maxs[[pred]]) - min(mins[[pred]])) + min(mins[[pred]])
        }
        optims[[pred]] = XX[wch]
    }
    optim = as.data.frame(optims) %>% mutate(Boot=paste0('Boot.',1:n()))
    optim = split(optim, optim$Boot)
    lapply(optim, function(x) x %>% dplyr::select(-Boot))
}

## ----end

## ---- summarize.values
summarize_values = function(val, type='optim',data.trans=data.trans, trans=trans) {
    optim = do.call('rbind',val) 
    optim1 = optim
    #if (type=='optim') {
    #    if (nrow(data.trans)==1) {
    #        optim1 = optim1 %>% cbind(data.trans) %>%
    #            mutate_(.dots=trans)
    #        optim1 = optim1 %>% dplyr:::select(-one_of(colnames(data.trans)))
    #    } else {
    #        optim1 = optim1 %>% left_join(data.trans) %>%
    #            mutate_(.dots=trans)
    #        optim1 = optim1 %>% dplyr:::select(-one_of(colnames(data.trans)))
    #    }
                                        #}
    if (type=='Rel.inf') {
        optim1 = optim1 %>% dplyr::rename('Var'='var', 'Value'='rel.inf') %>% group_by(Var)
    } else {
        optim1 = optim1 %>%
            dplyr::select_if(is.numeric) %>%
            gather(key=Var, value=Value) %>%
            group_by(Var)
    }
    
                                        #dplyr::summarize(Mean=sprintf('%2.2f (%2.2f,%2.2f)',mean(Value, na.rm=TRUE), lower=quantile(Value, p=0.025),upper=quantile(Value, p=0.975)))
    optim1 = optim1 %>%
        dplyr::summarize(Mean=sprintf('%2.2f',mean(Value, na.rm=TRUE)),
                         Median=sprintf('%2.2f',median(Value,na.rm=TRUE)),
                         lower=sprintf('%2.2f',quantile(Value, p=0.025, na.rm=TRUE)),
                         upper=sprintf('%2.2f',quantile(Value, p=0.975, na.rm=TRUE))) 
    optim2 = optim %>% dplyr::select_if(is.factor)
    if (ncol(optim2)>0) {
        optim2=optim2 %>% gather(key=Var, value=Value) %>%
        group_by(Var) %>%
                                        #dplyr::summarize(Mean=sprintf('%s',names(rev(sort(table(Value))))[1]))
        dplyr::summarize(Mean=names(rev(sort(table(Value))))[1], Median='', lower='', upper='')
        optim1=optim1 %>% full_join(optim2)
    }
    optim1
}

## ----end


## ---- get.response
get_response <- function(mod) {
    if (length(mod[[2]])==1) return(mod[[2]])
    if (length(mod[[2]])==2) return(mod[[2]][[2]])
}
## ----end


## ---- prettytables
pretty.stats = function(stats, var.lookup) {
    stats = stats %>% left_join(var.lookup %>% dplyr::select(pretty.name, Var=Abbreviation))
    stats %>%
        arrange(-as.numeric(as.character(Median.rel.inf))) %>%
        mutate(Optimum=sprintf('%s (%s-%s)', Mean.optim, lower.optim, upper.optim),
               `R-sq`=sprintf('%s (%s-%s)', Median.R2, lower.R2, upper.R2),
               `Rel inf`=sprintf('%s (%s-%s)', Median.rel.inf, lower.rel.inf, upper.rel.inf)) %>%
        select(Covariate=pretty.name, Optimum, `R-sq`, `Rel inf`)
}

pretty.thresholds = function(thresholds, stats, var.lookup) {
    thresholds =
        bind_rows(thresholds, .id='Var') %>%
        left_join(stats %>% dplyr::select(Var,Median.rel.inf)) %>%
        arrange(-as.numeric(as.character(Median.rel.inf))) %>%
        left_join(var.lookup %>% dplyr::select(pretty.name, Var=Abbreviation)) %>%
        select(Covariate=pretty.name, everything(), -Var,-Median.rel.inf)

    thresholds
}

## ----end


## ---- findThresholds
findThreshold = function(x, y, tol=0.5, deriv=1, type=c('low','high','max','average')) {
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
## ----end



## ---- plot.abts
plot.abts = function(mod, var.lookup,    center=FALSE, return.grid=TRUE,type='response', trans=NULL,groupby=NULL, ylab=NULL,manual.trans.y, pt.size=10, mins=NULL, maxs=NULL) {
    N=length(mod)
    mdata=mod[[1]]$mdata
    pdata=mod[[1]]$mdata[,-1]
    preds = colnames(pdata)
    p = list()
    thresholds=vector('list',length(preds))
    names(thresholds) <- preds
    Range = c(1e10,1e-05) # initialize range with too high min and too low max
    for (i in 1:length(preds)) {
        VAR = var.lookup[var.lookup$Abbreviation==preds[i],]
        pred=preds[i]
        xlab = as.character(VAR$pretty.name)
        
        grouped=FALSE
        if (is.null(groupby) | groupby=='') {
            newdata = plot.abt(mod, c(i), npts=100,center=center, type=type, return.grid=TRUE) %>%
                mutate(X = !!sym(preds[i]))
        } else if(preds[i]==groupby) {
            grouped=TRUE
            pretty.groupby=as.character(var.lookup[var.lookup$Abbreviation==groupby,'pretty.name'])
            newdata = plot.abt(mod, c(i), npts=100, center=center, type=type, return.grid=TRUE) %>%
                mutate_(.dots=setNames(preds[i], 'X')) %>%
                mutate_(.dots=setNames(groupby, 'group'))
        } else {
            grouped=TRUE
            pretty.groupby=as.character(var.lookup[var.lookup$Abbreviation==groupby,'pretty.name'])
            newdata = plot.abt(mod, c(which(preds==groupby), i), npts=100, center=center, type=type, return.grid=TRUE) %>%
                mutate_(.dots=setNames(preds[i], 'X')) %>%
                mutate_(.dots=setNames(groupby, 'group'))
        }

        resp = ifelse(length(mod[[1]]$response.name)==1, mod[[1]]$response.name, mod[[1]]$response.name[2])
        trans = as.character(var.lookup[var.lookup$Abbreviation==resp,'Transform'])
        trans = ifelse(trans=='log', 'exp', trans)
        newdata = newdata %>% mutate_at(vars(y,lo,hi), list(trans))

        
        ## newdata <- newdata %>%
        ##     mutate(across(any_of(c('NTR.Pooled')),
        ##                   function(x=.x) ifelse(x=='NTR', 'NTMR', as.character(x))))
        
        if (is.numeric(newdata$X)) {
            ## adjust the predictor to reflect its uncentered values
            if (!is.null(mins)) {
                newdata =
                    newdata %>%
                    left_join(mins %>% dplyr::select(!!groupby, Min=!!pred)) %>%
                    left_join(maxs %>% dplyr::select(!!groupby, Max=!!pred)) %>%
                    group_by(!!groupby) %>%
                    mutate(X=((X-min(X))/(max(X)-min(X))) * (Max-Min) + Min) 
            }
            if (grouped) {
                p[[i]] <- ggplot(newdata, aes(y=y, x=X, group=group, group=group, color=group, fill=group)) +
                    geom_ribbon(aes(ymin=lo, ymax=hi), color=NA,alpha=0.3) +
                    scale_fill_discrete(pretty.groupby) +
                    scale_color_discrete(pretty.groupby) + 
                    geom_line() +
                                        #scale_y_continuous(paste0('f(',parse(text=xlab),')')) +
                    scale_x_continuous(xlab) +
                    theme_classic(pt.size)
                if (groupby=='NTR') {
                    p[[i]] = p[[i]] + scale_fill_manual(pretty.groupby, values=c('blue','lightgreen','darkgreen')) 
                    p[[i]] = p[[i]] + scale_color_manual(pretty.groupby, values=c('blue','lightgreen','darkgreen'))     
                }
                if (groupby=='NTR.Pooled') {
                    p[[i]] = p[[i]] + scale_fill_manual(pretty.groupby, values=c('#01b0f1','#92d14f')) 
                    p[[i]] = p[[i]] + scale_color_manual(pretty.groupby, values=c('#01b0f1','#92d14f'))   
                }
                
                thresholds[[preds[i]]] = newdata %>% group_by_at(groupby) %>%
                    summarize(threshold.low=mean(findThreshold(x=X, y=y, deriv=1, type='low')),
                              threshold.high=mean(findThreshold(x=X, y=y, deriv=1, type='high')),
                              threshold.max=mean(findThreshold(x=X, y=y, deriv=1, type='max')),
                              threshold.av=mean(findThreshold(x=X, y=y, deriv=1, type='average'))) %>%
                    as.data.frame
                                        #print(paste('group=',unique(newdata$group)))
                                        #print(paste('groupby=',groupby))
                                        #print(paste('labels=',labels[[groupby]]))
            } else {
              X=newdata$X
              y=newdata$y
                thresholds[[preds[i]]] = c(threshold.low=mean(findThreshold(x=X, y=y, deriv=1, type='low')),
                                           threshold.high=mean(findThreshold(x=X, y=y, deriv=1, type='high')),
                                           threshold.max=mean(findThreshold(x=X, y=y, deriv=1, type='max')),
                                           threshold.av=mean(findThreshold(x=X, y=y, deriv=1, type='average')))
                p[[i]] <- ggplot(newdata, aes(y=y, x=X)) +
                    geom_ribbon(aes(ymin=lo, ymax=hi), fill='grey') +
                    geom_line() +
                                        #scale_y_continuous(paste0('f(',parse(text=xlab),')')) +
                    scale_x_continuous(xlab) +
                    theme_classic(pt.size)
            }
        } else {
            thresholds[[preds[i]]]=NULL
            if (grouped) {
                p[[i]] <- ggplot(newdata, aes(y=y, x=X, group=group, color=group)) +
                    geom_blank() +
                    geom_linerange(aes(ymin=lo, ymax=hi), position=position_dodge(width=0.2)) +
                    geom_point(position=position_dodge(width=0.2)) +
                    scale_color_discrete(pretty.groupby) + 
                                        #scale_y_continuous(paste0('f(',parse(text=xlab),')')) +
                    scale_x_discrete(xlab) +
                    theme_classic(pt.size)
                if (groupby=='NTR') {
                    p[[i]] = p[[i]] + scale_color_manual(pretty.groupby, values=c('blue','lightgreen', 'darkgreen')) 
                }
                if (groupby=='NTR.Pooled') {
                    p[[i]] = p[[i]] + scale_color_manual(pretty.groupby, values=c('#01b0f1','#92d14f')) 
                }
                if (groupby=='REGION') {
                    p[[i]] = p[[i]] +
                        scale_x_discrete(xlab, breaks=c('Palm','Magnetic','Whitsunday','Keppel'), labels=c('PA','MI','WH','KE'))
                }
                                
            } else  {
                p[[i]] <- ggplot(newdata, aes(y=y, x=X)) +
                    geom_blank() +
                    geom_linerange(aes(ymin=lo, ymax=hi)) +
                    geom_point() +
                                        #scale_y_continuous(paste0('f(',parse(text=xlab),')')) +
                    scale_x_discrete(xlab) +
                    theme_classic(pt.size)
            }
        }
        Range[1] = ifelse(min(newdata$lo)<Range[1], min(newdata$lo), Range[1])
        Range[2] = ifelse(max(newdata$hi)>Range[2], max(newdata$hi), Range[2])
    }
    for (i in 1:length(preds)) {
        resp = ifelse(length(mod[[1]]$response.name)==1, mod[[1]]$response.name, mod[[1]]$response.name[2])
        pretty.resp = as.character(var.lookup$pretty.name[var.lookup$Abbreviation==resp])
        p[[i]] = p[[i]] + scale_y_continuous(stringr::str_wrap(pretty.resp,width=15), limits=Range) #+ scale_y_discrete(parse(text=ylab))
        #p[[i]] = p[[i]] + scale_y_continuous(ylab) #+ scale_y_discrete(parse(text=ylab)) 
    }
    r=NULL
    for (j in 1:length(mod)) {
        r = rbind(r, abt::relative.influence(mod[[j]], scale=FALSE))
    }
    if (any(rowSums(r)==0))  r=r[-which(rowSums(r)==0),]
    r=r %>% as.data.frame %>% mutate_all(function(x) 100*x/rowSums(.))
    rel.imps = apply(r, 2, function(x) c('Mean'=median(x),quantile(x, p=c(0.025,0.25,0.75,0.975)))) %>%
        as.data.frame %>%
        mutate(Stat=rownames(.)) %>%
        gather(key=Var, value=Value,-Stat) %>%
        left_join(var.lookup %>% dplyr::select(Var=Abbreviation, pretty.name)) %>%
        #left_join(xlabs %>% as.data.frame %>% gather(key=Var, value=var)) %>%
        spread(key=Stat, value=Value) %>% arrange(Mean) %>%
        mutate(Var=factor(Var, levels=unique(Var))) %>%
        dplyr::rename('Lower'=`2.5%`,'lower'=`25%`,'upper'=`75%`,'Upper'=`97.5%`) %>%
        mutate(Substantial=factor(ifelse(Mean>100/length(unique(Var)),1,0)), Substantial50=factor(ifelse(lower>100/length(unique(Var)),1,0)),Substantial95=factor(ifelse(Lower>100/length(unique(Var)),1,0))) 

    p2=ggplot(rel.imps, aes(y=Mean, x=Var)) +
            geom_linerange(aes(ymin=Lower,ymax=Upper, color=Substantial95), show.legend=FALSE) +
            geom_linerange(aes(ymin=lower,ymax=upper, color=Substantial50), show.legend=FALSE, size=1) +
            geom_point(aes(fill=Substantial, color=Substantial50),size=2, shape=21, show.legend=FALSE)+
            geom_hline(yintercept=100/nrow(rel.imps), linetype='dashed')+
            scale_y_continuous(expression(Relative~Importance)) +
            scale_x_discrete(labels=as.character(rel.imps$pretty.name)) + 
            scale_fill_manual(values=c('gray','black'))+
            scale_color_manual(values=c('gray','black'))+
            coord_flip() + theme_classic(pt.size) +
            #ggtitle(paste0(letters[ii],')')) +
            theme(axis.title.y=element_blank(),plot.title=element_text(margin=ggplot2::margin(t=10,b=-10), hjust=0.01),, plot.margin=unit(c(0,1,0,0), 'lines'),
                  panel.spacing=unit(0,'lines'))
    p[['rel.imp']] = p2
    ri = rev(as.character((rel.imps %>% filter(Substantial==1))$Var))
    print(ri)
    #print(preds)
    wch=match(ri,preds)
    print(wch)
    #print(names(p))
    ps = p[c(length(p),wch)]

    
    list(p=p, ps=ps, thresholds=thresholds)
}

## ----end
## ---- commonLegend
common_legend = function(gg) {
    g_legend<-function(p1){
        tmp <- ggplot_gtable(ggplot_build(p1))
        leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
        legend <- tmp$grobs[[leg]]
        return(legend)}
    legend <- g_legend(gg[[2]] + theme(legend.position='right'))
    gg = lapply(gg, function(x) x + theme(legend.position='none'))
    gg[['Legend']] = legend
    gg
}
## ----end

partial_plot_compilations_paper <- function(path, g, r, r2, rel.inf, ncol = 1, dpi = 100) {
  Vars <- rel.inf %>% filter(Flag) %>% pull(var)
  ## Go with a maximum of three
  Vars <- Vars[1:min(length(Vars), 3)]
  r2 <- get(load(file = r2))
  r2 <- r2 %>% filter(DV %in% Vars)
  if (nrow(r2)==0) {
    gw <- r 
    ggsave(path, gw, width = 8, height = 8, dpi = dpi)
  } else {
    mTop <- 0.7 / (3 - (length(Vars) - 1))
    wch <- match(Vars,r2$DV)
    r2 <- r2[wch,]
    g <- get(load(file = g))
    g <- g[Vars]
    gw <- g %>% 
      add_r2_values(r2) %>% 
      apply_consistent_y_lims() %>%
      suppressMessages() %>%
      suppressWarnings()
    
    gww <- wrap_plots(gw, ncol = 1, guides = "collect") &
      guides(fill = "none", color = "none") &
      ## guides(
      ##   fill = "none",
      ##   coloues(fill = "none", color = "none") &
      theme(
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#FFFFFF95"),
        legend.position = "bottom",
        text = element_text(size = 10))
    gw <- r +
      inset_element(gww, right = 1, bottom = 0.01, left = 0.45, top = mTop)
    ggsave(path, gw, width = 8, height = 8, dpi = dpi)
  }
}

partial_plot_compilations_paper_single_figure <- function(g, r, r2, rel.inf, ncol = 1) {
  Vars <- rel.inf %>% filter(Flag) %>% pull(var)
  ## Go with a maximum of three
  Vars <- Vars[1:min(length(Vars), 3)]
  r2 <- get(load(file = r2))
  r2 <- r2 %>% filter(DV %in% Vars)
  if (nrow(r2)==0) {
    gw <- r 
    gw
  } else {
    ## mTop <- 0.85 / (3 - (length(Vars) - 1))
      mTop <- 0.85
    wch <- match(Vars,r2$DV)
    r2 <- r2[wch,]
    g <- get(load(file = g))
    g <- g[Vars]
    gw <- g %>% 
      add_r2_values(r2) %>% 
      apply_consistent_y_lims() %>%
      suppressMessages() %>%
      suppressWarnings()

    gw <- lapply(gw, function(x) {
        labb <- capitalise_some(ggplot_build(x)$plot$scales$scales[[1]]$name)
        ## x + labs(x = labb) 
        x + scale_x_continuous(labb) 
        })

    if (nrow(r2) == 1) {
        mTop <- mTop/2
        gww <- wrap_plots(gw, guides = "collect") &
            guides(fill = "none", color = "none") &
            theme(
                panel.background = element_blank(),
                plot.background = element_rect(fill = "#FFFFFF95"),
                legend.position = "bottom",
                text = element_text(size = 10)
            )
        gw <- r + scale_y_discrete(labels = capitalise_some) +
            inset_element(gww, right = 1, bottom = 0.01, left = 0.7, top = mTop)
        gw
    } else {
    ## gww <- wrap_plots(gw, ncol = 1, guides = "collect") &
    design <- "#A
               BC"
    gww <- wrap_plots(gw, guides = "collect", design = design) &
      guides(fill = "none", color = "none") &
        ## & labs(x = toupper_label(., "x")) &
      ## scale_x_continuous(name = capitalise_some(waiver())) &
      ## guides(
      ##   fill = "none",
      ##   coloues(fill = "none", color = "none") &
      theme(
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#FFFFFF95"),
        legend.position = "bottom",
        text = element_text(size = 10)
        )
    gw <- r + scale_y_discrete(labels = capitalise_some) +
      ## inset_element(gww, right = 1, bottom = 0.01, left = 0.45, top = mTop)
      inset_element(gww, right = 1, bottom = 0.01, left = 0.4, top = mTop)
    gw
    }
  }
}

capitalise_some <- function(x) {
    gsub("% (.)", "% \\U\\1", x, perl = TRUE)
    }

paper_figure1 <- function() {
  ## Total density
  g <- fish.analysis.temporal.plots %>%
    filter(Response == "TFD", Model == "all1") %>%
    `[`(1,'PartialPlot') %>% `[[`(1) %>% `[[`(1)
  r <- fish.analysis.temporal.rel.inf %>%
    filter(Response == "TFD", Model == "all1") %>%
    `[`(1,'Rel.inf.plot') %>% `[[`(1) %>% `[[`(1) +
    ggtitle("a) Total density (#/1000m²)") +
    theme(text = element_text(size = 16),
          axis.title.x = element_text(margin = margin(t = 12, unit = "pt")))
  r2 <- fish.analysis.temporal.R2 %>%
    filter(Response == "TFD", Model == "all1") %>%
    `[`(1,'R2tab.4') %>% `[[`(1) %>% `[[`(1)
  rel.inf <- fish.analysis.temporal.R2 %>%
    filter(Response == "TFD", Model == "all1") %>%
    `[`(1,'Rel.inf.sum') %>% `[[`(1) %>% `[[`(1)
  gw1 <- partial_plot_compilations_paper_single_figure(g, r, r2, rel.inf, ncol = 1) 

  ## Species richness
  g <- fish.analysis.temporal.plots %>%
    filter(Response == "TFSR", Model == "all1") %>%
    `[`(1,'PartialPlot') %>% `[[`(1) %>% `[[`(1)
  r <- fish.analysis.temporal.rel.inf %>%
    filter(Response == "TFSR", Model == "all1") %>%
    `[`(1,'Rel.inf.plot') %>% `[[`(1) %>% `[[`(1) +
    ggtitle("b) Species richness") +
    theme(text = element_text(size = 16),
          axis.title.x = element_text(margin = margin(t = 12, unit = "pt")))
  r2 <- fish.analysis.temporal.R2 %>%
    filter(Response == "TFSR", Model == "all1") %>%
    `[`(1,'R2tab.4') %>% `[[`(1) %>% `[[`(1)
  rel.inf <- fish.analysis.temporal.R2 %>%
    filter(Response == "TFSR", Model == "all1") %>%
    `[`(1,'Rel.inf.sum') %>% `[[`(1) %>% `[[`(1)
  gw2 <- partial_plot_compilations_paper_single_figure(g, r, r2, rel.inf, ncol = 1) 

  ## PCO1 
  g <- fish.analysis.temporal.plots %>%
    filter(Response == "PCO1", Model == "all1") %>%
    `[`(1,'PartialPlot') %>% `[[`(1) %>% `[[`(1)
  r <- fish.analysis.temporal.rel.inf %>%
    filter(Response == "PCO1", Model == "all1") %>%
    `[`(1,'Rel.inf.plot') %>% `[[`(1) %>% `[[`(1) +
    ggtitle("c) PCO1 (Species composition)") +
    theme(text = element_text(size = 16),
          axis.title.x = element_text(margin = margin(t = 12, unit = "pt")))
  r2 <- fish.analysis.temporal.R2 %>%
    filter(Response == "PCO1", Model == "all1") %>%
    `[`(1,'R2tab.4') %>% `[[`(1) %>% `[[`(1)
  rel.inf <- fish.analysis.temporal.R2 %>%
    filter(Response == "PCO1", Model == "all1") %>%
    `[`(1,'Rel.inf.sum') %>% `[[`(1) %>% `[[`(1)
  gw3 <- partial_plot_compilations_paper_single_figure(g, r, r2, rel.inf, ncol = 1) 

  ## gw <- (gw1 + ggtitle("a) Total density (x1000)")) +
  ##   (gw2 + ggtile("b) Species richness")) +
  ##   (gw3 + ggtitle("c) PCO1 (Species composition)")
  gw <- gw1 + gw2 + gw3
  ggsave("test.png", gw, width = 25, height = 12)
  gw <- gw1 / gw2 / gw3
  ## ggsave("test1.png", gw, width = 15, height = 25)
  ggsave("test1.png", gw, width = 10, height = 10*1.618)
  ggsave("test1_hires.png", gw, width = 10, height = 10*1.618, dpi = 600)
  ggsave("test1.pdf", gw, width = 10, height = 10*1.618)
}
