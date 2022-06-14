
# Custom functions used in the book
# MAE - Mean Absolute Error
mae <- function(yhold, yfore, text = NA, round.digit = 3) {
  efore <- yhold - yfore # forecast errors
  mae <-  mean(abs(efore))
  if(is.na(text)){
    return(paste("MAE is", round(mae, round.digit), sep = " "))
  } else {
    
    return(paste(text, round(mae, round.digit), sep = " "))
  }
}
# MAPE- Mean Absolute Percentage Error
mape <- function(yhold, yfore, text = NA, round.digit = 3) {
  efore <- yhold - yfore # forecast errors
  mape <-  mean(abs(efore/yhold))
  if(is.na(text)){
    return(paste("MAPE is", round(mape*100, round.digit), sep = " "))
  } else {
    
    return(paste(text, round(mape*100, round.digit), sep = " "))
  }
}
# Model Selection Criteria - DIC, WAIC, PSBF and PIT
model.selection.criteria <- function(inla.result, plot.PIT = FALSE, n.train) {
  dic <- inla.result$dic$dic
  waic <- inla.result$waic$waic
  cpo <- inla.result$cpo$cpo
  psbf <- sum(log(cpo[1:n.train]))
  PIT <- inla.result$cpo$pit
  msc <- cbind(DIC=dic, WAIC = waic, PsBF = psbf)
  if(isTRUE(plot.PIT)){
    pit.hist <- hist(PIT, plot = F)
    return(list(msc = msc, hist=plot(pit.hist, main ="")))
    # return(list(msc = msc, pit.histogram=plot(pit.hist)))
  } else{
    return(msc = msc)
  }
  
}
## single line time series plot
tsline <- function(plot.series, title="", xlab="", ylab="", line.type="solid", line.size=1, line.color = "auto") {
  # line.type - twodash, solid, longdash,dotted, dotdash, dashed, blank
  # plot.data <- na.omit(plot.data)
  # The palette with black:
  cpalette <- c("#000000", "#56B4E9", "#009E73", "#D55E00", "#CC79A7","#E69F00") # color blind friendly
  cname <- c("black", "blue", "green", "red", "purple", "yellow")
  ## "#0072B2" "blue"
  # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
  n <- length(plot.series)
  mp <- cbind.data.frame(time = 1:n, dat = plot.series)
  mp.auto.color <- mp %>%
    ggplot(aes(x=time, y=dat)) +
    geom_line(size=line.size,linetype=line.type)+
    labs(x = xlab, y = ylab, colour="")+
    ggtitle(title)+ theme(legend.position = "none")
  colour <- cpalette[match(line.color, cname)]
  mp.custom.color <- mp %>%
    ggplot(aes(x=time, y=dat, color=colour)) +
    geom_line(size=line.size,linetype=line.type)+
    scale_color_manual(values=colour)+
    labs(x = xlab, y = ylab, colour="")+
    ggtitle(title) + 
    theme(legend.position = "none")
  ifelse(line.color=="auto", return(mp.auto.color), return(mp.custom.color))
}


# Multiple Line Plots
multiline.plot <- function(plot.data, title ="", xlab="", ylab="", line.type="solid", line.size=1, line.color = "auto") {
  # line.type - twodash, solid, longdash,dotted, dotdash, dashed, blank
  # plot.data <- na.omit(plot.data)
  # The palette with black:
  cpalette <- c("#000000", "#009E73", "#56B4E9", "#D55E00", "#CC79A7","#E69F00") # color blind friendly
  cname <- c("black", "green", "blue", "red", "purple","yellow")
  # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
  mp <- plot.data %>%
    # gather(key = "key", value = "dat", -time) %>%
    pivot_longer(-time, names_to = "key", values_to = "dat")
  u <- unique(mp$key)
  mp$key <- factor(mp$key, levels = u)
  levels(mp$key) <- u
    mp.auto.color <- mp %>%
      ggplot(aes(x=time, y=dat, color=key)) +
      geom_line(size=line.size,linetype=line.type)+
      labs(x = xlab, y = ylab, colour="")+
      ggtitle(title)
    mp.custom.color <- mp %>%
      ggplot(aes(x=time, y=dat, color=key)) +
      geom_line(size=line.size,linetype=line.type)+
      scale_color_manual(values=cpalette[match(line.color, cname)])+
      labs(x = xlab, y = ylab, colour="")+
      ggtitle(title)
    ifelse(line.color=="auto", return(mp.auto.color), return(mp.custom.color))
}
# Filter Estimates using INLA - for "rw1", "AR(1)", "AR(2)" with trend and level options
filt.inla <- function(data.series, model = "rw1", ar.order = 0, trend="no", alpha = "no") {
  pt <- proc.time()
  ### model formula
  eg <- expand.grid(model=c("rw1", "ar"),
                    trend=c("yes", "no"),
                    alpha=c("yes", "no")
  )
  eg <- apply(eg, 2, as.character)
  choice <- paste0(c(model, trend, alpha), collapse = ".")
  
  
  dat <- filt <- vector("list", length(data.series))
  for (k in 1:length(data.series)) {
    dat[[k]] <- data.series[1:k]
  }
  
  get.filter <- function(df, ar.order) {
    id <- trnd <- 1:length(df)
    inla.dat <- cbind.data.frame(data.series=df, trnd, id)
    ar.order <- as.numeric(ar.order)
    formula <- switch(choice, 
                      "rw1.no.no"=as.formula(paste0("data.series~-1+","f(id, model='rw1', constr=FALSE)")),
                      "rw1.yes.no"=as.formula(paste0("data.series~-1+trnd+","f(id, model='rw1', constr=FALSE)")),
                      "rw1.no.yes"=as.formula(paste0("data.series~ 1+","f(id, model='rw1', constr=FALSE)")),
                      "rw1.yes.yes"=as.formula(paste0("data.series~ 1+trnd+","f(id, model='rw1', constr=FALSE)")),
                      "ar.no.no"=as.formula(paste0("data.series~ -1+","f(id, model='ar', order = ar.order)")),
                      "ar.yes.no"=as.formula(paste0("data.series~ -1+trnd+","f(id, model='ar', order = ar.order)")),
                      "ar.no.yes"=as.formula(paste0("data.series~ 1+","f(id, model='ar', order = ar.order)")),
                      "ar.yes.yes"=as.formula(paste0("data.series~ 1+trnd+","f(id, model='ar', order = ar.order)")),
    )
    model.inla <- inla(
      formula,
      family = "gaussian",
      data = inla.dat,
      control.predictor = list(compute = TRUE),
      control.compute = list(dic = T, config = TRUE, cpo = TRUE),
      verbose = T
    ) 
    
    
    filt <- tail(model.inla$summary.random$id, 1)
    return(filt=filt)
  }
  #### model execution
  require(doParallel)
  require(foreach)
  cores <-  detectCores()
  if(cores >2){
    registerDoParallel(cores = detectCores()-1)
  }
  
  if(ar.order > 1){
    filt.all <- foreach (d=(2+ar.order):length(data.series),.packages = c('tidyverse','INLA'), .verbose = T) %dopar% {
      get.filter(dat[[d]], ar.order = ar.order)
      
    }
  } else{
    filt.all <- foreach (d=2:length(data.series),.packages = c('tidyverse','INLA'), .verbose = T) %dopar% {
      get.filter(dat[[d]], ar.order = ar.order)
      
    }
  }
  
  
  filt.all.bind <- bind_rows(filt.all)
  # filt.est <- c(NA, filt.all$mean)
  inla.time <- proc.time() - pt
  
  return(list(filt.all.bind = filt.all.bind, time.taken = inla.time))
  # return(list(filt=filt, summary.model=summary(model.inla)))
}
# Simulate data from random walk plus noise model
simulation.from.rw1 <- function(sample.size = 500, burn.in = 100, level =0, drift = 0, V = 0.5, W = 0.25, plot.data = FALSE, seed = 123457) {
  set.seed(seed)
  sigma2.v <- V
  sigma2.w <- W
  n <- sample.size 
  n.burn <- burn.in
  n.tot <- n + n.burn
  # variance.x <- sigma2.w/(1-phi^2)
  y.tot <- x.tot <- vector("numeric", length = n.tot)
  x0 <- 0
  w <- rnorm(n.tot, 0, sqrt(sigma2.w))
  v <- rnorm(n.tot, 0, sqrt(sigma2.v))
  x.tot[1] <- x0 + drift + w[1]
  y.tot[1] <- x.tot[1] + level + v[1]
  for (i in 2:n.tot) {
    x.tot[i] <- x.tot[i - 1] + drift + w[i]
    y.tot[i] <- x.tot[i] + level + v[i]
  }
  
  y <- tail(y.tot, n)
  # 
  if(isTRUE(plot.data)){
    # sim.plot <- ts.plot(y, xlab="t", ylab = "y") # using ts.plot
    sim.plot <- tsline(y, line.color = "red", xlab = "t", ylab = "y",
                       line.size = 0.6) + theme(legend.position = "none") # using ggplot
    return(list(sim.data=y, sim.plot = sim.plot))
  } else{
    return(list(sim.data=y))
  }
  
}
# simulate data from AR (of order >=1) plus noise model
simulation.from.ar <- function(sample.size = 500, burn.in = 100, phi = c(0.5), level =0, drift = 0, V = 0.25, W = 0.25, plot.data = FALSE, seed = 123457) {
  set.seed(seed)
  sigma2.v <- V
  sigma2.w <- W
  phi <- phi 
  n <- sample.size 
  n.burn <- burn.in
  n.tot <- n + n.burn
  # variance.x <- sigma2.w/(1-phi^2)
  y.tot <- x.tot <- vector("numeric", length = n.tot)
  v <- rnorm(n.tot,0,sqrt(sigma2.v))
  x.tot <-arima.sim(list(ar = phi,ma = 0), n = n.tot, sd = sqrt(sigma2.w)) + drift
  y.tot <- level + x.tot + v
  
  y <- tail(y.tot, n)
  
  if(isTRUE(plot.data)){
    # sim.plot = ts.plot(y, xlab="t", ylab = "y", col="red") ## using ts.plot
    sim.plot <- tsline(y, line.color = "red", xlab = "t", ylab = "y",
                       line.size = 0.6) + theme(legend.position = "none") ## using ggplot
    return(list(sim.data=y, sim.plot = sim.plot))
  } else{
    return(list(sim.data=y))
  }
  
}
# Simulate date from Poisson time series and  the latent process is a Gaussian AR(1) process
simulation.from.ar.poisson <- function(sample.size = 500, burn.in = 100, phi = c(0.5), level =0, drift = 0, W = 0.25, plot.data = FALSE, seed = 123457) {
  set.seed(seed)
  sigma2.w <- W
  phi <- phi 
  n <- sample.size 
  n.burn <- burn.in
  n.tot <- n + n.burn
  y.tot <- x.tot <- vector("numeric", length = n.tot)
  x.tot <-arima.sim(list(ar = phi,ma = 0), n = n.tot, sd = sqrt(sigma2.w)) + drift
  y.tot <- rpois(n.tot, exp(level+x.tot))
  y <- tail(y.tot, n)
  
  if(isTRUE(plot.data)){
    # sim.plot = ts.plot(y, xlab="t", ylab = "y", col="red") ## using ts.plot
    sim.plot <- tsline(y, line.color = "red", xlab = "t", ylab = "y",
                       line.size = 0.6) + theme(legend.position = "none") ## using ggplot
    return(list(sim.data=y, sim.plot = sim.plot))
  } else{
    return(list(sim.data=y))
  }
  
}
# Simulate data from AR(1) with covariates plus noise model
simulation.from.ar1c <- function(sample.size = 500, burn.in = 100, phi.ar1 = 0.6, ncol.z = 2, beta.z = rep(0.5, ncol.z), V = 0.25, W = 0.25, plot.data = FALSE, seed = 123457) {
  set.seed(seed)
  sigma2.v <- V
  sigma2.w <- W
  phi <- phi.ar1
  x0 <- 0
  n <- 500
  n.burn <- 100
  n.tot <- n + n.burn
  y.tot <- x.tot <- z1 <- z2 <- vector("numeric", length = n.tot)
  v <- rnorm(n.tot, 0, sqrt(sigma2.v))
  w <- rnorm(n.tot, 0, sqrt(sigma2.w))
  Z <- matrix(0, nrow = n.tot, ncol = ncol.z)
  for (i in 1:ncol.z) {
    Z[,i] <- rnorm(n.tot, 0, 1)
  }
  x.tot[1] <- rnorm(1,0, sqrt(sigma2.w/(1-phi^2)))
  y.tot[1] <- x.tot[1] + v[1]
  beta <- matrix(beta.z, ncol = 1)
  beta.times.Z <- Z %*% beta
  for (i in 2:n.tot) {
    x.tot[i] <- phi * x.tot[i - 1] + beta.times.Z[i-1]+ w[i]
    y.tot[i] <- x.tot[i] + v[i]
  }
  y <- tail(y.tot, n)
  Z <- tail(Z, n)
  out <- list(y = y, Z = Z)
  if(isTRUE(plot.data)){
    # sim.plot = ts.plot(y, xlab="t", ylab = "y", col="red") ## using ts.plot
    sim.plot <- tsline(y, line.color = "red", xlab = "t", ylab = "y",
                       line.size = 0.6) + theme(legend.position = "none") ## using ggplot
    return(list(sim.data=y, sim.plot = sim.plot))
  } else{
    return(list(sim.data = out))
  }
  
}
# Simulate data from a second order DLM
simulation.from.second.order.dlm <- function(sample.size = 200, V = 0.01, W1 = 1e-04, W2 = 1e-04, plot.data = FALSE, seed = 123457) {
  set.seed(seed)
  sigma2.v <- V
  sigma2.w1 <- W1
  sigma2.w2 <- W2
  n <- n.tot <- sample.size 
  # n.burn <- burn.in
  # n.tot <- n + n.burn
  # variance.x <- sigma2.w/(1-phi^2)
  y.tot <- vector("numeric", length = n.tot)
  x.tot <- matrix(0, n.tot, 2)
  x0 <- c(0, 0)
  w <- cbind(rnorm(n.tot, 0, sqrt(sigma2.w1)), rnorm(n.tot, 
                                                     0, sqrt(sigma2.w2)))
  v <- rnorm(n.tot, 0, sqrt(sigma2.v))
  x.tot[1, 2] <- x0[2] + w[1, 2]
  x.tot[1, 1] <- x0[1] - x0[2] + w[1, 1]
  y.tot[1] <- x.tot[1, 1] + v[1]
  for (i in 2:n.tot) {
    x.tot[i, 1] <- x.tot[i - 1, 1] + x.tot[i - 1, 2] + 
      w[i, 1]
    x.tot[i, 2] <- x.tot[i - 1, 2] + w[i, 2]
    y.tot[i] <- x.tot[i, 1] + v[i]
  }
  if(isTRUE(plot.data)){
    # sim.plot = ts.plot(y.tot, xlab="t", ylab = "y", col="red") ## using ts.plot
    sim.plot <- tsline(y.tot, line.color = "red", xlab = "t", ylab = "y",
                       line.size = 0.6) + theme(legend.position = "none") ## using ggplot
    return(list(sim.data=y.tot, sim.plot = sim.plot))
  } else{
    return(list(sim.data=y.tot))
  }
  
}

get_legend <- function(myggplot) {
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x)
    x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
} # code from http://www.sthda.com/english/wiki/wiki.php?id_contents=7930

format.inla.out <- function(df, digits = 3) {
  df <- rownames_to_column(as.data.frame(df), var = "name")
  # df <- as.data.frame(df, row.names = FALSE)
  # df <- as_tibble(df)
  df$name <- df$name %>% 
    str_remove_all("the") %>% 
    str_replace_all("observations", replacement = "obs") %>% 
    str_trim("both") %>% 
    str_squish() %>% 
    str_replace_all("Predictor", replacement = "Pred") %>% 
    str_replace_all("fitted", replacement = "fit") %>% 
    str_remove_all("[()]")
  
  colnames(df) <- colnames(df) %>% 
    str_replace_all("quant", replacement = "q")
  # name <- df$name
  # df <- df %>% 
  #   select(-name) %>% 
  #   map_dbl(function(x) round(x, digits = 3)) %>% 
  #   as_tibble() %>% 
  #   mutate(name = name) %>% 
  #   select(name, everything())
  # print(df, pillar.bold = TRUE, pillar.neg = FALSE,
  #       pillar.min_chars = Inf,pillar.max_dec_width =3)
  # df <- df %>% 
  #   mutate(across(where(is.double),function(x)round(x,digits = digits))) 
  
  g.id <- grep("ID", colnames(df))
  if(length(g.id)>0){
    # select(df, -name) %>% 
    #   gt %>% 
    #   fmt_number(columns = where(is.double),decimals = digits, use_seps = FALSE) %>% 
    #   tab_options(table.align = "left",row.striping.include_table_body = FALSE,
    #               row.striping.include_stub = FALSE,
    #               table.border.top.color = "white",
    #               heading.border.bottom.color = "white",
    #               row_group.border.bottom.color = "white",
    #               row_group.border.top.color = "white")
    df <- select(df, -name) %>% 
      mutate(across(where(is.double),function(x)round(x,digits = digits))) 
    print(df, right = FALSE)
    # cat(format(df, pillar.bold = TRUE, pillar.neg = FALSE,
    #            pillar.min_chars = Inf,pillar.max_dec_width =10, width = 70)[-c(1L,3L)], sep = "\n")
  }else{
          # df %>% 
          #   gt %>% 
          #   fmt_number(columns = where(is.double),decimals = digits, use_seps = FALSE) %>% 
          #   tab_options(table.align = "left",row.striping.include_table_body = FALSE,
          #               row.striping.include_stub = FALSE,
          #               table.border.top.color = "white",
          #               heading.border.bottom.color = "white",
          #               row_group.border.bottom.color = "white",
          #               row_group.border.top.color = "white")
  # cat(format(df, pillar.bold = TRUE, pillar.neg = FALSE,
  #       pillar.min_chars = Inf,pillar.max_dec_width =10, width = 70)[-c(1L,3L)], sep = "\n")
    df <- df %>% 
      mutate(across(where(is.double),function(x)round(x,digits = digits))) 
    print(df, right = FALSE)
  }
  # https://stackoverflow.com/questions/64734495/suppressing-the-column-classes-for-tibbles
  # return(df)
}
# , pillar.min_title_chars = Inf

# summary.inla.custom <- function(inla.model) {
#   s.temp <- summary(inla.model)
#   if(length(s.temp$hyperpar)>0){
# 
#   s.temp$hyperpar <- format.inla.out(s.temp$hyperpar[,c(1,2)]) 
#   }
#   if(length(s.temp$fixed)>0){
# 
#   s.temp$fixed <- format.inla.out(s.temp$fixed[,c(1:5)]) 
#   }
#   cat(s.temp, sep = "\n")
# }


# precision of state variable - AR(1)
prec.state.ar1 <- function(state.sigma2, phi) {
  return((1/state.sigma2)*(1-phi^2))
}

## rgeneric
# Define previous variables as global to avoid warnings()
utils::globalVariables(c("X","p","k","n"))
inla.rgeneric.VAR_p <-
  function(cmd = c("graph", "Q", "mu", "initial", "log.norm.const",
                   "log.prior", "quit"), theta = NULL)
  {
    envir = parent.env(environment())
    # Interpret.theta: Returns the list of real parameters from theta values:
    interpret.theta <- function(){
      n_phi <- k*k*p  
      n_prec <- k   
      n_tot <- n_phi + n_prec  
      #Phi matrix      
      phi_VAR <- sapply(theta[as.integer(1:n_phi)], function(x) { x })
      #W matrix  precisions     
      wprec <- sapply(theta[as.integer((n_phi+1):(n_phi+n_prec))], function(x) { exp(x) })
      param <- c(phi_VAR, wprec)  
      # Initial I_k for error cov W 
      W <- diag(1, n_prec)
      # sqrt(W)
      st.dev <- 1 / sqrt(wprec)
      # Matrix of st. dev.
      st.dev.mat <- matrix(st.dev, ncol = 1) %*% matrix(st.dev, nrow = 1)
      # Diagonal W
      W <- W * st.dev.mat # Element wise multiplication
      # Invert W
      PREC <- solve(W)
      return (list(param = param, VACOV = W, PREC = PREC, phi_vec=c(phi_VAR)
                   ,n_phi=n_phi,n_prec=n_prec))
    }
    
    # Graph function: Q (precision) matrix
    graph = function() {
      return (inla.as.sparse(Q()))
    }
    # Precision matrix
    Q <- function()
    {
      # Parameters in model 
      param <- interpret.theta()
      # Precision matrix
      Q <- bdiag(rep(list(param$PREC),n))
      return (inla.as.sparse(Q))
    }
    # Mean of model
    mu = function() {
      par = interpret.theta()
      # Form lag_list of the regressor matrix: each list has vectorized form:
      # Ref: Helmut's book
      a <- apply(X,1,function(x) as.vector(x))
      l <- list()
      for(i in 1:p){
        rm_col <- (ncol(a)-(i-1)) : ncol(a)
        l[[i]] <- cbind(matrix(0,nrow=nrow(a),ncol = i),a[,-rm_col])
      }
      Z <- do.call(rbind,l)
      X_star <- bdiag(rep(list(t(Z)),k))
      beta_star <- matrix(par$phi_vec)  
      res <- X_star %*% beta_star
      return(as.numeric(res))
    }
    # Log-normalizing constant
    log.norm.const <- function() {
      n <- n
      k <- k
      Q1 <- Q()
      val <- -(n*k)/2 *log(2*pi) + 0.5*log(det(Q1))
      return (val)
    }
    # Same prior as used in package INLAMSM
    # return the log-prior for the hyperparameters.
    log.prior <- function() {
      param <- interpret.theta()
      pars <- param$param
      k <- k
      total_par <- param$n_phi + param$n_prec 
      # Normal-inverse gamma prior
      # Normal prior for phi's:
      theta_phi <- theta[1:param$n_phi]
      phi_prior <- sum(sapply(theta_phi,function(x) dnorm(x,mean=0,sd=1,log=TRUE)))
      theta_prec <- theta[(param$n_phi + 1):(param$n_phi+param$n_prec)]
      prec <- pars[(param$n_phi + 1):(param$n_phi+param$n_prec)]
      prec_prior <- sum(sapply(prec,function(x) dgamma(x,shape = 1,scale = 1,log=TRUE)))
      prec_jacob <- sum(theta_prec) #for precision terms
      prior_val <- phi_prior + prec_prior + prec_jacob 
      return (prior_val)
    }
    # initial: Returns the initial values of theta 
    initial = function(theta) {
      phi_init <- rep(as.vector(diag(0.5,ncol = k,nrow = k)),p)
      prec_init <- rep(1,k)
      init <- c(phi_init,prec_init)
      return (init)
    }
    quit <- function(theta) {
      return (invisible())
    }    
    # FIX for rgeneric to work on R >= 4
    # Provided by E. T. Krainski
    if (as.integer(R.version$major) > 3) {
      if (!length(theta))
        theta <- initial()
    } else {
      if (is.null(theta)) {
        theta <- initial()
      }
    }
    val <- do.call(match.arg(cmd), args = list())
    return (val)
  }
