# Project:   latentMAR
# Objective: initialization script
# Author:    Edoardo Costantini
# Created:   2021-07-27
# Modified:  2021-07-27

# Packages ----------------------------------------------------------------

  pack_list <- c("parallel",
                 "MASS",
                 "lavaan",
                 "rlecuyer",
                 "forcats",
                 "stringr",
                 "ggplot2",
                 "dplyr",
                 "mice")

  lapply(pack_list, library, character.only = TRUE, verbose = FALSE)

# Load Functions ----------------------------------------------------------

  # Subroutines
  all_subs <- paste0("./subroutines/",
                     list.files("./subroutines/"))
  lapply(all_subs, source)

  # Functions
  all_funs <- paste0("./functions/",
                     list.files("./functions/"))
  lapply(all_funs, source)

  # Helper
  all_help <- paste0("./helper/",
                     list.files("./helper/"))
  lapply(all_help, source)

# Fixed Parameters --------------------------------------------------------

  # Empty List
  parms    <- list()

  # Simulation
  parms$rps <- 1e3
  parms$rep_counter <- 0
  parms$seed <- 2021
  parms$nStreams <- 1000

  # Data generation
  parms$N <- 1e3 # sample size
  parms$L <- 2 # number of latent variables
  parms$J <- 4 # number of measured items for latent variable
  parms$P <- parms$L*parms$J # number of latent variables
  parms$pm <- .3 # proportion of missings level
  parms$lv_mean   <- 0 # true latent mean
  parms$lv_var    <- 1 # true latent variance
  parms$lv_cov    <- .7 # true latent cov for target variables
  parms$item_mean <- 5 # 5 # true item mean
  parms$item_var  <- (2.5)^2 # true item variance

  # Latent Variables Map
  parms$vmap_lv <- list(ta = 1, # TArget of analysis
                        mp = 2)

  # Observed Variables (items) Map
  ta <- 1:(max(parms$vmap_lv$ta)*parms$J)
  mp <- (max(ta)+1):(max(ta)+(parms$J*length(parms$vmap_lv$mp)))
  parms$vmap_it <- list(ta = ta, mp = mp)

  # CFA model
  lv_items <- split(x = paste0("z", 1:(length(parms$vmap_lv$ta)*parms$J)),
                    f = rep(parms$vmap_lv$ta, each = parms$J))
  lv_models <- sapply(1:length(lv_items), function(it){
    paste0("l", it,
           " =~ ",
           paste0(lv_items[[it]], collapse = " + ")
    )
  }
  )
  parms$CFA_model <- paste(lv_models, collapse = "\n")

  # Imputation Routine
  parms$mice_ndt <- 5
  parms$mice_iters <- 25

  # Storing location
  parms$outDir <- "../output/"

# Experimental Conditions -------------------------------------------------

  mar_pred <- c("LV", "IT")
  lambda <- seq(.3, .9, .1)
  method <- c("OG", "MI")

  # Make Conditionsa
  conds <- expand.grid(mar_pred = mar_pred,
                       lambda = lambda,
                       method = method,
                       stringsAsFactors = FALSE)

  # Append Condition Tag
  conds$tag <- sapply(1:nrow(conds), function(i) {
    paste0(colnames(conds), conds[i, ], collapse = "_")
  }
  )