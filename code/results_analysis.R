# Project:   latentMAR
# Objective: Script to plot the results of the simulation
# Author:    Edoardo Costantini
# Created:   2021-07-27
# Modified:  2021-07-27

# Support Functions -------------------------------------------------------

  source("init.R")

# Load Results ------------------------------------------------------------

  inDir <- "../output/"
  runName <- "20210820_123036"

  # Read output
  gg_shape <- readRDS(paste0(inDir, runName, "_res.rds"))

# number of predictors fixed ----------------------------------------------

  # Define objects to select data
  parm <- unique(gg_shape$par)[1] # what parameter to plot
  result <- unique(gg_shape$variable)[2] # what result to plot
  met_cond <- paste0(unique(gg_shape$method),
                     collapse = "|")
  mar_cond <- unique(gg_shape$mar_pred)
  lam_cond <- unique(gg_shape$lambda)

  # Estiamtes distribution
  plot1 <- gg_shape %>%
    # Subset
    filter(grepl(parm, par)) %>%
    filter(grepl(met_cond, method)) %>%
    filter(grepl(result, variable)) %>%
    filter(n_mar_pred == 1 & mar_pred == "LV" | n_mar_pred == 4 & mar_pred == "IT") %>%

    # Main Plot
    ggplot(aes(x = variable,
               y = value,
               group = method,
               fill = method)) +
    geom_boxplot() +

    # Grid
    facet_grid(rows = vars(factor(mar_pred,
                                  labels = paste0(mar_cond, " as MAR predictor"))),
               cols = vars(factor(lambda,
                                  labels = paste0("fl = ", lam_cond)))) +
    # Format
    theme(text = element_text(size = 15),
          plot.title = element_text(hjust = 0.5),
          axis.text = element_text(size = 10),
          axis.text.x = element_blank(),
          axis.title = element_text(size = 15)) +
    labs(title = paste0(result, " for ", parm),
         x     = NULL,
         y     = NULL)

  plot1

  # FMI and RIV
  result <- unique(gg_shape$variable)[5] # what result to plot

  plot_2 <- gg_shape %>%
    # Subset
    filter(grepl(parm, par)) %>%
    filter(grepl(met_cond, method)) %>%
    filter(grepl(result, variable)) %>%
    filter(n_mar_pred == 1 & mar_pred == "LV" | n_mar_pred == 4 & mar_pred == "IT") %>%

    # Main Plot
    ggplot(aes(x = variable,
               y = value,
               group = mar_pred,
               fill = mar_pred)) +
    geom_boxplot() +

    # Grid
    facet_grid(cols = vars(factor(lambda,
                                  labels = paste0("fl = ", lam_cond)))) +
    # Format
    theme(text = element_text(size = 15),
          plot.title = element_text(hjust = 0.5),
          axis.text = element_text(size = 10),
          axis.text.x = element_blank(),
          axis.title = element_text(size = 15)) +
    labs(title = paste0(result, " for ", parm),
         x     = NULL,
         y     = NULL)
  plot_2

# number of predictors as exp factor --------------------------------------

  # Define objects to select data
  parm <- unique(gg_shape$par)[1] # what parameter to plot
  result <- unique(gg_shape$variable)[2] # what result to plot
  mar_cond <- unique(gg_shape$mar_pred)[1]
  met_cond <- paste0(unique(gg_shape$method)[1],
                     collapse = "|")
  nmar_cond <- unique(gg_shape$n_mar_pred)
  lam_cond <- unique(gg_shape$lambda)

  # Obtain plot
  plot1 <- gg_shape %>%
    # Subset
    filter(grepl(parm, par)) %>%
    filter(grepl(met_cond, method)) %>%
    filter(grepl(result, variable)) %>%

    # Main Plot
    ggplot(aes(x = variable,
               y = value,
               group = mar_pred,
               fill = mar_pred)) +
    geom_boxplot() +

    # Grid
    facet_grid(cols = vars(factor(n_mar_pred,
                                  labels = paste0(nmar_cond, " as MAR predictor"))),
               rows = vars(factor(lambda,
                                  labels = paste0("fl = ", lam_cond)))) +
    # Format
    theme(text = element_text(size = 15),
          plot.title = element_text(hjust = 0.5),
          axis.text = element_text(size = 10),
          axis.text.x = element_blank(),
          axis.title = element_text(size = 15)) +
    labs(title = paste0(result, " for ", parm),
         x     = NULL,
         y     = NULL)

  plot1