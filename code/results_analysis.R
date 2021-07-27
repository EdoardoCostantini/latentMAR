# Project:   latentMAR
# Objective: Script to plot the results of the simulation
# Author:    Edoardo Costantini
# Created:   2021-07-27
# Modified:  2021-07-27

## Make sure we have a clean environment:
  # rm(list = ls())

# Packages ----------------------------------------------------------------

  pack_list <- c("ggplot2",
                 "dplyr",
                 "forcats",
                 "stringr")

  lapply(pack_list, library, character.only = TRUE, verbose = FALSE)

# Load Results ----------------------------------------------------------

  # inDir <- "../output/"
  # runName <- "20210727_170009"
  #
  # # Read output
  # gg_shape <- readRDS(paste0(inDir, runName, "_res.rds"))
  #
  # # Support Functions
  # source("init.R")

# Plots -------------------------------------------------------------------

  ## Obtain plots
  parm <- unique(gg_shape$par)[1] # what parameter to plot
  result <- unique(gg_shape$variable)[5] # what result to plot
  met_cond <- unique(gg_shape$method)[1]
  mar_cond <- unique(gg_shape$mar_pred)
  lam_cond <- unique(gg_shape$lambda)

  plot1 <- gg_shape %>%
    # Subset
    filter(grepl(parm, par)) %>%
    filter(grepl(met_cond, method)) %>%
    filter(grepl(result, variable)) %>%
    # Change labels of X axis
    # mutate(variable = fct_relabel(variable, str_replace, result, "")) %>%

    # Main Plot
    ggplot(aes(x = variable,
               y = value,
               group = method)) +
    geom_boxplot() +

    # Grid
    facet_grid(rows = vars(factor(mar_pred,
                                  labels = paste0("MAR = ", mar_cond))),
               cols = vars(factor(lambda,
                                  labels = paste0("lambda = ", lam_cond))))
    # Format
    theme(text = element_text(size = 15),
          plot.title = element_text(hjust = 0.5),
          axis.text = element_text(size = 15),
          axis.text.x = element_text(angle = 45, hjust = 0.95),
          axis.title = element_text(size = 15)) +
    labs(title = "Some useful label",
         x     = NULL,
         y     = NULL)

  plot1