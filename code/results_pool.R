# Project:   latentMAR
# Objective: Pool the results of a simulation study given its id folder
# Author:    Edoardo Costantini
# Created:   2021-07-27
# Modified:  2021-07-27

## Make sure we have a clean environment:
rm(list = ls())

## Initialize the environment:
source("./init.R")

# Load Results ----------------------------------------------------------

  inDir <- "../output/"
  files <- grep("tar", list.files(inDir), value = TRUE)
  target_tar <- files[length(files)]
  output <- readTarGz(target_tar)

# Restructure Results -----------------------------------------------------
# list of conditions containing results for every repetition

  out <- do.call(rbind, output$out)
  gg_shape <- reshape2::melt(out, id.var = colnames(out)[1:7])

  # Save
  saveRDS(gg_shape,
          file = paste0("../output/",
                        output$name_run,
                        "_res",
                        ".rds")
  )