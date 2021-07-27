# Project:   latentMAR
# Objective: costum MI fitting function for saturated model
# Author:    Edoardo Costantini
# Created:   2021-07-27
# Modified:  2021-07-27

miFitSat <- function (mi_data, model){
  # Fit saturated model
  fits <- lapply(mi_data, function (x){
    lavaan::sem(model = model,
                data = x,
                likelihood = "wishart",
                std.lv = TRUE)
  })
}
