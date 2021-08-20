# Project:   latentMAR
# Objective: subroutine runCell a single condition for a single repetition
# Author:    Edoardo Costantini
# Created:   2021-07-27
# Modified:  2021-07-27

runCell <- function(rp, cond, parms, fs) {

# Example Internals -------------------------------------------------------

  # rp   = 1
  # cond = conds[9, ]

# Data Generation ---------------------------------------------------------

  ## Data according to condition
  dat_list <- dataGen(parms = parms, cond = cond)

  ## Define mar predictors based on condition
  if(cond$mar_pred == "LV"){
    pred_index <- seq_along(dat_list$dat_lv)[-parms$vmap_lv$ta][1:cond$n_mar_pred]
    preds <- dat_list$dat_lv[, pred_index, drop = FALSE]
  }
  if(cond$mar_pred == "IT"){
    pred_index <- seq_along(dat_list$dat_ob)[-parms$vmap_it$ta][1:cond$n_mar_pred]
    preds <- dat_list$dat_ob[, pred_index, drop = FALSE]
  }

  ## Impose Missingness according to condition
  target_miss_lv <- amputePerVar(targets = dat_list$dat_ob[, parms$vmap_it$ta],
                                 preds = preds,
                                 pm = parms$pm,
                                 type = "high")
  dat_miss_lv <- cbind(target_miss_lv, dat_list$dat_ob[, -parms$vmap_it$ta])

# Imputation --------------------------------------------------------------

  mids_out <- mice(dat_miss_lv,
                   m = parms$mice_ndt,
                   maxit = parms$mice_iters,
                   printFlag = FALSE)

# Analyze and pool --------------------------------------------------------

  # MI data
  if(cond$method == "MI"){
    mi_sat_fits <- miFitSat(mi_data = complete(mids_out, "all"),
                            model = satModWrite(names(dat_miss_lv[,
                                                        parms$vmap_it$ta]))
    )
    mi_sat_pool <- miPool(mi_fits = mi_sat_fits,
                          m = parms$mice_ndt,
                          N = parms$N)
    result <- mi_sat_pool
  }
  if(cond$method == "OG"){
    # Fit on complete data
    og_sat_fit <- miFitSat(mi_data = list(dat_list$dat_ob),
                           model = satModWrite(names(dat_miss_lv[,
                                                       parms$vmap_it$ta]))
    )[[1]]
    og_est_all <- parameterEstimates(og_sat_fit, standardized = TRUE)
    og_est <- og_est_all[, c("std.all", "est", "ci.lower", "ci.upper")]
    result <- cbind(par = apply(og_est_all[, 1:3], 1, paste0, collapse = ""),
                    og_est,
                    fmi = NA,
                    riv = NA)
  }

  # Attach descriptor
  row.names(cond) <- NULL # to avoid a warning in the cbind
  result <- cbind(rp = rp, cond, result)

# Store Output ------------------------------------------------------------

  ## Store Results
  saveRDS(result,
          file = paste0(fs$outDir,
                        "rep_", rp, "_", cond$tag,
                        ".rds")
  )

}