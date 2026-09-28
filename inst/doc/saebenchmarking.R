## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----setup--------------------------------------------------------------------
library(saebenchmarking)

## -----------------------------------------------------------------------------
head(data_eblup[, c("area", "weight", "direct", "vardir", "eblup", "mse")])

## -----------------------------------------------------------------------------
methods <- c("difference", "ratio", "optimum")
bench <- lapply(methods, function(m) {
  sae_benchmarking(method = m, direct = "direct", weight = "weight",
                   estimate = "eblup", mse = "mse", vardir = "vardir",
                   phi_source = "mse", data = data_eblup)
})
names(bench) <- methods

sapply(bench, function(x) x$aggregation[, c("Estimate", "Bench", "Target")])

## -----------------------------------------------------------------------------
Z   <- cbind(1, data_eblup$z)
s2v <- attr(data_eblup, "sigma2_v")

mse_diff <- mse_benchmarking(bench$difference, estimator = "eblup",
                             z = Z, sigma2_v = s2v, fitting_method = "REML")

mse_opt <- mse_benchmarking(bench$optimum, estimator = "eblup",
                            z = Z, sigma2_v = s2v, B = 100, seed = 2026)

comparison <- data.frame(
  EBLUP      = data_eblup$mse,
  Difference = mse_diff$mse,
  Optimum    = mse_opt$mse
)
head(round(comparison, 4))

## -----------------------------------------------------------------------------
bm_hb <- sae_benchmarking("optimum", direct = "direct", weight = "weight",
                          estimate = "theta_hb", vardir = "vardir",
                          phi_source = "vardir", data = data_hb)

mse_hb <- mse_benchmarking(bm_hb, estimator = "hb",
                           theta_hb = data_hb$theta_hb, V_hb = data_hb$var_hb)
head(data.frame(V_hb = data_hb$var_hb, PMSE = mse_hb$mse))

