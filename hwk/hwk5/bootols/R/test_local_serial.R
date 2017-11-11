source("bootreg.R")
load("data.RData")

# testing bootstrapped OLS
local_rlm_time <- system.time(
  boot.Rlm_local <- bootols.R(X, Y, B=1000, method="lm"))
local_rinv_time <- system.time(
  boot.Rinv_local <- bootols.R(X, Y, B=1000, method="inv"))
local_rsolve_time <- system.time(
  beta.Rsolve_local <- bootols.R(X, Y, B=1000, method="solve"))
local_rcp_time <- system.time(
  beta.Rcp_local <- bootols.R(X, Y, B=1000, method="cp"))
local_cinv_time <- system.time(
  boot.Cinv_local <- bootols(X, Y, B=1000, inv=TRUE))
local_csolve_time <- system.time(
  beta.Csolve_local <- bootols(X, Y, B=1000))

save(local_rlm_time, boot.Rlm_local, 
     local_rinv_time, boot.Rinv_local,
     local_rsolve_time, beta.Rsolve_local,
     local_rcp_time, beta.Rcp_local,
     local_cinv_time, boot.Cinv_local,
     local_csolve_time, beta.Csolve_local,
     file = "local_serial.RData")
