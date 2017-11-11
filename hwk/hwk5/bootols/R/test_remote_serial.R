source("bootreg.R")
load("data.RData")

# testing bootstrapped OLS
remote_rlm_time <- system.time(
  boot.Rlm_remote <- bootols.R(X, Y, B=1000, method="lm"))
remote_rinv_time <- system.time(
  boot.Rinv_remote <- bootols.R(X, Y, B=1000, method="inv"))
remote_rsolve_time <- system.time(
  beta.Rsolve_remote <- bootols.R(X, Y, B=1000, method="solve"))
remote_rcp_time <- system.time(
  beta.Rcp_remote <- bootols.R(X, Y, B=1000, method="cp"))
remote_cinv_time <- system.time(
  boot.Cinv_remote <- bootols(X, Y, B=1000, inv=TRUE))
remote_csolve_time <- system.time(
  beta.Csolve_remote <- bootols(X, Y, B=1000))

save(remote_rlm_time, boot.Rlm_remote, 
     remote_rinv_time, boot.Rinv_remote,
     remote_rsolve_time, beta.Rsolve_remote,
     remote_rcp_time, beta.Rcp_remote,
     remote_cinv_time, boot.Cinv_remote,
     remote_csolve_time, beta.Csolve_remote,
     file = "remote_serial.RData")
