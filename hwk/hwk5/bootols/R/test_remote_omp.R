source("bootreg.R")
load("data.RData")

remote_cinv_time_omp <- system.time(
  boot.Cinv_remote_omp <- bootols(X, Y, B=1000, inv=TRUE))
remote_csolve_time_omp <- system.time(
  beta.Csolve_remote_omp <- bootols(X, Y, B=1000))

save(remote_cinv_time_omp, boot.Cinv_remote_omp,
     remote_csolve_time_omp, beta.Csolve_remote_omp,
     file = "remote_omp.RData")
