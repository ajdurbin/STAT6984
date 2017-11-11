source("bootreg.R")
load("data.RData")

local_cinv_time_omp <- system.time(
  boot.Cinv_local_omp <- bootols(X, Y, B=1000, inv=TRUE))
local_csolve_time_omp <- system.time(
  beta.Csolve_local_omp <- bootols(X, Y, B=1000))

save(local_cinv_time_omp, boot.Cinv_local_omp,
     local_csolve_time_omp, beta.Csolve_local_omp,
     file = "local_omp.RData")
