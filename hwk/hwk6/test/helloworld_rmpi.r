# File: helloworld_rmpi.r
# Description: Rmpi Hello World
# Run: Rscript helloworld_rmpi.r

# Load the R MPI package if it is not already loaded.
if (!is.loaded("mpi_initialize")) {
 library("Rmpi")
 }
 
# Spawn as many slaves as possible
# This example uses the PBS_NP environment variable
# to get the number of available processors
us = as.character(Sys.getenv("PBS_NP"))
us2 = as.numeric(us) - 1 
mpi.spawn.Rslaves(nslaves=us2)
 
# In case R exits unexpectedly, have it automatically clean up
# resources taken up by Rmpi (slaves, memory, etc...)
.Last <- function(){
if (is.loaded("mpi_initialize")){
  if (mpi.comm.size(1) > 0){
    print("Please use mpi.close.Rslaves() to close slaves.")
    mpi.close.Rslaves()
    }
  print("Please use mpi.quit() to quit R")
  .Call("mpi_finalize")
  }
}
 
# Tell all slaves to return a message identifying themselves
mpi.remote.exec(paste("I am",mpi.comm.rank(),"of",mpi.comm.size()))
 
# Tell all slaves to close down, and exit the program
mpi.close.Rslaves(dellog = FALSE)
mpi.quit()
 
# Since mpi_finalize is called in mpi.quit(), no more mpi commands
# should be invoked here.  Thus, detach the Rmpi library.
detach("Rmpi")
