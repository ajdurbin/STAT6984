# File name: mcpi_rmpi.r
# Description: Calculate pi using Monte Carlo (Rmpi)
# Run: Rscript mcpi_rmpi.r

# Load the Rmpi library
library(Rmpi)

# Number of points to use
n.pts <- 1000000

# The number of slaves to use
nsl <- 7

# Start timer for total time
tm.tot.st = Sys.time()

# Spawn nsl slaves
mpi.spawn.Rslaves(nslaves=nsl)

# Start timer for compute time
tm.comp.st = Sys.time()

# Function to calculate whether a point is in the unit circle
in.ucir <- function(x) { as.integer((x[,1]^2 + x[,2]^2) <= 1) }

# Function to generate n.pts random points in the unit
# square and count the number in the unit circle
count.in.cir <- function(n.pts) {
  # Create a list of n.pts random (x,y) pairs
  m <- matrix(runif(n.pts*2),n.pts,2)
  # Determine whether each point is in unit circle
  in.cir <- in.ucir(m)
  # Count the points in the unit circle
  return ( sum(in.cir) )
}

# Send variable n.pts and functions in.ucir and count.in.cir to slaves
mpi.bcast.Robj2slave(n.pts)
mpi.bcast.Robj2slave(in.ucir)
mpi.bcast.Robj2slave(count.in.cir)

# Call count.in.cir() on slaves
mpi.bcast.cmd(n.in.cir <- count.in.cir(n.pts))
# Call count.in.cir() on master
n.in.cir <- count.in.cir(n.pts)

# Use mpi.reduce() to total across all processes
# Have to do in two steps to avoid MPI hang
mpi.bcast.cmd( mpi.reduce(n.in.cir, type=1, op="sum") ) #Slaves
n.in.cir <- mpi.reduce(n.in.cir, type=1, op="sum")      #Master

# pi is roughly 4 * the number of points in the unit
# circle / the number of pts
pi.approx <- 4*n.in.cir/( mpi.comm.size()*n.pts )
pi.err <- abs(pi - pi.approx)/pi

# Stop timer for compute time
tm.comp = as.numeric(Sys.time() - tm.comp.st, units="secs")

# Print results
cat(paste('approximate value of pi is: ',pi.approx,'\n'))
cat('  relative error = ',pi.err,'\n')
cat(paste('calculated using ',mpi.comm.size()*n.pts,' points\n'))

# Shut down the slaves
mpi.close.Rslaves(dellog = FALSE)

# Stop timer for total time and print results
tm.tot = as.numeric(Sys.time() - tm.tot.st, units="secs")
cat('  computational time = ',tm.comp,'\n')
cat('          total time = ',tm.tot,'\n')

# Clean up resources and close R
mpi.quit()
