######################################
##### Model estimation in sim. B #####
######### (group effect = 0) #########
######################################

# load R package
library(ptmixed)

# set case, subcase and nsub (this can be trivially parallelized)
# case (1-5): 1 is a = -5, 5 is a = 1 (see script B1)
# subcase (1-3): 1 is n = 10, 2 is n = 20, 3 is n = 40
# nsub (1-1000): repetition number
case = 1
subcase = 1
nsub = 1

load(paste('data/data-', case, '-', subcase, '.RData', sep = ''))

y = data[,nsub]
df = data.frame(y, group, time)

mixed.model = try( 
  ptmixed(y ~ group + time, id = id, 
          data = df, npoints = 20) )

if (!inherits(mixed.model, 'try-error')) {
  if (mixed.model$convergence == 0) {
    null.model = try( 
      ptmixed(y ~ time, id = id, 
              data = df, npoints = 20) )
  }
}

save.image( paste('results/', case, '/res-', case, '-', subcase, '-', nsub, '.RData', sep='') )

