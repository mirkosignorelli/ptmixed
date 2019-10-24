######################################
##### Model estimation in sim. A #####
######### (time effect = 0) ##########
######################################
# load R package
library(ptmixed)

# set case, subcase and nsub (this can be trivially parallelized)
# case (6-10): 6 is a = -5, 10 is a = 1 (see script A1)
# subcase (1-3): 1 is n = 10, 2 is n = 20, 3 is n = 40
# nsub (1-1000): repetition number
case = 6
subcase = 1
nsub = 1

load(paste('data/data-', case, '-', subcase, '.RData', sep = ''))

y = data[,nsub]
df = data.frame(y, group, time)

mixed.model = try( 
  ptmixed(y ~ group + time, id = id, 
          data = df, npoints = 20,
          hessian = T, trace = T, theta.start = NULL) )

if (!inherits(mixed.model, 'try-error')) {
  if (mixed.model$convergence == 0) {
    null.model = try( 
      ptmixed(y ~ group, id = id, 
              data = df, npoints = 20,
              hessian = T, trace = T, theta.start = NULL) )
  }
}

if (exists('null.model')) {
  save(mixed.model, null.model,
       file = paste('results/', case, '/res-', case, '-', subcase, '-', nsub, '.RData', sep='') )
}
if (!exists('null.model')) {
  save(mixed.model,
       file = paste('results/', case, '/res-', case, '-', subcase, '-', nsub, '.RData', sep='') )
}

