######################################
##### Model estimation in sim. D #####
######################################
# load R package
library(ptmixed)

# set case nsub (this can be trivially parallelized)
case = 1 # 1 to 3 (n = 10, 20, 40)
nseq = 1 # 1 to 500 (number of simulated genes)

# repeat each 10 times:
nrep = 10

for (h in 1:nrep) {
  cat(paste('Model estimation, replicate number', h)); cat('\n')
  
  # prepare data
  load(paste('data/D/2-data-case-', case, '-nrep-', h, '.RData', sep = ''))
  y = data[,nseq]
  data.long <- data.frame(id = id, group = group, time = time, y = y, log.offset = log.offset)
  
  # estimate full model
  mixed.model = try( 
    ptmixed(y ~ group + time, id = id,
            data = data.long, npoints = 20) )

  # estimate model under the null that group = 0
  if (mixed.model$convergence == 0) {
    null.model = try( 
      ptmixed(y ~ time, id = id,
              data = data.long, npoints = 20) )
  }
  
  # save data:
  save.image( paste('results/D', case, '/2.', case, '-', nseq,'-rep-', h, '.RData', sep='') )
  if (exists('mixed.model')) rm(mixed.model)
  if (exists('null.model')) rm(null.model)
}