######################################
##### Model estimation in sim. C #####
######################################
library(ptmixed)

# set progressive gene id, 1 to 500 
# (run this parallelizing to obtain results faster)
nsub = 1

cat('simulation number', nsub, '\n') # simulation number

nrep = 50
n.agh = 5

for (case in 1:3) {
  print('***************************')
  print(paste('Case number', case))
  print('***************************')
  for (h in 1:nrep) {
    print(paste('*** Repetition number', h, '***'))
    load(paste('data/1/1-data-case-', case, '-nrep-', h, '.RData', sep = ''))
    
    y = data[,nsub]
    data.long = data.frame(id = id, group = group, 
                time = time, y = y, log.offset = log.offset)
    
    # estimate full model
    t.comp <- system.time( mixed.model <- try( 
      ptmixed(y ~ group + time, id = id, 
              offset = log.offset, trace = F,
              data = data.long, npoints = n.agh) ) )
    
    # estimate model under the null that time = 0
    if (!inherits(mixed.model, 'try-error')) {
      if (mixed.model$convergence == 0) {
        null.model = try( 
          ptmixed(y ~ group, id = id, offset = log.offset,
                  trace = F, hessian = F,
                  data = data.long, npoints = n.agh) )
      }
    }
    
    # save data:
    filename = paste('results/1.2.', case, '/1.2.', 
                     case, '-', nsub,'-rep-', h, '.RData', sep='')
    if (exists('null.model')) {
      save(t.comp, mixed.model, null.model, 
           file = filename)
    }
    if (!exists('null.model')) {
      save(t.comp, mixed.model, file = filename)
    }
    if (exists('mixed.model')) rm(mixed.model)
    if (exists('null.model')) rm(null.model)
    rm(data.long)
  }
}

