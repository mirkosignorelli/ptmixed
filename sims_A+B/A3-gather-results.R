######################################
###### Gather results in sim. A ######
## (once all computations are done) ##
######################################

case = 6 # 6 to 10
subcase = 1 # 1 to 3

nrep = 1000
library(ptmixed)
b.hat = matrix(NA, nrow = nrep, ncol = 3)
a.hat = D.hat = s.hat = rep(NA, nrep)
lik.h1 = lik.h0 = rep(NA, nrep)
wald.stat = lrt.stat = rep(NA, nrep)

failed.sims = c()
no.conv = c()
safe.hess = c()

for (ind in 1:nrep) {
  is.ok = try(load( paste('results/', case, '/res-', case, '-', subcase, '-', ind, '.RData', sep='') ))
  if (inherits(is.ok, 'try-error')) {
    failed.sims = c(failed.sims, ind)
  }
  else if (!inherits(is.ok, 'try-error')) {
    if (inherits(mixed.model, 'try-error')) failed.sims = c(failed.sims, ind)
    else {
      if (mixed.model$convergence != 0) no.conv = c(no.conv, ind)
      else {
        # MLE:
        b.hat[ind,] = mixed.model$mle[1:3]
        D.hat[ind] = mixed.model$mle[4]
        a.hat[ind] = mixed.model$mle[5]
        s.hat[ind] = mixed.model$mle[6]
        # Wald test
        temp = try( summary(mixed.model) )
        if (!inherits(temp, 'try-error')) {
          wald.stat[ind] = as.matrix(temp$coefficients)[3,3]  
        }
        # VALUES FOR LRT:
        lik.h1[ind] = mixed.model$logl
        if (exists('null.model')) {
          if (!inherits(null.model, 'try-error')) {
            if (null.model$convergence ==0) {
              lik.h0[ind] = null.model$logl
              lrt.stat[ind] = 2*(lik.h1[ind]-lik.h0[ind])
            }
          }
        }
      }
    }
    if (exists('mixed.model')) rm(mixed.model)
    if (exists('null.model')) rm(null.model)
    if (exists('temp')) rm(temp)
  }
  print(ind)
}

save.image(paste('results/full-results/', case, '-', subcase, '.RData', sep = ''))
