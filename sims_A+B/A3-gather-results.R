######################################
###### Gather results in sim. A ######
## (once all computations are done) ##
######################################

library(ptmixed)
nrep = 5000

for (case in 6:10) {
  for (subcase in 5:5) {
    b.hat = matrix(NA, nrow = nrep, ncol = 3)
    a.hat = D.hat = s.hat = rep(NA, nrep)
    var.hess = matrix(NA, nrow = nrep, ncol = 6)
    lrt = wald = rep(NA, nrep)
    for (ind in 1:nrep) {
      is.ok = try(load( paste('results/', case, '/res-', case, '-', subcase, '-', ind, '.RData', sep='') ))
      if (!inherits(is.ok, 'try-error')) {
        if (!inherits(mixed.model, 'try-error')) {
          if (mixed.model$convergence == 0) {
            b.hat[ind,] = mixed.model$mle[1:3]
            D.hat[ind] = mixed.model$mle[4]
            a.hat[ind] = mixed.model$mle[5]
            s.hat[ind] = mixed.model$mle[6]
            lik.h1 = mixed.model$logl
            temp = try(summary(mixed.model))
            if (!inherits(temp, 'try-error')) {
              wald[ind] = temp$coefficients['time', 'p.value']
            }
            if (!inherits(null.model, 'try-error')) {
              if (null.model$convergence == 0) {
                lik.h0 = null.model$logl
                lrt.stat = 2*(lik.h1 - lik.h0)
                lrt[ind] = pchisq(lrt.stat, 1, lower.tail = F)
              }
            }
          }
        }
        rm(list = c('mixed.model', 'null.model', 't.est',
                    'lik.h0', 'lik.h1'))
      }
      if (ind %% 100 == 0) cat(ind)
    }
    save.image(paste('results/gathered/', case, '-', subcase, '.RData', sep = ''))
  }
}

