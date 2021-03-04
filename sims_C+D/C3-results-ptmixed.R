######################################
###### Gather results of sim. C ######
######################################
library(ptmixed)
case = 1 # 1 to 3
nrep = 50

n.genes = 500

for (h in 1:nrep) {
  b.hat = matrix(NA, nrow = n.genes, ncol = 3)
  a.hat = D.hat = s.hat = rep(NA, n.genes)
  p.wald = p.lrt = lrt.stat = t.ptmixed = rep(NA, n.genes)
  for (ind in 1:n.genes) {
    is.ok = try(load( paste('results/1.2.', case, '/1.2.', case, '-', ind,'-rep-', h, '.RData', sep='') ))
    if (!inherits(is.ok, 'try-error')) {
      if (!inherits(mixed.model, 'try-error')) {
        if (mixed.model$convergence == 0) {
          # MLE:
          b.hat[ind,] = mixed.model$mle[1:3]
          D.hat[ind] = mixed.model$mle[4]
          a.hat[ind] = mixed.model$mle[5]
          s.hat[ind] = mixed.model$mle[6]
          # computing time
          t.ptmixed[ind] = summary(t.comp)[3]
          # Wald test:
          temp = try(summary(mixed.model, silent = T))
          if (!inherits(temp, 'try-error')) p.wald[ind] = temp$coefficients[3, 4]
          # likelihood ratio test:
          if (exists('null.model')) {
            if (!inherits(null.model, 'try-error')) {
              if (null.model$convergence == 0) {
                lrt.stat[ind] = 2 * (mixed.model$logl - null.model$logl)
                if (lrt.stat[ind] >= -1e-2) {
                  p.lrt[ind] = pchisq(lrt.stat[ind], df = 1, lower.tail = F)
                }
              }
            }
          }
        }
      }
      if (exists('mixed.model')) rm('mixed.model')
      if (exists('null.model')) rm('null.model')
      if (exists('t.comp')) rm('t.comp')
      #print(paste(h, ind))
      if (ind %% 100 == 0) cat(ind)
    }
  }
  save.image(paste('results/1.2-aggr/1.2.', case, '-rep', h, '.RData', sep = ''))
}

job.done()
