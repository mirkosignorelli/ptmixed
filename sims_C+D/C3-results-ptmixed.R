######################################
###### Gather results of sim. C ######
######################################
library(ptmixed)
case = 1 # 1 to 3
nrep = 10
ngenes = 500
rmse = fpr.wald = fpr.lrt = tpr.wald = tpr.lrt = rep(NA, nrep)

get.rmse = function(est, true) {
  rem = which(is.na(est))
  if (length(rem) > 0) {est = est[-rem]; true = true[-rem]}
  out = sqrt( sum((est-true)^2) / length(est) )
  return(out)
}
get.fpr = function(x, alpha) {
  n = length(which(!is.na(x) & !is.nan(x)))
  cov = length(which(x<alpha))/n
  return(cov)
}
get.tpr = function(x, alpha) {
  n = length(which(!is.na(x) & !is.nan(x)))
  pow = length(which(x<alpha))/n
  return(pow)
}

for (h in 1:nrep) {
  beta.hat = p.wald = p.lrt = rep(NA, ngenes)
  for (ind in 1:ngenes) {
    is.ok = try(load( paste('results/C', case, '/1.', case, '-', ind,'-rep-', h, '.RData', sep='') ))
    if (exists('mixed.model')) {
      beta.hat[ind] = mixed.model$mle[3]
      if (mixed.model$convergence == 0) {
        p.wald[ind] = summary(mixed.model)$coefficients[3, 4]
        logl.h1 = mixed.model$logl
        if (exists('null.model')) {
          if (null.model$convergence == 0) {
          logl.h0 = null.model$logl
          lrt.stat = 2 * (logl.h1 - logl.h0)
          p.lrt[ind] = pchisq(lrt.stat, df = 1, lower.tail = F)
          }
        }
      }
      if (exists('mixed.model')) rm('mixed.model')
      if (exists('null.model')) rm('null.model')
    }
  }
  rmse[h] = get.rmse(beta.hat, beta.time)
  fpr.wald[h] = get.fpr(p.wald[type == 'not de'], 0.05)
  fpr.lrt[h] = get.fpr(p.lrt[type == 'not de'], 0.05)
  tpr.wald[h] = get.fpr(p.wald[type == 'not de'], 0.05)
  tpr.lrt[h] = get.fpr(p.lrt[type == 'not de'], 0.05)
}
