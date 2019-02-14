library(matrixcalc)

case = 1 # 1 to 5
subcase = 1 # 1 to 3

load(paste('results/full-results/', case, '-', subcase, '.RData', sep = ''))

p.wald = p.lrt = rep(NA, nrep)
p.wald = 2*(1-pnorm(abs(wald.stat)))
p.lrt = pchisq(lrt.stat, 1, lower.tail = F)

get.rmse = function(est, true) {
  rem = which(is.na(est))
  if (length(rem) > 0) {est = est[-rem]}
  out = sqrt( sum((est-true)^2) / length(est) )
  return(out)
}

type1e = function(x, alpha) {
  n = length(which(!is.na(x)))
  cov = length(which(x<alpha))/n
  round(cov, 4)
}

# RMSE:
get.rmse(est = b.hat[ , 2], true = 0)

# Wald test type 1 error
type1e(p.wald, alpha = 0.05)

# LRT type 1 error
type1e(p.lrt, alpha = 0.05)
