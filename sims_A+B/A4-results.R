rm(list = ls())
library(msigno)
set.cf(); clear.screen()

new.result.df = F
if (new.result.df) {
  rm(list = ls())
  out.wald = matrix(nrow = 5, ncol = 5)
  colnames(out.wald) = paste('n =', c(10, 25, 50, 100, 150))
  rownames(out.wald) = paste('a = ', c(-5, -1, 0, 0.5, 1))
  mean.mle = array(NA, dim  = c(5, 6, 5))
  out.rmse = out.lrt = n.wald = n.lrt = out.wald
  save.image('results/results-time.RData')
}

#case = 10 # 6 to 10
#subcase = 4 # 1 to 3

for (case in 6:10) {
  for (subcase in 1:5){

load(paste('results/gathered/', case, '-', subcase, '.RData', sep = ''))

par(mfrow = c(2, 1))
hist(wald, 20)
hist(lrt, 20)

table(lrt >=0, useNA = 'ifany')

get.rmse = function(est, true) {
  rem = which(is.na(est))
  if (length(rem) > 0) {est = est[-rem]}
  out = sqrt( sum((est-true)^2) / length(est) )
  return(out)
}

type1e = function(x, alpha, print = T) {
  n = length(which(!is.na(x)))
  cov = length(which(x<alpha))/n
  if (print) cat(paste(round(cov,3), ' (n = ', n, ')', sep = ''))
  else return(list('t1e' = cov, 'n' = n))
}

load('results/results-time.RData')
out.wald[case - 5, subcase] = type1e(wald, 0.05, print=F)$t1e
n.wald[case - 5, subcase] = type1e(wald, 0.05, print=F)$n
out.lrt[case - 5, subcase] = type1e(lrt, 0.05, print=F)$t1e
n.lrt[case - 5, subcase] = type1e(lrt, 0.05, print=F)$n
out.rmse[case - 5, subcase] = get.rmse(est = b.hat[ , 3], 
                                   true = 0)

mean.mle[subcase, , case-5] = c(apply(b.hat, 2, function(x) mean(x, na.rm = T)),
                              mean(D.hat, na.rm = T), 
                              mean(a.hat, na.rm = T), 
                              mean(s.hat, na.rm = T))

save(out.rmse, out.wald, out.lrt, n.wald, n.lrt, mean.mle,
     file = 'results/results-time.RData')

type1e(wald, 0.05, print = T)
type1e(lrt, 0.05, print = T)

  }
}

round(out.rmse, 3)
round(out.wald, 3)
round(out.lrt, 3)
