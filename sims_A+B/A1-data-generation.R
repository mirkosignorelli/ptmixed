library(tweeDEseq)
set.seed(123)

nrep = 5000
t = 5
beta0 = 2.5
beta.time = 0
beta.group = 0.3
deviance = 3
sigma2 = 0.5

for (case in 6:10) {
  for (subcase in 5:5) {
    power = c(-5, -1, 0, 0.5, 1)[case-5]
    n = c(10, 25, 50, 100, 150)[subcase]
    id = rep(1:n, each = t)
    time = rep(0:(t-1), n)
    group = rep(0, n*t); group[(0.5*n*t+1):(n*t)] = 1
    data = matrix(NA, n*t, nrep)
    
    for (i in 1:nrep) {
      random.int = rep(rnorm(n, mean = 0, sd = sqrt(sigma2)), each = t)
      log.mean = beta0 + random.int + beta.group*group + beta.time*time
      for (j in 1:(n*t)) data[j,i] = rPT(1, mu = exp(log.mean[j]),
                                         D = deviance, a = power, tol = 1e-4, max = 10*max(exp(log.mean[j]), 1)*deviance)
      if (i %% 500 == 0) cat(paste(i, ''))
    }
    save.image(paste('data/data-', case, '-', subcase, '.RData', sep = ''))
  }
}

