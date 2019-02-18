######################################
##### Data generation in sim. C ######
######################################
library(tweeDEseq)

case = 1 # run 3 times, with case = 1, 2, 3
n = c(10, 20, 40)[case]
t = 5
nrep = 10

for (h in 1:nrep) {
  cat(paste('Generating replicate number', h, '...'))
  set.seed(h)
  ngenes = 500
  perc.up = perc.down = 0.1 # 10% genes up-, 10% down-regulated
  prop.negbin = 0.8 # 80% genes are NB, 20% no
  type = rep('not de', ngenes)
  type[(ngenes*(1-perc.up-perc.down)+1):ngenes] = 'up'
  type[(ngenes*(1-perc.down)+1):ngenes] = 'down'
  id = rep(1:n, each = t)
  # NB: let time start from 0
  time = rep(0:(t-1), n)
  group = rep(0, n*t); group[(0.5*n*t+1):(n*t)] = 1
  randint.var = runif(ngenes, min = 0.2, max = 0.8)
  # offset for different library sizes
  log.offset = rnorm(n*t, sd = 0.5)
  # regression coefficients:
  beta0 = rnorm(ngenes, mean = 3, sd = 0.5)
  beta.group =  rnorm(ngenes, mean = 0, sd = 0.3)
  beta.time = rep(0, ngenes)
  beta.time[type == 'up'] = runif(perc.up*ngenes, min = 0.1, max = 0.5)
  beta.time[type == 'down'] = runif(perc.down*ngenes, min = -0.5, max = -0.1)
  # deviance, power and variance
  deviance = 1+rgamma(ngenes, shape = 4, rate = 1)
  power = rep(0, ngenes)
  no.negbin = sample(1:ngenes, ngenes*(1-prop.negbin))
  power[no.negbin] = runif(length(no.negbin), min = -10, max = -3)
  data = matrix(NA, n*t, ngenes)
  for (i in 1:ngenes) {
    random.int = rep(rnorm(n, mean = 0, sd = sqrt(randint.var[i])), each = t)
    log.mean = beta0[i] + random.int + beta.group[i]*group + beta.time[i]*time + log.offset
    for (j in 1:(n*t)) data[j,i] = rPT(1, mu = exp(log.mean[j]),
                                       D = deviance[i], a = power[i], tol = 1e-4,
                                       max = 10*max(exp(log.mean[j]), 1)*deviance[i])
  }
  save.image(paste('data/C/1-data-case-', case, '-nrep-', h, '.RData', sep = ''))
  cat(' done'); cat('\n')
}