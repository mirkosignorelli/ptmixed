######################################
##### Model estimation in sim. A #####
######### (time effect = 0) ##########
######################################
# load R package
library(ptmixed)

# case (6-10): 6 is a = -5, 10 is a = 1 (see script A1)
# subcase (1-3): 1 is n = 10, 2 is n = 20, 3 is n = 40
# nsub (1-5000): repetition number

# execute code for the first repetition with:
nsub = 1

for (case in 6:10) {
  for (subcase in 1:4) {
    load(paste('data/data-', case, '-', subcase, '.RData', sep = ''))
    y = data[,nsub]
    df = data.frame(y, group, time)
    # mle full model
    t.est = system.time(
      mixed.model <- try( ptmixed(y ~ group + time, id = id, 
                                  data = df, npoints = 5,
                                  hessian = T, trace = F, 
                                  freq.updates = 200) )
    )
    # mle under H0: beta_group = 0
    null.model <- try( ptmixed(y ~ group, id = id, 
                               data = df, npoints = 5,
                               hessian = F, trace = F, 
                               freq.updates = 200) )
    # save results
    resfile = paste('results/', case, '/res-', case, '-', subcase, '-', nsub, '.RData', sep='')
    save(t.est, mixed.model, null.model,
         file = resfile)
  }
}
