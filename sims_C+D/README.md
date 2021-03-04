# Code for simulations C and D

Before running these simulations, create the following subdirectories in your working directory:
- `data`, with subfolders `1` and `2`
- `results`, with subfolders `1.2.1`, `1.2.2`, `1.2.3`, `2.2.1`, `2.2.2`, `2.2.3`

Each simulation comprises 3 scripts that should be run sequentially:
- the first generates the data (500 genes x 50 repetitions)
- the second estimates the mixed models (computations may be time consuming - the code provided can be trivially parallelized and be run on multiple cores)
- the third collects the results generated with the second script and allows to compute the RMSE, FPR and TPR
