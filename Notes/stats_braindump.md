* Don't know that much about Maher/O'Regan: straight brute-force MLE?
* simulations (sparse-matrix mult/m747 stuff?): https://ms.mcmaster.ca/~bolker/classes/m747/notes/durrettlevin.pdf ; https://ms.mcmaster.ca/~bolker/classes/m747/notes/durrettlevin.Rnw (make sparse adjacency matrix, then multiply to simulate dynamics)

* Smith et al rabies stuff; comparing first date of appearance. Survival analysis problem, with stepwise hazard determined by infection status of neighbours. I.e. we can calculate probability of infection at each time step (=$1-(1-p)^n$); probability for each individual up to time $t$ is $\prod \left(1-(1-p)^{n_i}\right)$ \ldots Smith et al. instead use a slightly weird simulation-distribution approach
* individual frailty?
* without observation error could do direct MLE
* with observation error: sequential Monte Carlo (e.g. pomp)? ABC?
* what is the exact correspondence with multi-membership GLMM/log-hazard combinations (did I write this down already?)

- important to write simulations!
- start with adjacency-matrix simulator
  - construct adjacency matrix $A$
  - vector of infection status $v$
  - $n = A v$ is the number of infected neighbours (we could subset $A$ to only rows corresponding to uninfected counties)
  - transition from 0 to 1 is binomial with probability $1-(1-p)^n$
  - repeat
  


