## January 17th 

Bens ideas

- Recreate rabies paper with oreagan stuff more carefully (look at testing procedure ??)
  Option 1 recreate exactly what they did with the network based on county boundaries
  and include river boundaries
  Option 2 look a little more into biology and spatial resistance type (read about this)
  Look to minimize cost path for the path 

 - Look into a proper GLMM for the O'Reagan stuff method
 
 - Look into R0 in spatial epidemics (could be a paper topic on its own)
 
 ## January 22 & 20
 
 - Submitted plan for the semester
 - Take 50 by hand and run our actual filters and see how specific and sensitive the filtering works and so it is justifiable. 
- Just eliminate the phrases the words that are synonymous with the phrases we dont want and filter our the "sayings" or having baseball in the same phrase. 
- Transportation Flows data set/package exists and is out there so look for that and stuff.
- Set aside a validation set
-R0 is not worth it (think county to county rather than cave to cave) 
- Approximate Bayesian Computation for parameter estimation (sample from prior distributions, run simulations, calculate distance from output to result, the set of simulation that are closest to your actual is the most optimal and is selected)
- Run 1000 simulations at least.
- Start a journal

## January 29th

- no adrian
- my brain hurts
-model too complicated
-simplify to have just a beta 
- create a probability matrix that simply infects the neighbors in a binomial fashion
- try and avoid the linear model unless you can prove it for the $x_j$ in the probability 
$p_{i,t} = 1 - \Pi^N_{j=1} (1 - \beta_{i,j} * x_{j,t})
- \PI^N_t=w binom(1-(1-p_i)^I_t, S_t)
- glm(newinf ~ 1 + log(I_t), size = S_t, family = binom(cloglog))
- Kinda sneaky that the binom turns to log (I_t)
- Look into other papers that do network stuff and see what they do
- glm only offers intellectual satisfaction and computational (Ben is keen on)
- Need document of bonehead simple model of the data through caves