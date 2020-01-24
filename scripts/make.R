### Make
# Restart R.
interactive()
#> [1] TRUE

source("scripts/packages.R")
source("scripts/plan.R")

config <- drake_config(plan)
vis_drake_graph(config)