envir <- new.env(parent = globalenv())
source("scripts/plan.R") # source into global envir
ls()
ls(envir)
drake_config(plan, envir = envir)
