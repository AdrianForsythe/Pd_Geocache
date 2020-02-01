### Source packages first
source("scripts/packages.R")

#Load Dependencies
source('examples/ORegan-2016-resources/WNS-GT-Duration.R')
source('examples/ORegan-2016-resources/CountyDurationsScript.R')

### Individual scripts
source("scripts/coordinate-match.R")
source("scripts/coordinate-overlap.R")
source("scripts/CountyDurationScript.R")
source("scripts/county-fix.R")
source("scripts/geocache_mapping.R")
source("scripts/GLMmodelfitccavedata.R")
source("scripts/filter_descriptions.R")
source("scripts/mantel.R")
source("scripts/sample_descriptions.R")
source("scripts/scrape_geocaches.R")
source("scripts/sim_runner.R")
source("scripts/spatial-weight-matrix.R")
source("scripts/user_lookup.R")
source("scripts/wns-presence.R")

### Drake plan
source("scripts/plan.R")

### Make
make(
  plan,
  verbose = 2
)