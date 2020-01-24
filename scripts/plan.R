### Plan
plan <- drake_plan(
  raw_data = "data/euro-cave-not-complete.csv",
  scrape = "scripts/scrape_geocaches.R",
  rev_lookup = "scripts/user_lookup.R",
  mapping = "scripts/geocache_mapping.R",
  overlap = "scripts/coordinate-overlap.R",
  match = "scripts/coordinate-match.R",
  presence = "scripts/wns-presence.R",
  county = "scripts/county-fix.R",
  adjacency = "scripts/spatial-weight-matrix.R",
  mantel = "scripts/mantel.R ",
  
    quiet = TRUE
  )