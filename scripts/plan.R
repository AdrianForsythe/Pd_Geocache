### Plan
plan <- drake_plan(
  # Read in GC list
  gc_dat = read.csv("data/gc-list-unfiltered.csv",header=T,fill = T,sep = ",",na.strings = "",quote = "",comment.char = ""),
  # sample
  sample_descriptions = sample_descriptions(gc_dat),
  # cleaning script based on sample
  filter_descriptions = filter_descriptions(sample_descriptions),
  # report on success of cleaning
  # scrape with clean data
  scrape_dat = start_scrape(filter_descriptions),
  
  # rev_lookup = rev_lookup(scrape_dat),
  # mapping = mapping(gc_dat,scrape_dat),
  presence = grab_presence(),
  overlap = find_overlap(scrape_dat),
  match = match_locations(scrape_dat),
  
  counties = read.csv("data/all-counties.csv", header = T) %>%
    separate(1, c("county", "state.province", "Country"), sep = "\t"),
  
  can.shape = readOGR("shape/lcd_000b16a_e/lcd_000b16a_e.shp"),
  usa.shape = map("county", regions = counties[counties$Country == "USA", ]$state.province, fill = T),
  county_fix = county_fix(presence.df,counties,can.shape,usa.shape),
  adjacency_matrix = make_adjacency(county),
  # mantel = run_mantel(),
  OReagan_data = OReagan_county(),
  county_duration = county_duration(OReagan),
  run_sim = run_sim(duration),
  glm_model = glm_model()
  )

good_config <- drake_config(plan)
vis_drake_graph(good_config, targets_only = TRUE)
