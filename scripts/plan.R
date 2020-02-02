##### Drake Plan

plan <- drake_plan(
  # Read in GC list
  gc_dat = read.csv("data/gc-list-unfiltered.csv",header=T,fill = T,sep = ",",na.strings = "",quote = "",comment.char = ""),
  
  # sample descriptions from GC pages
  sample_descriptions = sample_descriptions(gc_dat),
  
  # cleaning script based on sample
  filter_descriptions = filter_descriptions(gc_dat,sample_descriptions),
  
  # scrape with cleaned dataset
  scrape_dat = start_scrape(filter_descriptions),
  
  # rev_lookup = rev_lookup(scrape_dat),
  
  # mapping = mapping(scrape_dat),
  
  presence = grab_presence(scrape_dat),
  
  overlap = find_overlap(scrape_dat,presence),
  
  match = match_locations(scrape_dat,presence),
  
  counties = read.csv("data/all-counties.csv", header = T) %>%
    separate(1, c("county", "state.province", "Country"), sep = "\t"),
  
  can.shape = readOGR("shape/lcd_000b16a_e/lcd_000b16a_e.shp"),
  usa.shape = map("county", regions = counties[counties$Country == "USA", ]$state.province, fill = T),
  
  county_fix = county_fix(presence,counties,can.shape,usa.shape),
  
  adjacency_matrix = make_adjacency(county_fix),
  
  # mantel = run_mantel(),
  
  # OReagan_data = OReagan_county(),
  
  #Read data: US County data (uc)
  uc=read.csv('examples/ORegan-2016-resources/us_data_dur.csv', header=TRUE),
  
  wns_duration = wns_duration(),
  county_duration = county_duration(),
  run_sim = run_sim(uc,wns_duration,county_duration),
  glm_model = glm_model(run_sim,overlap)
  )

good_config <- drake_config(plan)
vis_drake_graph(good_config, targets_only = TRUE,file = "figures/drake-plan.png")
