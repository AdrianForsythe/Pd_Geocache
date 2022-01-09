# Pd_Geocache
Search geocaching databases for evidence of people visiting caves!

## Big Update!
1. Pipeline is now managed with `snakemake`
	- no more `drake`!
	- `snakemake` pipeline can handle the entire workflow:
		- input is an unfiltered list of geocache sites
	- reproducibility via a `conda` environment
		-	many dependencies needed spatial operations in `R`!
		- `R` version 4.04
		- To start: `conda env create -f env.yml`
		- Then: `snakemake --use-conda ...`

2. No longer using a Selenium web driver
	-	instead, using [`pycaching`](https://github.com/tomasbedrich/pycaching) python module
		- still issues with scraping, mostly due to "premium only" or non-standard cache types. The work around was to use a geocache premium subscription
	- this method is maybe X10 faster, as using `selenium` web-driver. Directly uses GroundSpeak's API.

3. Included more US States and CAD provinces impacted by WNS
	- increased amount of sites covered and shared users between sites

## General Workflow
### Scraping user records from geocaching.com
- pulls user info, visits to a site, date, log entry
- aggregating records in a usable format
	- showing temporal trends, etc.
	- geocache_mapping.R

- Not yet re-implemented:
	- reverse-lookup of users
		- user_lookup.R

![Average monthly visitors to geocache sites](workflow/figures/num-geocache-year.png)

### Incorporating spatial information
- Find which geocaching sites overlap with which counties
	- `coordinate-overlap.R`
- Find closest match to known sites by fuzzing matching of lat/lon
	- `coordinate-match.R`

### Incorporating WNS incidence records
- Grabbing WNS records
	- `wns-presence.R`
- raster/shape files of US/Canada counties
	- `county-fix.R`
- Adjacency matrix
	- `spatial-weight-matrix.R`

### Statistical tests
#### Does the spread of WNS correspond to the levels of human activity?
- Update these methods here

## TO-DO
### Urgent
- ~~Be better at filtering out false-positive sites~~
	- ~~confirm accuracy of geocache locations -- Mines/Caves are actually in Mines/Caves~~
	- ~~try a few terms to exclude from site descriptions~~
		- ~~this involves changing scraping script to include description~~

- ~~Adjacency matrix at the geocache level, instead of county~~

- Animations are currently broken
	- caused by updates to `gganimate`?
	- Specifically having trouble with `transition_states` on dates.

- Our county records for sim params

- Estimation of R0?
	- include bat population data (i.e. growth rate)
		- data from monitoring populations impacted by WNS for multiple years

- Proportion of shared visits for all possible edges (path)

### Not as urgent
- ~~Smarter regex strategy for finding user records with bat sightings~~
	- ~~i.e. "spotted a bat" vs. "blind as a bat"~~
- update README with statistical methods
- clean up data dir
