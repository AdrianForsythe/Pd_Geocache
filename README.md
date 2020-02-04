# Pd_Geocache
Search geocaching databases for evidence of people visiting caves!

## Prerequisites
Some packages for spatial operations are needed
- GDAL (for package "rgdal")
	- `sudo apt-get install libgdal1-dev libproj-dev`
- Selenium (for scraping purposes)
	- [install via docker is recommended](https://github.com/SeleniumHQ/docker-selenium)

### Selenium install on Linux
- `sudo docker pull selenium/standalone-firefox`

### To Start RSelenium
- `sudo docker run -d -p 4445:4444 --shm-size 2g selenium/standalone-firefox`
  - if you get the error " Unrecognized content encoding type. libcurl understands deflate, gzip content encodings." do a clean install of R package `stringi`.

## Primer on Drake Workflow


## TO-DO
### Urgent
- ~~Be better at filtering out false-positive sites~~
	- ~~confirm accuracy of geocache locations -- Mines/Caves are actually in Mines/Caves~~
	- ~~try a few terms to exclude from site descriptions~~
		- ~~this involves changing scraping script to include description~~

- Adjacency matrix at the geocache level, instead of county
	- re-run mantel
	- useful when I get around to integrating new methods

- Our county records for sim params
- 

- Estimation of R0?
	- include bat population data (i.e. growth rate)
		- data from monitoring populations impacted by WNS for multiple years

- Proportion of shared visits for all possible edges (path)

### Not as urgent
- Smarter regex strategy for finding user records with bat sightings
	- i.e. "spotted a bat" vs. "blind as a bat"
- ~~try out snakemake or other pipeline management software?~~

## General Workflow

### Scraping user records from geocaching.com
- pull user info, visits to a site, date, log entry
	- scrape-geocaches.R
- reverse-lookup of users
	- user_lookup.R
- aggregating records in a usable format
	- showing temporal trends, etc.
	- geocache_mapping.R

![Average monthly visitors to geocache sites](figures/num-geocache-year.png)

### Incorporating spatial information
- Find which geocaching sites overlap with which counties
	- coordinate-overlap.R
- Find closest match to known sites by fuzzing matching of lat/lon
	- coordinate-match.R

### Incorporating WNS incidence records
- Grabbing WNS records
	- wns-presence.R
- raster/shape files of US/Canada counties
	- county-fix.R
- Adjacency matrix
	- spatial-weight-matrix.R

### Statistical tests
- Does the spread of WNS correspond to the levels of human activity?
	- mantel test
	- new methods???
