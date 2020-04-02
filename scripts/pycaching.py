import pandas as pd
import pycaching
from pycaching.geo import to_decimal
from pycaching.cache import Type

geocaching = pycaching.login("a_forsythe", "v3X@s45aqePN")

data = pd.read_csv("data/gc-list-filtered.csv")

d=[]
for ind in data.index:
  cache = geocaching.get_cache(data['GC'][ind])
  for log in cache.load_logbook():
    d.append({'GC':data['GC'][ind],'Title': cache.name,'lon': cache.location[0], 'lat': cache.location[1], 'Date': log.visited, 'Type': log.type, 'User': log.author, 'Log':log.text
  })
df=pd.DataFrame(d)
  
df.to_csv('data/gc-scrape-results.csv',index=False)
