import pandas as pd
import pycaching
from pycaching.geo import to_decimal
from pycaching.cache import Type

geocaching = pycaching.login("a_forsythe", "v3X@s45aqePN")

data = pd.read_csv("data/gc-list-eu-unfiltered.csv")

d=[]
for ind in data.index:
    cache = geocaching.get_cache(data['GC'][ind])
    d.append({'GC':data['GC'][ind],'Title': cache.name,'lon': cache.location[0], 'lat': cache.location[1],'Description':cache.description})

df=pd.DataFrame(d)

df.to_csv('data/gc-eu-descriptions.csv',index=False)
