import pandas as pd
import pycaching
from pycaching.cache import Cache, Type, Size, Waypoint
from pycaching.errors import ValueError as PycachingValueError, LoadError, PMOnlyException
from pycaching.geo import Point
from pycaching.geocaching import Geocaching
from pycaching.log import Log, Type as LogType
from pycaching.util import parse_date
from pycaching.geo import to_decimal
from time import sleep
from cleantext import clean

def ScrapeGC(data_path,out_path):
  geocaching = pycaching.login("a_forsythe", "v3X@s45aqePN")
  sleep_time = 2
  num_retries = 4
  data = pd.read_csv(data_path)
  d=[]
  df=[]
  for ind in data.index:
    print(data['GC'][ind])
    if data['type'][ind] != 'Type.virtual':
        cache = geocaching.get_cache(data['GC'][ind])
        if hasattr(cache,'name'):
            try:
                title = cache.name
                lon = cache.location[0]
                lat = cache.location[1]
                if hasattr(cache,'_Cache__logbook_token'):
                    for log in cache.load_logbook():
                        date = log.visited
                        logtype = log.type
                        user = log.author
                        text = log.text
                        d.append({'GC':data['GC'][ind],'Title': title,'lon': lon, 'lat': lat, 'Date': date, 'Type': logtype, 'User': user, 'Log':clean(text,no_urls=True,no_line_breaks=True,no_emails=True,no_punct=True)})
            except AttributeError:
                continue
        else:
            continue
    else:
        pass

  df=pd.DataFrame(d)

  df.to_csv(out_path,index=False)

ScrapeGC(snakemake.input[0],snakemake.output[0])
