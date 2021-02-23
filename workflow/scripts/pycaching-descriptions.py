import sys
import pandas as pd
import pycaching
from cleantext import clean
from pycaching.geo import to_decimal
from pycaching.cache import Type
from time import sleep

def ScrapeDescriptions(data_path,out_path):
  geocaching = pycaching.login("a_forsythe", "v3X@s45aqePN")
  data = pd.read_csv(data_path,usecols=range(9),lineterminator='\n',quotechar='"', delimiter=',',escapechar='-',encoding="utf-8", skipinitialspace=True, error_bad_lines=False)
  sleep_time = 0
  num_retries = 2

  d=[]
  for ind in data.index:
      print(data['GC'][ind])
      cache = geocaching.get_cache(data['GC'][ind])
      # if hasattr(cache,'pm_only'):
      #     d.append({'GC':data['GC'][ind],'lon': "", 'lat': "",'Description':""})
      # else:
      for x in range(0, num_retries):
          try:
              title = cache.name
              lon = cache.location[0]
              lat = cache.location[1]
              type = cache.type
              description = cache.description
              str_error = None
          except Exception as str_error:
              pass

              if str_error:
                  sleep(sleep_time)  # wait before trying to fetch the data again
              else:
                  break
      d.append({'GC':data['GC'][ind],'lon': lon, 'lat': lat,'type':type,'Description':clean(description,no_urls=True,no_line_breaks=True,no_emails=True,no_punct=True)})

  df=pd.DataFrame(d)

  df.to_csv(out_path,index=False)

ScrapeDescriptions(snakemake.input[0],snakemake.output[0])
