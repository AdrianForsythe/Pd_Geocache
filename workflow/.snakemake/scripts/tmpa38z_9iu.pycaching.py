
######## snakemake preamble start (automatically inserted, do not edit) ########
import sys; sys.path.extend(['/home/adrian/anaconda3/lib/python3.7/site-packages', '/home/adrian/PhD-Laptop/Pd_Geocache/workflow/scripts']); import pickle; snakemake = pickle.loads(b'\x80\x03csnakemake.script\nSnakemake\nq\x00)\x81q\x01}q\x02(X\x05\x00\x00\x00inputq\x03csnakemake.io\nInputFiles\nq\x04)\x81q\x05X\x19\x00\x00\x00data/gc-list-filtered.csvq\x06a}q\x07(X\x06\x00\x00\x00_namesq\x08}q\tX\x12\x00\x00\x00_allowed_overridesq\n]q\x0b(X\x05\x00\x00\x00indexq\x0cX\x04\x00\x00\x00sortq\reh\x0ccfunctools\npartial\nq\x0ecbuiltins\ngetattr\nq\x0fcsnakemake.io\nNamedlist\nq\x10X\x0f\x00\x00\x00_used_attributeq\x11\x86q\x12Rq\x13\x85q\x14Rq\x15(h\x13)}q\x16X\x05\x00\x00\x00_nameq\x17h\x0csNtq\x18bh\rh\x0eh\x13\x85q\x19Rq\x1a(h\x13)}q\x1bh\x17h\rsNtq\x1cbubX\x06\x00\x00\x00outputq\x1dcsnakemake.io\nOutputFiles\nq\x1e)\x81q\x1fX\x12\x00\x00\x00data/gc-scrape.csvq a}q!(h\x08}q"h\n]q#(h\x0ch\reh\x0ch\x0eh\x13\x85q$Rq%(h\x13)}q&h\x17h\x0csNtq\'bh\rh\x0eh\x13\x85q(Rq)(h\x13)}q*h\x17h\rsNtq+bubX\x06\x00\x00\x00paramsq,csnakemake.io\nParams\nq-)\x81q.}q/(h\x08}q0h\n]q1(h\x0ch\reh\x0ch\x0eh\x13\x85q2Rq3(h\x13)}q4h\x17h\x0csNtq5bh\rh\x0eh\x13\x85q6Rq7(h\x13)}q8h\x17h\rsNtq9bubX\t\x00\x00\x00wildcardsq:csnakemake.io\nWildcards\nq;)\x81q<}q=(h\x08}q>h\n]q?(h\x0ch\reh\x0ch\x0eh\x13\x85q@RqA(h\x13)}qBh\x17h\x0csNtqCbh\rh\x0eh\x13\x85qDRqE(h\x13)}qFh\x17h\rsNtqGbubX\x07\x00\x00\x00threadsqHK\x01X\t\x00\x00\x00resourcesqIcsnakemake.io\nResources\nqJ)\x81qK(K\x01K\x01e}qL(h\x08}qM(X\x06\x00\x00\x00_coresqNK\x00N\x86qOX\x06\x00\x00\x00_nodesqPK\x01N\x86qQuh\n]qR(h\x0ch\reh\x0ch\x0eh\x13\x85qSRqT(h\x13)}qUh\x17h\x0csNtqVbh\rh\x0eh\x13\x85qWRqX(h\x13)}qYh\x17h\rsNtqZbhNK\x01hPK\x01ubX\x03\x00\x00\x00logq[csnakemake.io\nLog\nq\\)\x81q]}q^(h\x08}q_h\n]q`(h\x0ch\reh\x0ch\x0eh\x13\x85qaRqb(h\x13)}qch\x17h\x0csNtqdbh\rh\x0eh\x13\x85qeRqf(h\x13)}qgh\x17h\rsNtqhbubX\x06\x00\x00\x00configqi}qjX\x04\x00\x00\x00ruleqkX\x08\x00\x00\x00ScrapeGCqlX\x0f\x00\x00\x00bench_iterationqmNX\t\x00\x00\x00scriptdirqnX4\x00\x00\x00/home/adrian/PhD-Laptop/Pd_Geocache/workflow/scriptsqoub.'); from snakemake.logging import logger; logger.printshellcmds = False; __real_file__ = __file__; __file__ = '/home/adrian/PhD-Laptop/Pd_Geocache/workflow/scripts/pycaching.py';
######## snakemake preamble end #########
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
  sleep_time = 1
  num_retries = 2
  data = pd.read_csv(data_path)[329:331]
  d=[]
  df=[]
  for ind in data.index:
    print(data['GC'][ind])
    title = None
    while title is None:
        try:
            cache = geocaching.get_cache(data['GC'][ind])
            title = cache.name
            lon = cache.location[0]
            lat = cache.location[1]
            for log in cache.load_logbook():
                date = log.visited
                logtype = log.type
                user = log.author
                text = log.text
                d.append({'GC':data['GC'][ind],'Title': title,'lon': lon, 'lat': lat, 'Date': date, 'Type': logtype, 'User': user, 'Log':clean(text,no_urls=True,no_line_breaks=True,no_emails=True,no_punct=True)})
        except:
             pass
  df=pd.DataFrame(d)

  df.to_csv(out_path,index=False)

ScrapeGC(snakemake.input[0],snakemake.output[0])
