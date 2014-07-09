#!/usr/bin/env python 

# build-merged-ibutton.py script to merge all ibutton data to
# a single csv file for each sensor. Each download event should be stored to
# another folder under the raw-ibutton/T or /H folders. It does not matter if
# the csv files use Farenheit or Celsius, that is dealt with in the R scripts.

__version__ =    '''0.2'''
__program__ =    '''build-merged-ibutton.py'''
__author__  =    '''Dylan Schwilk'''
__usage__   =    '''buil-merged-ibutton.py [options]'''

from collections import defaultdict
import os
import csv
import datetime
import dateutil.parser
import logging
logging.basicConfig(format='%(levelname)s: %(message)s')
mylogger = logging.getLogger('mylogger')

BASE_DIR = "../microclimate"
RENAME_TABLE = "../microclimate/sensor-rename-table.csv"
TOMERGE = [("../microclimate/raw-ibutton/DM/T", "../microclimate/merged-ibutton/DM/T"), ("../microclimate/raw-ibutton/GM/T", "../microclimate/merged-ibutton/GM/T"), ("../microclimate/raw-ibutton/CM/T", "../microclimate/merged-ibutton/CM/T"),("../microclimate/raw-ibutton/DM/H", "../microclimate/merged-ibutton/DM/H"), ("../microclimate/raw-ibutton/GM/H", "../microclimate/merged-ibutton/GM/H"),("../microclimate/raw-ibutton/CM/H", "../microclimate/merged-ibutton/CM/H"),]
TIMESTAMP_FILE = "../microclimate/merged-ibutton/LAST_BUILD"

def modification_time(filename):
    t = os.path.getmtime(filename)
    return datetime.datetime.fromtimestamp(t)

def ensure_dir(f):
    if os.path.isfile(f):
        d = os.path.dirname(f)
    else : 
        d = f
    if not os.path.exists(d):
        os.makedirs(d)
	
def getbname(s):
    return(os.path.splitext(s)[0])

def sensor_rename_table():
    rtable = {}
    csvReader = csv.reader(open(RENAME_TABLE, 'rb'), delimiter=',', quotechar='"')
    for row in csvReader:
        if csvReader.line_num != 1:
            rtable[row[0]] = row[1]
    return(rtable)

## go through folders and build list of sensors
def build_sensor_file_map(thedir):
    # get list of download dates
    dw_dates = sorted(os.listdir(thedir))
    rename_table = sensor_rename_table()
    sensor_map = defaultdict(list)
    for d in dw_dates:
        p = os.path.join(thedir,d)
        sensors =   sorted(os.listdir(p))  #map(getbname,os.listdir(p))
        for s in sensors:
            sname, ext =  os.path.splitext(s)
            if ext == ".csv" :
                ns = rename_table.get(sname, sname)
                sensor_map[ns].append(os.path.join(p,s))
    return(sensor_map)

def strip_file(f,leave_header=True):
    """Strip out metadata lines and (optionally) header.  Return list of lines"""
    thefile = open(f)
    if leave_header : n=1
    else : n = 2
    r = map(str.strip,thefile.readlines())
    thefile.close()
    try :
        r =  r[r.index("")+n:]
    except :
        print( "Incorrect headers in %s" % f)
        
    return(r)

def merge_one_sensor(slist):
    """Take list of files and create a merged file with only one header line"""
    r = strip_file(slist[0],leave_header=True)
    for s in slist[1:]:
        r += strip_file(s,leave_header=False)
    return r

def write_list(l, fname):
    """ write list of lines to file """
    thefile = open(fname, "w")
    for line in l:
        thefile.write("%s\n" % line)
    thefile.close()
 
def build_merged_dir(smap, outdir):
    """Create set of merged sensor files in output directory"""
    for (s,sl) in smap.iteritems():
        write_list(merge_one_sensor(sl), os.path.join(outdir,s+".csv"))
        
def cleandir(thedir):
    files = os.listdir(thedir)
    for f in files:
        os.remove(os.path.join(thedir,f))

def do_merge_all():
    """Merge all in tomerge list"""
    for rawd, merged in TOMERGE:
        mylogger.info("cleaning " + merged)
        ensure_dir(merged)
        cleandir(merged)
        mylogger.info("merging " + rawd + " to " + merged)
        build_merged_dir(build_sensor_file_map(rawd), merged)

    # add timestamp file
	f = open(TIMESTAMP_FILE,"w")
	f.write(str(datetime.datetime.now()))
	f.close()

def main():
    """Command-line tool. """
    import sys, itertools
    from optparse import OptionParser

    parser = OptionParser(usage=__usage__, version ="%prog " + __version__)
    parser.add_option("-f", "--force", action="store_true", dest="force", default=False,
                      help="Force overwrite of merged directories ignoring last build timestamp")
    parser.add_option("-v", "--verbose", action="store_true", dest="verbose", default=False,
                      help="Print INFO messages to stdout, default=%default")

    (options, args) = parser.parse_args()

    if options.verbose:
        mylogger.setLevel(logging.INFO)

    # if --force, then rebuild no matter what
    if options.force:
        do_merge_all()
        return()

    # if not --force, then check last build time
    try:
        last_build =  dateutil.parser.parse(open(TIMESTAMP_FILE).read())
    except:
        mylogger.info("No last build timestamp found, rebuilding")
        do_merge_all()
        return()
    raw_dirs = [ os.listdir(x[0]) for x in TOMERGE ]
    raw_dirs = list(itertools.chain(*raw_dirs))
    raw_dates= map(lambda s : datetime.datetime.strptime(s, "%y%m%d"), raw_dirs)
    if any  ([last_build < d for d in raw_dates]):
        mylogger.info("Last build timestamp older than newest raw data, rebuilding")
        do_merge_all()
        return()

    # last build was recent, just return
    mylogger.info("Last build timestamp newer than newest raw data, not rebuilding")
    return()

# Main program
if __name__ == "__main__" :
    main()
