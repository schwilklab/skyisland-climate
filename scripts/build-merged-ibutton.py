#! /usr/bin/python
# build-merged-ibutton.py script to merge all ibutton data to a single csv file
# for each sensor. Each download event should be stored to another folder under
# the raw-ibutton/T or /H folders. It does not matter if the csv files use F or
# C, that is dealt with in the R script.

# this script does not need to 

from collections import defaultdict
import os
import csv

__version__ = """0.1"""

BASE_DIR = "../microclimate"

RENAME_TABLE = "../microclimate/sensor-rename-table.csv"
TOMERGE = [("../microclimate/raw-ibutton/DM/T", "../microclimate/merged-ibutton/DM/T"), ("../microclimate/raw-ibutton/GM/T", "../microclimate/merged-ibutton/GM/T"), ("../microclimate/raw-ibutton/CM/T", "../microclimate/merged-ibutton/CM/T"),("../microclimate/raw-ibutton/DM/H", "../microclimate/merged-ibutton/DM/H"), ("../microclimate/raw-ibutton/GM/H", "../microclimate/merged-ibutton/GM/H"),("../microclimate/raw-ibutton/CM/H", "../microclimate/merged-ibutton/CM/H"),]


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
    
def main():
    """Command-line tool. """

    ### merge all in tomerge list
    for rawd, merged in TOMERGE:
        print("cleaning " + merged)
        ensure_dir(merged)
        cleandir(merged)
        print("merging " + rawd + " to " + merged)
        build_merged_dir(build_sensor_file_map(rawd), merged)

   
# Main program
if __name__ == "__main__" :
    main()
