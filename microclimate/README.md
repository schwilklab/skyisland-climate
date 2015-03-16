Microclimate data
=================

We have a network of temperature and relative humidity sensors deployed across three mountain ranges in west Texas. Each of this sensors is an [iButton][iButton] data logging device mounted inside a radiation shield (two nested plastic funnels 4" and 5") and hang at ~2m height in a tree in order to measure air temperature and/or relative humidity.

# Data storage capacity of iButtons #
   
  - each sensor can hold 8160 logs [Check: 8192 logs]
  - 15 minutes = 85 days
  - 30 minutes = 170 days
  - 40 minutes = 226 days
  - 42 minutes = 240 days
  - 45 minutes = 255 days

REMINDER: Times in the program are in seconds. So, 45 mins = 2700 secs.

# Data organization and analyses#

## Raw iButton timeseries data ##

Each time we conduct a sensor reset trip, we download all of the data on each sensor and save that data as a csv file. These files go in raw-ibutton/ directory tree organized by mtn range, then T or H, then date in yymmdd format. The actual files are named by sensor name. See for example [BC519.csv](./raw-ibutton/CM/T/111029/BC519.csv).

NOTE: the silly windows XP operating system saved times in local time, but used the wrong dates for the standard time/daylight time transitions. This means that there are apparent gaps in the spring and duplicated times in the fall for 2010, 2011 and 2012 [check this? maybe only 2010 and 2011?]. The problem was fixed after 2012. Rather than spend a lot of code checking and correcting that, I've ignored it for now and simply have the code in iButtons.R delete the Fall duplicates. It leads to the loss of some data, but does not really effect the daily summaries which everything else is based on. 

## Building merged ibutton data files ##

The [python script](../scripts/build-merged-ibutton.py) merges by appending the separate dated downloads into a similar directory structure under "merged-ibutton", but lacking the directories named by date.

After each trip, merge all sensors:

```python build-merged-ibutton.py```

The timestamp file under ./merged-ibutton/LAST_BUILD gives the date and time of the last merge.

## Sensor location data ##

The coordinates, notes and other data (such as gravimetric soil water) associated with each sensor are stored in "sensors.csv". See "sensors-metadata.csv" for details. Some sensors were renamed at some point after initial deployment. This information is stored in "sensor-rename-table.csv", see the corresponding metadata file. The rename table is used by `build-merged-ibuttons.py` to write the merged data with the correct current name.

The bash script build-sensor-data.sh will run the python code to merge iButtons and then run the R code to produce the daily and monthly summaries (load-sensor-data.R). These scripts are smart about only re-reading and making new summaries if data has changed.

# Uploading sensor locations to GPS #

The file "/field-sheets/sensors-to-upload.csv" is a paired down version of this master file with only lat, on, elev, and sensor name. This file is in a format suitable for uploading to a gps using gpsbabel and the unicsv format and is created by the `/scripts/make-sensors-to-upload.R` script.  To upload the waypoints to a garmin GPS attached via usb use gpsbabel.  The gps-load-sensors.sh script will create this file and load the sensors in one step:

  ```
  ./gps-load-sensors.sh
  ```

# General Metadata and notes #

The file [sensor-notes.md](./sensor-notes.md) contains notes from each on sensor replacement, etc.

[iButton]: http://www.maximintegrated.com/en/products/ibutton/ibuttons/thermochron.cfm
