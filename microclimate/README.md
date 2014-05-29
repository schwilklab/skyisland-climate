Microclimate data
=================

Notes on iButton climate sensors

# Data storage capacity of iBUttons #
   
  - each sensor can hold 8160 logs [Check: 8192 logs]
  - 15 minutes = 85 days
  - 30 minutes = 170 days
  - 40 minutes = 226 days
  - 42 minutes = 240 days
  - 45 minutes = 255 days

 REMINDER: Times in the program are in seconds. So, 45 mins = 2700 secs.

# Data organization #

The coordinates, notes and other data (such as gravimetric soil water) associated with each sensor are stored in"SensorMaster.csv".

# Uploading sensor locations to GPS #

   The file "sensors-to-upload.csv" is a paired down version of this master file with only lat, on, elev, and sensor name. Tis file is in a format suitable for uploading to a gps using gpsbabel and the unicsv format: 

  ```
  gpsbabel -i unicsv -o garmin -f sensors-to-upload.csv -F usb:
  ```

## Raw data storage ##
raw csv files go in raw-ibutton/ directory tree organized by mtn range, then T or H, then date in yymmdd format. The actual files are named by sensor name. The [python script](../scripts/build-merged-ibutton.py) merges by appending the separate dated downloads into a similar directory structure under "merged-ibutton", but lacking the directories named by date.
