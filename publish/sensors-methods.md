# microclimate #

Starting in 2011, we set out a network of temperature and relative humidity sensors deployed across the three mountain ranges in West Texas. Each of the sensors is an iButton data logging device (Maxim Integrated, San Jose, CA, U.S.) mounted inside a radiation shield (two nested plastic funnels) and hung at 2 m height in a tree in order to measure air temperature. We set out 30 sensors in each mountain range stratified across elevation (1500-2300 m) and across relative slope position (valley, midslope, and local ridge). These sensors were set to record temperature every 30 minutes to 1 hour and data was collected by visiting these sensors every three months and downloading the data. The data were summarized to a continuous daily time series of daily minimum and maximum temperatures with python and R scripts. Two fires in the Davis mountains over this time period caused some interruption to the time series and we lost several sensors to animals, presumably black bears. But we replaced all lost sensors and left these recording through spring 2016.

We collected soil to measure soil gravimetric water content at each sensor location in spring (April) 2013, 2014 and 2015. This timing was intended to capture a period of near minimum annual soil moisture.

#  topographic variables" #

Topographic predictors of local microclimate that that were derived from ASTER 30-m resolution digital elevation model (DEM) data from gdex.cr.usgs.gov/gdex/ and clipped to selected 12-digit hydrologic units that included ibutton sensor locations and adjacent hydrologic units in each mountain range.

# downscaled dailyclimate #

For each of our mountain ranges, we chose the closest long term weather station with daily temperature and precipitation data (https://www.ncdc.noaa.gov). For the Chisos Mountains this was the Chisos Basin (USC00411715), for the Davis Mountains this was Fort Davis (USC00413262) and for the Guadalupe Mountains this was Panther Junction (USC00416792). We used the recent historical data to fit models of the temporal component to our microclimate measurements (See “Modeling microclimate”, below) and the longer term data (1960-2000) to reconstruct historical microclimate.

To project microclimate under future scenarios, we produced alternative future projected daily time series for each of this weather stations using regional downscaling from earth system models. This downscaling step was accomplished by Anne Stoner using the Regional Asynchronous Regression Model (Stoner et al. 2013). For each weather station, we alternative time series for nine CMIP5 earth system models and two emissions scenarios (rcp4.5 and rcp8.5, Taylor et al).


