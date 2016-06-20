DEM-terrain derivatives
=======================

## ASTER 30-m resolution digital elevation model (DEM) data were downloaded and clipped ##

1. Downloaded 12-digit hydrologic unit codes from the NRCS Geospatial Gateway (https://gdg.sc.egov.usda.gov/GDGHome_StatusMaps.aspx). 
2. Imported ibutton sensor lat lons.
3. Selected 12-digit hydrologic units that included sensors and adjacent HUCs to sensors in each mountain range.
4. Clipped DEM to boundary of merged 12-digit HUCs within each mountain range.
5. Filled holes in DEM: used spatial analyst> hydrology> fill toolbox to fill holes/imperfections in DEM data.

## Landscape metrics ##

Note: There are two different types of measures for linear distances: flow path and Euclidean. I have calculated both, and although this is for an old version of ArcMap, the last two figures in this link show the difference in the two calculations; http://courses.washington.edu/gis250/lessons/hydrology/ The flow path uses the flow direction grid for its calculation.

1. `elev`: Elevation in meters (m). Taken as the elevation values in the filled and clipped DEM
2. `slope`: Calculated in degrees using the raw elevation in meters using the surface analysis- slope operation. Done in raw WGS 84 datum an projection using a Z-factor of 0.00001036.
3. `flow_accum`: Smoothed DEM (elev layer) 10 times using a neighborhood (focal) statistic with a 3 x 3 cell window in each iteration. Flow accumulation grid was then calculated where the accumulated flow is based on the number of cells flowing into each cell in the output raster. Note the current processing cell is not considered in this accumulation. Dependent up on flow direction grid calculates the direction (values range from 1-255) of the steepest drop from the current processing cell. 
4. `ldist_ridge`: Using filled DEM that is reprojected in a projection and datum with meters (i.e. UTM Zone 13 NAD83), calculate the following: Flow direction and flow accumulation grids. Reclassify flow accumulation to threshold out values greater than 0. In other words, grid cells with zero values have no flow accumulation and are thus ridges. Then calculate Euclidean distances of each grid cell to the nearest ridge with the extent set to the elev layer so that it does not calculate distances beyond the size of the grids. Reproject product to WGS1984 to match other grids.
5. `ldist_ridge2`: Using flow direction grid, perform flow length operation in the hydrology toolbox using upstream cells.
6. `ldist_valley`: Use flow accumulation grid projected in UTM Zone 13 NAD83 to identify valley bottom pixels by thresholding the flow accumulation grid to values > 100 using the reclassify tool to generate a new grid that has values of no data and 1 (for pixels > 100). Doing this produces a new grid that identifies pixels that have more than 100 pixels draining into them from the flow accumulation grid. Note: both valley bottom and ridge calculations will work with rasters of ridge/valley, but those rasters can also be converted to features (i.e. shapefiles) first and then operations can be done in the same manner if you wish to retain the ridge and valley features.  Then calculate Euclidean distances of each grid cell to the nearest valley with the extent set to the elev layer so that it does not calculate distances beyond the size of the grids. Reproject product to WGS1984 to match other grids.
7. `ldist_valley2`: Use flow direction grid and the downstream flow length tool to calculate downstream distance along the flow path for each cell. Reproject product to WGS1984 to match other grids after calculation in UTM NAD83 (a projection in meters). 
8. `z_ridge`: Use the Euclidean allocation toolbox under the distance section of Spatial Analyst. Specify the ridges (layer generated in step 5) as the input raster or feature source data, and specify the elevation layer as the input value raster. Under environments, specify the elev layer as the cell size, mask, and output extent.
9. `z_distridge`: Subtract the elev layer from the z_distridge layer to generate a new layer that gives you the difference in elevation (m) from the pixel to the nearest (Euclidean distance) ridge.
10. `z_valley`: Use the Euclidean allocation toolbox under the distance section of Spatial Analyst. Specify the valley layer (layer generated in step 3) as the input raster or feature source data, and specify the elevation layer as the input value raster. Under environments, specify the elev layer as the cell size, mask, and output extent.
11. `z_distvalley`: Subtract the elev layer from the z_valley layer to generate a new layer that gives you the difference in elevation (m) from the pixel to the nearest (Euclidean distance) valley.
12. `msd`: multiscalar dissection index: A measure of topographic dissections using multiple sized windows where dissection is calculated as: D = (z-zmin)/(zmax-zmin) for the following pixel window sizes: 3, 5, 7, 9, 11, 13, 15, 21, 27, 30 pixels where high values are more topographically dissected. Zmin and Zmax for each window were estimated using neighborhood focal statistics. D for each window size was calculated independently, and then msd was generated by summing all of the individual dissection indices for each window size.
13. `radiation`: Incoming solar radiation calculated for the whole year with a monthly interval (i.e. on the 15th of each month) and an hourly interval calculation. Latitudes were specified by site (CM- 29.2500, DM-30.5958, GM- 31.9167).
14. `relelev_l`: Relative l position channel to divide, based on l_distridge and l_distvalley. Relelev_l= l_distridge– l_distvalley so negative values mean that the point is closer to valley than to ridge.
 
