# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# ldist_ridge2.py
# Created on: 2016-06-05 09:15:01.00000
#   (generated by ArcGIS/ModelBuilder)
# Description: 
# ---------------------------------------------------------------------------

# Import arcpy module
import arcpy


# Local variables:
flowdir = "C:\\Users\\hmpou\\Documents\\TXDEM051915\\smoothed060216\\flowdir"
ldist_ridge2 = "C:\\Users\\hmpou\\Documents\\TXDEM051915\\smoothed060216\\ldist_ridge2"
ldist_ridwgs = "C:\\Users\\hmpou\\Documents\\TXDEM051915\\smoothed060216\\ldist_ridwgs"
elev_asc = "C:\\Users\\hmpou\\Documents\\TXDEM051915\\final_zipped_grids\\CM\\elev.asc"
maskedcm = "C:\\Users\\hmpou\\Documents\\ArcGIS\\Default.gdb\\maskedcm"
ldist_ridge2_asc = "C:\\Users\\hmpou\\Documents\\TXDEM051915\\final_zipped_grids\\CM\\ldist_ridge2.asc"
elev_asc__3_ = "C:\\Users\\hmpou\\Documents\\TXDEM051915\\final_zipped_grids\\GM\\elev.asc"
maskedgm = "C:\\Users\\hmpou\\Documents\\ArcGIS\\Default.gdb\\maskedgm"
ldist_ridge2_asc__2_ = "C:\\Users\\hmpou\\Documents\\TXDEM051915\\final_zipped_grids\\GM\\ldist_ridge2.asc"
elev_asc__2_ = "C:\\Users\\hmpou\\Documents\\TXDEM051915\\final_zipped_grids\\DM\\DM\\elev.asc"
maskeddm = "C:\\Users\\hmpou\\Documents\\ArcGIS\\Default.gdb\\maskeddm"
ldist_ridge2_asc__3_ = "C:\\Users\\hmpou\\Documents\\TXDEM051915\\final_zipped_grids\\DM\\DM\\ldist_ridge2.asc"

# Process: Flow Length upstream
tempEnvironment0 = arcpy.env.snapRaster
arcpy.env.snapRaster = "smothdem83"
tempEnvironment1 = arcpy.env.extent
arcpy.env.extent = "490780.95083911 3212665.34217014 687305.825475889 3561030.62767418"
tempEnvironment2 = arcpy.env.cellSize
arcpy.env.cellSize = "28.921983022337"
tempEnvironment3 = arcpy.env.mask
arcpy.env.mask = "smothdem83"
arcpy.gp.FlowLength_sa(flowdir, ldist_ridge2, "UPSTREAM", "")
arcpy.env.snapRaster = tempEnvironment0
arcpy.env.extent = tempEnvironment1
arcpy.env.cellSize = tempEnvironment2
arcpy.env.mask = tempEnvironment3

# Process: Project Raster
arcpy.ProjectRaster_management(ldist_ridge2, ldist_ridwgs, "GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]]", "NEAREST", "2.8355922196592E-04 2.8355922196592E-04", "WGS_1984_(ITRF00)_To_NAD_1983", "", "PROJCS['NAD_1983_UTM_Zone_13N',GEOGCS['GCS_North_American_1983',DATUM['D_North_American_1983',SPHEROID['GRS_1980',6378137.0,298.257222101]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Transverse_Mercator'],PARAMETER['False_Easting',500000.0],PARAMETER['False_Northing',0.0],PARAMETER['Central_Meridian',-105.0],PARAMETER['Scale_Factor',0.9996],PARAMETER['Latitude_Of_Origin',0.0],UNIT['Meter',1.0]]")

# Process: extract mask CM
tempEnvironment0 = arcpy.env.snapRaster
arcpy.env.snapRaster = "C:\\Users\\hmpou\\Documents\\TXDEM051915\\final_zipped_grids\\CM\\elev.asc"
tempEnvironment1 = arcpy.env.extent
arcpy.env.extent = "-103.6698019919 29.032009197477 -102.942579769096 29.529231420097"
tempEnvironment2 = arcpy.env.cellSize
arcpy.env.cellSize = "2.77777778000001E-04"
tempEnvironment3 = arcpy.env.mask
arcpy.env.mask = "C:\\Users\\hmpou\\Documents\\TXDEM051915\\final_zipped_grids\\CM\\elev.asc"
arcpy.gp.ExtractByMask_sa(ldist_ridwgs, elev_asc, maskedcm)
arcpy.env.snapRaster = tempEnvironment0
arcpy.env.extent = tempEnvironment1
arcpy.env.cellSize = tempEnvironment2
arcpy.env.mask = tempEnvironment3

# Process: Raster to ASCII
arcpy.RasterToASCII_conversion(maskedcm, ldist_ridge2_asc)

# Process: extract mask GM
tempEnvironment0 = arcpy.env.snapRaster
arcpy.env.snapRaster = "C:\\Users\\hmpou\\Documents\\TXDEM051915\\final_zipped_grids\\GM\\elev.asc"
tempEnvironment1 = arcpy.env.extent
arcpy.env.extent = "-105.1699300894 31.718225977625 -104.48520786663 32.186559311333"
tempEnvironment2 = arcpy.env.cellSize
arcpy.env.cellSize = "2.77777778000002E-04"
tempEnvironment3 = arcpy.env.mask
arcpy.env.mask = "C:\\Users\\hmpou\\Documents\\TXDEM051915\\final_zipped_grids\\GM\\elev.asc"
arcpy.gp.ExtractByMask_sa(ldist_ridwgs, elev_asc__3_, maskedgm)
arcpy.env.snapRaster = tempEnvironment0
arcpy.env.extent = tempEnvironment1
arcpy.env.cellSize = tempEnvironment2
arcpy.env.mask = tempEnvironment3

# Process: Raster to ASCII (2)
arcpy.RasterToASCII_conversion(maskedgm, ldist_ridge2_asc__2_)

# Process: extract mask DM
tempEnvironment0 = arcpy.env.snapRaster
arcpy.env.snapRaster = "C:\\Users\\hmpou\\Documents\\TXDEM051915\\final_zipped_grids\\DM\\DM\\elev.asc"
tempEnvironment1 = arcpy.env.extent
arcpy.env.extent = "-104.57461785062 30.448603245144 -103.671562294342 31.073047690088"
tempEnvironment2 = arcpy.env.cellSize
arcpy.env.cellSize = "2.77777778000002E-04"
tempEnvironment3 = arcpy.env.mask
arcpy.env.mask = "C:\\Users\\hmpou\\Documents\\TXDEM051915\\final_zipped_grids\\DM\\DM\\elev.asc"
arcpy.gp.ExtractByMask_sa(ldist_ridwgs, elev_asc__2_, maskeddm)
arcpy.env.snapRaster = tempEnvironment0
arcpy.env.extent = tempEnvironment1
arcpy.env.cellSize = tempEnvironment2
arcpy.env.mask = tempEnvironment3

# Process: Raster to ASCII (3)
arcpy.RasterToASCII_conversion(maskeddm, ldist_ridge2_asc__3_)

