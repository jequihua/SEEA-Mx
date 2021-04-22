# Load packages
library("raster")
library("rgdal")
library("readxl")

# Load INEGI series rasters.
s3 = raster("D:/work9seea/5_extension3/data_water/serie3_ipccv3_R_1_sinSec.tif")
s4 = raster("D:/work9seea/5_extension3/data_water/serie4_ipccv3_R_1_sinSec.tif")
s5 = raster("D:/work9seea/5_extension3/data_water/serie5_ipccv3_R_1_sinSec.tif")
s6 = raster("D:/work9seea/5_extension3/data_water/serie6_ipccv3_R_1_sinSec.tif")

# Create universal water mask.
water_mask = (s3==16) | (s4==16) | (s5==16) | (s6==16) 
#wm = writeRaster(water_mask, filename="D:/work9seea/5_extension3/data_water/universal_watermask_s3s4s5s6.tif",
#                 format="GTiff", datatype="INT2U", options="COMPRESS=LZW", overwrite=TRUE)

# Mask INEGI series.
s3[water_mask == 1] = 16
s4[water_mask == 1] = 16
s5[water_mask == 1] = 16
s6[water_mask == 1] = 16

writeRaster(s3, filename="D:/work9seea/5_extension3/data_wateryes/serie3_ipccv3_R_1_sinSec_wmask.tif",
                                  format="GTiff", options="COMPRESS=LZW", overwrite=TRUE)

writeRaster(s4, filename="D:/work9seea/5_extension3/data_wateryes/serie4_ipccv3_R_1_sinSec_wmask.tif",
            format="GTiff", options="COMPRESS=LZW", overwrite=TRUE)

writeRaster(s5, filename="D:/work9seea/5_extension3/data_wateryes/serie5_ipccv3_R_1_sinSec_wmask.tif",
            format="GTiff", options="COMPRESS=LZW", overwrite=TRUE)

writeRaster(s6, filename="D:/work9seea/5_extension3/data_wateryes/serie6_ipccv3_R_1_sinSec_wmask.tif",
            format="GTiff", options="COMPRESS=LZW", overwrite=TRUE)