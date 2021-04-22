
# Load packages
library("raster")
library("sp")

# Load ref raster
ref = raster("D:/work9seea/1_extension/data_extension_raster_aea/serie3_ipccv3_R_1.tif")

# Raster list to be harmonized
rasts = list.files("D:/work7/ie_nodes/rasters/2018h/",
                   pattern = "\\.tif$",
                   full.names = TRUE)

# Harmonize
for (i in 1:length(rasts)){
  print(i)
  rast = raster(rasts[i])
  
  
  
  rast = projectRaster(rast,ref,method="ngb")
  print(extent(rast)==extent(ref))
  
  writeRaster(rast, filename=paste0("D:/work9seea/2_condition/data/2018h/",basename(rasts[i])),
              format="GTiff", overwrite=TRUE,
              options="COMPRESS=LZW")
}
