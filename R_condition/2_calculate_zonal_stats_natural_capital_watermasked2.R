
# Load packages.
library("raster")
library("rgdal")
library("data.table")
library("dplyr")
library("readxl")
library("writexl")
library("ggplot2")



#########################################################

ie_2004 = raster("D:/work9seea/1_extension/layers_harmon/ie_2004_st_v2.tif")
in_2004 = raster("D:/work9seea/1_extension/layers_harmon/serie3_ipccv3_R_1_sinSec_wmsk.tif")
brik_2004 = brick()
brik_2004 = addLayer(brik_2004,in_2004)
brik_2004 = addLayer(brik_2004,ie_2004)

ie_2018 = raster("D:/work9seea/1_extension/layers_harmon/ie_2018_st_v2.tif")
in_2018 = raster("D:/work9seea/1_extension/layers_harmon/serie6_ipccv3_R_1_sinSec_wmsk.tif")
brik_2018 = brick()
brik_2018 = addLayer(brik_2018,in_2018)
brik_2018 = addLayer(brik_2018,ie_2018)

# To data.frames
dat_2004_df = as.data.frame(rasterToPoints(brik_2004))
dat_2018_df = as.data.frame(rasterToPoints(brik_2018))

stats_2004 = dat_2004_df %>% 
             group_by(serie3_ipccv3_R_1_sinSec_wmsk) %>% 
             summarise(extension2004=n(),
                       capitalnatural2004=sum(ie_2004_st_v2,na.rm = TRUE))

stats_2018 = dat_2018_df %>% 
  group_by(serie6_ipccv3_R_1_sinSec_wmsk) %>% 
  summarise(extension2014=n(),
            capitalnatural2018=sum(ie_2018_st_v2,na.rm = TRUE))

# Stats 2004 and 2018
stats_2004_2018 = data.frame(inegi=stats_2004$serie3_ipccv3_R_1_sinSec_wmsk, stats_2004, stats_2018)
stats_2004_2018 = stats_2004_2018[-20,c(1,3,4,6,7)]

# Load inegi 
ine = as.data.frame(read_excel("D:/work9seea/1_extension/tasas_cambio.xlsx"))

stats_2004_2018$inegi_nom = ""
for (i in 1:nrow(ine)){
  val = ine$value...2[i]
  print(as.numeric(val))
  stats_2004_2018$inegi_nom[stats_2004_2018$inegi==as.numeric(val)] = as.character(ine$veg2[i])
}

for (i in 2:5){
  stats_2004_2018[,i] = stats_2004_2018[,i]*250*250/1000000
}

write_xlsx(stats_2004_2018, "D:/work9seea/2_condition/reports/SEEA_extension_y_capitalnatural_v3.xlsx")

View(stats_2004_2018)
