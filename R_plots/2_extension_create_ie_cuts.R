# library
library(tidyverse)
library(raster)
library(arules)

# Load IE
ie = raster("D:/work9seea/1_extension/layers_harmon/ie_2018_st_v2.tif")


# Load S6
s6 = raster("D:/work9seea/5_extension3/data_waterno/serie6_ipccv3_R_1_sinSec_wmask.tif")

# IE and S6 data.frame
brik = brick()
brik = addLayer(brik, ie)
brik = addLayer(brik, s6)
dat = data.frame(rasterToPoints(brik))
discie = discretize(dat$ie_2018_st_v2,
                    method = "fixed",
                    breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))

dat$ie = discie
discie
# Create summary
dat_counts = as.data.frame.matrix(table(dat$serie6_ipccv3_R_1_sinSec,dat$ie))

interval_labels = names(dat_counts)

# add labels
in_table = read.csv("D:/work9seea/5_pres/inegi_style.csv", header = TRUE)
in_table$veg1 = NULL
in_table$color = NULL
in_table$tipo = NULL
dat_fin = data.frame(in_table,dat_counts)
head(dat_fin)
names(dat_fin)[2:ncol(dat_fin)] = interval_labels

# Transform data in a tidy format (long format)
dat_ln <- dat_fin %>% gather(key = "observation", value="value",`[0,0.1)`:`[0.9,1]`) 
names(dat_ln)[1:2]=c("individual","group")

# Add percentage per class
grp = as.character(unique(dat_ln$individual))
dat_ln$iecuts2018p=0
for (i in 1:length(grp)){

  grpint = grp[i]
  dat_ln_sub = dat_ln[dat_ln$individual==grpint,]
  
  proportions = dat_ln_sub$value/sum(dat_ln_sub$value,na.rm = TRUE)
proportions
  dat_ln$iecuts2018p[dat_ln$individual==grpint] = proportions
  
}

names(dat_ln)[1:3] = c("veg2","cuts","iecuts2018")

dat_ln = dat_ln[order(dat_ln$veg2),]
dat_ln$iecuts2018 = dat_ln$iecuts2018*0.0625
write.csv(dat_ln,"D:/work9seea/8_create_final_condition_plots/ie_cuts.csv",row.names = FALSE)

head(dat_ln)
