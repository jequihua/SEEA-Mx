# Load packages
library("raster")
library("rgdal")
library("readxl")

# Load INEGI series rasters.
s3 = raster("D:/work9seea/5_extension3/data_wateryes/serie3_ipccv3_R_1_sinSec_wmask.tif")
s4 = raster("D:/work9seea/5_extension3/data_wateryes/serie4_ipccv3_R_1_sinSec_wmask.tif")
s5 = raster("D:/work9seea/5_extension3/data_wateryes/serie5_ipccv3_R_1_sinSec_wmask.tif")
s6 = raster("D:/work9seea/5_extension3/data_wateryes/serie6_ipccv3_R_1_sinSec_wmask.tif")

# Pixel counts
s3_counts = data.frame(freq(s3))
s4_counts = data.frame(freq(s4))
s5_counts = data.frame(freq(s5))
s6_counts = data.frame(freq(s6))

s3_counts = s3_counts[!is.na(s3_counts$value),]
s4_counts = s4_counts[!is.na(s4_counts$value),]
s5_counts = s5_counts[!is.na(s5_counts$value),]
s6_counts = s6_counts[!is.na(s6_counts$value),]

allcounts = data.frame(value=s3_counts$value,
                       s3=s3_counts$count*0.0625,
                       s4=s4_counts$count*0.0625,
                       s5=s5_counts$count*0.0625,
                       s6=s6_counts$count*0.0625)


allcounts$TasaCambio_s3_s4 = tasa_cambio(allcounts$s3,allcounts$s4,n=5)
allcounts$TasaCambio_s4_s5 = tasa_cambio(allcounts$s4,allcounts$s5,n=4)
allcounts$TasaCambio_s5_s6 = tasa_cambio(allcounts$s5,allcounts$s6,n=3)
allcounts$TasaCambio_s3_s6 = tasa_cambio(allcounts$s3,allcounts$s6,n=12)

allcounts$CambioNeto_s3_s4 = allcounts$s4 - allcounts$s3
allcounts$CambioNeto_s4_s5 = allcounts$s5 - allcounts$s4
allcounts$CambioNeto_s5_s6 = allcounts$s6 - allcounts$s5
allcounts$CambioNeto_s3_s6 = allcounts$s6 - allcounts$s3

write.csv(allcounts,"D:/work9seea/5_extension3/data_tablesw/1_extension_change_table.csv",row.names = TRUE)

#####
# Create change matrices
s3_s4_counts = as.data.frame.matrix(table(values(s3),
                                          values(s4), useNA = "always")*0.0625)
row.names(s3_s4_counts) = c(names(s3_s4_counts)[1:19],"NA")

s4_s5_counts = as.data.frame.matrix(table(values(s4),
                                          values(s5), useNA = "always")*0.0625)
row.names(s4_s5_counts) = c(names(s4_s5_counts)[1:19],"NA")

s5_s6_counts = as.data.frame.matrix(table(values(s5),
                                          values(s6), useNA = "always")*0.0625)
row.names(s4_s5_counts) = c(names(s4_s5_counts)[1:19],"NA")

s3_s6_counts = as.data.frame.matrix(table(values(s3),
                                          values(s6), useNA = "always")*0.0625)
row.names(s3_s6_counts) = c(names(s3_s6_counts)[1:19],"NA")

write.csv(s3_s4_counts,"D:/work9seea/5_extension3/data_tablesw/2_matriz_cambios_s3_s4_wmask.csv",row.names = TRUE)
write.csv(s4_s5_counts,"D:/work9seea/5_extension3/data_tablesw/2_matriz_cambios_s4_s5_wmask.csv",row.names = TRUE)
write.csv(s5_s6_counts,"D:/work9seea/5_extension3/data_tablesw/2_matriz_cambios_s5_s6_wmask.csv",row.names = TRUE)
write.csv(s3_s6_counts,"D:/work9seea/5_extension3/data_tablesw/2_matriz_cambios_s3_s6_wmask.csv",row.names = TRUE)

### Create adiciones y regresiones table.

source("D:/work9seea/5_extension3/R/1_add_reg_function.R")

s3_s4_addreg = calcAddReg(s3_s4_counts)
s4_s5_addreg = calcAddReg(s4_s5_counts)
s5_s6_addreg = calcAddReg(s5_s6_counts)
s3_s6_addreg = calcAddReg(s3_s6_counts)

write.csv(s3_s4_addreg,"D:/work9seea/5_extension3/data_tablesw/3_matriz_cambios_s3_s4_wmask_AdicionesRegresiones.csv",row.names = TRUE)
write.csv(s4_s5_addreg,"D:/work9seea/5_extension3/data_tablesw/3_matriz_cambios_s4_s5_wmask_AdicionesRegresiones.csv",row.names = TRUE)
write.csv(s5_s6_addreg,"D:/work9seea/5_extension3/data_tablesw/3_matriz_cambios_s5_s6_wmask_AdicionesRegresiones.csv",row.names = TRUE)
write.csv(s3_s6_addreg,"D:/work9seea/5_extension3/data_tablesw/3_matriz_cambios_s3_s6_wmask_AdicionesRegresiones.csv",row.names = TRUE)

### Create final accounting table

# Load labels
labels = readRDS("D:/work9seea/1_extension2/tables/extension_change_table.RDS")
labels = labels$veg2

s3_s4_final = final_accounting(allcounts,"s3","s4", s3_s4_addreg, n=5, labels = labels)
s4_s5_final = final_accounting(allcounts,"s4","s5", s4_s5_addreg, n=4, labels = labels)
s5_s6_final = final_accounting(allcounts,"s5","s6", s5_s6_addreg, n=3, labels = labels)
s3_s6_final = final_accounting(allcounts,"s3","s6", s3_s6_addreg, n=12, labels = labels)

write.csv(s3_s4_final,"D:/work9seea/5_extension3/data_tables/4_s3_s4_wmask_finalaccounting.csv",row.names = TRUE)
write.csv(s4_s5_final,"D:/work9seea/5_extension3/data_tables/4_s4_s5_wmask_finalaccounting.csv",row.names = TRUE)
write.csv(s5_s6_final,"D:/work9seea/5_extension3/data_tables/4_s5_s6_wmask_finalaccounting.csv",row.names = TRUE)
write.csv(s3_s6_final,"D:/work9seea/5_extension3/data_tables/4_s3_s6_wmask_finalaccounting.csv",row.names = TRUE)


View(s3_s4_final)
