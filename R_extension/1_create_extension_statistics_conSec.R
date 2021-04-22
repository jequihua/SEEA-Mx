# Load packages
library("raster")
library("rgdal")
library("readxl")

# Load INEGI series rasters.
s3 = raster("D:/work9seea/1_extension/data_extension_raster_aea/serie3_ipccv3_R_1.tif")
s4 = raster("D:/work9seea/1_extension/data_extension_raster_aea/serie4_ipccv3_R_1.tif")
s5 = raster("D:/work9seea/1_extension/data_extension_raster_aea/serie5_ipccv3_R_1.tif")
s6 = raster("D:/work9seea/1_extension/data_extension_raster_aea/serie6_ipccv3_R_1.tif")

# Load watermask
wmask = raster("D:/work9seea/5_extension3/data_water/universal_watermask_s3s4s5s6.tif")

# Mask water
s3[wmask == 1] = 16
s4[wmask == 1] = 16
s5[wmask == 1] = 16
s6[wmask == 1] = 16

# Pixel counts
s3_counts = data.frame(freq(s3))
s4_counts = data.frame(freq(s4))
s5_counts = data.frame(freq(s5))
s6_counts = data.frame(freq(s6))

s3_counts = s3_counts[!is.na(s3_counts$value),]
s4_counts = s4_counts[!is.na(s4_counts$value),]
s5_counts = s5_counts[!is.na(s5_counts$value),]
s6_counts = s6_counts[!is.na(s6_counts$value),]

insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

# Since s3 and s4 are missing "Especial Otros Tipos No Lenoso Secundario"
# We insert them
s3_counts = insertRow(s3_counts,c(15,0),15)
s4_counts = insertRow(s4_counts,c(15,0),15)

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

write.csv(allcounts,"D:/work9seea/5_extension4/data_tables/1_extension_change_table.csv",row.names = TRUE)

#####
# Create change matrices

s3_s4_counts = as.data.frame.matrix(table(c(values(s3), 15),
                                          c(values(s4),15), useNA = "always")*0.0625)
row.names(s3_s4_counts) = c(as.character(1:32),"NA")
s3_s4_countss = s3_s4_counts[c(-16),c(-16)]

s4_s5_counts = as.data.frame.matrix(table(c(values(s4),15),
                                          c(values(s5),NA), useNA = "always")*0.0625)
row.names(s4_s5_counts) = c(as.character(1:32),"NA")
s4_s5_countss = s4_s5_counts[c(-16),c(-16)]

s5_s6_counts = as.data.frame.matrix(table(values(s5),
                                          values(s6), useNA = "always")*0.0625)
row.names(s5_s6_counts) = c(as.character(1:32),"NA")
s5_s6_countss = s5_s6_counts[c(-16),c(-16)]

s3_s6_counts = as.data.frame.matrix(table(c(values(s3), 15),
                                          c(values(s6), NA), useNA = "always")*0.0625)
row.names(s3_s6_counts) = c(as.character(1:32),"NA")
s3_s6_countss = s3_s6_counts[c(-16),c(-16)]

write.csv(s3_s4_countss,"D:/work9seea/5_extension4/data_tables_c/2_matriz_cambios_s3_s4_wmask.csv",row.names = TRUE)
write.csv(s4_s5_countss,"D:/work9seea/5_extension4/data_tables_c/2_matriz_cambios_s4_s5_wmask.csv",row.names = TRUE)
write.csv(s5_s6_countss,"D:/work9seea/5_extension4/data_tables_c/2_matriz_cambios_s5_s6_wmask.csv",row.names = TRUE)
write.csv(s3_s6_countss,"D:/work9seea/5_extension4/data_tables_c/2_matriz_cambios_s3_s6_wmask.csv",row.names = TRUE)

### Create adiciones y regresiones table.

source("D:/work9seea/5_extension3/R/1_add_reg_function_conSec.R")

s3_s4_addreg = calcAddReg(s3_s4_countss)
s4_s5_addreg = calcAddReg(s4_s5_countss)
s5_s6_addreg = calcAddReg(s5_s6_countss)
s3_s6_addreg = calcAddReg(s3_s6_countss)

write.csv(s3_s4_addreg,"D:/work9seea/5_extension4/data_tables_c/3_matriz_cambios_s3_s4_wmask_AdicionesRegresiones.csv",row.names = TRUE)
write.csv(s4_s5_addreg,"D:/work9seea/5_extension4/data_tables_c/3_matriz_cambios_s4_s5_wmask_AdicionesRegresiones.csv",row.names = TRUE)
write.csv(s5_s6_addreg,"D:/work9seea/5_extension4/data_tables_c/3_matriz_cambios_s5_s6_wmask_AdicionesRegresiones.csv",row.names = TRUE)
write.csv(s3_s6_addreg,"D:/work9seea/5_extension4/data_tables_c/3_matriz_cambios_s3_s6_wmask_AdicionesRegresiones.csv",row.names = TRUE)

### Create final accounting table

# Load labels
labels = as.data.frame(read_excel("D:/work9seea/5_extension4/data_tables/usueyv_csv.xlsx"))
labels = as.character(labels$clase)[-16]

s3_s4_final = final_accounting(allcounts[-16,],"s3","s4", s3_s4_addreg, n=5, labels = labels)
s4_s5_final = final_accounting(allcounts[-16,],"s4","s5", s4_s5_addreg, n=4, labels = labels)
s5_s6_final = final_accounting(allcounts[-16,],"s5","s6", s5_s6_addreg, n=3, labels = labels)
s3_s6_final = final_accounting(allcounts[-16,],"s3","s6", s3_s6_addreg, n=12, labels = labels)

write.csv(s3_s4_final,"D:/work9seea/5_extension4/data_tables_c/4_s3_s4_wmask_finalaccounting.csv",row.names = TRUE)
write.csv(s4_s5_final,"D:/work9seea/5_extension4/data_tables_c/4_s4_s5_wmask_finalaccounting.csv",row.names = TRUE)
write.csv(s5_s6_final,"D:/work9seea/5_extension4/data_tables_c/4_s5_s6_wmask_finalaccounting.csv",row.names = TRUE)
write.csv(s3_s6_final,"D:/work9seea/5_extension4/data_tables_c/4_s3_s6_wmask_finalaccounting.csv",row.names = TRUE)


View(s3_s4_final)
