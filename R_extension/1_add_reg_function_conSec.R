tasa_cambio = function(t1,t2,n){
  outlist = list()
  counter = 0
  for (i in 1:length(t1)){
    counter = counter+1
    if (t1[i]>t2[i]){
      outlist[[counter]] = -1*(1-(1-(t1[i]-t2[i])/t1[i])^(1/n))*100
    } else {
      outlist[[counter]] = -1*((1-(1-(t1[i]-t2[i])/t1[i])^(1/n)))*100
    }
    
  }
  outlist = unlist(outlist)
  return(outlist)
} 
##########################################################################3

calcAddReg = function(change_mat){
  
  #change_mat = s3_s6_countss
  
  # initialize ad y re output table.
  adre = data.frame(matrix(0,8,31))
  names(adre)=as.character(c(1:15,17:32))
  row.names(adre) = c("Adiciones manejadas","Adiciones naturales", "Regresiones manejadas", 
                      "Regresiones naturales","Cambio desde otra vegetación", "Cambio a otra vegetación", 
                      "Falsos cambios positivos", "Falsos cambios negativos")

  # 1 - Acuícola
  ad_manej_1 = sum(change_mat[c(2:3,5:31),1])
  re_manej_1 = -1*sum(change_mat[1,c(2:5)])
  re_natur_1 = -1*sum(change_mat[1,c(6:31)])
  
  adre[1,1] = ad_manej_1
  adre[3,1] = re_manej_1
  adre[4,1] = re_natur_1
  
  # 2 - Agrícola Anual
  ad_manej_2 = sum(change_mat[c(1,3,5:31),2])
  re_manej_2 = -1*sum(change_mat[2,c(1,3:5)])
  re_natur_2 = -1*sum(change_mat[2,c(6:31)])
  
  adre[1,2] = ad_manej_2
  adre[3,2] = re_manej_2
  adre[4,2] = re_natur_2
  
  # 3 - Agrícola Anual
  ad_manej_3 = sum(change_mat[c(1:2,5:31),3])
  re_manej_3 = -1*sum(change_mat[3,c(1:2,4:5)])
  re_natur_3 = -1*sum(change_mat[3,c(6:31)])
  
  adre[1,3] = ad_manej_3
  adre[3,3] = re_manej_3
  adre[4,3] = re_natur_3
  
  # 4 - Asentamientos
  ad_manej_4 = sum(change_mat[c(1:3,5:31),4])
  
  adre[1,4] = ad_manej_4
  
  ### fill in statistics for natural vegetation
  
  for (i in 5:31){

    adre[2,i] = sum(change_mat[1:3,i])
    
    adre[3,i] = -1*sum(change_mat[i,1:4])
    
    idx = setdiff((5:31),i)

    # "Cambio desde otra vegetación"
    adre[5,i] = sum(change_mat[idx,i])
    # "Cambio a otra vegetación"
    adre[6,i] = -1*sum(change_mat[i,idx])
  }

  ### Fill in false changes
  for (f in 1:31){

    idx = setdiff((1:32),f)

    # Falsos cambios positivos
    adre[7,f] = sum(change_mat[idx,f]) - sum(adre[1:2,f]) - adre[5,f]
    
    # Falsos cambios negativos
    adre[8,f] = -1*(sum(change_mat[f, idx]) + sum(adre[3:4,f]) + adre[6,f])

  }
  return(adre)
}

##################################################################################################
##################################################################################################
tasa_cambio = function(t1,t2,n){
  outlist = list()
  counter = 0
  for (i in 1:length(t1)){
    counter = counter+1
    if (t1[i]>t2[i]){
      outlist[[counter]] = -1*(1-(1-(t1[i]-t2[i])/t1[i])^(1/n))*100
    } else {
      outlist[[counter]] = -1*((1-(1-(t1[i]-t2[i])/t1[i])^(1/n)))*100
    }
    
  }
  outlist = unlist(outlist)
  return(outlist)
} 


##################################################################################################
##################################################################################################
final_accounting = function(original_extensions,var_t1, var_t2, adre, n, labels){
  #original_extensions = allcounts[-16,]
  #var_t1 = "s3"
  #var_t2 = "s4"
  #adre = s3_s4_addreg
  #n=5
  
  
  fintab = data.frame(matrix(0,13,33))
  names(fintab) = c(labels,"Agua","Total")
  row.names(fintab) = c("Extensión apertura","Adiciones Manejadas", "Adiciones Naturales",
                        "Regresiones Manejadas", "Regresiones Naturales","Cambio desde otra vegetación", "Cambio a otra vegetación", 
                        "Falsos cambios positivos", "Falsos cambios negativos",
                        "Cambio Neto", "Tasa de Cambio Anual", "% de Cambio Respecto a Apertura",
                        "Extensión cierre") 
  
  # Impute water mask
  fintab$Agua[1] = 27547.6875
  fintab$Agua[13] = 27547.6875
  
  ### introduce extensiones
  fintab[1,1:31] = original_extensions[,var_t1]
  fintab[1,33] = sum(fintab[1,1:32])
  fintab[13,1:31] = original_extensions[,var_t2]
  fintab[13,33] = sum(fintab[13,1:32])

  #############################################################
  ### introduce additions
  # manejadas
  fintab[2,1:31] = as.numeric(adre[1,])

  fintab[2,33] = sum(fintab[2,1:32])
  # naturales
  fintab[3,1:31] = as.numeric(adre[2,])
  fintab[3,33] = sum(fintab[3,1:32])

  ### introduce regressions
  # manejadas
  fintab[4,1:31] = as.numeric(adre[3,])
  fintab[4,33] = sum(fintab[4,1:32])
  # naturales
  fintab[5,1:31] = as.numeric(adre[4,])
  fintab[5,33] = sum(fintab[5,1:32])
  
  ### introduce changes between vegetation
  # positives
  fintab[6,1:31] = adre[5,]
  fintab[6,33] = sum(fintab[6,1:32])
  # negatives
  fintab[7,1:31] = as.numeric(adre[6,])
  fintab[7,33] = sum(fintab[7,1:32])

  ### introduce false changes
  # positives
  fintab[8,1:31] = as.numeric(adre[7,])
  fintab[8,33] = sum(fintab[8,1:32])
  # negatives
  fintab[9,1:31] = as.numeric(adre[8,])
  fintab[9,33] = sum(fintab[9,1:32])
  #############################################################
  
  ### Introduce net change
  fintab[10,1:33] = t(fintab[13, 1:33]) - t(fintab[1, 1:33])
  
  ### Introduce tasa de cambio anual
  tasacambio = tasa_cambio(t(fintab[1,1:33]),t(fintab[13,1:33]),n=n)
  fintab[11,1:33] = tasacambio
  
  ### Introduce Porcentaje de cambio respecto a apertura
  fintab[12,1:33] = fintab[10,1:33] / fintab[1,1:33] *100
  
  return(fintab)
}

##################################################################################################
##################################################################################################
final_accountingw = function(original_extensions,var_t1, var_t2, adre, n, labels, water){

  colnames(adre) = names(adre)=c("1","2","3","4","5","6","8","10","12","14","17","31","21","22","23","25","27","29","31")
  
  fintab = data.frame(matrix(0,13,21))
  names(fintab) = c(labels[1:10],"Agua",labels[11:31],"Total")
  row.names(fintab) = c("Extensión apertura","Adiciones Manejadas", "Adiciones Naturales",
                        "Regresiones Manejadas", "Regresiones Naturales","Cambio desde otra vegetación", "Cambio a otra vegetación", 
                        "Falsos cambios positivos", "Falsos cambios negativos",
                        "Cambio Neto", "Tasa de Cambio Anual", "% de Cambio Respecto a Apertura",
                        "Extensión cierre") 

  ### introduce extensiones
  fintab[1,1:32] = original_extensions[,var_t1] 
  fintab[1,21] = sum(fintab[1,1:32])
  fintab[13,1:32] = original_extensions[,var_t2]
  fintab[13,21] = sum(fintab[13,1:32])
  fintab = fintab[,c(1:10,12:32,11,21)]

  #############################################################
  ### introduce additions
  # manejadas
  fintab[2,1:31] = adre[1,]
  fintab[2,21] = sum(fintab[2,1:31])
  # naturales
  fintab[3,1:31] = adre[2,]
  fintab[3,21] = sum(fintab[3,1:31])
  
  ### introduce regressions
  # manejadas
  fintab[4,1:31] = adre[3,]
  fintab[4,21] = sum(fintab[4,1:31])
  # naturales
  fintab[5,1:31] = adre[4,]
  fintab[5,21] = sum(fintab[5,1:31])
  
  ### introduce changes between vegetation
  # positives
  fintab[6,1:31] = adre[5,]
  fintab[6,21] = sum(fintab[6,1:31])
  # negatives
  fintab[7,1:31] = adre[6,]
  fintab[7,21] = sum(fintab[7,1:31])
  
  ### introduce false changes
  # positives
  fintab[8,1:31] = adre[7,]
  fintab[8,21] = sum(fintab[8,1:31])
  # negatives
  fintab[9,1:31] = adre[8,]
  fintab[9,21] = sum(fintab[9,1:31])
  #############################################################
  
  ### Introduce net change
  fintab[10,1:21] = t(fintab[13, 1:21]) - t(fintab[1, 1:21])
  
  ### Introduce tasa de cambio anual
  tasacambio = tasa_cambio(t(fintab[1,1:21]),t(fintab[13,1:21]),n=n)
  fintab[11,1:21] = tasacambio
  
  ### Introduce Porcentaje de cambio respecto a apertura
  fintab[12,1:21] = fintab[10,1:21] / fintab[1,1:21] *100

  return(fintab)
}
