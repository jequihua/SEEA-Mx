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
  
  #change_mat = s3_s6_counts
  
  # initialize ad y re output table.
  adre = data.frame(matrix(0,8,19))
  names(adre)=c("1","2","3","4","5","6","8","10","12","14","17","19","21","22","23","25","27","29","31")
  row.names(adre) = c("Adiciones manejadas","Adiciones naturales", "Regresiones manejadas", 
                      "Regresiones naturales","Cambio desde otra vegetación", "Cambio a otra vegetación", 
                      "Falsos cambios positivos", "Falsos cambios negativos")

  # 1 - Acuícola
  ad_manej_1 = sum(change_mat[c(2:3,5:19),1])
  re_manej_1 = -1*sum(change_mat[1,c(2:5)])
  re_natur_1 = -1*sum(change_mat[1,c(6:19)])
  
  adre[1,1] = ad_manej_1
  adre[3,1] = re_manej_1
  adre[4,1] = re_natur_1
  
  # 2 - Agrícola Anual
  ad_manej_2 = sum(change_mat[c(1,3,5:19),2])
  re_manej_2 = -1*sum(change_mat[2,c(1,3:5)])
  re_natur_2 = -1*sum(change_mat[2,c(6:19)])
  
  adre[1,2] = ad_manej_2
  adre[3,2] = re_manej_2
  adre[4,2] = re_natur_2
  
  # 3 - Agrícola Anual
  ad_manej_3 = sum(change_mat[c(1:2,5:19),3])
  re_manej_3 = -1*sum(change_mat[3,c(1:2,4:5)])
  re_natur_3 = -1*sum(change_mat[3,c(6:19)])
  
  adre[1,3] = ad_manej_3
  adre[3,3] = re_manej_3
  adre[4,3] = re_natur_3
  
  # 4 - Asentamientos
  ad_manej_4 = sum(change_mat[c(1:3,5:19),4])
  
  adre[1,4] = ad_manej_4
  
  ### fill in statistics for natural vegetation
  
  for (i in 5:19){

    adre[2,i] = sum(change_mat[1:3,i])
    
    adre[3,i] = -1*sum(change_mat[i,1:4])
    
    idx = setdiff((5:19),i)
    
    # "Cambio desde otra vegetación"
    adre[5,i] = sum(change_mat[idx,i])
    # "Cambio a otra vegetación"
    adre[6,i] = -1*sum(change_mat[i,idx])
  }

  ### Fill in false changes
  for (f in 1:19){

    idx = setdiff((1:20),f)
    
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
final_accounting = function(original_extensions,var_t1, var_t2, adre, n, labels, water){
  
  fintab = data.frame(matrix(0,13,21))
  names(fintab) = c(labels,"Total")
  row.names(fintab) = c("Extensión apertura","Adiciones Manejadas", "Adiciones Naturales",
                        "Regresiones Manejadas", "Regresiones Naturales","Cambio desde otra vegetación", "Cambio a otra vegetación", 
                        "Falsos cambios positivos", "Falsos cambios negativos",
                        "Cambio Neto", "Tasa de Cambio Anual", "% de Cambio Respecto a Apertura",
                        "Extensión cierre") 
  
  ### introduce extensiones
  fintab[1,1:19] = original_extensions[,var_t1] 
  fintab[1,20] = sum(fintab[1,1:19])
  fintab[13,1:19] = original_extensions[,var_t2]
  fintab[13,20] = sum(fintab[13,1:19])
  
  #############################################################
  ### introduce additions
  # manejadas
  fintab[2,1:19] = adre[1,]
  fintab[2,20] = sum(fintab[2,1:19])
  # naturales
  fintab[3,1:19] = adre[2,]
  fintab[3,20] = sum(fintab[3,1:19])
  
  ### introduce regressions
  # manejadas
  fintab[4,1:19] = adre[3,]
  fintab[4,20] = sum(fintab[4,1:19])
  # naturales
  fintab[5,1:19] = adre[4,]
  fintab[5,20] = sum(fintab[5,1:19])
  
  ### introduce changes between vegetation
  # positives
  fintab[6,1:19] = adre[5,]
  fintab[6,20] = sum(fintab[6,1:19])
  # negatives
  fintab[7,1:19] = adre[6,]
  fintab[7,20] = sum(fintab[7,1:19])

  ### introduce false changes
  # positives
  fintab[8,1:19] = adre[7,]
  fintab[8,20] = sum(fintab[8,1:19])
  # negatives
  fintab[9,1:19] = adre[8,]
  fintab[9,20] = sum(fintab[9,1:19])
  #############################################################
  
  ### Introduce net change
  fintab[10,1:20] = t(fintab[13, 1:20]) - t(fintab[1, 1:20])
  
  ### Introduce tasa de cambio anual
  tasacambio = tasa_cambio(t(fintab[1,1:20]),t(fintab[13,1:20]),n=n)
  fintab[11,1:20] = tasacambio
  
  ### Introduce Porcentaje de cambio respecto a apertura
  fintab[12,1:20] = fintab[10,1:20] / fintab[1,1:20] *100
  
  return(fintab)
}

##################################################################################################
##################################################################################################
final_accountingw = function(original_extensions,var_t1, var_t2, adre, n, labels, water){

  colnames(adre) = names(adre)=c("1","2","3","4","5","6","8","10","12","14","17","19","21","22","23","25","27","29","31")
  
  fintab = data.frame(matrix(0,13,21))
  names(fintab) = c(labels[1:10],"Agua",labels[11:19],"Total")
  row.names(fintab) = c("Extensión apertura","Adiciones Manejadas", "Adiciones Naturales",
                        "Regresiones Manejadas", "Regresiones Naturales","Cambio desde otra vegetación", "Cambio a otra vegetación", 
                        "Falsos cambios positivos", "Falsos cambios negativos",
                        "Cambio Neto", "Tasa de Cambio Anual", "% de Cambio Respecto a Apertura",
                        "Extensión cierre") 

  ### introduce extensiones
  fintab[1,1:20] = original_extensions[,var_t1] 
  fintab[1,21] = sum(fintab[1,1:20])
  fintab[13,1:20] = original_extensions[,var_t2]
  fintab[13,21] = sum(fintab[13,1:20])
  fintab = fintab[,c(1:10,12:20,11,21)]

  #############################################################
  ### introduce additions
  # manejadas
  fintab[2,1:19] = adre[1,]
  fintab[2,21] = sum(fintab[2,1:19])
  # naturales
  fintab[3,1:19] = adre[2,]
  fintab[3,21] = sum(fintab[3,1:19])
  
  ### introduce regressions
  # manejadas
  fintab[4,1:19] = adre[3,]
  fintab[4,21] = sum(fintab[4,1:19])
  # naturales
  fintab[5,1:19] = adre[4,]
  fintab[5,21] = sum(fintab[5,1:19])
  
  ### introduce changes between vegetation
  # positives
  fintab[6,1:19] = adre[5,]
  fintab[6,21] = sum(fintab[6,1:19])
  # negatives
  fintab[7,1:19] = adre[6,]
  fintab[7,21] = sum(fintab[7,1:19])
  
  ### introduce false changes
  # positives
  fintab[8,1:19] = adre[7,]
  fintab[8,21] = sum(fintab[8,1:19])
  # negatives
  fintab[9,1:19] = adre[8,]
  fintab[9,21] = sum(fintab[9,1:19])
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
