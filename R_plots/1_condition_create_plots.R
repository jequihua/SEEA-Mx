
# Load packages
library("raster")
library("rgdal")
library("readxl")
library("plotly")
library("orca")
library("processx")

allcounts = read.csv("D:/work9seea/5_pres/datos.csv", header=TRUE)

# save to disk
#saveRDS(allcounts,"D:/work9seea/1_extension2/tables/extension_change_table.RDS")

### PLOTS

## Extension
# S3

allcounts$veg2 = factor(allcounts$veg2, levels = allcounts$veg2[order(allcounts$s3)])
allcounts$s3 = round(allcounts$s3)

fig <- plot_ly(allcounts, x = ~s3, y = ~veg2 , type = 'bar', orientation = "h",
               text = allcounts$s3, textposition = 'outside',
               marker = list(color = allcounts$color.y,
                             line = list(color = 'black',
                                         width = 1)))
fig <- fig %>% layout(title = "",
                      xaxis = list(title = "Kilómetros cuadrados",range = c(0,450000)),
                      yaxis = list(title = ""))


fig

server = orca_serve()
server$export(fig, "D:/work9seea/1_extension2/plots/1_extension_barcharts/ext_s3_barchart.png",
              scale = 1.5)

# S4
allcounts$veg2 = factor(allcounts$veg2, levels = allcounts$veg2[order(allcounts$s4)])
allcounts$s4 = round(allcounts$s4)
config(.Last.value, mathjax = 'cdn')
fig <- plot_ly(allcounts, x = ~s4, y = ~veg2 , type = 'bar', orientation = "h",
               text = allcounts$s4, textposition = 'outside',
               marker = list(color = allcounts$color.y,
                             line = list(color = 'black',
                                         width = 1)))
fig <- fig %>% layout(title = "",
                      xaxis = list(title = "Kilómetros cuadrados",range = c(0,450000)),
                      yaxis = list(title = ""))
fig
server$export(fig, "D:/work9seea/1_extension2/plots/1_extension_barcharts/ext_s4_barchart.png",
              scale = 1.5)

# S5

allcounts$veg2 = factor(allcounts$veg2, levels = allcounts$veg2[order(allcounts$s5)])
allcounts$s5 = round(allcounts$s5)
fig <- plot_ly(allcounts, x = ~s5, y = ~veg2 , type = 'bar', orientation = "h",
               text = allcounts$s5, textposition = 'outside',
               marker = list(color = allcounts$color.y,
                             line = list(color = 'black',
                                         width = 1)))
fig <- fig %>% layout(title = "",
                      xaxis = list(title = "Kilómetros cuadrados",range = c(0,450000)),
                      yaxis = list(title = ""))


server$export(fig, "D:/work9seea/1_extension2/plots/1_extension_barcharts/ext_s5_barchart.png",
              scale = 1.5)

# S6

allcounts$veg2 = factor(allcounts$veg2, levels = allcounts$veg2[order(allcounts$s6)])
allcounts$s6 = round(allcounts$s6)
fig <- plot_ly(allcounts, x = ~s6, y = ~veg2 , type = 'bar', orientation = "h",
               text = allcounts$s6, textposition = 'outside',
               marker = list(color = allcounts$color.y,
                             line = list(color = 'black',
                                         width = 1)))
fig <- fig %>% layout(title = "",
                      xaxis = list(title = "Kilómetros cuadrados",range = c(0,450000)),
                      yaxis = list(title = ""))

fig

server$export(fig, "D:/work9seea/1_extension2/plots/1_extension_barcharts/ext_s6_barchart.png",
              scale = 1.5)

#################################################################################################
## Cambios netos
# s3 vs s4
allcounts$neg = (allcounts$CambioNeto_s3_s4 >= 0)*1
allcounts$neg_color = "#ff0000"
for (i in 1:nrow(allcounts)){
  negv = allcounts$neg[i]
  if (negv==1){
    allcounts$neg_color[i] = "#5bf513"
  }
}

allcounts$veg2 = factor(allcounts$veg2, levels = allcounts$veg2[order(allcounts$s4)])
allcounts$CambioNeto_s3_s4 = round(allcounts$CambioNeto_s3_s4)
fig <- plot_ly(allcounts, x = ~CambioNeto_s3_s4, y = ~veg2 , type = 'bar', orientation = "h",
               text = allcounts$CambioNeto_s3_s4, textposition = 'outside',
               marker = list(color = allcounts$neg_color,
                             line = list(color = "Black",
                                         width = 1)))
fig <- fig %>% layout(title = "",
                      xaxis = list(title = "Kilómetros cuadrados",range = c(-8000,16000)),
                      yaxis = list(title = ""))
fig

server$export(fig, "D:/work9seea/1_extension2/plots/2_cambioneto_barcharts/cambioneto_s3s4_barchart.png",
              scale = 1.5)

# s4 vs s5
allcounts$neg = (allcounts$CambioNeto_s4_s5 >= 0)*1
allcounts$neg_color = "#ff0000"
for (i in 1:nrow(allcounts)){
  negv = allcounts$neg[i]
  if (negv==1){
    allcounts$neg_color[i] = "#5bf513"
  }
}

allcounts$veg2 = factor(allcounts$veg2, levels = allcounts$veg2[order(allcounts$s5)])
allcounts$CambioNeto_s4_s5 = round(allcounts$CambioNeto_s4_s5)
fig <- plot_ly(allcounts, x = ~CambioNeto_s4_s5, y = ~veg2 , type = 'bar', orientation = "h",
               text = allcounts$CambioNeto_s4_s5, textposition = 'outside',
               marker = list(color = allcounts$neg_color,
                             line = list(color = "Black",
                                         width = 1)))
fig <- fig %>% layout(title = "",
                      xaxis = list(title = "Kilómetros cuadrados",range = c(-8000,8000)),
                      yaxis = list(title = ""))
fig

server$export(fig, "D:/work9seea/1_extension2/plots/2_cambioneto_barcharts/cambioneto_s4s5_barchart.png",
              scale = 1.5)

# s5 vs s6
allcounts$neg = (allcounts$CambioNeto_s5_s6 >= 0)*1
allcounts$neg_color = "#ff0000"
for (i in 1:nrow(allcounts)){
  negv = allcounts$neg[i]
  if (negv==1){
    allcounts$neg_color[i] = "#5bf513"
  }
}

allcounts$veg2 = factor(allcounts$veg2, levels = allcounts$veg2[order(allcounts$s6)])
allcounts$CambioNeto_s5_s6 = round(allcounts$CambioNeto_s5_s6)
fig <- plot_ly(allcounts, x = ~CambioNeto_s5_s6, y = ~veg2 , type = 'bar', orientation = "h",
               text = allcounts$CambioNeto_s5_s6, textposition = 'outside',
               marker = list(color = allcounts$neg_color,
                             line = list(color = "Black",
                                         width = 1)))
fig <- fig %>% layout(title = "",
                      xaxis = list(title = "Kilómetros cuadrados",range = c(-4000,4000)),
                      yaxis = list(title = ""))
fig

server$export(fig, "D:/work9seea/1_extension2/plots/2_cambioneto_barcharts/cambioneto_s5s6_barchart.png",
              scale = 1.5)

# s3 vs s6
allcounts$neg = (allcounts$CambioNeto_s3_s6 >= 0)*1
allcounts$neg_color = "#ff0000"
for (i in 1:nrow(allcounts)){
  negv = allcounts$neg[i]
  if (negv==1){
    allcounts$neg_color[i] = "#5bf513"
  }
}

allcounts$veg2 = factor(allcounts$veg2, levels = allcounts$veg2[order(allcounts$s6)])
allcounts$CambioNeto_s3_s6 = round(allcounts$CambioNeto_s3_s6)
fig <- plot_ly(allcounts, x = ~CambioNeto_s3_s6, y = ~veg2 , type = 'bar', orientation = "h",
               text = allcounts$CambioNeto_s3_s6, textposition = 'outside',
               marker = list(color = allcounts$neg_color,
                             line = list(color = "Black",
                                         width = 1)))
fig <- fig %>% layout(title = "",
                      xaxis = list(title = "Kilómetros cuadrados",range = c(-11000,21000)),
                      yaxis = list(title = ""))
fig

server$export(fig, "D:/work9seea/1_extension2/plots/2_cambioneto_barcharts/cambioneto_s3s6_barchart.png",
              scale = 1.5)

#################################################################################################
## Tasas de Cambio
# s3 vs s4
allcounts$neg = (allcounts$TasaCambio_s3_s4 >= 0)*1
allcounts$neg_color = "#ff0000"
for (i in 1:nrow(allcounts)){
  negv = allcounts$neg[i]
  if (negv==1){
    allcounts$neg_color[i] = "#5bf513"
  }
}

allcounts$veg2 = factor(allcounts$veg2, levels = allcounts$veg2[order(allcounts$s4)])
allcounts$TasaCambio_s3_s4 = round(allcounts$TasaCambio_s3_s4, digits = 2)
fig <- plot_ly(allcounts, x = ~TasaCambio_s3_s4, y = ~veg2 , type = 'bar', orientation = "h",
               text = allcounts$TasaCambio_s3_s4, textposition = 'outside',
               marker = list(color = allcounts$neg_color,
                             line = list(color = "Black",
                                         width = 1)))
fig <- fig %>% layout(title = "",
                      xaxis = list(title = "",range = c(-3,7)),
                      yaxis = list(title = ""))
fig

server$export(fig, "D:/work9seea/1_extension2/plots/3_tasacambio_barcharts/tasacambio_s3s4_barchart.png",
              scale = 1.5)

# s4 vs s5
allcounts$neg = (allcounts$TasaCambio_s4_s5 >= 0)*1
allcounts$neg_color = "#ff0000"
for (i in 1:nrow(allcounts)){
  negv = allcounts$neg[i]
  if (negv==1){
    allcounts$neg_color[i] = "#5bf513"
  }
}

allcounts$veg2 = factor(allcounts$veg2, levels = allcounts$veg2[order(allcounts$s5)])
allcounts$TasaCambio_s4_s5 = round(allcounts$TasaCambio_s4_s5, digits = 2)
fig <- plot_ly(allcounts, x = ~TasaCambio_s4_s5, y = ~veg2 , type = 'bar', orientation = "h",
               text = allcounts$TasaCambio_s4_s5, textposition = 'outside',
               marker = list(color = allcounts$neg_color,
                             line = list(color = "Black",
                                         width = 1)))
fig <- fig %>% layout(title = "",
                      xaxis = list(title = "",range = c(-5,17)),
                      yaxis = list(title = ""))
fig

server$export(fig, "D:/work9seea/1_extension2/plots/3_TasaCambio_barcharts/TasaCambio_s4s5_barchart.png",
              scale = 1.5)

# s5 vs s6
allcounts$neg = (allcounts$TasaCambio_s5_s6 >= 0)*1
allcounts$neg_color = "#ff0000"
for (i in 1:nrow(allcounts)){
  negv = allcounts$neg[i]
  if (negv==1){
    allcounts$neg_color[i] = "#5bf513"
  }
}

allcounts$veg2 = factor(allcounts$veg2, levels = allcounts$veg2[order(allcounts$s6)])
allcounts$TasaCambio_s5_s6 = round(allcounts$TasaCambio_s5_s6, digits = 2)
fig <- plot_ly(allcounts, x = ~TasaCambio_s5_s6, y = ~veg2 , type = 'bar', orientation = "h",
               text = allcounts$TasaCambio_s5_s6, textposition = 'outside',
               marker = list(color = allcounts$neg_color,
                             line = list(color = "Black",
                                         width = 1)))
fig <- fig %>% layout(title = "",
                      xaxis = list(title = "",range = c(-2,6)),
                      yaxis = list(title = ""))
fig

server$export(fig, "D:/work9seea/1_extension2/plots/3_TasaCambio_barcharts/TasaCambio_s5s6_barchart.png",
              scale = 1.5)

# s3 vs s6
allcounts$neg = (allcounts$TasaCambio_s3_s6 >= 0)*1
allcounts$neg_color = "#ff0000"
for (i in 1:nrow(allcounts)){
  negv = allcounts$neg[i]
  if (negv==1){
    allcounts$neg_color[i] = "#5bf513"
  }
}

allcounts$veg2 = factor(allcounts$veg2, levels = allcounts$veg2[order(allcounts$s6)])
allcounts$TasaCambio_s3_s6 = round(allcounts$TasaCambio_s3_s6, digits = 2)
fig <- plot_ly(allcounts, x = ~TasaCambio_s3_s6, y = ~veg2 , type = 'bar', orientation = "h",
               text = allcounts$TasaCambio_s3_s6, textposition = 'outside',
               marker = list(color = allcounts$neg_color,
                             line = list(color = "Black",
                                         width = 1)))
fig <- fig %>% layout(title = "",
                      xaxis = list(title = "",range = c(-3,9)),
                      yaxis = list(title = ""))
fig

server$export(fig, "D:/work9seea/1_extension2/plots/3_TasaCambio_barcharts/TasaCambio_s3s6_barchart.png",
              scale = 1.5)


server$close()

