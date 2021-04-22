
# load packages
library("rgdal")
library("raster")
library("leaflet")
library("leaflet.extras")
library("dplyr")
library("readxl")
library("ggplot2")
library("plotly")
library("tibble")
library("orca")
library("processx")


#Niveles de integridad ecosistémica:
  
#  * La integridad de los ecosistemas naturales es alta y cubre una extensión importante.
#  * Los usos de suelo antrópicos tienen integridades muy bajas. En la agricultura anual además la extensión es importante.

# Load inegi data
inegi = read.csv("D:/work9seea/8_create_final_condition_plots/final_extension.csv",header=TRUE)
inegi$veg2 = factor(inegi$veg2, levels = unique(inegi$veg2[order(inegi$s6)]))

# Load cuts data
cuts = read.csv("D:/work9seea/8_create_final_condition_plots/ie_cuts.csv", header=TRUE)
cuts$cuts = factor(cuts$cuts, levels = unique(cuts$cuts))

cuts = merge(cuts,inegi,by="veg2")
cuts$veg2 = factor(cuts$veg2, levels = unique(cuts$veg2[order(cuts$s6)]))
cuts$iecuts2018p = round(cuts$iecuts2018p,digits=3)

cuts = cuts[order(cuts$s6, decreasing = TRUE),]

stacked = plot_ly(data = cuts,
                  x = ~iecuts2018, 
                  y = ~veg2, 
                  color= ~cuts,
                  colors = 'RdYlGn',
                  type = 'bar') %>% 
  layout(title = "",
    yaxis = list(title = ''),
    xaxis = list(title = 'Kilómetros cuadrados', range = c(0,450000)),
    barmode = 'stack') 

server = orca_serve()
server$export(stacked, "D:/work9seea/8_create_final_condition_plots/plots/1_ielevels_extension_s6.png",
              scale = 1.5)

################################################

stacked = plot_ly(data = cuts,
                  x = ~iecuts2018p, 
                  y = ~veg2, 
                  color= ~cuts,
                  colors = 'RdYlGn',
                  type = 'bar') %>% 
  layout(title = "",
    yaxis = list(title = ''),
    xaxis = list(title = '%'),
    barmode = 'stack')

stacked

server$export(stacked, "D:/work9seea/8_create_final_condition_plots/plots/2_ielevels_proporcion_s6.png",
              scale = 1.5)

####################################################### 

#El área efectiva de algunos tipos de vegetación se ha reducido notablemente debido a la pérdida de integridad.

##################################################################################
dat = read_excel("D:/work9seea/2_condition/reports/SEEA_extension_y_capitalnatural_v3_km2.xlsx")

# S6
dat$inegi_nom = factor(dat$inegi_nom, levels = dat$inegi_nom[order(dat$capitalnatural2018)])
dat$capitalnatural2018 = round(dat$capitalnatural2018)
fig <- plot_ly(dat, x = ~capitalnatural2018, y = ~inegi_nom , type = 'bar', orientation = "h",
               text = dat$capitalnatural2018, textposition = 'outside',
               marker = list(color = dat$color.y,
                             line = list(color = 'black',
                                         width = 1)))
fig <- fig %>% layout(title = "",
                      xaxis = list(title = "Kilómetros cuadrados",range = c(0,450000)),
                      yaxis = list(title = ""))

fig

server$export(fig, "D:/work9seea/8_create_final_condition_plots/plots/3_areaefectiva_ie18PorS6_s6.png",
              scale = 1.5)


######################################################################################

# S6
dat$c2018 = round(dat$c2018,2)
dat$c2018 = dat$c2018*100
dat$inegi_nom = factor(dat$inegi_nom, levels = dat$inegi_nom[order(dat$c2018)])
fig <- plot_ly(dat, x = ~c2018, y = ~inegi_nom , type = 'bar', orientation = "h",
               text = paste0(round(dat$c2018,2),"%"), textposition = 'outside',
               marker = list(color = "green",
                             line = list(color = 'black',
                                         width = 1)))
fig <- fig %>% layout(title = "",
                      xaxis = list(title = "%",range = c(0,100)),
                      yaxis = list(title = ""))

fig

server$export(fig, "D:/work9seea/8_create_final_condition_plots/plots/4_capnat_ie18PorS6_s6.png",
              scale = 1.5)



########################## Bubblins plot

dat$extension2014 = inegi$s6

p <- plot_ly(dat,
             x = ~extension2014,
             y = ~c2018,
             type = 'scatter',
             mode = 'markers',  
             size = ~capitalnatural2018, 
             text = ~inegi_nom,
             
             #Choosing the range of the bubbles' sizes:
             sizes = c(20, 75), 
             marker = list(opacity = 1, sizemode = 'diameter',
                           color = ~color.y,
                           line = list(
                             color = "black"))) %>%
  layout(title = '',
         xaxis = list(title = "Extensión (Kilómetros cuadrados)", showgrid = TRUE),
         yaxis = list(title = "Capital Natural (%)", showgrid = TRUE),
         showlegend = FALSE)%>% 
  
        add_annotations(x = dat$extension2014,
                        y = dat$c2018,
                        text = inegi$veg2,
                        xref = "x",
                        yref = "y",
                        showarrow = FALSE,
                        align = "right",
                        xshift = 50)

p

server$export(p, "D:/work9seea/8_create_final_condition_plots/plots/5_EX_CN_AE_ie18PorS6_s6.png",
              scale = 1.5)

 


?export
