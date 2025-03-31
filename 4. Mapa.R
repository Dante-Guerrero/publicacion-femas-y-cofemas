######################################################################
##                                                                  ##  
##  4. GRÁFICO DEL MAPA - CANTIDAD DE REQUERIMIENTOS POR REGIÓN   ##
##                                                                  ##
######################################################################

########################
# Librerías utilizadas #
########################

library(sf)
library(purrr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(scales)
library(paletteer)
library(ggthemes)

######################
# Cargar información #
######################

load("data/mapas.RData")
load("data/tabla_rai.RData") 

#######################################################
# Cálculos y manejo de información previos al gráfico #
#######################################################

cantidadXregion <- as.data.frame(table(df$`UBICACION (DEPARTAMENTO)`)) %>% 
  arrange(desc(Freq)) %>% 
  rename(
    region = Var1, 
    cantidad = Freq) 

departamentos <- mapas$departamentos

mapa<- merge(cantidadXregion, departamentos,
             by.x="region",
             by.y="DEPARTAMENTO",
             all.x=T) 

if (!inherits(mapa, "sf")) {
  mapa <- st_as_sf(mapa)
}

#-- Etiquetas

mapa_con_centroide <- st_centroid(mapa)

mapa_con_centroide <- mapa_con_centroide %>% 
  mutate(
    lng = unlist(map(mapa_con_centroide$geometry, 1)),
    lat = unlist(map(mapa_con_centroide$geometry, 2))
  )

etiquetas <- data.frame(
  region = mapa_con_centroide$region, 
  longitud = mapa_con_centroide$lng, 
  latitud = mapa_con_centroide$lat,
  cantidad = mapa_con_centroide$cantidad
)

etiquetas <- etiquetas %>%
  mutate(label = paste0(region, "\n", cantidad)) %>%
  select(longitud, latitud, label)

############
# Gráfico  #
############

colores <- paletteer_c("ggthemes::Classic Gray", 30)

p<- ggplot(data = mapa, aes())+
  geom_sf(aes(fill = cantidad), color = "black", lwd = 0.3)+
  #scale_fill_gradient(low = "#D3D3D3", high = "#8B0000") +
  scale_fill_gradientn(colors = colores) +
  theme_void()+
  theme(
    panel.background = element_rect(fill = "white"),
    legend.position = c(0.1, 0.1),
    legend.background = element_rect(fill="white", color = "black"),
    legend.title = element_blank(),
    legend.margin = margin(20, 20, 20, 20),
    legend.text = element_text(size = 16),  # Aumenta tamaño de la leyenda
  )+
  geom_sf(data = mapas$lago_titicaca, color='black', fill=alpha("lightblue",0.4))+
  geom_text_repel(data = etiquetas, aes(x = longitud, y = latitud, label = label),
                  size = 8, fontface = "bold", color = "black",
                  bg.color = "white", # Fondo blanco
                  bg.r = 0.1, # Tamaño del fondo alrededor del texto
                  box.padding = 0.3, point.padding = 0.1,
                  min.segment.length = 0.1, force = 0.3, max.overlaps = 15)
p

##############################
# Exportar el gráfico en png # 
##############################

ggsave(file.path("images/mapa_cantidad_pedidos_por_region_2019-2024.png"),
       plot = p, width= 15, height=20, dpi=400)

#######
# FIN # 
#######