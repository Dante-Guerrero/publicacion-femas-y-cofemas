##################################################################
##                                                              ##  
##  8. BOXPLOT - TIEMPOS DE ATENCIÓN POR TIPO DE REQUERIMIENTO  ##
##                                                              ##
##################################################################

# https://r-graph-gallery.com/boxplot.html

########################
# Librerías utilizadas #
########################

library(readxl)
library(ggplot2)
library(dplyr)
library(viridis)

######################
# Cargar información #
######################

load("data/tabla_rai.RData")

#######################################################
# Cálculos y manejo de información previos al gráfico #
#######################################################

df_sin_extremos <- df %>% 
  filter(
    tiempo_respuesta < 70,
    tiempo_respuesta > 0
  )

###########
# GRÁFICO #
###########

p <- df_sin_extremos%>%
  ggplot( aes(x= TAREA, y=tiempo_respuesta, fill= TAREA)) +
  geom_boxplot() +
  xlab("")+
  scale_fill_manual(
    "TAREA", 
    values = c(
      "PEDIDOS DE INFORMACION" = "#DDDDDD",
      "DILIGENCIAS" = "#B2B2B2",
      "DELIMITACION DE COMPETENCIAS" = "#969696",
      "INFORME FUNDAMENTADO" = "#808080",
      "PRINCIPIO DE OPORTUNIDAD" = "#5F5F5F"
      )
    ) +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_text(colour = "black", size = 14),
        axis.text.y = element_text(colour = "black", size = 16),
        axis.title = element_blank(),
        plot.title = element_text( hjust = 0.5, vjust = 0.5),
        legend.position="none")
p 

##############################
# Exportar el gráfico en png # 
##############################

ggsave(file.path("images/boxplot_distribucion_de_tiempo_por_tipo_de_pedido_2019-2024.png"),
       plot = p, width= 16, height=9, dpi=400)

#######
# FIN # 
#######