####################################################################
##                                                                ##  
##  6. GRÁFICO DE BARRAS POLARES - CANTIDAD POR DISTRITO FISCAL   ##
##                                                                ##
####################################################################

########################
# Librerías utilizadas #
########################

library(readxl)
library(ggplot2)
library(dplyr)

######################
# Cargar información #
######################

load("data/tabla_rai.RData")

#######################################################
# Cálculos y manejo de información previos al gráfico #
#######################################################

#-- Parte 1: Generar una tabla donde se muestran las cantidades de pedidos por cada distrito fiscal

cantidadXdistrito_fiscal <- as.data.frame(table(df$distrito_fiscal)) %>% 
  arrange(desc(Freq)) %>% 
  filter(
    Var1 != "NO APLICA",
    Var1 != ""
  ) %>%
  rename(
    distrito_fiscal = Var1, 
    cantidad = Freq) 

cantidadXdistrito_fiscal <- cantidadXdistrito_fiscal %>% 
  mutate(id = seq(1,nrow(cantidadXdistrito_fiscal))) 

#-- Parte 2: Incorporar variables que serán útiles para el gráfico

# Generamos etiquetas para los números que irán sobre las barras
labeled_df <- cantidadXdistrito_fiscal %>%
  mutate(label= paste0(cantidad))

# Calculamos los ángulos de las etiquetas, uno para los números y otros para el texto
number_of_bar <- nrow(labeled_df)
angle <-  90 - 360 * (labeled_df$id) /number_of_bar
angle2 <-  90 - 360 * (labeled_df$id-0.5)  /number_of_bar 

# Calcular la alineación de las etiquetas: izquierda o derecha
# En la parte izquierda del gráfico, las etiquetas tienen un ángulo < -90
labeled_df$hjust <- ifelse(angle < -90, 1, 0)
labeled_df$hjust2 <- ifelse(angle2 < -90, 1, 0)

# Volvear los ángulos para mejorar la legibilidad
labeled_df$angle <- ifelse(angle < -90, angle+180, angle)
labeled_df$angle2 <- ifelse(angle2 < -90, angle2+180, angle2)

# Convertir id en factor
cantidadXdistrito_fiscal$id <- factor(cantidadXdistrito_fiscal$id, levels = cantidadXdistrito_fiscal$id)

#-- Parte 3: Obtener información adicional para el texto en el centro del gráfico

total_pedidos <- nrow(df)

# Asignamos una variable a la cantidad de distritos fiscales, para que sea modificable en el futuro
# Sería ideal calcularlo contando número de filas de cantidadXdistrito_fiscal; sin embargo hay 2 inconvenientes:
# 1) Por un lado, no se reportaron requerimientos del distrito fiscal de Sullana
# 2) Por otro, no fue posible distinguir los pedidos de Lima Centro, Lima Sur y Lima Este, por lo que se juntaron.
cantidad_distritos_fiscales <- 33 

# Asignamos una variable a la cantidad total de pedidos remitidos por fiscalías a UFAFEMA
total_pedidos_fiscalias <- sum(cantidadXdistrito_fiscal$cantidad)

###########
# GRÁFICO #
###########

# La primera parte sólo dibujará las barras
p <- ggplot(cantidadXdistrito_fiscal, aes(x=id, y=cantidad)) +  # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat="identity", fill= "black") +   # This add the bars with a black color
  ylim(-max(cantidadXdistrito_fiscal$cantidad) * 0.6, max(cantidadXdistrito_fiscal$cantidad) * 1.1) +  # Limits of the plot, the negative value controls the size of the inner circle, the positive one is useful to add size over each bar
  theme_minimal() + # Custom the theme: no axis title and no cartesian grid
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")      # Adjust the margin to make in sort labels are not truncated!
  ) +
  coord_polar(start = 0)+ # Utilizar las coordenadas polares en lugar de las cartesianas
  # Agregamos las etiquetas de los números
  geom_text(data=labeled_df, 
            aes(x=id, 
                y=cantidad+30, 
                label= label, 
                hjust=ifelse(labeled_df$angle<-90, hjust+0.5, hjust-0.5)), 
            color="black", 
            fontface="bold",
            #alpha=0.6, 
            size=4.5, 
            angle= ifelse(labeled_df$angle<0, labeled_df$angle+90,labeled_df$angle -90),
            inherit.aes = FALSE, 
            check_overlap = FALSE )+
  theme(
    panel.background = element_rect(fill = "white"),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.title = element_blank()
  )+
  # Agregamos las etiquetas de texto
  geom_text(data=labeled_df, 
            aes(x=id, 
                y=cantidad + 70, 
                label= distrito_fiscal, 
                hjust=hjust2), 
            color="black", 
            fontface="bold",
            #alpha=0.6, 
            size=4, 
            angle= labeled_df$angle2,
            inherit.aes = FALSE, 
            check_overlap = FALSE ) +
  geom_text(
    x = 0, 
    y = -350,
    label = paste0(
      total_pedidos_fiscalias
    ),
    size = 16,
    lineheight = 0.87,
    color = "black"
  )+
  geom_text(
    x = 0, 
    y = -580,
    label = paste0(
      "REQUERIMIENTOS \n",
      "DE FISCALÍAS"
    ),
    size = 7,
    lineheight = 0.87,
    color = "black"
  )

p # Esta línea dispara la visualización del gráfico

##############################
# Exportar el gráfico en png # 
##############################

ggsave(file.path("images/requerimientos_por_distrito_fiscal.png"),
       plot = p, width= 10, height=10)

#######
# FIN # 
#######