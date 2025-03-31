####################################################
##                                                ##  
##  7. PASTELES POR AÑO Y TIPO DE REQUERIMIENTO   ##
##                                                ##
####################################################

########################
# Librerías utilizadas #
########################

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(paletteer)
library(ggthemes)

######################
# Cargar información #
######################

load("data/tabla_rai.RData")

#######################################################
# Cálculos y manejo de información previos al gráfico #
#######################################################

dfXtarea <- as.data.frame(table(df$TAREA))

df_counts <- df %>%
  group_by(TAREA, anno_ingreso) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = anno_ingreso, values_from = count, values_fill = 0)

# Transformar de formato ancho a largo
df_long <- df_counts %>%
  pivot_longer(cols = -TAREA, names_to = "anio", values_to = "counts") %>%
  group_by(anio) %>%
  mutate(porcentaje = (counts / sum(counts)) * 100,
         porcentaje_redondeado = round(porcentaje))

###########
# GRÁFICO #
###########

colores <- paletteer_d("ggthemes::excel_Grayscale")

p <- ggplot(data = df_long, aes(x = "", y = porcentaje, fill = reorder(TAREA, -porcentaje))) +
  geom_bar(stat = "identity", color = "black") +
  coord_polar("y") +
  geom_text(aes(x = 1.65, label = counts), position = position_stack(vjust = 0.5), size = 4.5) +
  labs(x = "", y = "") +
  scale_fill_manual(values=colores) +
  theme(panel.background = element_rect(fill = "white"),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom",
        legend.justification = "center",
        legend.box = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(colour = "#666666", size = 12),
        strip.text = element_text(size = 14, face = "bold"))+  # Aumenta el tamaño de los años+
  guides(fill = guide_legend(nrow = 3)) +  # Divide la leyenda en 2 filas
  facet_wrap(~anio, ncol = 2)

p # Esta línea dispara la visualización del gráfico

##############################
# Exportar el gráfico en png # 
##############################

ggsave(file.path("images/pastel_por_tipo_de_pedido_2019-2024.png"),
       plot = p, width= 9, height=16, dpi=400)

#######
# FIN # 
#######