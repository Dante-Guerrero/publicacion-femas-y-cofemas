###########################################
##                                       ##  
##  3. GRÁFICO DE BARRAS POR MES Y AÑO   ##
##                                       ##
###########################################

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

df_Xmes_y_anno_de_ingreso <- df %>%
  group_by(anno_ingreso, mes_ingreso_numero, mes_ingreso) %>%
  summarise(count = n(), .groups = "drop")

df_Xmes_y_anno_de_ingreso <- df_Xmes_y_anno_de_ingreso %>%
  mutate(
    anno_ingreso = factor(anno_ingreso, levels = rev(sort(unique(anno_ingreso))))
  )

#############
# Gráfico 1 #
#############

dodge_width <- 0.8  # Ajusta el ancho del desplazamiento

p <- ggplot(
  df_Xmes_y_anno_de_ingreso, 
  aes(x = reorder(mes_ingreso, mes_ingreso_numero), y = count, fill = as.factor(anno_ingreso))
  ) +
  geom_bar(
    stat = "identity", 
    position = position_dodge(width = dodge_width), 
    colour= "black") +
  labs(
    x = "",
    y = ""
  ) +
  geom_text(
    aes(x = mes_ingreso, y = count, label = count, group = as.factor(anno_ingreso)),
    position = position_dodge(width = dodge_width),
    vjust = 0.5, 
    hjust=-0.2, 
    angle = 90,
    size = 4.5, 
    family = "mono"
  )+
  scale_fill_manual(
    "Año", 
    values = c(
      "2019" = "#DDDDDD", 
      "2020" = "#B2B2B2",
      "2021" = "#969696", 
      "2022" = "#808080",
      "2023" = "#5F5F5F",
      "2024" = "#4D4D4D"
      )
    ) +
  theme(axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text( colour = "black", 
                                    size = 12, 
                                    face = "bold",),
        axis.title = element_blank(),
        legend.position="bottom",
        legend.direction="horizontal",
        legend.title = element_blank(),
        legend.text = element_text(colour="#666666", 
                                   size=16))+
  guides(fill = guide_legend(nrow = 1))
p

#############
# Gráfico 2 #
#############

dodge_width <- 0.8  # Ajusta el ancho del desplazamiento
bar_width <- 0.9

p <- ggplot(
  df_Xmes_y_anno_de_ingreso, 
  aes(
    x = reorder(mes_ingreso, -mes_ingreso_numero), 
    y = count, 
    fill = anno_ingreso)
) +
  geom_bar(
    stat = "identity", 
    position = position_dodge(width = dodge_width), 
    colour= "black",
    width = bar_width
    ) +
  labs(x = "", y = "") +
  geom_text(
    aes(x = mes_ingreso, 
        y = count, 
        label = count, 
        group = anno_ingreso),
    position = position_dodge(width = dodge_width),  # 🔹 Mismo valor en ambas capas
    vjust = 0.5,  # 🔹 Centra las etiquetas en cada barra
    hjust = -0.2, # 🔹 Desplaza las etiquetas fuera de la barra para mejor visibilidad
    angle = 0,  # 🔄 Texto en horizontal para mejor legibilidad
    size = 4
  ) +
  scale_fill_manual(
    "Año", 
    values = c(
      "2019" = "#DDDDDD", 
      "2020" = "#B2B2B2",
      "2021" = "#969696", 
      "2022" = "#808080",
      "2023" = "#5F5F5F",
      "2024" = "#4D4D4D"
    )
  ) +
  theme(
    axis.ticks.x = element_blank(),  # 🔹 Quita las marcas del eje X
    axis.text.x = element_blank(),  # ❌ Elimina etiquetas del eje X
    axis.text.y = element_text(colour = "black", size = 12, face = "bold"),  # ✅ Mantiene el eje Y visible
    axis.title = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(colour = "#666666", size = 16)
  ) +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE)) +
  coord_flip()+  # 🔄 Gira el gráfico a horizontal
  ylim(0, max(df_Xmes_y_anno_de_ingreso$count) * 1.2)
p

##############################
# Exportar el gráfico en png # 
##############################

ggsave(file.path("images/barras_cantidad_de_pedidos_pormes_y_anno_2019-2024.png"),
       plot = p, width= 8, height=12, dpi=400)

#######
# FIN # 
#######
