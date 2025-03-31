#####################################
##                                 ##  
##  2. GRÁFICO DE BARRAS POR AÑO   ##
##                                 ##
#####################################

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

dfXanno_ingreso <- as.data.frame(table(df$anno_ingreso))

df_Xmes_y_anno_de_ingreso <- df %>%
  group_by(anno_ingreso, df$mes_ingreso) %>%
  summarise(count = n(), .groups = "drop")

###########
# Gráfico #
###########

p <- ggplot(dfXanno_ingreso, aes(x = factor(Var1), y = Freq, fill = factor(Var1))) +
  geom_bar(stat = "identity", color="black") + 
  geom_text(aes(label = Freq), vjust = -0.5, color = "black", size = 10, fontface = "bold") +  # Ajuste de etiquetas
  scale_fill_manual(values = c(
    "2019" = "#DDDDDD", 
    "2020" = "#B2B2B2",
    "2021" = "#969696", 
    "2022" = "#808080",
    "2023" = "#5F5F5F",
    "2024" = "#4D4D4D"
  ))+
  labs(x = NULL, y = NULL) +  
  theme(axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text( colour = "black", 
                                    size = 30, 
                                    face = "bold",),
        axis.title = element_blank(),
        legend.position = "none"  # Oculta la leyenda
  )+
  ylim(0, max(dfXanno_ingreso$Freq) * 1.2)

p

##############################
# Exportar el gráfico en png # 
##############################

ggsave(file.path("images/barras_cantidad_de_pedidos_por_anno_2019-2024.png"),
       plot = p, width= 16, height=9, dpi=400)

#######
# FIN # 
#######