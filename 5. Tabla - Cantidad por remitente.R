##################################################
##                                              ##  
##  5. TABLA - CANTIDAD POR TIPO DE REMITENTE   ##
##                                              ##
##################################################

########################
# Librerías utilizadas #
########################

library(readxl)
library(dplyr)
library(ggplot2)

######################
# Cargar información #
######################

load("data/tabla_rai.RData")

#######################################################
# Cálculos y manejo de información previos al gráfico #
#######################################################

cantidadXtipo_de_remitente <- as.data.frame(table(df$tipo_remitente)) %>% 
  mutate(tipo_remitente_2 = case_when(
    grepl("FEMA", Var1, ignore.case = TRUE) ~ "FISCALÍAS",
    grepl("OTRAS FISCALIAS", Var1, ignore.case = TRUE) ~ "FISCALÍAS",
    grepl("OEFA", Var1, ignore.case = TRUE) ~ "OTRAS ENTIDADES",
    grepl("PRIVADO", Var1, ignore.case = TRUE) ~ "PRIVADOS",
    grepl("GORE", Var1, ignore.case = TRUE) ~ "GORES",
    grepl("MINISTERIO", Var1, ignore.case = TRUE) ~ "MINISTERIOS",
    grepl("MUNICIPALIDAD", Var1, ignore.case = TRUE) ~ "MUNICIPALIDADES",
    TRUE ~ Var1 
  )) %>% 
  select(
    "tipo_remitente" = "tipo_remitente_2",
    "cantidad" = "Freq"
  ) %>% 
  group_by(tipo_remitente) %>% 
  summarise(sum(cantidad))%>% 
  select(
    "tipo_remitente",
    "cantidad" = "sum(cantidad)"
  ) %>% 
  arrange(desc(cantidad))

# Compute percentages
cantidadXtipo_de_remitente$fraction <- cantidadXtipo_de_remitente$cantidad / sum(cantidadXtipo_de_remitente$cantidad)

# Compute the cumulative percentages (top of each rectangle)
cantidadXtipo_de_remitente$ymax = cumsum(cantidadXtipo_de_remitente$fraction)

# Compute the bottom of each rectangle
cantidadXtipo_de_remitente$ymin = c(0, head(cantidadXtipo_de_remitente$ymax, n=-1))

####################
# OPCIÓN 1 GRÁFICO #
####################

# Make the plot
ggplot(cantidadXtipo_de_remitente, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=tipo_remitente)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) + # Try to remove that to see how to make a pie chart
  theme_void()

##################
# OPCIÓN 2 TABLA #
##################

# Pendiente de incorporar



