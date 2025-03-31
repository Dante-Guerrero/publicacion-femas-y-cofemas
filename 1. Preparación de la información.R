#####################################
##                                 ##  
##  1. PREPARANDO LA INFORMACIÓN   ##
##                                 ##
#####################################

########################
# Librerías utilizadas #
########################

library(readxl)
library(lubridate)
library(dplyr)
library(bizdays)

########################
# Importar información #
########################

# Importamos la información obtenida a través de acceso a la información pública
df <- read_excel("acceso_a_la_informacion_publica/bd_rai.xlsx", sheet = 1)
df <- as.data.frame(df)

# Importamos la tabla de feriados
feriados <- read_excel("data/feriados.xlsx", sheet = 1)
feriados <- as.data.frame(feriados)

################################### 
# Exclusión de datos del análisis #
###################################

# Elimina filas donde la tarea es "PROYECTO DE RESPUESTA"
df <- df %>%
  filter(TAREA != "PROYECTO DE RESPUESTA")  

# Elimina filas donde la columna REMITENTE se encuentra vacía
df <- df %>% 
  filter(REMITENTE != "")

########################################################
# Incorporación de variables para el análisis temporal #
########################################################

# Configuramos R para que los datos del sistema sean locales 
# Esto permite extraer información en español.
Sys.setlocale("LC_TIME", "es_ES.UTF-8")

# Agregamos una columna que extrae el año de la fecha de ingreso
df$anno_ingreso <- year(df$`FECHA DE INGRESO`) 
# Agregamos una columna que extrae el año de la fecha de ingreso
df$mes_ingreso_numero <- month(df$`FECHA DE INGRESO`) 
# Agregamos una columna que extrae el nombre del mes de la fecha de ingreso
df$mes_ingreso <- tolower(format(df$`FECHA DE INGRESO`, "%B"))

# Generamos un calendario con los días no laborables de Perú
feriados <- as.Date(feriados$FERIADOS)
bizdays::create.calendar(
  name= "dias_no_laborables_peru", 
  weekdays = c("saturday", "sunday"), 
  holidays = feriados)
bizdays::holidays("dias_no_laborables_peru")

df$tiempo_respuesta <- bizdays(
  df$`FECHA DE INGRESO`, 
  df$`FECHA DEL DOCUMENTO DE RESPUESTA`,
  "dias_no_laborables_peru"
  )+1

##########################################
# Simplificando las categorías de tareas # 
##########################################

# Estandarizamos la información para que sólo existan 5 categorías en la columna "Tareas"
# La categoría "Informe fundamentado" no necesita ser estandarizada

# Categoría "Pedidos de información"

df$TAREA <- replace(df$TAREA, df$TAREA == "ABSOLUCION DE CONSULTA A", "PEDIDOS DE INFORMACION")
df$TAREA <- replace(df$TAREA, df$TAREA == "ABSOLUCION DE CONSULTA B", "PEDIDOS DE INFORMACION")
df$TAREA <- replace(df$TAREA, df$TAREA == "ABSOLUCION DE CONSULTA", "PEDIDOS DE INFORMACION")
df$TAREA <- replace(df$TAREA, df$TAREA == "REMISION DE INFORMACION", "PEDIDOS DE INFORMACION")
df$TAREA <- replace(df$TAREA, df$TAREA == "REMISIÓN DE INFORMACIÓN", "PEDIDOS DE INFORMACION")
df$TAREA <- replace(df$TAREA, df$TAREA == "REMISION DE INFORMACION A", "PEDIDOS DE INFORMACION")
df$TAREA <- replace(df$TAREA, df$TAREA == "REMISION DE INFORMACION B", "PEDIDOS DE INFORMACION")
df$TAREA <- replace(df$TAREA, df$TAREA == "SOLICITUD DE INFORMACIÓN", "PEDIDOS DE INFORMACION")

# Categoría "Delimitación de competencias"

df$TAREA <- replace(df$TAREA, df$TAREA == "DELIMITACIÓN DE COMPETENCIA", "DELIMITACION DE COMPETENCIAS")
df$TAREA <- replace(df$TAREA, df$TAREA == "DELIMITACION DE COMPETENCIAS A", "DELIMITACION DE COMPETENCIAS")
df$TAREA <- replace(df$TAREA, df$TAREA == "DELIMITACION DE COMPETENCIAS B", "DELIMITACION DE COMPETENCIAS")
df$TAREA <- replace(df$TAREA, df$TAREA == "IDENTIFICACION DE AUTORIDAD AMBIENTAL COMPETENTE PARA ELABORAR EL IF", "DELIMITACION DE COMPETENCIAS")

# Categoría diligencias

df$TAREA <- replace(df$TAREA, df$TAREA == "SOLICITUD DE VERIFICACIÓN", "DILIGENCIAS")

# Categoría "Principio de oportunidad

df$TAREA <- replace(df$TAREA, df$TAREA == "PPO", "PRINCIPIO DE OPORTUNIDAD")

###################################
# Agrupando por tipo de remitente #
###################################

# Se añade la columna "tipo_remitente" para agrupar la información de la columna "REMITENTE"

df <- df %>%
  mutate(tipo_remitente = case_when(
    grepl("OEFA", REMITENTE, ignore.case = TRUE) ~ "OEFA",
    grepl("OE ESPINAR", REMITENTE, ignore.case = TRUE) ~ "OEFA",
    grepl("OE PICHANAKI", REMITENTE, ignore.case = TRUE) ~ "OEFA",
    grepl("UFAFEMA", REMITENTE, ignore.case = TRUE) ~ "OEFA",
    grepl("DSEM", REMITENTE, ignore.case = TRUE) ~ "OEFA",
    grepl("SEFA", REMITENTE, ignore.case = TRUE) ~ "OEFA",
    grepl("FEMA", REMITENTE, ignore.case = TRUE) ~ "FEMA",
    grepl("FPEMA", REMITENTE, ignore.case = TRUE) ~ "FEMA",
    grepl("FPCEMA", REMITENTE, ignore.case = TRUE) ~ "FEMA",
    grepl("MATERIA AMBIENTAL", REMITENTE, ignore.case = TRUE) ~ "FEMA",
    grepl("MINISTERIO", REMITENTE, ignore.case = TRUE) ~ "MINISTERIO",
    grepl("MINAM", REMITENTE, ignore.case = TRUE) ~ "MINISTERIO",
    grepl("MINEM", REMITENTE, ignore.case = TRUE) ~ "MINISTERIO",
    grepl("PRODUCE", REMITENTE, ignore.case = TRUE) ~ "MINISTERIO",
    grepl("MINAGRI", REMITENTE, ignore.case = TRUE) ~ "MINISTERIO",
    grepl("SENACE", REMITENTE, ignore.case = TRUE) ~ "OTRAS ENTIDADES",
    grepl("SERNANP", REMITENTE, ignore.case = TRUE) ~ "OTRAS ENTIDADES",
    grepl("OSINERGMIN", REMITENTE, ignore.case = TRUE) ~ "OTRAS ENTIDADES",
    grepl("ANA", REMITENTE, ignore.case = TRUE) ~ "OTRAS ENTIDADES",
    grepl("CONTRALORIA", REMITENTE, ignore.case = TRUE) ~ "OTRAS ENTIDADES",
    grepl("OCI", REMITENTE, ignore.case = TRUE) ~ "OTRAS ENTIDADES",
    grepl("DEFENSORIO", REMITENTE, ignore.case = TRUE) ~ "OTRAS ENTIDADES",
    grepl("COORDINACION FEMA", REMITENTE, ignore.case = TRUE) ~ "OTRAS ENTIDADES",
    grepl("GORE", REMITENTE, ignore.case = TRUE) ~ "GORE",
    grepl("MUNICIPALIDAD", REMITENTE, ignore.case = TRUE) ~ "MUNICIPALIDAD",
    grepl("PODER JUDICIAL", REMITENTE, ignore.case = TRUE) ~ "PODER JUDICIAL",
    grepl("CORTE", REMITENTE, ignore.case = TRUE) ~ "PODER JUDICIAL",
    grepl("DIVISIÓN", REMITENTE, ignore.case = TRUE) ~ "PNP",
    grepl("DEPIAC", REMITENTE, ignore.case = TRUE) ~ "PNP",
    grepl("DEPINCRI", REMITENTE, ignore.case = TRUE) ~ "PNP",
    grepl("PNP", REMITENTE, ignore.case = TRUE) ~ "PNP",
    grepl("PERSONA", REMITENTE, ignore.case = TRUE) ~ "PRIVADO",
    grepl("EMPRESA", REMITENTE, ignore.case = TRUE) ~ "PRIVADO",
    grepl("FISCALIA", REMITENTE, ignore.case = TRUE) ~ "OTRAS FISCALIAS",
    grepl("FISCALÍA", REMITENTE, ignore.case = TRUE) ~ "OTRAS FISCALIAS",
    grepl("FEPD", REMITENTE, ignore.case = TRUE) ~ "OTRAS FISCALIAS",
    grepl("FPPC", REMITENTE, ignore.case = TRUE) ~ "OTRAS FISCALIAS",
    grepl("FPPD", REMITENTE, ignore.case = TRUE) ~ "OTRAS FISCALIAS",
    grepl("FPEPD", REMITENTE, ignore.case = TRUE) ~ "OTRAS FISCALIAS",
    grepl("FPEDACPI", REMITENTE, ignore.case = TRUE) ~ "OTRAS FISCALIAS",
    grepl("FEPREV", REMITENTE, ignore.case = TRUE) ~ "OTRAS FISCALIAS",
    grepl("FISLAAPD", REMITENTE, ignore.case = TRUE) ~ "OTRAS FISCALIAS",
    grepl("OFICINA DESCONCENTRADA", REMITENTE, ignore.case = TRUE) ~ "OEFA",
    TRUE ~ ""  # Si no cumple ninguna condición, se asigna vacío
  ))

##############################################################
# Agrupando a las FEMA y otras fiscalías por Distrito Fiscal #
##############################################################

df <- df %>%
  mutate(distrito_fiscal = case_when(
    grepl("FPEMA PASCO", REMITENTE, ignore.case = TRUE) ~ "PASCO",
    grepl("FEMA CHICLAYO", REMITENTE, ignore.case = TRUE) ~ "LAMBAYEQUE",
    grepl("FEMA NAUTA", REMITENTE, ignore.case = TRUE) ~ "LORETO",
    grepl("FEMA LIMA NORTE", REMITENTE, ignore.case = TRUE) ~ "LIMA NORTE",
    grepl("FEMA CUSCO", REMITENTE, ignore.case = TRUE) ~ "CUSCO",
    grepl("FEMA LIMA NOROESTE", REMITENTE, ignore.case = TRUE) ~ "LIMA NOROESTE",
    grepl("FEMA AYACUCHO", REMITENTE, ignore.case = TRUE) ~ "AYACUCHO",
    grepl("FPEMA HUANUCO", REMITENTE, ignore.case = TRUE) ~ "HUÁNUCO",
    grepl("FEMA TACNA", REMITENTE, ignore.case = TRUE) ~ "TACNA",
    grepl("FEMA ICA", REMITENTE, ignore.case = TRUE) ~ "ICA",
    grepl("FPEMA APURIMAC", REMITENTE, ignore.case = TRUE) ~ "APURÍMAC",
    grepl("FEMA CHACHAPOYAS", REMITENTE, ignore.case = TRUE) ~ "AMAZONAS",
    grepl("FPEMA SANTA", REMITENTE, ignore.case = TRUE) ~ "SANTA",
    grepl("FEMA JUNIN", REMITENTE, ignore.case = TRUE) ~ "JUNÍN",
    grepl("FPEMA CAJAMARCA", REMITENTE, ignore.case = TRUE) ~ "CAJAMARCA",
    grepl("FEMA ANCASH", REMITENTE, ignore.case = TRUE) ~ "ÁNCASH",
    grepl("FEMA TUMBES", REMITENTE, ignore.case = TRUE) ~ "TUMBES",
    grepl("FPEMA AREQUIPA", REMITENTE, ignore.case = TRUE) ~ "AREQUIPA",
    grepl("FEMA LAMBAYEQUE", REMITENTE, ignore.case = TRUE) ~ "LAMBAYEQUE",
    grepl("FPEMA PUNO", REMITENTE, ignore.case = TRUE) ~ "PUNO",
    grepl("FEMA LIMA, LIMA ESTE Y LIMA SUR", REMITENTE, ignore.case = TRUE) ~ "LIMA CENTRO, ESTE Y SUR",
    grepl("FPEMA LEONCIO PRADO", REMITENTE, ignore.case = TRUE) ~ "HUÁNUCO",
    grepl("FPEMA CHANCHAMAYO", REMITENTE, ignore.case = TRUE) ~ "SELVA CENTRAL",
    grepl("FPEMA VENTANILLA", REMITENTE, ignore.case = TRUE) ~ "LIMA NOROESTE",
    grepl("FEMA IQUITOS", REMITENTE, ignore.case = TRUE) ~ "LORETO",
    grepl("FPEMA HUANCAVELICA", REMITENTE, ignore.case = TRUE) ~ "HUANCAVELICA",
    grepl("FEMA LORETO", REMITENTE, ignore.case = TRUE) ~ "LORETO",
    grepl("FPEMA LA LIBERTAD", REMITENTE, ignore.case = TRUE) ~ "LA LIBERTAD",
    grepl("FPEMA HUARAZ", REMITENTE, ignore.case = TRUE) ~ "ÁNCASH",
    grepl("FEMA LIMA", REMITENTE, ignore.case = TRUE) ~ "LIMA CENTRO, ESTE Y SUR",
    grepl("1° FPCEMA-UCAYALI", REMITENTE, ignore.case = TRUE) ~ "UCAYALI",
    grepl("FEMA BAGUA", REMITENTE, ignore.case = TRUE) ~ "AMAZONAS",
    grepl("FEMA MOYOBAMBA", REMITENTE, ignore.case = TRUE) ~ "SAN MARTÍN",
    grepl("FPEMA PUQUIO", REMITENTE, ignore.case = TRUE) ~ "AYACUCHO",
    grepl("2°FPEMA-UCAYALI", REMITENTE, ignore.case = TRUE) ~ "UCAYALI",
    grepl("FEMA MAYNAS", REMITENTE, ignore.case = TRUE) ~ "LORETO",
    grepl("FEMA CALLAO", REMITENTE, ignore.case = TRUE) ~ "CALLAO",
    grepl("FEMA PIURA", REMITENTE, ignore.case = TRUE) ~ "PIURA",
    grepl("FPEMA HUANCAYO", REMITENTE, ignore.case = TRUE) ~ "JUNÍN",
    grepl("FEMA SAN MARTIN", REMITENTE, ignore.case = TRUE) ~ "SAN MARTÍN",
    grepl("FEMA MADRE DE DIOS", REMITENTE, ignore.case = TRUE) ~ "MADRE DE DIOS",
    grepl("FEMA SAN FRANCISCO", REMITENTE, ignore.case = TRUE) ~ "AYACUCHO",
    grepl("FEMA LIMA CENTRO 1ER DESPACHO", REMITENTE, ignore.case = TRUE) ~ "LIMA CENTRO, ESTE Y SUR",
    grepl("2°FPEMA-PUNO", REMITENTE, ignore.case = TRUE) ~ "PUNO",
    grepl("1° FPEMA PUNO", REMITENTE, ignore.case = TRUE) ~ "PUNO",
    grepl("FPEMA ATALAYA", REMITENTE, ignore.case = TRUE) ~ "UCAYALI",
    grepl("FPEMA AYNA SAN FRANCISCO", REMITENTE, ignore.case = TRUE) ~ "AYACUCHO",
    grepl("FEMA ALTO AMAZONAS", REMITENTE, ignore.case = TRUE) ~ "SAN MARTÍN",
    grepl("FEMA BONGARA", REMITENTE, ignore.case = TRUE) ~ "AMAZONAS",
    grepl("FPEMA SELVA CENTRAL", REMITENTE, ignore.case = TRUE) ~ "SELVA CENTRAL",
    grepl("FPEMA BARRANCA", REMITENTE, ignore.case = TRUE) ~ "HUAURA",
    grepl("FPEMA JUANJUI", REMITENTE, ignore.case = TRUE) ~ "SAN MARTÍN",
    grepl("FEMA LIMA CENTRO Y LIMA SUR 1ER DESPACHO", REMITENTE, ignore.case = TRUE) ~ "LIMA CENTRO, ESTE Y SUR",
    grepl("FEMA LIMA Y LIMA SUR - 1ER DESPACHO", REMITENTE, ignore.case = TRUE) ~ "LIMA CENTRO, ESTE Y SUR",
    grepl("FEMA RODRIGUEZ DE MENDOZA", REMITENTE, ignore.case = TRUE) ~ "AMAZONAS",
    grepl("FEMA SULLANA", REMITENTE, ignore.case = TRUE) ~ "SULLANA",
    # A continuación algunas fiscalías que no es tan obvio que sean FEMAS:
    grepl("FEPREV ILO", REMITENTE, ignore.case = TRUE) ~ "MOQUEGUA",
    grepl("FPPD HUAURA", REMITENTE, ignore.case = TRUE) ~ "HUAURA",
    grepl("FISCALIA DE PREVENCION DELDELITO Y MATERIA AMBIENTAL - CAÑETE", REMITENTE, ignore.case = TRUE) ~ "CAÑETE",
    grepl("FISCALIA DE PREVENCION DEL DELITO Y MATERIA AMBIENTAL - CAÑETE", REMITENTE, ignore.case = TRUE) ~ "CAÑETE",
    grepl("FISCALIA DE PREVENCION DEL DELITO-MOQUEGUA", REMITENTE, ignore.case = TRUE) ~ "MOQUEGUA",
    grepl("FISCALIA DE PREVENCION DEL DELITO-ICA", REMITENTE, ignore.case = TRUE) ~ "ICA",
    grepl("FISCALIA PROVINCIAL DE MARISCAL NIETO", REMITENTE, ignore.case = TRUE) ~ "MOQUEGUA",
    grepl("FISCALIA DE PREVENCION DEL DELITO- HUARAL", REMITENTE, ignore.case = TRUE) ~ "HUAURA",
    grepl("FPEPD TUMBES", REMITENTE, ignore.case = TRUE) ~ "TUMBES",
    grepl("FISCALIA PROVINCIAL PENAL CORPORATIVA MADRE DE DIOS", REMITENTE, ignore.case = TRUE) ~ "MADRE DE DIOS",
    grepl("2°FEPD CHICLAYO", REMITENTE, ignore.case = TRUE) ~ "LAMBAYEQUE",
    grepl("FISCALÍA PROVINCIAL CORPORATIVA ESPECIALIZADA EN DELITOS DE LAVADO DE ACTIVOS - PUNO", REMITENTE, ignore.case = TRUE) ~ "PUNO",
    grepl("FPPD ABANCAY", REMITENTE, ignore.case = TRUE) ~ "APURÍMAC",
    grepl("SEGUNDA FISCALIA PROVINCIAL PENAL CORPORATIVA DE LA MOLINA", REMITENTE, ignore.case = TRUE) ~ "LIMA CENTRO, ESTE Y SUR",
    grepl("1° FISCALÍA PROVINCIAL ESPECIALIZADA EN DELITOS DE LAVADO DE ACTIVOS DE LIMA", REMITENTE, ignore.case = TRUE) ~ "LIMA CENTRO, ESTE Y SUR",
    grepl("FISCALIA EN DELITOS DE LAVADO DE ACTIVOS - AREQUIPA", REMITENTE, ignore.case = TRUE) ~ "AREQUIPA",
    grepl("FISCALIA PROVINCIAL TRANSITORIA DE EXTINCION DE DOMINIO APURIMAC", REMITENTE, ignore.case = TRUE) ~ "APURÍMAC",
    grepl("1° FISCALIA PROVINCIAL ESPECIALIZADA EN DELITOS DE CORRUPCIÓN DE LIMA", REMITENTE, ignore.case = TRUE) ~ "LIMA CENTRO, ESTE Y SUR",
    grepl("3° FISCALIA PROVINCIAL PENAL CORPORATIVA DE SAN JUAN DE LURIGANCHO - 4° DESPACHO", REMITENTE, ignore.case = TRUE) ~ "LIMA CENTRO, ESTE Y SUR",
    grepl("3° FPPC - SANTA ANITA", REMITENTE, ignore.case = TRUE) ~ "LIMA CENTRO, ESTE Y SUR",
    grepl("1° FISCALIA PENAL CORPORATIVA DE LA MOLINA - TERCER DESPACHO", REMITENTE, ignore.case = TRUE) ~ "LIMA CENTRO, ESTE Y SUR",
    grepl("FISCALIA DE PREVENCION DEL DELITO- HUARA", REMITENTE, ignore.case = TRUE) ~ "HUAURA",
    grepl("FISCALIA EN DELITOS DE CORRUPCION-ICA", REMITENTE, ignore.case = TRUE) ~ "ICA",
    grepl("FISCALIA PROVINCIAL TRANSITORIA ESPECIALIZADA EN MATERIA AMBIENTAL DE LA LIBERTAD", REMITENTE, ignore.case = TRUE) ~ "LA LIBERTAD",
    grepl("FPPC TACNA", REMITENTE, ignore.case = TRUE) ~ "TACNA",
    grepl("FISCALIA CORPORATIVA DE LA CONVENCION", REMITENTE, ignore.case = TRUE) ~ "LIMA CENTRO, ESTE Y SUR",
    grepl("FISCALÍA PROVINCIAL ESPECIALIZADA EN MATERIA AMBIENTAL DISTRITO FISCAL DE LA SELVA CENTRAL", REMITENTE, ignore.case = TRUE) ~ "SELVA CENTRAL",
    grepl("FISCALIA PROVINCIAL PENAL CORPORATICA DEL CALLAO", REMITENTE, ignore.case = TRUE) ~ "CALLAO",
    grepl("FISCALIA TRANSITORIA DE EXTINCIÓN DE DOMINIO DE LAMBAYEQUE", REMITENTE, ignore.case = TRUE) ~ "LAMBAYEQUE",
    grepl("1° FISCALÍA PROVINCIAL PENAL CORPORATIVA DE SAN JUAN DE LURIGANCHO – 2° DESPACHO", REMITENTE, ignore.case = TRUE) ~ "LIMA CENTRO, ESTE Y SUR",
    grepl("2° FISCALIA PROVINCIAL TRANSITORIA DE EXTINCION DE DOMINIO DE LIMA", REMITENTE, ignore.case = TRUE) ~ "LIMA CENTRO, ESTE Y SUR",
    grepl("5° FPPC -SANTA ANITA", REMITENTE, ignore.case = TRUE) ~ "LIMA CENTRO, ESTE Y SUR",
    grepl("FISCALIA PROVINCIAL ESPECIALIZADA EN PREVENCION DEL DELITO DE HUANTA", REMITENTE, ignore.case = TRUE) ~ "AYACUCHO",
    grepl("FPEDACPI TUMBES", REMITENTE, ignore.case = TRUE) ~ "TUMBES",
    grepl("2° FISCALIA PROVINCIAL PENAL CORPORATIVA DE SAN JUAN DE LURIGANCHO - 3° DESPACHO - ZONA MEDIA", REMITENTE, ignore.case = TRUE) ~ "LIMA CENTRO, ESTE Y SUR",
    grepl("3°FISCALIA PROVINCIAL CORPORATIVA-AREQUIPA", REMITENTE, ignore.case = TRUE) ~ "AREQUIPA",
    grepl("4° FPPC -SANTA ANITA", REMITENTE, ignore.case = TRUE) ~ "LIMA CENTRO, ESTE Y SUR",
    grepl("FISCALIA DE PREVENCION DEL DELITO-CUSCO", REMITENTE, ignore.case = TRUE) ~ "CUSCO",
    grepl("FISCALIA EN DELITOS DE TRAFICO ILICITO DE DROGAS-PUCALLPA", REMITENTE, ignore.case = TRUE) ~ "UCAYALI",
    grepl("FISCALIA PROVINCIAL MIXTA CORPORATIVA DE TARATA", REMITENTE, ignore.case = TRUE) ~ "TACNA",
    grepl("FISCALIA PROVINCIAL MIXTA DE CANDARAVE", REMITENTE, ignore.case = TRUE) ~ "TACNA",
    grepl("FISCALIA PROVINCIAL MIXTA DE CHALLHUAHUACHO", REMITENTE, ignore.case = TRUE) ~ "APURÍMAC",
    grepl("FISCALIA PROVINCIAL PENAL CORPORATIVA DATEM DEL MARAÑON", REMITENTE, ignore.case = TRUE) ~ "LORETO",
    grepl("FISCALIA PROVINCIAL PENAL DE LIMA", REMITENTE, ignore.case = TRUE) ~ "LIMA CENTRO, ESTE Y SUR",
    grepl("FISCALÍA PROVINCIAL TRANSITORIA DE EXTINCIÓN DE DOMINIO DEL SANTA", REMITENTE, ignore.case = TRUE) ~ "SANTA",
    grepl("FPPC HUARAZ", REMITENTE, ignore.case = TRUE) ~ "ÁNCASH",
    grepl("FPPD TACNA", REMITENTE, ignore.case = TRUE) ~ "TACNA",
    grepl("PRIMERA FISCALIA PROVINCIAL DE PREVENCION DEL DELITO CAJAMARCA", REMITENTE, ignore.case = TRUE) ~ "CAJAMARCA",
    grepl("PRIMERA FISCALIA PROVINCIAL PENAL CORPORATIVA DE PASCO", REMITENTE, ignore.case = TRUE) ~ "PASCO",
    grepl("PRIMERA FISCALIA PROVINCIAL PENAL CORPORATIVA SAN JUAN DE LURIGANCHO-ZONA MEDIA-2° DESPACHO", REMITENTE, ignore.case = TRUE) ~ "LIMA CENTRO, ESTE Y SUR",
    grepl("1° FISCALIA PROVINCIAL CIVIL DE LIMA", REMITENTE, ignore.case = TRUE) ~ "LIMA CENTRO, ESTE Y SUR",
    grepl("1° FISCALIA PROVINCIAL PENAL CORPORATIVA DE CHOSICA – 1° DESPACHO", REMITENTE, ignore.case = TRUE) ~ "LIMA CENTRO, ESTE Y SUR",
    grepl("2° FISCALÍA PENAL CORPORATIVA DE SAN JUAN DE LURIGANCHO ZONA BAJA", REMITENTE, ignore.case = TRUE) ~ "LIMA CENTRO, ESTE Y SUR",
    grepl("2° FISCALIA PROVINCIAL DE AMBO", REMITENTE, ignore.case = TRUE) ~ "HUÁNUCO",
    grepl("2°FISCALIA PROVINCIAL PENAL RIOJA", REMITENTE, ignore.case = TRUE) ~ "SAN MARTÍN",
    grepl("26° FISCALIA PENAL DE LIMA", REMITENTE, ignore.case = TRUE) ~ "LIMA CENTRO, ESTE Y SUR",
    grepl("5° FISCALIA PROVINCIAL CIVIL DE LIMA", REMITENTE, ignore.case = TRUE) ~ "LIMA CENTRO, ESTE Y SUR",
    grepl("FISCALIA DE PREVENCION DEL DELITO CONTRA LA ECOLOGIA Y PREVENCION DEL DELITO DE TAMBOPATA", REMITENTE, ignore.case = TRUE) ~ "MADRE DE DIOS",
    grepl("FISCALIA PROVINCIAL ESPECIALIZADA TRANSITORIA DE EXTINCION DE DOMINIO DE PUNO", REMITENTE, ignore.case = TRUE) ~ "PUNO",
    grepl("FISCALIA PROVINCIAL MIXTA CORPORATIVA DE EL PORVENIR", REMITENTE, ignore.case = TRUE) ~ "LA LIBERTAD",
    grepl("FISCALIA PROVINCIAL MIXTA DE LA BANDA DE SHILCAYO", REMITENTE, ignore.case = TRUE) ~ "SAN MARTÍN",
    grepl("FISCALIA PROVINCIAL MIXTA YONAN - TEMBLADERA", REMITENTE, ignore.case = TRUE) ~ "CAJAMARCA",
    grepl("FISCALIA PROVINCIAL PENAL CORPORATIVA DE ISLAY", REMITENTE, ignore.case = TRUE) ~ "AREQUIPA",
    grepl("FISCALIA PROVINCIAL PENAL CORPORATIVA DE SECHURA", REMITENTE, ignore.case = TRUE) ~ "PIURA",
    grepl("FISCALIA PROVINCIAL PENAL CORPORATIVA ESPECIALIZADA EN DELITOS ADUANEROS Y CONTRA LA PROPIEDAD INTELECTUAL DEL CALLAO - 2° DESPACHO", REMITENTE, ignore.case = TRUE) ~ "CALLAO",
    grepl("FISCALIA PROVINCIAL PENAL CORPORATIVA VILLA MARIA DEL TRIUNFO DISTRITO FISCAL DE LIMA SUR - 1° DESPACHO", REMITENTE, ignore.case = TRUE) ~ "LIMA CENTRO, ESTE Y SUR",
    grepl("FISCALIA PROVINCIAL PENAL DE CHUMBIVILCAS", REMITENTE, ignore.case = TRUE) ~ "CUSCO",
    grepl("FISCALIA PROVINCIAL PENAL DE RECUAY", REMITENTE, ignore.case = TRUE) ~ "ÁNCASH",
    grepl("FISCALIA PROVINCILA MIXTA CORPORATIVA DE SANCHEZ CARRION", REMITENTE, ignore.case = TRUE) ~ "LA LIBERTAD",
    grepl("FPPC HUAYLAS", REMITENTE, ignore.case = TRUE) ~ "ÁNCASH",
    grepl("FPPC LURIN", REMITENTE, ignore.case = TRUE) ~ "LIMA CENTRO, ESTE Y SUR",
    grepl("SEGUNDA FISCALIA PROVICNIAL PENAL PUENTE PIEDRA", REMITENTE, ignore.case = TRUE) ~ "LIMA CENTRO, ESTE Y SUR",
    grepl("SEGUNDA FISCALIA PROVINCIAL PENAL CORPORATIVA DE YUNGAY", REMITENTE, ignore.case = TRUE) ~ "ÁNCASH",
    # Revisar (no queda claro a qué distrito fiscal pertenecen):
    grepl("FISCALIA SUPRAPROVINCIAL DE LAVADOS DE ACTIVOS", REMITENTE, ignore.case = TRUE) ~ "LIMA CENTRO, ESTE Y SUR",
    grepl("FISCALIA ESPECIALIZADA CONTRA LA CRIMINALIDAD ORGANIZADA", REMITENTE, ignore.case = TRUE) ~ "LIMA CENTRO, ESTE Y SUR",
    grepl("FISCALIA PROVINCIAL CORPORATIVA ESPECIALIZADA EN DELITOS DE LAVADO DE ACTIVOS", REMITENTE, ignore.case = TRUE) ~ "LIMA CENTRO, ESTE Y SUR",
    grepl("FISCALIA CORDINADORA ESPECIALIZADA EN CONTRA LA CRIMINALIDAD", REMITENTE, ignore.case = TRUE) ~ "LIMA CENTRO, ESTE Y SUR",
    grepl("2FISLAAPD", REMITENTE, ignore.case = TRUE) ~ "LIMA CENTRO, ESTE Y SUR",
    grepl("4ta FISCALIA SUPRAPROVINCIAL CORPORATIVA ESPECIALIZADA CONTRA LA CRIMINALIDAD ORGANIZADA - 3er EQUIPO", REMITENTE, ignore.case = TRUE) ~ "LIMA CENTRO, ESTE Y SUR",
    grepl("COORDINACION FEMA", REMITENTE, ignore.case = TRUE) ~ "LIMA CENTRO, ESTE Y SUR",
    TRUE ~ ""  # Si no cumple ninguna condición, se asigna vacío
  ))

############################################################
# Exportar la base de datos resultante a un archivo .RData #
############################################################

save(df, file='data/tabla_rai.RData')






