library(tidyverse)
library(openxlsx)
library(readr)
library(tidyr)
library(readxl)
library(rvest)

# Directorio
setwd("./Futbol/Champions League")

######################################
## Introduce el enlace de la página ##
######################################

# Url
url <- "https://fbref.com/en/matches/2505e711/Juventus-Benfica-January-29-2025-Champions-League"


######################################################################
## A continuación, el resto del proceso de hace de forma automática ##
######################################################################

####################
## Tablas Equipos ##
####################
# Cargar página
pagina <- read_html(url)

# Otros datos del partido
datos_partido <- pagina %>% 
  html_nodes(".scorebox_meta") %>%
  html_text()

## Torneo ##
torneo <- sub(".*venue time\\)\\s*(UEFA Champions League).*", "\\1", datos_partido)
## Ronda ##
ronda <- sub(".*UEFA Champions League\\s*(\\(.*?\\)).*", "\\1", datos_partido)
ronda <- gsub("[()]", "", ronda)
ronda <- case_when(ronda == "Quarter-finals" ~ "Cuartos de Final",
                   ronda == "Semi-finals" ~ "Semifinales",
                   ronda == "Round of 16" ~ "Octavos de Final",
                   ronda == "Knockout phase play-offs" ~ "Play-offs",
                   ronda == "League phase" ~ "Jornada",
                   ronda == "Play-off round" ~ "Ronda de Play-off",
                   ronda == "Third qualifying round" ~ "Tercera Ronda",
                   ronda == "Second qualifying round" ~ "Segunda Ronda",
                   ronda == "First qualifying round" ~ "Primera Ronda")
## Fase ##
fase <- case_when(ronda == "Cuartos de Final" ~ "Eliminacion Directa",
                  ronda == "Semifinales" ~ "Eliminacion Directa",
                  ronda == "Octavos de final" ~ "Eliminacion Directa",
                  ronda == "Play-offs" ~ "Eliminacion Directa",
                  ronda == "Jornada" ~ "Fase de Liga",
                  ronda == "Ronda de Play-off" ~ "Play-off",
                  ronda == "Tercera Ronda" ~ "Clasificacion",
                  ronda == "Segunda Ronda" ~ "Clasificacion",
                  ronda == "Primera Ronda" ~ "Clasificacion")

## Fase del partido ##
fase_partido <- sub(".*\\),\\s*(Leg \\d+ of \\d+).*", "\\1", datos_partido)
# En caso de que sean partidos de liga
if (nchar(fase_partido) > 50) {
  fase_partido <- case_when(fase == "Fase de Liga" ~ "Unico")
} else {
  fase_partido <- case_when(fase_partido == "Leg 1 of 2" ~ "Ida",
                            fase_partido == "Leg 2 of 2" ~ "Vuelta")
}

## Fecha y Hora ##
fecha_hora <- sub("^\\s*([A-Za-z]+\\s+[A-Za-z]+\\s+\\d{1,2},\\s+\\d{4},\\s+\\d{2}:\\d{2}\\s*\\(venue time\\)).*", "\\1", datos_partido)
partes_fecha <- strsplit(fecha_hora, ", ")[[1]]
fecha_texto <- paste(partes_fecha[1], partes_fecha[2])
fecha_sin_dia <- sub("^[A-Za-z]+\\s+", "", fecha_texto)
old_locale <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "C")
fecha_formato <- as.Date(fecha_sin_dia, format = "%B %d %Y")
fecha_formateada <- format(fecha_formato, "%d/%m/%Y")
# Fecha
fecha = fecha_formateada
# Hora
hora <- partes_fecha[3]
hora <- sub("\\s*\\(.*\\)", "", hora)
## Temporada ##
temporada <- case_when(fecha_formato >= as.Date("09/07/2024", format = "%d/%m/%Y") & 
                         fecha_formato <= as.Date("31/05/2025", format = "%d/%m/%Y") ~ "2024-2025")
##
if (fase == "Fase de Liga" & temporada == "2024-2025"){
  ronda = case_when(fecha_formato >= as.Date("17/09/2024", format = "%d/%m/%Y") & fecha_formato <= as.Date("19/09/2024", format = "%d/%m/%Y") ~ "Jornada 1",
                    fecha_formato >= as.Date("01/10/2024", format = "%d/%m/%Y") & fecha_formato <= as.Date("02/10/2024", format = "%d/%m/%Y") ~ "Jornada 2",
                    fecha_formato >= as.Date("22/10/2024", format = "%d/%m/%Y") & fecha_formato <= as.Date("23/10/2024", format = "%d/%m/%Y") ~ "Jornada 3",
                    fecha_formato >= as.Date("05/11/2024", format = "%d/%m/%Y") & fecha_formato <= as.Date("06/11/2024", format = "%d/%m/%Y") ~ "Jornada 4",
                    fecha_formato >= as.Date("26/11/2024", format = "%d/%m/%Y") & fecha_formato <= as.Date("27/11/2024", format = "%d/%m/%Y") ~ "Jornada 5",
                    fecha_formato >= as.Date("10/12/2024", format = "%d/%m/%Y") & fecha_formato <= as.Date("11/12/2024", format = "%d/%m/%Y") ~ "Jornada 6",
                    fecha_formato >= as.Date("21/01/2025", format = "%d/%m/%Y") & fecha_formato <= as.Date("22/01/2025", format = "%d/%m/%Y") ~ "Jornada 7",
                    fecha_formato == as.Date("29/01/2025", format = "%d/%m/%Y") ~ "Jornada 8")
}

## Localizacion ##
venue <- sub(".*Venue:\\s*(.*?)(Officials:|$).*", "\\1", datos_partido)
venue <- trimws(venue)
venue <- strsplit(venue, ",\\s*")[[1]]
# Asistencia
asistencia <- sub(".*Attendance:\\s*([0-9,]+).*", "\\1", datos_partido)
asistencia <- gsub(",", "", asistencia)
asistencia <- as.integer(asistencia)
# Estadio del Local
estadio = venue[1]
# ciudad del Local
ciudad = venue[2]
## arbitro central ##
arbitro_central <- str_trim(str_match(datos_partido, "Officials:\\s*([^(·]+?)\\s*\\(Referee\\)")[2])
## Equipos ##
if (fase_partido == "Unico") {
  partido_data <- regmatches(datos_partido, regexpr("(?<=\\(League phase\\)).*?(?=Historical)", datos_partido, perl = TRUE))
  partido_data <- trimws(partido_data)
  partido_data <- strsplit(partido_data, "\\s+vs\\.\\s+")[[1]]
} else {
  partido_data <- sub(".*Leg \\d+ of \\d+(.*?)Historical Head-to-Head.*", "\\1", datos_partido)
  partido_data <- trimws(partido_data)
  partido_data <- strsplit(partido_data, "\\s+vs\\.\\s+")[[1]]
}





# Equipo Local #
equipo_local = partido_data[1]
# Equipo Visitante #
equipo_visitante = partido_data[2]

equipo_local <- recode(equipo_local,
                       "Barcelona" = "FC Barcelona",
                       "Internazionale" = "Inter de Milan",
                       "Dortmund" = "Borussia Dortmund"
)

equipo_visitante <- recode(equipo_visitante,
                           "Barcelona" = "FC Barcelona",
                           "Internazionale" = "Inter de Milan",
                           "Dortmund" = "Borussia Dortmund"
)

## Entrenadores de los equipos ##
manager <- pagina %>%
  html_nodes(".datapoint") %>%
  html_text()
manager <- sub("Manager: ", "", manager)
capitan <- sub("Captain: ", "", manager)
# Equipo local
entrenador_eq_local = manager[1]
# Equipo visitante
entrenador_eq_visitante = manager[3]
## Capitanes de los equipos ##
# Equipo Local
capitan_eq_local <- capitan[2]
# Equipo Visitante
capitan_eq_visitante <- capitan[4]

# Tablas de la página web
tablas <- html_table(html_nodes(pagina, "table"), fill = TRUE)

# Obtener tablas de la página web
## Equipo Local ##
# Tabla plantilla
equipolocal1 <- tablas[[4]]  
equipolocal2 <- tablas[[5]]
equipolocal3 <- tablas[[6]]
equipolocal4 <- tablas[[7]]
equipolocal5 <- tablas[[8]]
equipolocal6 <- tablas[[9]]
# Porteros
equipolocal_portero <- tablas[[10]]

## Equipo Visitante ##
# Plantilla
equipovisitante1 <- tablas[[11]] 
equipovisitante2 <- tablas[[12]] 
equipovisitante3 <- tablas[[13]] 
equipovisitante4 <- tablas[[14]] 
equipovisitante5 <- tablas[[15]] 
equipovisitante6 <- tablas[[16]]
# Porteros
equipovisitante_portero <- tablas[[17]]

## Cronologia Partido ##
acciones_partido <- tablas[[18]]

## Cambiar nombres de las variables
# Tabla 1 (equipo local y visitante)
bases_tabla1 <- c("equipolocal1","equipovisitante1")
for (base_tabla1 in bases_tabla1) {
  df = get(base_tabla1)
  colnames(df) <- c("N_Player","N_Num","N_Nation","N_Pos","N_Age","N_Min","Performance_Gls",
                    "Performance_Ast","Performance_PK","Performance_PKatt","Performance_Sh",
                    "Performance_SoT","Performance_CrdY...13","Performance_CrdR...14","Performance_Touches",
                    "Performance_Tkl","Performance_Int...17","Performance_Blocks","Expected_xG","Expected_npxG",
                    "Expected_xAG","SCA_SCA","SCA_GCA","Passes_Cmp","Passes_Att","Passes_CmpPorc","Passes_PrgP",
                    "Carries_Carries...28","Carries_PrgC...29","TakeOns_Att...30","TakeOns_Succ...31")
  df <- df %>% filter(!N_Min == "Min")
  df <- df[-nrow(df), ]
  assign(base_tabla1, df)
}

# Tabla 2 (local y visitante)
bases_tabla2 <- c("equipolocal2","equipovisitante2")
for (base_tabla2 in bases_tabla2) {
  df = get(base_tabla2)
  colnames(df) <- c("N_Player","N_Num","N_Nation","N_Pos","N_Age","N_Min","Total_Cmp","Total_Att","Total_CmpPorc","Total_TotDist",
                    "Total_PrgDist","Short_Cmp","Short_Att","Short_CmpPorc","Medium_Cmp","Medium_Att","Medium_CmpPorc","Long_Cmp",
                    "Long_Att","Long_CmpPorc","Pass_Ast","Pass_xAG","Pass_xA","Pass_KP","Pass_One_Third","Pass_PPA","Pass_CrsPA","Pass_PrgP")
  df <- df %>% filter(!N_Min == "Min")
  df <- df[-nrow(df), ]
  df <- df %>% select(-N_Num,-N_Nation,-N_Pos,-N_Age,-N_Min)
  assign(base_tabla2, df)
}

# Tabla 3 (local y visitante)
bases_tabla3 <- c("equipolocal3","equipovisitante3")
for (base_tabla3 in bases_tabla3) {
  df = get(base_tabla3)
  colnames(df) <- c("N_Player","N_Num","N_Nation","N_Pos","N_Age","N_Min","Pass_Att","Pass_Type_Live","Pass_Type_Dead","Pass_Type_FK",
                    "Pass_Type_TB","Pass_Type_Sw","Pass_Type_Crs","Pass_Type_TI","Pass_Type_CK","Corner_Kick_In","Corner_Kick_Out","Corner_Kick_Str",
                    "Outocomes_Cmp","Outocomes_Off","Outocomes_Blocks")
  df <- df %>% filter(!N_Min == "Min")
  df <- df[-nrow(df), ]
  df <- df %>% select(-N_Num,-N_Nation,-N_Pos,-N_Age,-N_Min)
  assign(base_tabla3, df)
}

# Tabla 4 (local y visitante)
bases_tabla4 <- c("equipolocal4","equipovisitante4")
for (base_tabla4 in bases_tabla4) {
  df = get(base_tabla4)
  colnames(df) <- c("N_Player","N_Num","N_Nation","N_Pos","N_Age","N_Min","Tackles_Tkl","Tackles_TklW","Tackles_Def3rd","Tackles_Mid3rd","Tackles_Att3rd","Challenges_Tkl","Challenges_Att",
                    "Challenges_TklPorc","Challenges_Lost","Blocks_Blocks","Blocks_Sh","Blocks_Pass","B_Int","B_TklInt","B_Clr","B_Err")
  df <- df %>% filter(!N_Min == "Min")
  df <- df[-nrow(df), ]
  df <- df %>% select(-N_Num,-N_Nation,-N_Pos,-N_Age,-N_Min)
  assign(base_tabla4, df)
}

# Tabla 5 (local y visitante)
bases_tabla5 <- c("equipolocal5","equipovisitante5")
for (base_tabla5 in bases_tabla5) {
  df = get(base_tabla5)
  colnames(df) <- c("N_Player","N_Num","N_Nation","N_Pos","N_Age","N_Min","Touches_Touches","Touches_Def Pen","Touches_Def 3rd","Touches_Mid 3rd","Touches_Att 3rd",
                    "Touches_Att Pen","Touches_Live","TakeOns_Att...92","TakeOns_Succ...93","TakeOns_Succ%","TakeOns_Tkld","TakeOns_Tkld%","Carries_Carries...97","Carries_TotDist",
                    "Carries_PrgDist","Carries_PrgC...100","Carries_One_Third","Carries_CPA","Carries_Mis","Carries_Dis","Receiving_Rec","Receiving_PrgR")
  df <- df %>% filter(!N_Min == "Min")
  df <- df[-nrow(df), ]
  df <- df %>% select(-N_Num,-N_Nation,-N_Pos,-N_Age,-N_Min)
  assign(base_tabla5, df)
}

# Tabla 6 (local y visitante)
bases_tabla6 <- c("equipolocal6","equipovisitante6")
for (base_tabla6 in bases_tabla6) {
  df = get(base_tabla6)
  colnames(df) <- c("N_Player","N_Num","N_Nation","N_Pos","N_Age","N_Min","Performance_CrdY...107","Performance_CrdR...108","Performance_2CrdY","Performance_Fls","Performance_Fld",
                    "Performance_Off","Performance_Crs","Performance_Int...114","Performance_TklW","Performance_PKwon","Performance_PKcon","Performance_OG","Performance_Recov",
                    "Aerial Duels_Won","Aerial Duels_Lost","Aerial Duels_Won%")
  df <- df %>% filter(!N_Min == "Min")
  df <- df[-nrow(df), ]
  df <- df %>% select(-N_Num,-N_Nation,-N_Pos,-N_Age,-N_Min)
  assign(base_tabla6, df)
}

# Porteros (local y visitante)
bases_tablaporteros <- c("equipolocal_portero","equipovisitante_portero")
for (base_tablaporteros in bases_tablaporteros) {
  df = get(base_tablaporteros)
  colnames(df) <- c("Player","Nation","Age","Min","SoTA","GA","Saves","SavesPorc","PSxG","Cmp","Att...11","CmpPorc","AttGK","Thr",
                    "LaunchPorc...15","AvgLen...16","Att...17","LaunchPorc...18","AvgLen...19","Opp","Stp","StpPorc","OPA","AvgDist")
  df <- df %>% filter(!Player == "Player")
  assign(base_tablaporteros, df)
}

# Tabla acciones del partido
colnames(acciones_partido) <- c("minuto","jugador","equipo","goles_esperados","goles_esperados_despues_tiro",
                                "resultado","distancia","parte_cuerpo","notas","SCA1_jugador","SCA1_evento",
                                "SCA2_jugador","SCA2_evento")
acciones_partido <- acciones_partido %>% mutate(across(c("distancia"), as.numeric))
acciones_partido <- acciones_partido %>% filter(!is.na(distancia))

################
## Unir bases ##
################

# Equipo Local
equipolocal <- merge(equipolocal1,equipolocal2,by="N_Player")
equipolocal <- merge(equipolocal,equipolocal3,by="N_Player")
equipolocal <- merge(equipolocal,equipolocal4,by="N_Player")
equipolocal <- merge(equipolocal,equipolocal5,by="N_Player")
equipolocal <- merge(equipolocal,equipolocal6,by="N_Player")

# Equipo Visitante
equipovisitante <- merge(equipovisitante1,equipovisitante2, by="N_Player")
equipovisitante <- merge(equipovisitante, equipovisitante3, by="N_Player")
equipovisitante <- merge(equipovisitante, equipovisitante4, by="N_Player")
equipovisitante <- merge(equipovisitante, equipovisitante5, by="N_Player")
equipovisitante <- merge(equipovisitante, equipovisitante6, by="N_Player")

#####################################################

## Leer la base desde la computadora ##
#equipolocal <- read_excel("equipolocal.xlsx",col_names=FALSE)
#equipolocal_portero <- read_excel("equipolocal_portero.xlsx")
#equipovisitante_portero <- read_excel("equipovisitante_portero.xlsx")
# Cambiar el nombre a las variables
#nombres_columnas <- paste(equipolocal[1, ],equipolocal[2, ],sep = "_")
# Bases con nuevos nombres
#equipolocal <- read_excel("equipolocal.xlsx",col_names = nombres_columnas)
#equipovisitante <- read_excel("equipovisitante.xlsx", col_names = nombres_columnas)


# Equipo Local
equipolocal$equipo <- equipo_local
equipolocal_portero$equipo <- equipo_local
equipolocal$condicion_partido <- "Local"
equipolocal_portero$condicion_partido <- "Local"
# Equipo Visitante
equipovisitante$equipo <- equipo_visitante
equipovisitante_portero$equipo <- equipo_visitante
equipovisitante$condicion_partido <- "Visitante"
equipovisitante_portero$condicion_partido <- "Visitante"

# Lista
bases_equipos <- c("equipolocal","equipovisitante")
for (base_equipos in bases_equipos) {
  df = get(base_equipos)
  df$torneo <- torneo
  df$fase <- fase
  df$ronda <- ronda
  df$fase_partido <- fase_partido
  df$temporada <- temporada
  df$fecha <- fecha
  df$estadio <- estadio
  df$ciudad <- ciudad
  df$hora <- hora
  df$asistencia <- asistencia
  assign(base_equipos, df)
}

bases_equipolocal <- c("equipolocal","equipolocal_portero")
for (base_equipolocal in bases_equipolocal) {
  df = get(base_equipolocal)
  df$entrenador <- entrenador_eq_local
  df$arbitro_central <- arbitro_central
  assign(base_equipolocal, df)
}

bases_equipovisitante <- c("equipovisitante","equipovisitante_portero")
for (base_equipovisitante in bases_equipovisitante) {
  df = get(base_equipovisitante)
  df$entrenador <- entrenador_eq_visitante
  df$arbitro_central <- arbitro_central
  assign(base_equipovisitante, df)
}


##############
## Porteros ##
##############
bases_porteros <- c("equipolocal_portero","equipovisitante_portero")
for (base_porteros in bases_porteros) {
  df = get(base_porteros)
  df$torneo <- torneo
  df$fase <- fase
  df$ronda <- ronda
  df$fase_partido <- fase_partido
  df$temporada <- temporada
  df$fecha <- fecha
  df$estadio <- estadio
  df$ciudad <- ciudad
  df$hora <- hora
  df$asistencia <- asistencia
  # Nacionalidad
  df$nacionalidad_siglas <- substr(df$Nation,4,6)
  # Edad (Años)
  df$edad_años <- substr(df$Age,1,2)
  # Edad (Dias)
  df$edad_dias <- substr(df$Age,4,6)
  # Posicion Principal
  df$posicion <- "POR"
  df <- df %>% rename(tiros_arco_contra = SoTA,
                      goles_permitidos = GA,
                      atajadas = Saves,
                      porcentaje_atajadas = SavesPorc,
                      goles_esperados_despues_tiro = PSxG,
                      pases_completados_por_l = Cmp,
                      pases_intentados_por_1 = Att...11,
                      porcentaje_pases_completados_por_l = CmpPorc,
                      pases_intentados_por = AttGK,
                      saques_mano_intentados_por = Thr,
                      porcentaje_pases_lanzados_por = `LaunchPorc...15`,
                      distancia_promedio_pases_por = AvgLen...16,
                      tiros_gol_intentados = Att...17,
                      porcentaje_tiros_gol_intentados = `LaunchPorc...18`,
                      distancia_promedio_tiros_gol = AvgLen...19,
                      centros_intentados = Opp,
                      centros_parados = Stp,
                      porcentaje_centros_parados = StpPorc,
                      acciones_defensivas_fuera_area_penal = OPA,
                      distancia_promedio_acciones_defensivas = AvgDist,
                      nombre_jugador = Player,
                      numero_minutos = Min)
  df <- df[, c("temporada","torneo","fase","ronda","fase_partido","fecha","hora","condicion_partido","asistencia","estadio","ciudad","arbitro_central",
               "equipo","entrenador","nombre_jugador","nacionalidad_siglas","posicion","edad_años","edad_dias","numero_minutos",
               "tiros_arco_contra","goles_permitidos","atajadas","porcentaje_atajadas","goles_esperados_despues_tiro",
               "pases_completados_por_l","pases_intentados_por_1","porcentaje_pases_completados_por_l","pases_intentados_por",
               "saques_mano_intentados_por","porcentaje_pases_lanzados_por","distancia_promedio_pases_por","tiros_gol_intentados",
               "porcentaje_tiros_gol_intentados","distancia_promedio_tiros_gol","centros_intentados","centros_parados","porcentaje_centros_parados",
               "acciones_defensivas_fuera_area_penal","distancia_promedio_acciones_defensivas")]
  df <- df %>% mutate(across(c("edad_años","edad_dias","numero_minutos",
                               "tiros_arco_contra","goles_permitidos","atajadas","porcentaje_atajadas","goles_esperados_despues_tiro",
                               "pases_completados_por_l","pases_intentados_por_1","porcentaje_pases_completados_por_l","pases_intentados_por",
                               "saques_mano_intentados_por","porcentaje_pases_lanzados_por","distancia_promedio_pases_por","tiros_gol_intentados",
                               "porcentaje_tiros_gol_intentados","distancia_promedio_tiros_gol","centros_intentados","centros_parados","porcentaje_centros_parados",
                               "acciones_defensivas_fuera_area_penal","distancia_promedio_acciones_defensivas"), as.numeric))
  assign(base_porteros, df)
}




# Portero Equipo Local
suma_equipolocal_portero <- equipolocal_portero %>% 
  group_by(equipo) %>%
  summarise(tiros_arco_contra_local = sum(tiros_arco_contra, na.rm=TRUE),
            goles_permitidos_local = sum(goles_permitidos, na.rm=TRUE),
            atajadas_local = sum(atajadas, na.rm=TRUE),
            porcentaje_atajadas_local = (tiros_arco_contra_local-goles_permitidos_local)/tiros_arco_contra_local,
            goles_esperados_despues_tiro_local = sum(goles_esperados_despues_tiro, na.rm=TRUE),
            pases_completados_por_l_local = sum(pases_completados_por_l, na.rm=TRUE),
            pases_intentados_por_1_local = sum(pases_intentados_por_1, na.rm=TRUE),
            porcentaje_pases_completados_por_l_local = pases_completados_por_l_local/pases_intentados_por_1_local,
            pases_intentados_por_local = sum(pases_intentados_por, na.rm=TRUE),
            saques_mano_intentados_por_local = sum(saques_mano_intentados_por, na.rm=TRUE),
            distancia_promedio_pases_por_local = sum(distancia_promedio_pases_por, na.rm=TRUE),
            tiros_gol_intentados_local = sum(tiros_gol_intentados, na.rm=TRUE),
            distancia_promedio_tiros_gol_local = sum(distancia_promedio_tiros_gol, na.rm=TRUE),
            centros_intentados_local = sum(centros_intentados, na.rm=TRUE),
            centros_parados_local = sum(centros_parados, na.rm=TRUE),
            porcentaje_centros_parados_local = centros_parados_local/centros_intentados_local,
            acciones_defensivas_fuera_area_penal_local = sum(acciones_defensivas_fuera_area_penal, na.rm=TRUE),
            distancia_promedio_acciones_defensivas_local = sum(distancia_promedio_acciones_defensivas, na.rm=TRUE)) 


# Portero Equipo Visitante
suma_equipovisitante_portero <- equipovisitante_portero %>% 
  group_by(equipo) %>%
  summarise(tiros_arco_contra_visita = sum(tiros_arco_contra, na.rm=TRUE),
            goles_permitidos_visita = sum(goles_permitidos, na.rm=TRUE),
            atajadas_visita = sum(atajadas, na.rm=TRUE),
            porcentaje_atajadas_visita = (tiros_arco_contra_visita-goles_permitidos_visita)/tiros_arco_contra_visita,
            goles_esperados_despues_tiro_visita = sum(goles_esperados_despues_tiro, na.rm=TRUE),
            pases_completados_por_l_visita = sum(pases_completados_por_l, na.rm=TRUE),
            pases_intentados_por_1_visita = sum(pases_intentados_por_1, na.rm=TRUE),
            porcentaje_pases_completados_por_l_visita = pases_completados_por_l_visita/pases_intentados_por_1_visita,
            pases_intentados_por_visita = sum(pases_intentados_por, na.rm=TRUE),
            saques_mano_intentados_por_visita = sum(saques_mano_intentados_por, na.rm=TRUE),
            distancia_promedio_pases_por_visita = sum(distancia_promedio_pases_por, na.rm=TRUE),
            tiros_gol_intentados_visita = sum(tiros_gol_intentados, na.rm=TRUE),
            distancia_promedio_tiros_gol_visita = sum(distancia_promedio_tiros_gol, na.rm=TRUE),
            centros_intentados_visita = sum(centros_intentados, na.rm=TRUE),
            centros_parados_visita = sum(centros_parados, na.rm=TRUE),
            porcentaje_centros_parados_visita = centros_parados_visita/centros_intentados_visita,
            acciones_defensivas_fuera_area_penal_visita = sum(acciones_defensivas_fuera_area_penal, na.rm=TRUE),
            distancia_promedio_acciones_defensivas_visita = sum(distancia_promedio_acciones_defensivas, na.rm=TRUE)) 

suma_equipolocal_portero$clave = 1
suma_equipovisitante_portero$clave = 1


#############
## Equipos ##
#############
# Bucle
for (base_equipos in bases_equipos) {
  df = get(base_equipos)
  # Filtrar
  df <- df %>% filter(!N_Player %in% c("N","Player"))
  # Nacionalidad
  df$nacionalidad_siglas <- substr(df$N_Nation,4,6)
  # Edad (Años)
  df$edad_años <- substr(df$N_Age,1,2)
  # Edad (Dias)
  df$edad_dias <- substr(df$N_Age,4,6)
  # Posicion Principal
  df$posicion_principal <- substr(df$N_Pos,1,2)
  # Posicion Secundaria
  df$posicion_secundaria <- substr(df$N_Pos,4,5)
  # Arbitro Central
  df$arbitro_central <- arbitro_central
  # Cambiar el nombre
  df <- df %>% rename(nombre_jugador = N_Player,
                      numero_dorsal = N_Num,
                      numero_minutos = N_Min,
                      posicion = N_Pos,
                      goles=Performance_Gls,
                      asistencias = Performance_Ast,
                      tiros_penal_generados_favor = Performance_PK,
                      tiro_penal_cobrado = Performance_PKatt,
                      tiro_penal_anotado = Performance_PKwon,
                      tiro_penal_generado_contra = Performance_PKcon,
                      tiros_total = Performance_Sh,
                      tiros_arco = Performance_SoT,
                      tarjetas_amarillas = Performance_CrdY...13,
                      tarjetas_rojas = Performance_CrdR...14,
                      toques_balon = Performance_Touches,
                      faltas = Performance_Tkl,
                      intercepciones = Performance_Int...17,
                      bloqueos = Performance_Blocks,
                      goles_esperados = Expected_xG,
                      goles_esperados_nopenal = Expected_npxG,
                      goles_asistidos_esperados = Expected_xAG,
                      acciones_generadoras_tiros = SCA_SCA,
                      acciones_generadoras_gol = SCA_GCA,
                      pases_completados = Passes_Cmp,
                      pases_intentados = Passes_Att,
                      porcentaje_pases_completados = Passes_CmpPorc,
                      pases_progresivos = Passes_PrgP,
                      n_posesiones_balon = Carries_Carries...28,
                      n_posesiones_balon_progresivas = Carries_PrgC...29,
                      n_encares = TakeOns_Att...30,
                      n_encares_exitosos = TakeOns_Succ...31,
                      distancia_total_pases = Total_TotDist,
                      distancia_pases_progresivos = Total_PrgDist,
                      pases_completados_cortos = Short_Cmp,
                      pases_intentados_cortos = Short_Att,
                      porcentaje_pases_completados_cortos = Short_CmpPorc,
                      pases_completados_medianos = Medium_Cmp,
                      pases_intentados_medianos = Medium_Att,
                      porcentaje_pases_completados_medioanos = Medium_CmpPorc,
                      pases_completados_largos = Long_Cmp,
                      pases_intentados_largos = Long_Att,
                      porcentaje_pases_completados_largos = Long_CmpPorc,
                      asistencias_esperadas = Pass_xA,
                      pases_clave = Pass_KP,
                      pases_ultimo_tercio = Pass_One_Third,
                      pases_area_penal = Pass_PPA,
                      cruces_area_penal = Pass_CrsPA,
                      pases_pelota_viva = Pass_Type_Live,
                      pases_pelota_muerta = Pass_Type_Dead,
                      pases_tiros_libres = Pass_Type_FK,
                      pases_filtrados = Pass_Type_TB,
                      pases_cambio_juego = Pass_Type_Crs,
                      saques_manos = Pass_Type_TI,
                      tiros_esquina = Pass_Type_CK,
                      tiros_esquina_comba_dentro = Corner_Kick_In,
                      tiros_esquina_comba_fuera = Corner_Kick_Out,
                      tiros_esquina_recto = Corner_Kick_Str,
                      fuera_juego = Outocomes_Off,
                      pases_bloqueados_oponente = Outocomes_Blocks,
                      jugadores_tacleados = Tackles_Tkl,
                      tacles_ganados = Tackles_TklW,
                      tacles_tercio_defensivo = Tackles_Def3rd,
                      tacles_tercio_mitad = Tackles_Mid3rd,
                      tacles_tercio_ofensivo = Tackles_Att3rd,
                      regateadores_tacleados = Challenges_Att,
                      regateos_intentados = Challenges_Att,
                      porcentaje_regateadores_tacleados = Challenges_TklPorc,
                      enfrentamientos_perdidos = Challenges_Lost,
                      tiros_bloqueados = Blocks_Sh,
                      pases_bloqueados = Blocks_Pass,
                      suma_intercepciones_tacles = B_TklInt,
                      barridas = B_Clr,
                      errores = B_Err,
                      toques_area_penal_def = `Touches_Def Pen`,
                      toques_tercio_defensivo = `Touches_Def 3rd`,
                      toques_tercio_mitad = `Touches_Mid 3rd`,
                      toques_tercio_ofensivo = `Touches_Att 3rd`,
                      toques_area_penal_ofen = `Touches_Att Pen`,
                      porcentaje_encares_exitoso = `TakeOns_Succ%`,
                      n_tacles_encare = `TakeOns_Tkld`,
                      porcentaje_tacles_encare = `TakeOns_Tkld%`,
                      distancia_posesiones_balon = Carries_TotDist,
                      distancia_posesiones_balon_progresivas = Carries_PrgDist,
                      posesiones_balon_tercio_final = Carries_One_Third,
                      posesiones_balon_area_penal = Carries_CPA,
                      descontrol_posicion_balon = Carries_Mis,
                      desposesion_balon = Carries_Dis,
                      pases_recibidos = Receiving_Rec,
                      pases_progresivos_recibidos = Receiving_PrgR,
                      segunda_amarilla = Performance_2CrdY,
                      faltas_cometidas = Performance_Fls,
                      faltas_recibidas = Performance_Fld,
                      autogoles = Performance_OG,
                      balones_recuperados = Performance_Recov,
                      duelos_aereos_ganados = `Aerial Duels_Won`,
                      duelos_aereos_perdidos = `Aerial Duels_Lost`,
                      porcentaje_duelos_aereos_ganados = `Aerial Duels_Won%`)
  # Eliminar variables
  df <- df %>% select(-N_Nation,-N_Age,-Total_Cmp,-Total_Att,-Total_CmpPorc,-Pass_Ast,-Challenges_Tkl,
                      -Pass_xAG,-Pass_PrgP,-Pass_Att,-Pass_Type_Sw,-Outocomes_Cmp,
                      -Blocks_Blocks,-B_Int,-Touches_Touches,-Touches_Live,
                      -TakeOns_Att...92,-TakeOns_Succ...93,-Carries_Carries...97,
                      -Carries_PrgC...100,-Performance_CrdY...107,-Performance_CrdR...108,
                      -Performance_Off,-Performance_Crs,-Performance_Int...114,-Performance_TklW)
  # Convertir las columnas a variable numerica
  df <- df %>% mutate(across(c(numero_dorsal,numero_minutos,goles,asistencias,tiros_penal_generados_favor,
                               tiro_penal_cobrado,tiros_total,tiros_arco,tarjetas_amarillas,tarjetas_rojas,
                               toques_balon,faltas,intercepciones,bloqueos,goles_esperados,goles_esperados_nopenal,
                               goles_asistidos_esperados,acciones_generadoras_tiros,acciones_generadoras_gol,
                               pases_completados,pases_intentados,porcentaje_pases_completados,pases_progresivos,
                               n_posesiones_balon,n_posesiones_balon_progresivas,n_encares,n_encares_exitosos,
                               distancia_total_pases,distancia_pases_progresivos,pases_completados_cortos,
                               pases_intentados_cortos,porcentaje_pases_completados_cortos,pases_completados_medianos,
                               pases_intentados_medianos,porcentaje_pases_completados_medioanos,pases_intentados_largos,
                               pases_completados_largos,porcentaje_pases_completados_largos,asistencias_esperadas,
                               pases_clave,pases_ultimo_tercio,pases_area_penal,cruces_area_penal,pases_pelota_viva,
                               pases_pelota_muerta,pases_tiros_libres,pases_filtrados,pases_cambio_juego,saques_manos,
                               tiros_esquina,tiros_esquina_comba_dentro,tiros_esquina_comba_fuera,tiros_esquina_recto,
                               fuera_juego,pases_bloqueados_oponente,jugadores_tacleados,tacles_ganados,tacles_tercio_defensivo,
                               tacles_tercio_mitad,tacles_tercio_ofensivo,regateos_intentados,porcentaje_regateadores_tacleados,
                               enfrentamientos_perdidos,tiros_bloqueados,pases_bloqueados,suma_intercepciones_tacles,barridas,
                               errores,toques_area_penal_def,toques_tercio_defensivo,toques_tercio_mitad,toques_tercio_ofensivo,
                               toques_area_penal_ofen,porcentaje_encares_exitoso,n_tacles_encare,porcentaje_tacles_encare,
                               distancia_posesiones_balon,distancia_posesiones_balon_progresivas,posesiones_balon_tercio_final,
                               posesiones_balon_area_penal,descontrol_posicion_balon,desposesion_balon,pases_recibidos,
                               pases_progresivos_recibidos,segunda_amarilla,faltas_cometidas,faltas_recibidas,tiro_penal_anotado,
                               tiro_penal_generado_contra,autogoles,balones_recuperados,duelos_aereos_ganados,duelos_aereos_perdidos,
                               porcentaje_duelos_aereos_ganados,edad_años,edad_dias),as.numeric))
  df <- df[, c("temporada","torneo","fase","ronda","fase_partido","fecha","hora","condicion_partido","asistencia","estadio","ciudad","arbitro_central","equipo","entrenador",
               "nombre_jugador","numero_dorsal","nacionalidad_siglas","posicion_principal","posicion_secundaria",
               "edad_años","edad_dias","numero_minutos","goles","asistencias","asistencias_esperadas","pases_clave","tiros_total","tiros_arco","tiros_bloqueados",
               "goles_esperados","goles_esperados_nopenal","goles_asistidos_esperados","acciones_generadoras_tiros",
               "acciones_generadoras_gol","tiros_penal_generados_favor","tiro_penal_cobrado","tiro_penal_anotado",
               "tiro_penal_generado_contra","autogoles","pases_completados","pases_intentados","porcentaje_pases_completados",
               "pases_progresivos","n_posesiones_balon","n_posesiones_balon_progresivas","distancia_pases_progresivos",
               "distancia_total_pases","pases_completados_cortos","pases_intentados_cortos","porcentaje_pases_completados_cortos",
               "pases_completados_medianos","pases_intentados_medianos","porcentaje_pases_completados_medioanos","pases_completados_largos",
               "pases_intentados_largos","porcentaje_pases_completados_largos","pases_ultimo_tercio","pases_area_penal","cruces_area_penal",
               "pases_pelota_viva","pases_pelota_muerta","pases_tiros_libres","pases_filtrados","pases_cambio_juego","saques_manos",
               "tiros_esquina","tiros_esquina_comba_dentro","tiros_esquina_comba_fuera","tiros_esquina_recto","fuera_juego","pases_bloqueados_oponente",
               "jugadores_tacleados","tacles_ganados","tacles_tercio_defensivo","tacles_tercio_mitad","tacles_tercio_ofensivo","n_tacles_encare",
               "porcentaje_tacles_encare","faltas","tarjetas_amarillas","segunda_amarilla","tarjetas_rojas","toques_balon","intercepciones","bloqueos",
               "n_encares","n_encares_exitosos","porcentaje_encares_exitoso","regateos_intentados","porcentaje_regateadores_tacleados","enfrentamientos_perdidos","pases_bloqueados",
               "suma_intercepciones_tacles","barridas","errores","toques_area_penal_def","toques_tercio_defensivo","toques_tercio_mitad",
               "toques_tercio_ofensivo","toques_area_penal_ofen","distancia_posesiones_balon","distancia_posesiones_balon_progresivas","posesiones_balon_tercio_final",
               "posesiones_balon_area_penal","descontrol_posicion_balon","desposesion_balon","pases_recibidos","pases_progresivos_recibidos","faltas_cometidas",
               "faltas_recibidas","balones_recuperados","duelos_aereos_ganados","duelos_aereos_perdidos","porcentaje_duelos_aereos_ganados")]
  assign(base_equipos, df)
}

# Equipo Local
suma_equipolocal <- equipolocal %>% 
  group_by(equipo) %>%
  summarise(goles_local = sum(goles,na.rm = TRUE),
            asistencias_local = sum(asistencias, na.rm=TRUE),
            tiros_penal_generados_favor_local = sum(tiros_penal_generados_favor, na.rm=TRUE),
            tiro_penal_cobrado_local = sum(tiro_penal_cobrado, na.rm=TRUE),
            tiros_total_local = sum(tiros_total, na.rm=TRUE),
            tiros_arco_local = sum(tiros_arco, na.rm=TRUE),
            tarjetas_amarillas_local = sum(tarjetas_amarillas, na.rm=TRUE),
            tarjetas_rojas_local = sum(tarjetas_rojas, na.rm=TRUE),
            toques_balon_local = sum(toques_balon, na.rm=TRUE),
            faltas_local = sum(faltas, na.rm = TRUE),
            intercepciones_local = sum(intercepciones, na.rm = TRUE),
            bloqueos_local = sum(bloqueos, na.rm = TRUE),
            goles_esperados_local = sum(goles_esperados, na.rm = TRUE),
            goles_esperados_nopenal_local = sum(goles_esperados_nopenal, na.rm = TRUE),
            goles_asistidos_esperados_local = sum(goles_asistidos_esperados, na.rm = TRUE),
            acciones_generadoras_tiros_local = sum(acciones_generadoras_tiros, na.rm = TRUE),
            acciones_generadoras_gol_local = sum(acciones_generadoras_gol, na.rm = TRUE),
            pases_completados_local = sum(pases_completados, na.rm = TRUE),
            pases_intentados_local = sum(pases_intentados, na.rm = TRUE),
            porcentaje_pases_completados_local = pases_completados_local/pases_intentados_local,
            pases_progresivos_local = sum(pases_progresivos, na.rm = TRUE),
            n_posesiones_balon_local = sum(n_posesiones_balon, na.rm = TRUE),
            n_posesiones_balon_progresivas_local = sum(n_posesiones_balon_progresivas, na.rm = TRUE),
            n_encares_local = sum(n_encares, na.rm = TRUE),
            n_encares_exitosos_local = sum(n_encares_exitosos, na.rm = TRUE),
            distancia_total_pases_local = sum(distancia_total_pases, na.rm = TRUE),
            distancia_pases_progresivos_local = sum(distancia_pases_progresivos, na.rm = TRUE),
            pases_completados_cortos_local = sum(pases_completados_cortos, na.rm = TRUE),
            pases_intentados_cortos_local = sum(pases_intentados_cortos, na.rm=TRUE),
            porcentaje_pases_completados_cortos_local = pases_completados_cortos_local/pases_intentados_cortos_local,
            pases_completados_medianos_local = sum(pases_completados_medianos, na.rm= TRUE),
            pases_intentados_medianos_local = sum(pases_intentados_medianos, na.rm = TRUE),
            porcentaje_pases_completados_medianos_local = pases_completados_medianos_local/pases_intentados_medianos_local,
            pases_completados_largos_local = sum(pases_completados_largos, na.rm = TRUE),
            pases_intentados_largos_local = sum(pases_intentados_largos, na.rm = TRUE),
            porcentaje_pases_completados_largos_local = pases_completados_largos_local/pases_intentados_largos_local,
            asistencias_esperadas_local = sum(asistencias_esperadas, na.rm = TRUE),
            pases_clave_local = sum(pases_clave, na.rm = TRUE),
            pases_ultimo_tercio_local = sum(pases_ultimo_tercio, na.rm = TRUE),
            pases_area_penal_local = sum(pases_area_penal, na.rm = TRUE),
            cruces_area_penal_local = sum(cruces_area_penal, na.rm = TRUE),
            pases_pelota_viva_local = sum(pases_pelota_viva, na.rm = TRUE),
            pases_pelota_muerta_local = sum(pases_pelota_muerta, na.rm = TRUE),
            pases_tiros_libres_local = sum(pases_tiros_libres, na.rm = TRUE),
            pases_filtrados_local = sum(pases_filtrados, na.rm = TRUE),
            pases_cambio_juego_local = sum(pases_cambio_juego, na.rm = TRUE),
            saques_manos_local = sum(saques_manos, na.rm = TRUE),
            tiros_esquina_local = sum(tiros_esquina, na.rm = TRUE),
            tiros_esquina_comba_dentro_local = sum(tiros_esquina_comba_dentro, na.rm = TRUE),
            tiros_esquina_comba_fuera_local = sum(tiros_esquina_comba_fuera, na.rm = TRUE),
            tiros_esquina_recto_local = sum(tiros_esquina_recto, na.rm = TRUE),
            fuera_juego_local = sum(fuera_juego, na.rm = TRUE),
            pases_bloqueados_oponente_local = sum(pases_bloqueados_oponente, na.rm = TRUE),
            Jugadores_tacleados_local = sum(jugadores_tacleados, na.rm = TRUE),
            tacles_ganados_local = sum(tacles_ganados, na.rm = TRUE),
            tacles_tercio_defensivo_local = sum(tacles_tercio_defensivo, na.rm = TRUE),
            tacles_tercio_mitad_local = sum(tacles_tercio_mitad, na.rm = TRUE),
            tacles_tercio_ofensivo_local = sum(tacles_tercio_ofensivo, na.rm = TRUE),
            regateos_intentados_local = sum(regateos_intentados, na.rm = TRUE),
            enfrentamientos_perdidos_local = sum(enfrentamientos_perdidos, na.rm = TRUE),
            tiros_bloqueados_local = sum(tiros_bloqueados, na.rm = TRUE),
            pases_bloqueados_local = sum(pases_bloqueados, na.rm = TRUE),
            suma_intercepciones_tacles_local = sum(suma_intercepciones_tacles, na.rm = TRUE),
            barridas_local = sum(barridas, na.rm = TRUE),
            errores_local = sum(errores, na.rm = TRUE),
            toques_area_penal_def_local = sum(toques_area_penal_def, na.rm = TRUE),
            toques_tercio_defensivo_local = sum(toques_tercio_defensivo, na.rm = TRUE),
            toques_tercio_mitad_local = sum(toques_tercio_mitad, na.rm = TRUE),
            toques_tercio_ofensivo_local = sum(toques_tercio_ofensivo, na.rm = TRUE),
            toques_area_penal_ofen_local = sum(toques_area_penal_ofen, na.rm = TRUE),
            n_tacles_encare_local = sum(n_tacles_encare, na.rm = TRUE),
            distancia_posesiones_balon_local = sum(distancia_posesiones_balon, na.rm = TRUE),
            distancia_posesiones_balon_progresivas_local = sum(distancia_posesiones_balon_progresivas, na.rm = TRUE),
            posesiones_balon_tercio_final_local = sum(posesiones_balon_tercio_final, na.rm = TRUE),
            posesiones_balon_area_penal_local = sum(posesiones_balon_area_penal, na.rm = TRUE),
            descontrol_posicion_balon_local = sum(descontrol_posicion_balon, na.rm = TRUE),
            desposesion_balon_local = sum(desposesion_balon, na.rm = TRUE),
            pases_recibidos_local = sum(pases_recibidos, na.rm = TRUE),
            pases_progresivos_recibidos_local = sum(pases_progresivos_recibidos, na.rm = TRUE),
            segunda_amarilla_local = sum(segunda_amarilla, na.rm = TRUE),
            faltas_cometidas_local = sum(faltas_cometidas, na.rm = TRUE),
            faltas_recibidas_local = sum(faltas_recibidas, na.rm = TRUE),
            tiro_penal_anotado_local = sum(tiro_penal_anotado, na.rm = TRUE),
            tiro_penal_generado_contra_local =  sum(tiro_penal_generado_contra, na.rm = TRUE),
            autogoloes_local = sum(autogoles, na.rm = TRUE),
            balones_recuperados_local = sum(balones_recuperados, na.rm = TRUE),
            duelos_aereos_ganados_local = sum(duelos_aereos_ganados, na.rm = TRUE),
            duelos_aereos_perdidos_local = sum(duelos_aereos_perdidos, na.rm = TRUE),
            duelos_aereos_total_local = duelos_aereos_ganados_local+duelos_aereos_perdidos_local,
            porcentaje_duelos_aereos_ganados_local = duelos_aereos_ganados_local/duelos_aereos_total_local)

# Equipo Visitante
suma_equipovisitante <- equipovisitante %>% 
  group_by(equipo) %>%
  summarise(goles_visita = sum(goles,na.rm = TRUE),
            asistencias_visita = sum(asistencias, na.rm=TRUE),
            tiros_penal_generados_favor_visita = sum(tiros_penal_generados_favor, na.rm=TRUE),
            tiro_penal_cobrado_visita = sum(tiro_penal_cobrado, na.rm=TRUE),
            tiros_total_visita = sum(tiros_total, na.rm=TRUE),
            tiros_arco_visita = sum(tiros_arco, na.rm=TRUE),
            tarjetas_amarillas_visita = sum(tarjetas_amarillas, na.rm=TRUE),
            tarjetas_rojas_visita = sum(tarjetas_rojas, na.rm=TRUE),
            toques_balon_visita = sum(toques_balon, na.rm=TRUE),
            faltas_visita = sum(faltas, na.rm = TRUE),
            intercepciones_visita = sum(intercepciones, na.rm = TRUE),
            bloqueos_visita = sum(bloqueos, na.rm = TRUE),
            goles_esperados_visita = sum(goles_esperados, na.rm = TRUE),
            goles_esperados_nopenal_visita = sum(goles_esperados_nopenal, na.rm = TRUE),
            goles_asistidos_esperados_visita = sum(goles_asistidos_esperados, na.rm = TRUE),
            acciones_generadoras_tiros_visita = sum(acciones_generadoras_tiros, na.rm = TRUE),
            acciones_generadoras_gol_visita = sum(acciones_generadoras_gol, na.rm = TRUE),
            pases_completados_visita = sum(pases_completados, na.rm = TRUE),
            pases_intentados_visita = sum(pases_intentados, na.rm = TRUE),
            porcentaje_pases_completados_visita = pases_completados_visita/pases_intentados_visita,
            pases_progresivos_visita = sum(pases_progresivos, na.rm = TRUE),
            n_posesiones_balon_visita = sum(n_posesiones_balon, na.rm = TRUE),
            n_posesiones_balon_progresivas_visita = sum(n_posesiones_balon_progresivas, na.rm = TRUE),
            n_encares_visita = sum(n_encares, na.rm = TRUE),
            n_encares_exitosos_visita = sum(n_encares_exitosos, na.rm = TRUE),
            distancia_total_pases_visita = sum(distancia_total_pases, na.rm = TRUE),
            distancia_pases_progresivos_visita = sum(distancia_pases_progresivos, na.rm = TRUE),
            pases_completados_cortos_visita = sum(pases_completados_cortos, na.rm = TRUE),
            pases_intentados_cortos_visita = sum(pases_intentados_cortos, na.rm=TRUE),
            porcentaje_pases_completados_cortos_visita = pases_completados_cortos_visita/pases_intentados_cortos_visita,
            pases_completados_medianos_visita = sum(pases_completados_medianos, na.rm= TRUE),
            pases_intentados_medianos_visita = sum(pases_intentados_medianos, na.rm = TRUE),
            porcentaje_pases_completados_medianos_visita = pases_completados_medianos_visita/pases_intentados_medianos_visita,
            pases_completados_largos_visita = sum(pases_completados_largos, na.rm = TRUE),
            pases_intentados_largos_visita = sum(pases_intentados_largos, na.rm = TRUE),
            porcentaje_pases_completados_largos_visita = pases_completados_largos_visita/pases_intentados_largos_visita,
            asistencias_esperadas_visita = sum(asistencias_esperadas, na.rm = TRUE),
            pases_clave_visita = sum(pases_clave, na.rm = TRUE),
            pases_ultimo_tercio_visita = sum(pases_ultimo_tercio, na.rm = TRUE),
            pases_area_penal_visita = sum(pases_area_penal, na.rm = TRUE),
            cruces_area_penal_visita = sum(cruces_area_penal, na.rm = TRUE),
            pases_pelota_viva_visita = sum(pases_pelota_viva, na.rm = TRUE),
            pases_pelota_muerta_visita = sum(pases_pelota_muerta, na.rm = TRUE),
            pases_tiros_libres_visita = sum(pases_tiros_libres, na.rm = TRUE),
            pases_filtrados_visita = sum(pases_filtrados, na.rm = TRUE),
            pases_cambio_juego_visita = sum(pases_cambio_juego, na.rm = TRUE),
            saques_manos_visita = sum(saques_manos, na.rm = TRUE),
            tiros_esquina_visita = sum(tiros_esquina, na.rm = TRUE),
            tiros_esquina_comba_dentro_visita = sum(tiros_esquina_comba_dentro, na.rm = TRUE),
            tiros_esquina_comba_fuera_visita = sum(tiros_esquina_comba_fuera, na.rm = TRUE),
            tiros_esquina_recto_visita = sum(tiros_esquina_recto, na.rm = TRUE),
            fuera_juego_visita = sum(fuera_juego, na.rm = TRUE),
            pases_bloqueados_oponente_visita = sum(pases_bloqueados_oponente, na.rm = TRUE),
            Jugadores_tacleados_visita = sum(jugadores_tacleados, na.rm = TRUE),
            tacles_ganados_visita = sum(tacles_ganados, na.rm = TRUE),
            tacles_tercio_defensivo_visita = sum(tacles_tercio_defensivo, na.rm = TRUE),
            tacles_tercio_mitad_visita = sum(tacles_tercio_mitad, na.rm = TRUE),
            tacles_tercio_ofensivo_visita = sum(tacles_tercio_ofensivo, na.rm = TRUE),
            regateos_intentados_visita = sum(regateos_intentados, na.rm = TRUE),
            enfrentamientos_perdidos_visita = sum(enfrentamientos_perdidos, na.rm = TRUE),
            tiros_bloqueados_visita = sum(tiros_bloqueados, na.rm = TRUE),
            pases_bloqueados_visita = sum(pases_bloqueados, na.rm = TRUE),
            suma_intercepciones_tacles_visita = sum(suma_intercepciones_tacles, na.rm = TRUE),
            barridas_visita = sum(barridas, na.rm = TRUE),
            errores_visita = sum(errores, na.rm = TRUE),
            toques_area_penal_def_visita = sum(toques_area_penal_def, na.rm = TRUE),
            toques_tercio_defensivo_visita = sum(toques_tercio_defensivo, na.rm = TRUE),
            toques_tercio_mitad_visita = sum(toques_tercio_mitad, na.rm = TRUE),
            toques_tercio_ofensivo_visita = sum(toques_tercio_ofensivo, na.rm = TRUE),
            toques_area_penal_ofen_visita = sum(toques_area_penal_ofen, na.rm = TRUE),
            n_tacles_encare_visita = sum(n_tacles_encare, na.rm = TRUE),
            distancia_posesiones_balon_visita = sum(distancia_posesiones_balon, na.rm = TRUE),
            distancia_posesiones_balon_progresivas_visita = sum(distancia_posesiones_balon_progresivas, na.rm = TRUE),
            posesiones_balon_tercio_final_visita = sum(posesiones_balon_tercio_final, na.rm = TRUE),
            posesiones_balon_area_penal_visita = sum(posesiones_balon_area_penal, na.rm = TRUE),
            descontrol_posicion_balon_visita = sum(descontrol_posicion_balon, na.rm = TRUE),
            desposesion_balon_visita = sum(desposesion_balon, na.rm = TRUE),
            pases_recibidos_visita = sum(pases_recibidos, na.rm = TRUE),
            pases_progresivos_recibidos_visita = sum(pases_progresivos_recibidos, na.rm = TRUE),
            segunda_amarilla_visita = sum(segunda_amarilla, na.rm = TRUE),
            faltas_cometidas_visita = sum(faltas_cometidas, na.rm = TRUE),
            faltas_recibidas_visita = sum(faltas_recibidas, na.rm = TRUE),
            tiro_penal_anotado_visita = sum(tiro_penal_anotado, na.rm = TRUE),
            tiro_penal_generado_contra_visita =  sum(tiro_penal_generado_contra, na.rm = TRUE),
            autogoloes_visita = sum(autogoles, na.rm = TRUE),
            balones_recuperados_visita = sum(balones_recuperados, na.rm = TRUE),
            duelos_aereos_ganados_visita = sum(duelos_aereos_ganados, na.rm = TRUE),
            duelos_aereos_perdidos_visita = sum(duelos_aereos_perdidos, na.rm = TRUE),
            duelos_aereos_total_visita = duelos_aereos_ganados_visita+duelos_aereos_perdidos_visita,
            porcentaje_duelos_aereos_ganados_visita = duelos_aereos_ganados_visita/duelos_aereos_total_visita)

# Cambiar nombre de variable equipo
suma_equipolocal <- suma_equipolocal %>% rename(equipolocal = equipo)
suma_equipovisitante <- suma_equipovisitante %>% rename(equipovisitante = equipo)


# Clave para unir
suma_equipolocal$clave = 1
suma_equipovisitante$clave = 1

#############
## Partido ##
#############

# Partido
partido <- merge(suma_equipolocal,suma_equipovisitante,by = "clave")
partido <- merge(partido, suma_equipolocal_portero, by="clave")
partido <- merge(partido, suma_equipovisitante_portero, by="clave")
partido$torneo <- torneo
partido$fase <- fase
partido$ronda <- ronda
partido$fase_partido <- fase_partido
partido$temporada <- temporada
partido$fecha <- fecha
# posesion (toques) del balon
partido$posesion_toques_local = partido$toques_balon_local/(partido$toques_balon_local+partido$toques_balon_visita)

partido$posesion_toques_visita = partido$toques_balon_visita/(partido$toques_balon_local+partido$toques_balon_visita)
# posesion (general) del balon
partido$posesion_local = partido$n_posesiones_balon_local/(partido$n_posesiones_balon_local+partido$n_posesiones_balon_visita)
partido$posesion_visita = partido$n_posesiones_balon_visita/(partido$n_posesiones_balon_local+partido$n_posesiones_balon_visita)
# Goles totales
partido$goles_totales_local = partido$goles_local + partido$autogoloes_visita
partido$goles_totales_visita = partido$goles_visita + partido$autogoloes_local
# Estadio local
partido$estadio_local = estadio
# Ciudad
partido$ciudad = ciudad
# Entrenador del equipo local
partido$entrenador_equipo_local = entrenador_eq_local
# Entrenador del visitante
partido$entrenador_equipo_visitante = entrenador_eq_visitante
# arbitro central
partido$arbitro_central = arbitro_central
# Hora
partido$hora <- hora
# Asistencia
partido$asistencia <- asistencia
# Capitan Equipo Local
partido$capitan_equipo_local <- capitan_eq_local
# Capitan Equipo Visitante
partido$capitan_equipo_visita <- capitan_eq_visitante


#Torneo
partido <- partido[, c("temporada","torneo","fase","ronda","fase_partido","fecha","hora","asistencia","estadio_local","ciudad","arbitro_central","equipolocal","entrenador_equipo_local","capitan_equipo_local","equipovisitante","entrenador_equipo_visitante","capitan_equipo_visita",
                       "goles_totales_local","goles_totales_visita","goles_local","goles_visita","posesion_toques_local","posesion_toques_visita","posesion_local","posesion_visita",
                       "pases_completados_local","pases_completados_visita","pases_intentados_local","pases_intentados_visita","porcentaje_pases_completados_local",
                       "porcentaje_pases_completados_visita","tiros_total_local","tiros_total_visita","tiros_arco_local","tiros_arco_visita","atajadas_local",
                       "atajadas_visita","porcentaje_atajadas_local","porcentaje_atajadas_visita","tarjetas_amarillas_local","tarjetas_amarillas_visita","segunda_amarilla_local",
                       "segunda_amarilla_visita","tarjetas_rojas_local","tarjetas_rojas_visita","faltas_local","faltas_visita","faltas_cometidas_local","faltas_cometidas_visita",
                       "faltas_recibidas_local","faltas_recibidas_visita","tiros_esquina_local","tiros_esquina_visita","toques_balon_local","toques_balon_visita","centros_intentados_local",
                       "centros_intentados_visita","centros_parados_local","centros_parados_visita","porcentaje_centros_parados_local","porcentaje_centros_parados_visita","cruces_area_penal_local",
                       "cruces_area_penal_visita","tacles_ganados_local","tacles_ganados_visita","n_tacles_encare_local","n_tacles_encare_visita","intercepciones_local","intercepciones_visita",
                       "duelos_aereos_ganados_local","duelos_aereos_ganados_visita","duelos_aereos_perdidos_local","duelos_aereos_perdidos_visita","duelos_aereos_total_local","duelos_aereos_total_visita",
                       "porcentaje_duelos_aereos_ganados_local","porcentaje_duelos_aereos_ganados_visita","fuera_juego_local","fuera_juego_visita","saques_manos_local",
                       "saques_manos_visita","saques_mano_intentados_por_local","saques_mano_intentados_por_visita","pases_filtrados_local","pases_filtrados_visita",
                       "goles_esperados_local","goles_esperados_visita","goles_esperados_nopenal_local","goles_esperados_nopenal_visita","goles_esperados_despues_tiro_local",
                       "goles_esperados_despues_tiro_visita","goles_asistidos_esperados_local","goles_asistidos_esperados_visita","autogoloes_local","autogoloes_visita","asistencias_local",
                       "asistencias_visita","asistencias_esperadas_local","asistencias_esperadas_visita","goles_permitidos_local","goles_permitidos_visita","tiros_gol_intentados_local",
                       "tiros_gol_intentados_visita","tiro_penal_anotado_local","tiro_penal_anotado_visita","tiro_penal_cobrado_local","tiro_penal_cobrado_visita","tiro_penal_generado_contra_local",
                       "tiro_penal_generado_contra_visita","tiros_penal_generados_favor_local","tiros_penal_generados_favor_visita","tiros_arco_contra_local","tiros_arco_contra_visita",
                       "tiros_bloqueados_local","tiros_bloqueados_visita","acciones_generadoras_tiros_local","acciones_generadoras_tiros_visita","acciones_generadoras_gol_local","acciones_generadoras_gol_visita",
                       "acciones_defensivas_fuera_area_penal_local","acciones_defensivas_fuera_area_penal_visita","tiros_esquina_comba_dentro_local","tiros_esquina_comba_dentro_visita","tiros_esquina_comba_fuera_local",
                       "tiros_esquina_comba_fuera_visita","tiros_esquina_recto_local","tiros_esquina_recto_visita","pases_completados_cortos_local","pases_completados_cortos_visita","pases_intentados_cortos_local",
                       "pases_intentados_cortos_visita","porcentaje_pases_completados_cortos_local","porcentaje_pases_completados_cortos_visita","pases_completados_medianos_local","pases_completados_medianos_visita",
                       "pases_intentados_medianos_local","pases_intentados_medianos_visita","porcentaje_pases_completados_medianos_local","porcentaje_pases_completados_medianos_visita","pases_completados_largos_local",
                       "pases_completados_largos_visita","pases_intentados_largos_local","pases_intentados_largos_visita","porcentaje_pases_completados_largos_local","porcentaje_pases_completados_largos_visita",
                       "pases_completados_por_l_local","pases_completados_por_l_visita","pases_intentados_por_1_local","pases_intentados_por_1_visita","porcentaje_pases_completados_por_l_local",
                       "porcentaje_pases_completados_por_l_visita","pases_pelota_viva_local","pases_pelota_viva_visita","pases_pelota_muerta_local","pases_pelota_muerta_visita","pases_progresivos_local",
                       "pases_progresivos_visita","pases_progresivos_recibidos_local","pases_progresivos_recibidos_visita","pases_recibidos_local","pases_recibidos_visita","pases_clave_local","pases_clave_visita",
                       "pases_area_penal_local","pases_area_penal_visita", "pases_bloqueados_local","pases_bloqueados_visita","pases_bloqueados_oponente_local","pases_bloqueados_oponente_visita",
                       "pases_cambio_juego_local","pases_cambio_juego_visita","pases_tiros_libres_local","pases_tiros_libres_visita","pases_ultimo_tercio_local","pases_ultimo_tercio_visita",
                       "pases_intentados_por_local","pases_intentados_por_visita","balones_recuperados_local","balones_recuperados_visita","barridas_local","barridas_visita","bloqueos_local","bloqueos_visita",
                       "descontrol_posicion_balon_local","descontrol_posicion_balon_visita","desposesion_balon_local","desposesion_balon_visita","distancia_pases_progresivos_local","distancia_pases_progresivos_visita",
                       "distancia_posesiones_balon_local","distancia_posesiones_balon_visita","distancia_posesiones_balon_progresivas_local","distancia_posesiones_balon_progresivas_visita","distancia_promedio_pases_por_local",
                       "distancia_promedio_pases_por_visita","distancia_promedio_acciones_defensivas_local","distancia_promedio_acciones_defensivas_visita","distancia_promedio_tiros_gol_local","distancia_promedio_tiros_gol_visita",
                       "distancia_total_pases_local","distancia_total_pases_visita","enfrentamientos_perdidos_local","enfrentamientos_perdidos_visita","errores_local","errores_visita","Jugadores_tacleados_local","Jugadores_tacleados_visita",
                       "n_encares_exitosos_local","n_encares_exitosos_visita","n_encares_local","n_encares_visita","n_posesiones_balon_local","n_posesiones_balon_progresivas_local","n_posesiones_balon_progresivas_visita","n_posesiones_balon_visita",
                       "posesiones_balon_area_penal_local","posesiones_balon_area_penal_visita","posesiones_balon_tercio_final_local","posesiones_balon_tercio_final_visita","regateos_intentados_local","regateos_intentados_visita",
                       "suma_intercepciones_tacles_local","suma_intercepciones_tacles_visita","tacles_tercio_defensivo_local","tacles_tercio_defensivo_visita","tacles_tercio_mitad_local","tacles_tercio_mitad_visita","tacles_tercio_ofensivo_local",
                       "tacles_tercio_ofensivo_visita", "toques_area_penal_def_local","toques_area_penal_def_visita","toques_area_penal_ofen_local","toques_area_penal_ofen_visita","toques_tercio_defensivo_local","toques_tercio_defensivo_visita",
                       "toques_tercio_mitad_local","toques_tercio_mitad_visita","toques_tercio_ofensivo_local","toques_tercio_ofensivo_visita")]

# Acciones del partido
# Resto de variables
acciones_partido$equipolocal <- equipo_local
acciones_partido$equipovisitante <- equipo_visitante
acciones_partido$torneo <- torneo
acciones_partido$fase <- fase
acciones_partido$ronda <- ronda
acciones_partido$fase_partido <- fase_partido
acciones_partido$temporada <- temporada
acciones_partido$fecha <- fecha
acciones_partido$estadio_local = estadio
acciones_partido$ciudad = ciudad
acciones_partido$entrenador_equipo_local = entrenador_eq_local
acciones_partido$entrenador_equipo_visitante = entrenador_eq_visitante
acciones_partido$arbitro_central = arbitro_central
acciones_partido$hora <- hora
acciones_partido$asistencia <- asistencia
acciones_partido$capitan_equipo_local <- capitan_eq_local
acciones_partido$capitan_equipo_visita <- capitan_eq_visitante
# Ordenar variables


acciones_partido <- acciones_partido[, c("temporada","torneo","fase","ronda","fase_partido","fecha","hora","asistencia","estadio_local","ciudad","arbitro_central","equipolocal","entrenador_equipo_local","capitan_equipo_local","equipovisitante","entrenador_equipo_visitante","capitan_equipo_visita",
                                         "minuto","jugador","equipo","goles_esperados","goles_esperados_despues_tiro","resultado","distancia","parte_cuerpo","notas","SCA1_jugador","SCA1_evento","SCA2_jugador","SCA2_evento")]

# Traducir datos internos
acciones_partido <- acciones_partido %>% mutate(resultado = case_when(resultado == "Blocked" ~ "Bloqueado",
                                                                      resultado == "Goal" ~ "Gol",
                                                                      resultado == "Off Target" ~ "Fuera de la Portería",
                                                                      resultado == "Saved" ~ "Atajado",
                                                                      resultado == "Woodwork" ~ "Impacto al Palo"),
                                                parte_cuerpo = case_when(parte_cuerpo == "Head" ~ "Cabeza",
                                                                         parte_cuerpo == "Left Foot" ~ "Pie Izquierdo",
                                                                         parte_cuerpo == "Right Foot" ~ "Pie Derecho"),
                                                notas = case_when(notas == "Volley" ~ "Volea"),
                                                SCA1_evento = case_when(SCA1_evento == "Fouled" ~ "Falta Recibida",
                                                                        SCA1_evento == "Pass (Dead)" ~ "Pase (Muerto)",
                                                                        SCA1_evento == "Pass (Live)" ~ "Pase (Vivo)",
                                                                        SCA1_evento == "Shot" ~ "Disparo",
                                                                        SCA1_evento == "Take-on" ~ "Encare"),
                                                SCA2_evento = case_when(SCA2_evento == "Pass (Dead)" ~ "Pase (Muerto)",
                                                                        SCA2_evento == "Pass (Live)" ~ "Pase (Vivo)",
                                                                        SCA2_evento == "Take-on" ~ "Encare"))

####################
## Exportar Bases ##
####################

cl_equipos <- list(
  "FC Barcelona" = list(
    carpeta = "FC Barcelona",
    plantilla = "CL_Plantilla_FC_Barcelona.xlsx",
    porteros = "CL_Porteros_FC_Barcelona.xlsx",
    plantilla_exp = "CL_Plantilla_FC_Barcelona.xlsx",
    porteros_exp = "CL_Porteros_FC_Barcelona.xlsx"
  ),
  "Inter de Milan" = list(
    carpeta = "Inter Milan",
    plantilla = "CL_Plantilla_Inter_Milan.xlsx",
    porteros = "CL_Porteros_Inter_Milan.xlsx",
    plantilla_exp = "CL_Plantilla_Inter_Milan.xlsx",
    porteros_exp = "CL_Porteros_Inter_Milan.xlsx"
  ),
  "Paris Saint-Germain" = list(
    carpeta = "Paris Saint Germain",
    plantilla = "CL_Plantilla_Paris_Saint_Germain.xlsx",
    porteros = "CL_Porteros_Paris_Saint_Germain.xlsx",
    plantilla_exp = "CL_Plantilla_Paris_Saint_Germain.xlsx",
    porteros_exp = "CL_Porteros_Paris_Saint_Germain.xlsx"
  ),
  "Arsenal" = list(
    carpeta = "Arsenal",
    plantilla = "CL_Plantilla_Arsenal.xlsx",
    porteros = "CL_Porteros_Arsenal.xlsx",
    plantilla_exp = "CL_Plantilla_Arsenal.xlsx",
    porteros_exp = "CL_Porteros_Arsenal.xlsx"
  ),
  "Aston Villa" = list(
    carpeta = "Aston Villa",
    plantilla = "CL_Plantilla_Aston_Villa.xlsx",
    porteros = "CL_Porteros_Aston_Villa.xlsx",
    plantilla_exp = "CL_Plantilla_Aston_Villa.xlsx",
    porteros_exp = "CL_Porteros_Aston_Villa.xlsx"
  ),
  "Borussia Dortmund" = list(
    carpeta = "Borussia Dortmund",
    plantilla = "CL_Plantilla_Borussia_Dortmund.xlsx",
    porteros = "CL_Porteros_Borussia_Dortmund.xlsx",
    plantilla_exp = "CL_Plantilla_Borussia_Dortmund.xlsx",
    porteros_exp = "CL_Porteros_Borussia_Dortmund.xlsx"
  ),
  "Bayern Munich" = list(
    carpeta = "Bayern Munich",
    plantilla = "CL_Plantilla_Bayern_Munich.xlsx",
    porteros = "CL_Porteros_Bayern_Munich.xlsx",
    plantilla_exp = "CL_Plantilla_Bayern_Munich.xlsx",
    porteros_exp = "CL_Porteros_Bayern_Munich.xlsx"
  ),
  "Real Madrid" = list(
    carpeta = "Real Madrid",
    plantilla = "CL_Plantilla_Real_Madrid.xlsx",
    porteros = "CL_Porteros_Real_Madrid.xlsx",
    plantilla_exp = "CL_Plantilla_Real_Madrid.xlsx",
    porteros_exp = "CL_Porteros_Real_Madrid.xlsx"
  ),
  "Atletico Madrid" = list(
    carpeta = "Atletico Madrid",
    plantilla = "CL_Plantilla_Atletico_Madrid.xlsx",
    porteros = "CL_Porteros_Atletico_Madrid.xlsx",
    plantilla_exp = "CL_Plantilla_Atletico_Madrid.xlsx",
    porteros_exp = "CL_Porteros_Atletico_Madrid.xlsx"
  ),
  "Bayer Leverkusen" = list(
    carpeta = "Bayer Leverkusen",
    plantilla = "CL_Plantilla_Bayer_Leverkusen.xlsx",
    porteros = "CL_Porteros_Bayer_Leverkusen.xlsx",
    plantilla_exp = "CL_Plantilla_Bayer_Leverkusen.xlsx",
    porteros_exp = "CL_Porteros_Bayer_Leverkusen.xlsx"
  ),
  "Benfica" = list(
    carpeta = "Benfica",
    plantilla = "CL_Plantilla_Benfica.xlsx",
    porteros = "CL_Porteros_Benfica.xlsx",
    plantilla_exp = "CL_Plantilla_Benfica.xlsx",
    porteros_exp = "CL_Porteros_Benfica.xlsx"
  ),
  "Club Brugge" = list(
    carpeta = "Club Brugge",
    plantilla = "CL_Plantilla_Club_Brugge.xlsx",
    porteros = "CL_Porteros_Club_Brugge.xlsx",
    plantilla_exp = "CL_Plantilla_Club_Brugge.xlsx",
    porteros_exp = "CL_Porteros_Club_Brugge.xlsx"
  ),
  "Feyenoord" = list(
    carpeta = "Feyenoord",
    plantilla = "CL_Plantilla_Feyenoord.xlsx",
    porteros = "CL_Porteros_Feyenoord.xlsx",
    plantilla_exp = "CL_Plantilla_Feyenoord.xlsx",
    porteros_exp = "CL_Porteros_Feyenoord.xlsx"
  ),
  "Lille" = list(
    carpeta = "Lille",
    plantilla = "CL_Plantilla_Lille.xlsx",
    porteros = "CL_Porteros_Lille.xlsx",
    plantilla_exp = "CL_Plantilla_Lille.xlsx",
    porteros_exp = "CL_Porteros_Lille.xlsx"
  ),
  "Liverpool" = list(
    carpeta = "Liverpool",
    plantilla = "CL_Plantilla_Liverpool.xlsx",
    porteros = "CL_Porteros_Liverpool.xlsx",
    plantilla_exp = "CL_Plantilla_Liverpool.xlsx",
    porteros_exp = "CL_Porteros_Liverpool.xlsx"
  ),
  "PSV Eindhoven" = list(
    carpeta = "PSV Eindhoven",
    plantilla = "CL_Plantilla_PSV_Eindhoven.xlsx",
    porteros = "CL_Porteros_PSV_Eindhoven.xlsx",
    plantilla_exp = "CL_Plantilla_PSV_Eindhoven.xlsx",
    porteros_exp = "CL_Porteros_PSV_Eindhoven.xlsx"
  ),
  "Sporting CP" = list(
    carpeta = "Sporting CP",
    plantilla = "CL_Plantilla_Sporting_CP.xlsx",
    porteros = "CL_Porteros_Sporting_CP.xlsx",
    plantilla_exp = "CL_Plantilla_Sporting_CP.xlsx",
    porteros_exp = "CL_Porteros_Sporting_CP.xlsx"
  ),
  "Atalanta" = list(
    carpeta = "Atalanta",
    plantilla = "CL_Plantilla_Atalanta.xlsx",
    porteros = "CL_Porteros_Atalanta.xlsx",
    plantilla_exp = "CL_Plantilla_Atalanta.xlsx",
    porteros_exp = "CL_Porteros_Atalanta.xlsx"
  ),
  "Celtic" = list(
    carpeta = "Celtic",
    plantilla = "CL_Plantilla_Celtic.xlsx",
    porteros = "CL_Porteros_Celtic.xlsx",
    plantilla_exp = "CL_Plantilla_Celtic.xlsx",
    porteros_exp = "CL_Porteros_Celtic.xlsx"
  ),
  "Manchester City" = list(
    carpeta = "Manchester City",
    plantilla = "CL_Plantilla_Manchester_City.xlsx",
    porteros = "CL_Porteros_Manchester_City.xlsx",
    plantilla_exp = "CL_Plantilla_Manchester_City.xlsx",
    porteros_exp = "CL_Porteros_Manchester_City.xlsx"
  ),
  "AC Milan" = list(
    carpeta = "AC Milan",
    plantilla = "CL_Plantilla_AC_Milan.xlsx",
    porteros = "CL_Porteros_AC_Milan.xlsx",
    plantilla_exp = "CL_Plantilla_AC_Milan.xlsx",
    porteros_exp = "CL_Porteros_AC_Milan.xlsx"
  ),
  "Juventus" = list(
    carpeta = "Juventus",
    plantilla = "CL_Plantilla_Juventus.xlsx",
    porteros = "CL_Porteros_Juventus.xlsx",
    plantilla_exp = "CL_Plantilla_Juventus.xlsx",
    porteros_exp = "CL_Porteros_Juventus.xlsx"
  ),
  "Brest" = list(
    carpeta = "Brest",
    plantilla = "CL_Plantilla_Brest.xlsx",
    porteros = "CL_Porteros_Brest.xlsx",
    plantilla_exp = "CL_Plantilla_Brest.xlsx",
    porteros_exp = "CL_Porteros_Brest.xlsx"
  ),
  "Monaco" = list(
    carpeta = "Monaco",
    plantilla = "CL_Plantilla_Monaco.xlsx",
    porteros = "CL_Porteros_Monaco.xlsx",
    plantilla_exp = "CL_Plantilla_Monaco.xlsx",
    porteros_exp = "CL_Porteros_Monaco.xlsx"
  ),
  "Dinamo Zagreb" = list(
    carpeta = "Dinamo Zagreb",
    plantilla = "CL_Plantilla_Dinamo_Zagreb.xlsx",
    porteros = "CL_Porteros_Dinamo_Zagreb.xlsx",
    plantilla_exp = "CL_Plantilla_Dinamo_Zagreb.xlsx",
    porteros_exp = "CL_Porteros_Dinamo_Zagreb.xlsx"
  ),
  "Slovan Bratislava" = list(
    carpeta = "Slovan Bratislava",
    plantilla = "CL_Plantilla_Slovan_Bratislava.xlsx",
    porteros = "CL_Porteros_Slovan_Bratislava.xlsx",
    plantilla_exp = "CL_Plantilla_Slovan_Bratislava.xlsx",
    porteros_exp = "CL_Porteros_Slovan_Bratislava.xlsx"
  ),
  "Young Boys" = list(
    carpeta = "Young Boys",
    plantilla = "CL_Plantilla_Young_Boys.xlsx",
    porteros = "CL_Porteros_Young_Boys.xlsx",
    plantilla_exp = "CL_Plantilla_Young_Boys.xlsx",
    porteros_exp = "CL_Porteros_Young_Boys.xlsx"
  ),
  "Shakhtar Donetsk" = list(
    carpeta = "Shakhtar Donetsk",
    plantilla = "CL_Plantilla_Shakhtar_Donetsk.xlsx",
    porteros = "CL_Porteros_Shakhtar_Donetsk.xlsx",
    plantilla_exp = "CL_Plantilla_Shakhtar_Donetsk.xlsx",
    porteros_exp = "CL_Porteros_Shakhtar_Donetsk.xlsx"
  ),
  "Red Bull Salzburg" = list(
    carpeta = "Red Bull Salzburg",
    plantilla = "CL_Plantilla_Red_Bull_Salzburg.xlsx",
    porteros = "CL_Porteros_Red_Bull_Salzburg.xlsx",
    plantilla_exp = "CL_Plantilla_Red_Bull_Salzburg.xlsx",
    porteros_exp = "CL_Porteros_Red_Bull_Salzburg.xlsx"
  ),
  "Girona" = list(
    carpeta = "Girona",
    plantilla = "CL_Plantilla_Girona.xlsx",
    porteros = "CL_Porteros_Girona.xlsx",
    plantilla_exp = "CL_Plantilla_Girona.xlsx",
    porteros_exp = "CL_Porteros_Girona.xlsx"
  ),
  "RB Leipzig" = list(
    carpeta = "RB Leipzig",
    plantilla = "CL_Plantilla_RB_Leipzig.xlsx",
    porteros = "CL_Porteros_RB_Leipzig.xlsx",
    plantilla_exp = "CL_Plantilla_RB_Leipzig.xlsx",
    porteros_exp = "CL_Porteros_RB_Leipzig.xlsx"
  ),
  "Sparta Prague" = list(
    carpeta = "Sparta Prague",
    plantilla = "CL_Plantilla_Sparta_Prague.xlsx",
    porteros = "CL_Porteros_Sparta_Prague.xlsx",
    plantilla_exp = "CL_Plantilla_Sparta_Prague.xlsx",
    porteros_exp = "CL_Porteros_Sparta_Prague.xlsx"
  ),
  "Sturm Graz" = list(
    carpeta = "Sturm Graz",
    plantilla = "CL_Plantilla_Sturm_Graz.xlsx",
    porteros = "CL_Porteros_Sturm_Graz.xlsx",
    plantilla_exp = "CL_Plantilla_Sturm_Graz.xlsx",
    porteros_exp = "CL_Porteros_Sturm_Graz.xlsx"
  ),
  "Red Star Belgrade" = list(
    carpeta = "Red Star Belgrade",
    plantilla = "CL_Plantilla_Red_Star_Belgrade.xlsx",
    porteros = "CL_Porteros_Red_Star_Belgrade.xlsx",
    plantilla_exp = "CL_Plantilla_Red_Star_Belgrade.xlsx",
    porteros_exp = "CL_Porteros_Red_Star_Belgrade.xlsx"
  ),
  "Bologna" = list(
    carpeta = "Bologna",
    plantilla = "CL_Plantilla_Bologna.xlsx",
    porteros = "CL_Porteros_Bologna.xlsx",
    plantilla_exp = "CL_Plantilla_Bologna.xlsx",
    porteros_exp = "CL_Porteros_Bologna.xlsx"
  ),
  "Stuttgart" = list(
    carpeta = "Stuttgart",
    plantilla = "CL_Plantilla_Stuttgart.xlsx",
    porteros = "CL_Porteros_Stuttgart.xlsx",
    plantilla_exp = "CL_Plantilla_Stuttgart.xlsx",
    porteros_exp = "CL_Porteros_Stuttgart.xlsx"
  )
)

# Funcion para procesar equipos
procesar_equipo <- function(nombre_equipo, df_plantilla, df_porteros) {
  if (nombre_equipo %in% names(cl_equipos)) {
    configuracion <- cl_equipos[[nombre_equipo]]
    ruta_base <- "C:/Users/leca_/OneDrive/Documentos/Data Analysis/Futbol/Champions League/Bases/Equipos"
    setwd(file.path(ruta_base, configuracion$carpeta))
    # Base de datos Plantilla
    base_original_plantilla <- read_excel(configuracion$plantilla)
    base_nueva_plantilla <- rbind(base_original_plantilla, df_plantilla)
    write.xlsx(base_nueva_plantilla, configuracion$plantilla_exp)
    # Base de datos Porteros
    base_original_porteros <- read_excel(configuracion$porteros)
    base_nueva_porteros <- rbind(base_original_porteros, df_porteros)
    write.xlsx(base_nueva_porteros, configuracion$porteros_exp)
  } else {
    cat("Equipo no reconocido:", nombre_equipo, "\n")
    cat("Equipos válidos:\n")
    print(names(cl_equipos))
  }
}

# Procesar equipo local
procesar_equipo(equipo_local, equipolocal, equipolocal_portero)
# Procesar equipo visitante
procesar_equipo(equipo_visitante, equipovisitante, equipovisitante_portero)

# Acciones del Partido
setwd("./Futbol/Champions League/Bases/Acciones del Partido")
cl_acciones_partido_original <- read_excel("CL_Acciones_Partido_2024_2025.xlsx")
cl_acciones_partido_nuevo <- rbind(cl_acciones_partido_original, acciones_partido)
write.xlsx(cl_acciones_partido_nuevo, "CL_Acciones_Partido_2024_2025.xlsx")

#Partidos
setwd("./Futbol/Champions League/Bases/General")
base_partidos <- read_excel("UEFA_Champions_League_2024_2025.xlsx")
union_partidos <- rbind(base_partidos,partido)
write.xlsx(union_partidos,"UEFA_Champions_League_2024_2025.xlsx")

# Restaurar el formato de fecha
Sys.setlocale("LC_TIME", old_locale)  # Restaurar


