library(tidyverse)
library(openxlsx)
library(readr)
library(tidyr)
library(readxl)

setwd("C:/Users/leca_/OneDrive/Documentos/Data Analysis/Futbol/Champions League")

###################################################
## Introduce la información del torneo y partido ##
###################################################

# Equipo Local #
equipo_local = "Arsenal"
# Equipo Visitante #
equipo_visitante = "Paris Saint-Germain"
# Torneo #
torneo = "Champions League"
# Etapa #
etapa = "Eliminacion directa"
# Ronda #
ronda = "Semifinal"
# Fase del Partido #
fase_partido = "Ida"
# Temporada #
temporada = "2024-2025"
# Fecha
fecha = "29/04/2025"

# Leer la base
equipolocal <- read_excel("equipolocal.xlsx",col_names=FALSE)
equipolocal_portero <- read_excel("equipolocal_portero.xlsx")
equipovisitante_portero <- read_excel("equipovisitante_portero.xlsx")
# Cambiar el nombre a las variables
nombres_columnas <- paste(equipolocal[1, ],equipolocal[2, ],sep = "_")
# Bases con nuevos nombres
equipolocal <- read_excel("equipolocal.xlsx",col_names = nombres_columnas)
equipovisitante <- read_excel("equipovisitante.xlsx", col_names = nombres_columnas)
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
  df$etapa <- etapa
  df$ronda <- ronda
  df$fase_partido <- fase_partido
  df$temporada <- temporada
  df$fecha <- fecha
  assign(base_equipos, df)
}

##############
## Porteros ##
##############
bases_porteros <- c("equipolocal_portero","equipovisitante_portero")
for (base_porteros in bases_porteros) {
  df = get(base_porteros)
  df$torneo <- torneo
  df$etapa <- etapa
  df$ronda <- ronda
  df$fase_partido <- fase_partido
  df$temporada <- temporada
  df$fecha <- fecha
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
                      porcentaje_atajadas = `Save%`,
                      goles_esperados_despues_tiro = PSxG,
                      pases_completados_por_l = Cmp,
                      pases_intentados_por_1 = Att...11,
                      porcentaje_pases_completados_por_l = `Cmp%`,
                      pases_intentados_por = `Att (GK)`,
                      saques_mano_intentados_por = Thr,
                      porcentaje_pases_lanzados_por = `Launch%...15`,
                      distancia_promedio_pases_por = AvgLen...16,
                      tiros_gol_intentados = Att...17,
                      porcentaje_tiros_gol_intentados = `Launch%...18`,
                      distancia_promedio_tiros_gol = AvgLen...19,
                      centros_intentados = Opp,
                      centros_parados = Stp,
                      porcentaje_centros_parados = `Stp%`,
                      acciones_defensivas_fuera_area_penal = `#OPA`,
                      distancia_promedio_acciones_defensivas = AvgDist,
                      nombre_jugador = Player,
                      numero_minutos = Min)
  df <- df[, c("temporada","torneo","etapa","ronda","fase_partido","fecha","condicion_partido",
               "equipo","nombre_jugador","nacionalidad_siglas","posicion","edad_años","edad_dias","numero_minutos",
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
                          porcentaje_pases_completados = `Passes_Cmp%`,
                          pases_progresivos = Passes_PrgP,
                          n_posesiones_balon = Carries_Carries...28,
                          n_posesiones_balon_progresivas = Carries_PrgC...29,
                          n_encares = TakeOns_Att...30,
                          n_encares_exitosos = TakeOns_Succ...31,
                          distancia_total_pases = Total_TotDist,
                          distancia_pases_progresivos = Total_PrgDist,
                          pases_completados_cortos = Short_Cmp,
                          pases_intentados_cortos = Short_Att,
                          porcentaje_pases_completados_cortos = `Short_Cmp%`,
                          pases_completados_medianos = Medium_Cmp,
                          pases_intentados_medianos = Medium_Att,
                          porcentaje_pases_completados_medioanos = `Medium_Cmp%`,
                          pases_completados_largos = Long_Cmp,
                          pases_intentados_largos = Long_Att,
                          porcentaje_pases_completados_largos = `Long_Cmp%`,
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
                          tacles_tercio_defensivo = `Tackles_Def 3rd`,
                          tacles_tercio_mitad = `Tackles_Mid 3rd`,
                          tacles_tercio_ofensivo = `Tackles_Att 3rd`,
                          regateadores_tacleados = Challenges_Att,
                          regateos_intentados = Challenges_Att,
                          porcentaje_regateadores_tacleados = `Challenges_Tkl%`,
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
  df <- df %>% select(-N_Nation,-N_Age,-Total_Cmp,-Total_Att,-`Total_Cmp%`,-Pass_Ast,-Challenges_Tkl,
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
  
  df <- df[, c("temporada","torneo","etapa","ronda","fase_partido","fecha","condicion_partido","equipo",
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

# Convertir a variable numerica
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
partido$etapa <- etapa
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

#Torneo
partido <- partido[, c("torneo","etapa","ronda","fase_partido","temporada","fecha","equipolocal","equipovisitante","goles_totales_local","goles_totales_visita",
                       "goles_local","goles_visita","posesion_toques_local","posesion_toques_visita","posesion_local","posesion_visita",
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

# Comprobar quien es el equipo local y el equipo visitante
equipo_local
equipo_visitante
torneo


####################
## Exportar Bases ##
####################

# Equipos
setwd("C:/Users/leca_/OneDrive/Documentos/Data Analysis/Futbol/Champions League/Bases/Equipos")
# Equipo Local
write.xlsx(equipolocal,"equipolocal_plantilla.xlsx")
write.xlsx(equipolocal_portero,"equipolocal_porteros.xlsx")
# Equipo Visitante
write.xlsx(equipovisitante,"equipovisitante_plantilla.xlsx")
write.xlsx(equipovisitante_portero,"equipovisitante_porteros.xlsx")


#Partidos
setwd("C:/Users/leca_/OneDrive/Documentos/Data Analysis/Futbol/Champions League/Bases/General")
write.xlsx(partido,"partido_cl.xlsx")



