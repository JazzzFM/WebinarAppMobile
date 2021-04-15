retencionAtencion <- function(bd, webinar, horas){
  Asist <- dplyr::filter(
    bd, id == {{webinar}}, Asistió == 'Sí', is.na(`Nombre de fuente`),
    !Nombre %in% c('TEAM EMILIO', 'TEAM', 'test')) %>% 
    dplyr::mutate(
      correo = as.factor(`Correo electrónico`),
      fecha_ini = mdy_hms(`Puesto de trabajo`),
      fecha_fin = mdy_hms(`Hora de salida`) ) %>% 
    dplyr::group_by(correo) %>%
    dplyr::mutate(fecha_max = max(fecha_fin)) %>% ungroup %>% 
    mutate(correo = fct_reorder(correo, fecha_max)) %>% 
    dplyr::select(correo, fecha,fecha_ini, fecha_fin)
  
fechaWeb <- pull(Asist, fecha)
fechaCompleta <- paste(fechaWeb, horas, sep = " ")
horas <- as_datetime(fechaCompleta)

Graph <- Asist %>%  
    ggplot() +
    geom_segment(
      aes(y = correo, yend = correo, x = fecha_ini, xend = fecha_fin),
      lineend = 'round', linejoin = 'round',
      size = 1, arrow = arrow(length = unit(0.05, "inches")))+ 
    geom_vline(xintercept = horas, linetype="dotted", color = "blue", size = 1.0) +
    annotate(x = as_datetime("2021-04-08 19:00:00"), y = 31.5, geom = "label", hjust = 0.5, label = "Historia previa") + 
    annotate(x = as_datetime("2021-04-08 20:00:00"), y = 28.5, geom = "label", hjust = 0.5, label = "3 Secretos") + 
    annotate(x = as_datetime("2021-04-08 20:30:00"), y = 25.5, geom = "label", hjust = 0.5,label = "Ronda de preguntas") +
    annotate(x = as_datetime("2021-04-08 20:45:00"), y = 10.5, geom = "label", hjust = 0.5, label = "Oferta") + 
    annotate(x = as_datetime("2021-04-08 21:10:00"), y = 5.5, geom = "label", hjust = 0.5, label = "Fin Webinar") +
    #labs(title = "Retención de la audiencia") +
    xlab("Tiempo de Webinar") +
    ylab("Correo de personas") +
    theme_minimal()
  
  return(Graph)
}

procesamiento_metricas <- function(bd){
  
  df <- bd %>% mutate(
    `Total Pauta` = `Pauta Branding` + `Pauta Marketing`,
    `Total Publicidad` = `Total Pauta` + `Incentivo(AppleWatch)`,
    porcentAsistencia = 100*AsistentesTotales/Registros,
    porcentAsistenciaMáxima = 100*AsistenciaMáxima/Registros,
    `$CostoXlead` = `Total Pauta`/Registros,
    porcentConversiónLegacy = VentasLegacy/AsistenciaMáxima,
    `$VentasWebinar` = VentasLegacy*costo,
    `$Ganancia` = `$VentasWebinar`- `Total Publicidad`,
    ventaSeguimiento = c(0,0,0),
    ComisionesVentas = ventaSeguimiento*0.7*costo,
    porcentConversiónSeguimiento = ventaSeguimiento/AsistenciaMáxima,
    `$VentatotalSeguimiento` = ventaSeguimiento*costo,
    VentasTotales = ventaSeguimiento + VentasLegacy,
    porcentConversiónTotal = porcentConversiónSeguimiento + porcentConversiónLegacy,
    `$VentaTotal` = `$VentasWebinar` + `$VentatotalSeguimiento`,
    UtilidadNeta = `$VentaTotal` - ComisionesVentas - `Total Publicidad`,
    ROAS = `$VentaTotal`/`Total Pauta`
  )
  
  return(df)
}


graficar_pautas <- function(bd){
  p1 <- ggplot(bd, aes(x = Fecha, y = `Total Publicidad`)) + geom_path() + geom_point() + theme_minimal()
  p2 <- ggplot(bd, aes(x = Fecha, y = `Pauta Branding`)) + geom_path() + geom_point() + theme_minimal()
  p3 <- ggplot(bd, aes(x = Fecha, y = `Pauta Marketing`)) + geom_path() + geom_point() + theme_minimal()
  Graph <- p3 / p2 / p1 + plot_layout(heights = c(3,3,3))
  return(Graph)
}

graficar_registros <- function(bd){
  bd <- bd %>% select(Fecha, Registros, '$Ganancia')

  Graph <- ggplot() + 
    geom_line(data = bd, aes(x = Fecha, y = `$Ganancia`), color = "green") +
    theme_minimal()
  
  return(Graph)
}