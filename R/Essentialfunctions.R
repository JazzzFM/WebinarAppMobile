retencion_atencion <- function(bd, horas){
  
  if(bd %>% pull(id) %>% unique >= 4){
  Asist <- dplyr::filter(
    bd, Asistió == 'Sí',
    !Nombre %in% c('TEAM EMILIO', 'TEAM', 'test')) %>% 
    dplyr::mutate(
      correo = as.factor(`Correo electrónico`),
      fecha_ini = floor_date(mdy_hms(`Puesto de trabajo`), unit = "minute"),
      fecha_fin = floor_date(mdy_hms(`Hora de salida`), unit = "minute")) %>% 
    dplyr::group_by(correo) %>%
    dplyr::mutate(fecha_max = max(fecha_fin)) %>% ungroup %>% 
    mutate(correo = fct_reorder(correo, fecha_max)) %>% 
    dplyr::select(correo, fecha,fecha_ini, fecha_fin)
  }else{
    Asist <- dplyr::filter(
      bd, Asistió == 'Sí',
      !Nombre %in% c('TEAM EMILIO', 'TEAM', 'test')) %>% 
      dplyr::mutate(
        correo = as.factor(`Correo electrónico`),
        fecha_ini = gsub("mar.", "", `Puesto de trabajo`),
        fecha_fin = gsub("mar.", "", `Hora de salida`),
        fecha_ini = paste("March ", fecha_ini) %>% mdy_hms() %>% floor_date(unit = "minute"),
        fecha_fin = paste("March ", fecha_fin) %>% mdy_hms() %>% floor_date(unit = "minute")) %>% 
      dplyr::group_by(correo) %>%
      dplyr::mutate(fecha_max = max(fecha_fin)) %>% ungroup %>% 
      mutate(correo = fct_reorder(correo, fecha_max)) %>% 
      dplyr::select(correo, fecha,fecha_ini, fecha_fin)
  }
  
  y_0 <- pull(Asist, correo) %>% unique %>% length
  
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
    annotate(x = horas[1] - (15*60), y = y_0 - 3, geom = "label", hjust = 0.5, label = "Historia previa") +
    annotate(x = horas[2] - (30*60), y = y_0 - 3, geom = "label", hjust = 0.5, label = "3 Secretos") +
    annotate(x = horas[3] - (7*60) , y = y_0 - 3, geom = "label", hjust = 0.5,label = "Oferta") +
    annotate(x = horas[4] - (15*60), y = y_0 - 5, geom = "label", hjust = 0.5, label = "Ronda de preguntas") +
    annotate(x = horas[4], y = y_0 - 8, geom = "label", hjust = 0.5, label = "Fin Webinar") +
    labs(title = "Retención de la audiencia") +
    xlab("Tiempo de Webinar") +
    ylab("Correo de personas") +
    theme_minimal()
  
  return(Graph)
}

timel_pct_audiencia <- function(bd, horas){
  if(bd %>% pull(id) %>% unique >= 4){
  bd <- dplyr::filter(bd,
          Asistió == "Sí", is.na(`Nombre de fuente`)) %>% 
        dplyr::group_by(`Correo electrónico`) %>%
        dplyr::mutate(
          fecha_ini = min(floor_date(mdy_hms(`Puesto de trabajo`),unit = "minute")),
          fecha_fin = max(floor_date(mdy_hms(`Hora de salida`),unit = "minute"))) 
  }else{
    bd <- dplyr::filter(bd,
            Asistió == "Sí", is.na(`Nombre de fuente`)) %>% 
          dplyr::group_by(`Correo electrónico`) %>%
          dplyr::mutate(
            fecha_ini = gsub("mar.", "", `Puesto de trabajo`),
            fecha_fin = gsub("mar.", "", `Hora de salida`),
            fecha_ini = paste("March ", fecha_ini) %>% mdy_hms() %>% floor_date(unit = "minute"),
            fecha_fin = paste("March ", fecha_fin) %>% mdy_hms() %>% floor_date(unit = "minute")) 
  }
  
  df <- bd %>%
        complete(fecha_fin = seq(
          from=as.POSIXct(min(fecha_ini), tz="UTC"),
          to=as.POSIXct(max(fecha_fin), tz="UTC"),
          by="1 min"), fill = list(fecha_fin = NA)) %>% 
        ungroup %>% 
        # filter(grepl(x=`Correo electrónico`,pattern = "forest")) %>%
        distinct(`Correo electrónico`, fecha_fin) %>% 
        count(fecha_fin) %>% mutate(max = n == max(n)) %>% 
        mutate(pct = n/max(n), abs = n, color_pct = cut(pct,c(0,.25,.5,.75,.9,1), 
          c("(0%-25%]","(25%-50%]","(50%-75%]","(75%-90%]","(90%-100%]")))
  
  fechaWeb <- pull(bd, fecha)
  fechaCompleta <- paste(fechaWeb, horas, sep = " ")
  horas <- as_datetime(fechaCompleta)
 
  x <- bd %>% pull(`Correo electrónico`) %>% unique %>% length
  
  Graph <- df %>% ggplot(aes(x = fecha_fin, y = n, fill=color_pct, color=color_pct)) +
      geom_bar(stat = "identity") +
      geom_vline(xintercept = horas, linetype="dotted", color = "blue", size = 1.0) +
      annotate(x = horas[1] - (15*60), y = x, geom = "label", hjust = 0.5, label = "Historia previa") +
      annotate(x = horas[2] - (30*60), y = x, geom = "label", hjust = 0.5, label = "3 Secretos") +
      annotate(x = horas[3] - (5*60) , y = x , geom = "label", hjust = 0.5,label = "Oferta") +
      annotate(x = horas[4] - (15*60), y = x-1, geom = "label", hjust = 0.5, label = "Ronda de preguntas") +
      annotate(x = horas[4], y = x - 3, geom = "label", hjust = 0.5, label = "Fin Webinar") +
      annotate(x = min(df$fecha_fin), y = x + 4, geom = "label", hjust = 0,
             label = glue(" Asistentes máximos: {max(df$n)} ({count(df,max) %>% filter(max) %>% pull(n)} mins)")) +
    annotate(x = min(df$fecha_fin), y = x + 2, geom = "label", hjust = 0, label = glue(" Asistentes únicos: {x}")) +
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