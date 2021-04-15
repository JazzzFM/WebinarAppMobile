library(shiny)
library(shinyMobile)
library(apexcharter)
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(forcats)
library(tidyr)
library(glue)
library(purrr)
library(readxl)
library(patchwork)
source("R/Essentialfunctions.R")

contactos_1 <- read_csv("data/18M.csv") %>% mutate(id = 1, fecha = as.Date('2021/03/18'))
contactos_2 <- read_csv("data/20M.csv") %>% mutate(id = 2, fecha = as.Date('2021/03/20'))
contactos_3 <- read_csv("data/26M.csv") %>% mutate(id = 3, fecha = as.Date('2021/03/26'))
contactos_4 <- read_csv("data/08A.csv") %>% mutate(id = 4, fecha = as.Date('2021/04/08'))

reporte_zoom <- dplyr::bind_rows(contactos_1, contactos_2) %>% 
                dplyr::bind_rows(contactos_3) %>% 
                dplyr::bind_rows(contactos_4)

Registros <- dplyr::filter(reporte_zoom, is.na(`Nombre de fuente`), !Nombre %in% c('TEAM EMILIO', 'TEAM', 'test')) %>% 
             dplyr::group_by(`Correo electrónico`, fecha) %>% 
             dplyr::summarise('a' = n()) %>%
             dplyr::group_by(fecha) %>%
             dplyr::summarise('Registrados' = n()) 

MAXWEB <- dplyr::filter(reporte_zoom, Asistió == 'Sí',  is.na(`Nombre de fuente`), !Nombre %in% c('TEAM EMILIO', 'TEAM', 'test')) %>% 
          dplyr::group_by(`Correo electrónico`, fecha) %>%
          dplyr::summarise('a' = n()) %>% 
          dplyr::group_by(fecha) %>%
          dplyr::summarise('Sí asistieron' = n()) 

NOWEB <- dplyr::filter(reporte_zoom, Asistió == 'No', !Nombre %in% c('TEAM EMILIO', 'TEAM', 'test')) %>% 
         dplyr::group_by(`Correo electrónico`, fecha) %>% 
         dplyr::summarise('a' = n()) %>% 
         dplyr::group_by(fecha) %>%
         dplyr::summarise('No asistieron' = n()) 

historico <- dplyr::left_join(Registros, MAXWEB, by = "fecha") %>% dplyr::left_join(NOWEB, by = "fecha")
longer_historico <- pivot_longer(historico, Registrados:`No asistieron`, names_to = "categoria", values_to = "count")


 M <- readxl::read_excel("data/Metricas.xlsx", na = "---") %>% select(-"...2")

shinyApp(
    ui = f7Page(
        title = "Webinar Metrics App Mobile",
        f7SingleLayout(
            navbar = f7Navbar(
                title = "Análisis del Webinar",
                hairline = TRUE,
                shadow = TRUE
            ),
            # main content
            f7Shadow(
                intensity = 20,
                hover = TRUE,
                f7Card(
                    title = "Retención de audiencia",
                        plotOutput("Atencion")
                )
            ),
            f7Shadow(
                intensity = 20,
                hover = TRUE,
                f7Card(
                    title = "Porcentaje de asistencia",
                    plotOutput("porcentaje")
                )
            ),
            f7Shadow(
                intensity = 20,
                hover = TRUE,
                f7Card(
                    title = "Porcentaje de asistencia vs no asistencia",
                    plotOutput("porcentajesA")
                )
            ),
            f7Shadow(
                intensity = 20,
                hover = TRUE,
                f7Card(
                    title = "Registros de personas que no asistieron",
                    plotOutput("NoAsistieron")
                )
            ),
            f7Shadow(
                intensity = 20,
                hover = TRUE,
                f7Card(
                    title = "Métricas: Timeline de Ganancias",
                    plotOutput("metricaRegistros")
                )
            ),
            f7Shadow(
                intensity = 20,
                hover = TRUE,
                f7Card(
                    title = "Métricas: Finanzas de Marketing",
                    plotOutput("metricaMarketing")
                )
            )

    )),
    server = function(input, output) {
        
        output$Atencion <- renderPlot({
            horas <- c('Historia previa'='19:30:00', '3 Secretos' = '20:30:00', 'Ronda de preguntas' = '20:40:00', 'Oferta' = '20:50:00')
            retencionAtencion(reporte_zoom, webinar = 4, horas = horas)
        })
        
        output$porcentaje <- renderPlot({
            a <- contactos_4
            a <- a %>% filter(Asistió == "Sí", is.na(`Nombre de fuente`)) %>% 
                group_by(`Correo electrónico`) %>%
                mutate(fecha_ini = min(floor_date(mdy_hms(`Puesto de trabajo`),unit = "minute")),
                       fecha_fin = max(floor_date(mdy_hms(`Hora de salida`),unit = "minute"))
                ) 
            b <- a %>%
                complete(fecha_fin = seq(
                    from=as.POSIXct(min(fecha_ini), tz="UTC"),
                    to=as.POSIXct(max(fecha_fin), tz="UTC"),
                    by="1 min"
                ), fill = list(fecha_fin = NA)) %>% 
                ungroup %>% 
                # filter(grepl(x=`Correo electrónico`,pattern = "forest")) %>%
                distinct(`Correo electrónico`, fecha_fin) %>% 
                count(fecha_fin) %>% mutate(max = n == max(n)) %>% 
                mutate(pct = n/max(n), abs = n, 
                       color_pct = cut(pct,c(0,.25,.5,.75,.9,1),c("(0%-25%]","(25%-50%]","(50%-75%]","(75%-90%]","(90%-100%]")))
            
            # b %>% count(color_pct)
            
            ####
            fecha <- ("2021/04/8")
            horas <- c('Historia previa'='19:30:00', '3 Secretos' = '20:30:00',
                       'Ronda de preguntas' = '20:40:00', 'Oferta' = '20:50:00')
            fechaCompleta <- paste(fecha, horas, sep = " ")
            horas <- as_datetime(fechaCompleta)
            ###
            x <- a %>% pull(`Correo electrónico`) %>% unique %>% length
            b %>% ggplot(aes(x = fecha_fin, y = n, fill=color_pct, color=color_pct)) +
                geom_bar(stat = "identity") + geom_vline(xintercept = horas, linetype="dotted", color = "blue", size = 1.0) +
                annotate(x = as_datetime("2021-04-08 19:00:00"), y = 20.75, geom = "label", hjust = 0.5, label = "Historia previa") + 
                annotate(x = as_datetime("2021-04-08 20:00:00"), y = 20.75, geom = "label", hjust = 0.5, label = "3 Secretos") + 
                annotate(x = as_datetime("2021-04-08 20:30:00"), y = 20.65, geom = "label", hjust = 0.5,label = "Ronda de preguntas") +
                annotate(x = as_datetime("2021-04-08 20:45:00"), y = 15.5, geom = "label", hjust = 0.5, label = "Oferta") + 
                annotate(x = as_datetime("2021-04-08 21:10:00"), y = 10.3, geom = "label", hjust = 0.5, label = "Fin Webinar") +
                annotate(x = min(b$fecha_fin), y = 25, geom = "label", hjust = 0,
                         label = glue(" Asistentes máximos: {max(b$n)} ({count(b,max) %>% filter(max) %>% pull(n)} mins)")) +
                annotate(x = min(b$fecha_fin), y = 23.5, geom = "label", hjust = 0, label = glue(" Asistentes únicos: {x}")) +
                theme_minimal()
            
            # b %>%  
            #     ggplot(aes(x = fecha_fin, y = pct, fill = color_pct)) + geom_bar(stat = "identity") + 
            #     geom_vline(xintercept = horas, linetype="dotted", color = "blue", size = 1.0) +
            #     annotate(x = as_datetime("2021-04-08 19:00:00"), y = 0.75, geom = "label", hjust = 0.5, label = "Historia previa") + 
            #     annotate(x = as_datetime("2021-04-08 20:00:00"), y = 0.75, geom = "label", hjust = 0.5, label = "3 Secretos") + 
            #     annotate(x = as_datetime("2021-04-08 20:30:00"), y = 0.65, geom = "label", hjust = 0.5,label = "Ronda de preguntas") +
            #     annotate(x = as_datetime("2021-04-08 20:45:00"), y = 0.5, geom = "label", hjust = 0.5, label = "Oferta") + 
            #     annotate(x = as_datetime("2021-04-08 21:10:00"), y = 0.3, geom = "label", hjust = 0.5, label = "Fin Webinar") + 
            #     annotate(x = min(b$fecha_fin), y = max(b$pct),geom = "label", hjust = 0,
            #              label = glue(" Asistentes máximos: {max(x)} ({count(b,max) %>% filter(max) %>% pull(n)} mins)")) +
            #     scale_fill_manual(values = rev(colorRamps::green2red(5)) %>% set_names(levels(b$color_pct)),
            #                       name = "% de asistencia") + scale_y_continuous(labels = scales::percent) +
            # 
            #     theme_minimal()
            # 
        })
        
        
        output$porcentajesA <- renderPlot({
            longer_historico %>% filter(categoria != "Registrados") %>% group_by(fecha) %>%  mutate(perc = 100*count/sum(count)) %>% 
                ggplot(aes(x = fecha, y = perc, fill = categoria)) +  ylab("Porcentaje Asistencia vs No Asistencia") +
                geom_bar(stat='identity') + theme_minimal()
            
        })
        
        output$NoAsistieron <- renderPlot({
            bd <- reporte_zoom %>%
                  dplyr::filter(
                         is.na(`Nombre de fuente`),
                         !Nombre %in% c('TEAM EMILIO', 'TEAM', 'test')) %>%
                  dplyr::mutate(
                      horaRegistro = substr(`Hora de registro`, 15, 23),
                      Registro = paste(fecha, horaRegistro, sep = " ") %>% as_datetime()) %>% 
                  dplyr::select(Asistió, Registro)
            
            fechas <- c('2021/03/18 19:00:00', '2021/03/24 12:00:00', '2021/03/26 12:00:00', '2021/04/08 19:00:00') %>% as_datetime()
            
            ggplot(bd, aes(x = Registro, color = Asistió, fill = Asistió)) +
                geom_histogram( position="dodge")+
                geom_vline(xintercept = fechas, linetype="dotted", color = "blue", size = 1.0) +
                annotate(x = fechas[1], y = 40, geom = "label", hjust = 0.5, label = "Primer Webinar") + 
                annotate(x = fechas[2], y = 40, geom = "label", hjust = 0.5, label = "Segundo Webinar") + 
                annotate(x = fechas[3], y = 30, geom = "label", hjust = 0.5, label = "Tercer Webinar") + 
                annotate(x = fechas[4], y = 40, geom = "label", hjust = 0.5, label = "Cuarto Webinar") +
                theme(legend.position="top") +
                theme_minimal()
        })
        
        output$timeline <- renderPlot({
           
        })

        output$metricaRegistros <- renderPlot({
            A <- as_tibble(t(slice(M, 1:7))) %>% select(-V2)
            names <- A %>% slice(1) %>% t
            colnames(names) <- "a"
            names <- names %>% as_tibble() %>% select(a) %>% pull
            colnames(A) <- names
            A <- slice(A, 2:4)
            
            B <- as_tibble(t(slice(M, 9:18))) %>% slice(2:4)
            colnames(B) <- c("Registros", "AsistentesTotales", "porcentAsistencia", "AsistenciaMáxima", "porcentAsistenciaMáxima",
                             "$CostoXlead", "VentasLegacy", "$VentasWebinar", "porcentConversiónLegacy", "$Ganancia")
            B <- select(B, c("Registros", "AsistentesTotales", "AsistenciaMáxima", "VentasLegacy"))
            
            bd <- A %>%
                mutate(
                    Fecha = as.numeric(Fecha),
                    `Pauta Branding` = as.numeric(`Pauta Branding`), `Pauta Marketing` = as.numeric(`Pauta Marketing`)) %>%
                select(
                    Fecha, `Pauta Branding`, `Pauta Marketing`, `Incentivo(AppleWatch)` = `Incentivo (Apple Watch)\r\n4,998 (Ch) y 5,598 (Gde)`) %>%
                mutate(`Incentivo(AppleWatch)` = case_when( is.na(`Incentivo(AppleWatch)`) ~ 0, !is.na(`Incentivo(AppleWatch)`) ~ as.numeric(`Incentivo(AppleWatch)`))) %>%
                bind_cols(B) %>% mutate(
                    AsistentesTotales = as.numeric(AsistentesTotales), Registros = as.numeric(Registros),
                    AsistenciaMáxima = as.numeric(AsistenciaMáxima), VentasLegacy = as.numeric(VentasLegacy),
                    ultimaActualizaciónn = c('2021/03/18', '2021/03/24', '2021/03/26'),
                    Comentarios = c('Primer webinar', 'Segundo webinar', 'Tercer webinar'),
                    ventaSeguimiento = c(0,0,0),
                    costo = c(6997, 6997, 3997)
                )
            
            BD <- procesamiento_metricas(bd)
            bd_1 <- longer_historico %>% filter(categoria == "Registrados")
            bd_2 <- tibble(fecha = bd_1 %>% pull(fecha), categoria = "Ganancias", count = c(BD %>% pull(`$Ganancia`), NA))
            
            bd <- dplyr::bind_rows(bd_1, bd_2)
            
            ggplot(bd, aes(x = fecha, y = count, fill = categoria, color = categoria)) +
                geom_line() + geom_point(alpha = 1.5)+
                theme_minimal()
        })
        
        output$metricaMarketing <- renderPlot({
            A <- as_tibble(t(slice(M, 1:7))) %>% select(-V2)
            names <- A %>% slice(1) %>% t
            colnames(names) <- "a"
            names <- names %>% as_tibble() %>% select(a) %>% pull
            colnames(A) <- names
            A <- slice(A, 2:4)

            B <- as_tibble(t(slice(M, 9:18))) %>% slice(2:4)
            colnames(B) <- c("Registros", "AsistentesTotales", "porcentAsistencia", "AsistenciaMáxima", "porcentAsistenciaMáxima",
                             "$CostoXlead", "VentasLegacy", "$VentasWebinar", "porcentConversiónLegacy", "$Ganancia")
            B <- select(B, c("Registros", "AsistentesTotales", "AsistenciaMáxima", "VentasLegacy"))

            bd <- A %>%
                mutate(
                    Fecha = as.numeric(Fecha),
                    `Pauta Branding` = as.numeric(`Pauta Branding`), `Pauta Marketing` = as.numeric(`Pauta Marketing`)) %>%
                select(
                    Fecha, `Pauta Branding`, `Pauta Marketing`, `Incentivo(AppleWatch)` = `Incentivo (Apple Watch)\r\n4,998 (Ch) y 5,598 (Gde)`) %>%
                mutate(`Incentivo(AppleWatch)` = case_when( is.na(`Incentivo(AppleWatch)`) ~ 0, !is.na(`Incentivo(AppleWatch)`) ~ as.numeric(`Incentivo(AppleWatch)`))) %>%
                bind_cols(B) %>% mutate(
                    AsistentesTotales = as.numeric(AsistentesTotales), Registros = as.numeric(Registros),
                    AsistenciaMáxima = as.numeric(AsistenciaMáxima), VentasLegacy = as.numeric(VentasLegacy),
                    ultimaActualizaciónn = c('2021/03/18', '2021/03/24', '2021/03/26'),
                    Comentarios = c('Primer webinar', 'Segundo webinar', 'Tercer webinar'),
                    ventaSeguimiento = c(0,0,0),
                    costo = c(6997, 6997, 3997)
                )

            BD <- procesamiento_metricas(bd)
            graficar_pautas(BD)
        })
        
})

