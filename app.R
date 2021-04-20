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


shinyApp(
    ui = f7Page(
        title = "Webinar Metrics App Mobile",
        f7SingleLayout(
            navbar = f7Navbar(
                title = "Análisis del Webinar",
                hairline = TRUE,
                shadow = TRUE
            ),
        selectInput("filtro", label = "Seleccione el webinar",
                       choices = c("Primer Webinar" = 1, "Segundo Webinar" = 2, "Tercer Webinar" = 3, "Cuarto Webinar" = 4, "Quinto Webinar" = 5),
                       selected = c("Primer Webinar" = 1, "Segundo Webinar" = 2, "Tercer Webinar" = 3, "Cuarto Webinar" = 4, "Quinto Webinar" = 5)
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
                    title = "Registros de personas",
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
        
        reporte_zoom <- reactiveValues(row = NULL, nota = NULL)
        bd.filtrada <- reactiveValues(row = NULL, nota = NULL)
        financial_metrics <- reactiveValues(row = NULL, nota = NULL)
        horas_web <- reactiveValues(row = NULL, nota = NULL)
        
        reporte_zoom <- reactive({
           reporte_zum <- list.files("data",pattern = ".csv") %>%
                imap(~read_csv(glue::glue("data/{.x}")) %>% 
                mutate(id = .y, fecha = ymd(gsub(".csv", "", .x)))) %>%
                reduce(bind_rows)
            
            return(reporte_zum)
        })

        financial_metrics <- reactive({
        M <- readxl::read_excel("data/Metricas.xlsx", na = "---") %>% select(-"...2")
        return(M)
        })
        
        horas_web <- reactive({
            if(!!input$filtro == 5){
                return(c('Historia previa'='19:24:00', '3 Secretos' = '20:30:00', 'Oferta' = '20:45:00', 'Preguntas' = '21:15:00'))  
            }
            if(!!input$filtro == 4){
                return(c('Historia previa'='19:30:00', '3 Secretos' = '20:30:00', 'Oferta' = '20:50:00', 'Preguntas' = '21:20:00')) 
            }
            if(!!input$filtro == 3){
                return(c('Historia previa'='12:44:00', '3 Secretos' = '13:51:00', 'Oferta' = '14:20:00', 'Preguntas' = '14:42:00')) 
            }
            if(!!input$filtro == 2){
                return(c('Historia previa'='12:20:00', '3 Secretos' = '13:25:00', 'Oferta' = '13:41:00', 'Preguntas' = '14:13:00')) 
            }
            if(!!input$filtro == 1){
                return(c('Historia previa'='19:16:00', '3 Secretos' = '20:02:00', 'Oferta' = '20:22:00', 'Preguntas' = '20:47:00')) 
            }
        })
        
        ################ Gráficas ##############################
        
        output$Atencion <- renderPlot({
            reporte_zoom() %>% filter(id %in% !!input$filtro) %>% retencion_atencion(horas = horas_web())
        })
        
        output$porcentaje <- renderPlot({
            reporte_zoom() %>% filter(id %in% !!input$filtro) %>% timel_pct_audiencia(horas = horas_web())
        })
        
        
        output$porcentajesA <- renderPlot({
            Registros <- dplyr::filter(reporte_zoom(), is.na(`Nombre de fuente`), !Nombre %in% c('TEAM EMILIO', 'TEAM', 'test')) %>% 
                dplyr::group_by(`Correo electrónico`, fecha) %>% 
                dplyr::summarise('a' = n()) %>%
                dplyr::group_by(fecha) %>%
                dplyr::summarise('Registrados' = n()) 
            
            MAXWEB <- dplyr::filter(reporte_zoom(), Asistió == 'Sí',  is.na(`Nombre de fuente`), !Nombre %in% c('TEAM EMILIO', 'TEAM', 'test')) %>% 
                dplyr::group_by(`Correo electrónico`, fecha) %>%
                dplyr::summarise('a' = n()) %>% 
                dplyr::group_by(fecha) %>%
                dplyr::summarise('Sí asistieron' = n()) 
            
            NOWEB <- dplyr::filter(reporte_zoom(), Asistió == 'No', !Nombre %in% c('TEAM EMILIO', 'TEAM', 'test')) %>% 
                dplyr::group_by(`Correo electrónico`, fecha) %>% 
                dplyr::summarise('a' = n()) %>% 
                dplyr::group_by(fecha) %>%
                dplyr::summarise('No asistieron' = n()) 
            
            historico <- dplyr::left_join(Registros, MAXWEB, by = "fecha") %>% dplyr::left_join(NOWEB, by = "fecha")
            longer_historico <- pivot_longer(historico, Registrados:`No asistieron`, names_to = "categoria", values_to = "count")
            
            longer_historico %>% filter(categoria != "Registrados") %>% group_by(fecha) %>%  mutate(perc = 100*count/sum(count)) %>% 
                ggplot(aes(x = fecha, y = perc, fill = categoria)) +  ylab("Porcentaje Asistencia vs No Asistencia") +
                geom_bar(stat='identity') + theme_minimal()
            
        })
        
        output$NoAsistieron <- renderPlot({
            bd <- reporte_zoom() %>%
                  dplyr::filter(
                         is.na(`Nombre de fuente`),
                         !Nombre %in% c('TEAM EMILIO', 'TEAM', 'test')) %>%
                  dplyr::mutate(
                      horaRegistro = substr(`Hora de registro`, 15, 23),
                      Registro = paste(fecha, horaRegistro, sep = " ") %>% as_datetime()) %>% 
                  dplyr::select(Asistió, Registro)
            
            fechas <- c('2021/03/18 19:00:00', '2021/03/20 12:00:00', '2021/03/26 12:00:00', '2021/04/08 19:00:00', '2021/04/15 19:00:00') %>% as_datetime()
            
            ggplot(bd, aes(x = Registro, color = Asistió, fill = Asistió)) +
                geom_histogram( position="dodge")+
                geom_vline(xintercept = fechas, linetype="dotted", color = "blue", size = 1.0) +
                annotate(x = fechas[1], y = 60, geom = "label", hjust = 0.5, label = "Primer Webinar") + 
                annotate(x = fechas[2], y = 50, geom = "label", hjust = 0.5, label = "Segundo Webinar") + 
                annotate(x = fechas[3], y = 40, geom = "label", hjust = 0.5, label = "Tercer Webinar") + 
                annotate(x = fechas[4], y = 40, geom = "label", hjust = 0.5, label = "Cuarto Webinar") +
                annotate(x = fechas[5], y = 40, geom = "label", hjust = 0.5, label = "Quinto Webinar") +
                theme(legend.position="top") +
                theme_minimal()
        })
        

        output$metricaRegistros <- renderPlot({
            A <- as_tibble(t(slice(financial_metrics(), 1:7))) %>% select(-V2)
            names <- A %>% slice(1) %>% t
            colnames(names) <- "a"
            names <- names %>% as_tibble() %>% select(a) %>% pull
            colnames(A) <- names
            A <- slice(A, 2:4)
            
            B <- as_tibble(t(slice(financial_metrics(), 9:18))) %>% slice(2:4)
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
            
            Registros <- dplyr::filter(reporte_zoom(), is.na(`Nombre de fuente`), !Nombre %in% c('TEAM EMILIO', 'TEAM', 'test')) %>% 
                dplyr::group_by(`Correo electrónico`, fecha) %>% 
                dplyr::summarise('a' = n()) %>%
                dplyr::group_by(fecha) %>%
                dplyr::summarise('Registrados' = n()) 
            
            MAXWEB <- dplyr::filter(reporte_zoom(), Asistió == 'Sí',  is.na(`Nombre de fuente`), !Nombre %in% c('TEAM EMILIO', 'TEAM', 'test')) %>% 
                dplyr::group_by(`Correo electrónico`, fecha) %>%
                dplyr::summarise('a' = n()) %>% 
                dplyr::group_by(fecha) %>%
                dplyr::summarise('Sí asistieron' = n()) 
            
            NOWEB <- dplyr::filter(reporte_zoom(), Asistió == 'No', !Nombre %in% c('TEAM EMILIO', 'TEAM', 'test')) %>% 
                dplyr::group_by(`Correo electrónico`, fecha) %>% 
                dplyr::summarise('a' = n()) %>% 
                dplyr::group_by(fecha) %>%
                dplyr::summarise('No asistieron' = n()) 
            
            historico <- dplyr::left_join(Registros, MAXWEB, by = "fecha") %>% dplyr::left_join(NOWEB, by = "fecha")
            longer_historico <- pivot_longer(historico, Registrados:`No asistieron`, names_to = "categoria", values_to = "count")
            

            bd_1 <- longer_historico %>% filter(categoria == "Registrados")
            bd_2 <- tibble(fecha = bd_1 %>% pull(fecha), categoria = "Ganancias", count = c(BD %>% pull(`$Ganancia`), NA, NA))
            
            bd <- dplyr::bind_rows(bd_1, bd_2)
            
            ggplot(bd, aes(x = fecha, y = count, fill = categoria, color = categoria)) +
                geom_line() + geom_point(alpha = 1.5)+
                theme_minimal()
        })
        
        output$metricaMarketing <- renderPlot({
            A <- as_tibble(t(slice(financial_metrics(), 1:7))) %>% select(-V2)
            names <- A %>% slice(1) %>% t
            colnames(names) <- "a"
            names <- names %>% as_tibble() %>% select(a) %>% pull
            colnames(A) <- names
            A <- slice(A, 2:4)

            B <- as_tibble(t(slice(financial_metrics(), 9:18))) %>% slice(2:4)
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

