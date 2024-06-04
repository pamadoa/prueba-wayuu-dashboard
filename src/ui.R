# -----------------------------------
# Autores/as: PAA
# Responsables: PAA
# Fecha: Junio 2024
# -----------------------------------
# prueba_wayuu/src/dashboard-prueba.R

pacman::p_load(shiny, shinydashboard, ggplot2,
               dplyr, tibble, leaflet, DT, shinyWidgets)

# Creando una base de datos cualquiera

cantidad <- 1000

# Generar datos ficticios
data_wayuu <- tibble(rancheria = 1:cantidad,
                     fecha_recib_info = sample(seq(as.Date('2024-03-06'), 
                                                   as.Date('2024-12-12'),
                                                   by = "day"), cantidad, 
                                               replace = TRUE),
                     municipio = sample(c("Uribia", "Manaure", "Maicao", "Riohacha"),
                                        cantidad, replace = TRUE),
                     tiene_informacion = sample(0:1, cantidad, replace = TRUE),
                     autoridad_indigena = sample(0:1, cantidad, replace = TRUE),
                     mas_de_una_autoridad = sample(0:1, cantidad, replace = TRUE),
                     tipo_autoridad = sample(1:4, cantidad, replace = TRUE), 
                     fecha_nacimiento = sample(seq(as.Date('1900-01-01'), 
                                                   as.Date('2024-03-06'),
                                                   by = "day"), cantidad, 
                                               replace = TRUE),
                     edad_encuestado = sample(0:100, cantidad, replace = TRUE), 
                     sexo_encuestado = sample(c("Mujer", "Hombre", "No sabe"), 
                                              cantidad, replace = TRUE),
                     ingreso_mensual_encuestado = sample(0:50000, cantidad, 
                                                         replace = TRUE),
                     idioma_hablado_hogar = sample(c("A", "B", "C", "D", "E"),
                                                   cantidad, replace = TRUE),
                     numero_de_hijos = sample(0:20, cantidad, replace = TRUE),
                     acceso_agua_potable = sample(c("Sí", "No"), cantidad, 
                                                  replace = TRUE),
                     acceso_servicio_salud = sample(c("Sí", "No"), cantidad, 
                                                    replace = TRUE),
                     lat = runif(1000, 11.5, 12.5), 
                     lon = runif(1000, -72, -71))


print(data_wayuu)


porcentaje_variable_filtros <- data_wayuu %>% 
    summarize(tiene_informacion = mean(tiene_informacion) * 100,
              autoridad_indigena = mean(autoridad_indigena) * 100,
              mas_de_una_autoridad  = mean(mas_de_una_autoridad) * 100) %>% 
    print()

# Interfaz del Dashboard

ui <- dashboardPage(
    dashboardHeader(title = "Dashboard Prueba Wayuu"),
    dashboardSidebar(
        
        # Info que vamos a poner a la parte izquierda del Dashboard
        sidebarMenu(
            menuItem("Indicadores - encuesta", tabName = "indicadores", icon = icon("bar-chart")),
            menuItem("Datos generales", tabName = "data", icon = icon("table")),
            menuItem("Mapa Guajira", tabName = "map", icon = icon("map")),
            menuItem("Información variables de interés", tabName = "informacion", icon = icon("layer-group"))
        ),
        
        # Lista de opciones (municipio, fecha y la variable de interés)
        pickerInput("municipio", "Seleccione el municipio:", choices = unique(data_wayuu$municipio), selected = unique(data_wayuu$municipio), multiple = TRUE),
        dateRangeInput("fecha", "Seleccione el rango de fechas:", start = min(data_wayuu$fecha_recib_info), end = max(data_wayuu$fecha_recib_info)),
        selectInput("variable", "Seleccione la variable:", choices = names(data_wayuu), selected = "tiene_informacion")
    ),
    
    # 
    dashboardBody(
        tabItems(# Secciones del Dashboard
            tabItem(tabName = "indicadores",
                    fluidRow(
                        box(title = "Gráfica de Distribución", status = "primary", solidHeader = TRUE, width = 12, 
                            plotOutput("distPlot"))
                    )
            ),
            tabItem(tabName = "data", 
                    fluidRow(
                        box(title = "Tabla general Wayúu", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12, dataTableOutput("dataTable"))
                    )),
            tabItem(tabName = "map",
                    fluidRow(
                        box(title = "Mapa de municipios de La Guajira", status = "primary", solidHeader = TRUE, width = 12, 
                            leafletOutput("map"))
                    )),
            tabItem(tabName = "informacion",
                    fluidRow(
                        box(title = "Información variables de interés", status = "primary", solidHeader = TRUE, width = 12,
                            selectInput("cruce_var1", "Variable 1:", choices = names(data_wayuu)),
                            selectInput("cruce_var2", "Variable 2:", choices = names(data_wayuu)),
                            plotOutput("crucePlot"))
                    ))
        )
    )
)

# Definir la lógica del servidor (Server)
server <- function(input, output, session) {
    
    # Según municipio
    filtered_data <- reactive({
        data_wayuu %>% 
            filter(municipio %in% input$municipio,
                   fecha_recib_info >= input$fecha[1],
                   fecha_recib_info <= input$fecha[2])
    })
    
    # PArte de la distribución (pestaña distribución)

    output$distPlot <- renderPlot({
        variable <- input$variable
        data <- filtered_data()
        
        if (is.numeric(data[[variable]])) { # Si es numérica
            ggplot(data, aes_string(x = variable)) +
                geom_histogram(fill = "maroon") +
                theme_minimal() +
                labs(title = paste("Distribución de", variable), x = variable, y = "Frecuencia")
        } else { # En caso de que no sea numérica
            ggplot(data, aes_string(x = variable)) +
                geom_bar(fill = "maroon") +
                theme_minimal() +
                labs(title = paste("Distribución de", variable), x = variable, y = "Frecuencia")
        }
    })
    
     # Mostrar la tabla
    output$dataTable <- renderDataTable({
        datatable(filtered_data(), options = list(pageLength = 10))
    })
    
    # Mostrar el mapa, preguntar si tal vez esto es necesario y qué más cosas le podemos meter
    output$map <- renderLeaflet({
        leaflet(filtered_data()) %>%
            addTiles() %>%
            addCircleMarkers(~lon, ~lat, popup = ~paste("Rancheria:", 
                                                        rancheria, "<br>", 
                                                        "Municipio:", 
                                                        municipio), radius = 5, color = "maroon")
    })
    
    output$crucePlot <- renderPlot({
        ggplot(filtered_data(), aes_string(x = input$cruce_var1, fill = input$cruce_var2)) +
            geom_bar(position = "dodge") +
            theme_minimal() +
            theme(axis.title = element_text(size = 8)) +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.55)) +
            theme(plot.title = element_text(size = 12, hjust = 0.5, 
                                            face = "bold", vjust = -1)) +
            theme(plot.subtitle = element_text(size = 10, hjust = 0.5)) +
            theme(plot.caption = element_text(size = 8, hjust = 0.01)) +
            theme(axis.text.x = element_text(size = 11, angle = 90),
                  axis.title.y = element_text(size = 11),
                  axis.ticks.x = element_line(size = 0.1)) +
            labs(title = paste("Cruce de", input$cruce_var1, "y", input$cruce_var2), x = input$cruce_var1, y = "Frecuencia", fill = input$cruce_var2)
    })
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)