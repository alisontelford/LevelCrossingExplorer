#' level_crossing_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_level_crossing_map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        selectizeInput(
          inputId = NS(id, "LX"), 
          label = "Choose a Level Crossing", 
          choices = NULL, 
          selected = character(0),
          options = list(
            maxOptions = 7000, 
            placeholder = 'Please select a level crossing',
            onInitialize = I('function() { this.setValue(""); }')
          )
        ),
        selectizeInput(
          inputId = NS(id, "CrossingTypeFilter"), 
          label = "Filter by Crossing Type", 
          choices = NULL, 
          multiple = TRUE, 
          options = list(
            placeholder = 'Please select a crossing type', 
            onInitialize = I('function() { this.setValue(""); }')
          )
        ),
        actionButton(
          inputId = NS(id, "reset"),
          label = "Reset"
        )
      ),
      mainPanel(
        leaflet::leafletOutput(
          outputId = NS(id, "map"), 
          width="100%", 
          height = 460
        )
      )
    )
  )
}
    
#' level_crossing_map Server Functions
#'
#' @noRd 
mod_level_crossing_map_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    observe({
      if (input$LX == ""){
        updateSelectizeInput(
          session,
          "LX",
          choices = lx_data |>
            dplyr::filter(
              is.null(input$CrossingTypeFilter) | {main_crossing_type %in% input$CrossingTypeFilter}
            ) |>
            dplyr::pull(unique_name) |>
            sort()
        )
      }
    })
    observe({
      if (input$LX == ""){
        updateSelectizeInput(
          session,
          "CrossingTypeFilter",
          choices = lx_data |>
            dplyr::pull(main_crossing_type) |>
            sort()
        )
      } else {
        updateSelectizeInput(
          session,
          "CrossingTypeFilter",
          selected = lx_data |> 
            dplyr::filter(unique_name == input$LX) |> 
            dplyr::pull(main_crossing_type),
          choices = lx_data |> 
            dplyr::filter(unique_name == input$LX) |> 
            dplyr::pull(main_crossing_type)
        )
      }
    })
    
    observeEvent(input$reset, {
      updateSelectizeInput(
        session,
        "LX",
        selected = character(0),
        choices = lx_data |>
          dplyr::pull(unique_name) |>
          sort()
      )
      updateSelectizeInput(
        session,
        "CrossingTypeFilter",
        selected = NULL,
        choices = lx_data |> 
          dplyr::pull(main_crossing_type)
      )
    })
    
    output$map = leaflet::renderLeaflet({
      if (input$LX == ""){
        lx_data |> 
          dplyr::filter(is.null(input$CrossingTypeFilter) | {main_crossing_type %in% input$CrossingTypeFilter}) |>
          leaflet::leaflet() |>
          leaflet::addTiles() |>
          leaflet::setView(lng = -5, lat = 55, zoom = 5) |>
          leaflet::addMarkers(
            lng = ~long,
            lat = ~lat,
            popup = ~paste0(
                "<b>Name:</b> ", 
                unique_name,
                "<br/>",
                "<b>Crossing Type:</b> ",
                main_crossing_type,
                "<br/>",
                "<b>Route:</b> ",
                route |> stringr::str_replace_all("_", " ") |> stringr::str_to_title(),
                "<br/>",
                "<b>ELR:</b> ",
                elr,
                "<br/>",
                "<b>Trains per day:</b> ",
                trains_per_day
              ),
            clusterOptions = leaflet::markerClusterOptions()
          )
      }else{
        lx_data |> 
          dplyr::filter(unique_name == input$LX) |> 
          leaflet::leaflet() |>
          leaflet::addTiles() |>
          leaflet::addMarkers(
            lng = ~long,
            lat = ~lat,
            popup = ~paste0(
              "<b>Name:</b> ", 
              unique_name,
              "<br/>",
              "<b>Crossing Type:</b> ",
              main_crossing_type,
              "<br/>",
              "<b>Route:</b> ",
              route |> stringr::str_replace_all("_", " ") |> stringr::str_to_title(),
              "<br/>",
              "<b>ELR:</b> ",
              elr,
              "<br/>",
              "<b>Trains per day:</b> ",
              trains_per_day
            ),
            clusterOptions = leaflet::markerClusterOptions()
          )
      }
    })
  })
}
    
## To be copied in the UI
# mod_level_crossing_map_ui("level_crossing_map_1")
    
## To be copied in the server
# mod_level_crossing_map_server("level_crossing_map_1")
