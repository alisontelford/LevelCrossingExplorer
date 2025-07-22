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
          choices = LXNames, 
          selected = LX,
          options = list(
            maxOptions = 7000, 
            placeholder = 'Please select a level crossing'
          )
        ),
        selectizeInput(
          inputId = NS(id, "CrossingTypeFilter"), 
          label = "Filter by Crossing Type", 
          choices = sort(as.vector(unique(Main$Main.Crossing.Type))), 
          selected = CrossingType,
          multiple = TRUE, 
          options = list(placeholder = 'Please select a crossing type')
        ),
        selectizeInput(
          inputId = NS(id, "ELRFilter"), 
          label = "Filter by ELR", 
          choices = unique(Main$ELR), 
          multiple = TRUE, 
          selected = ELR, 
          options = list(placeholder = 'Please select an ELR')
        ),
        numericInput(
          inputId = NS(id, "RenewalDate"), 
          label = "Renewal Year", 
          value = as.numeric(format(Sys.Date(), "%Y")), 
          min = as.numeric(format(Sys.Date(), "%Y")), 
          max = as.numeric(format(Sys.Date(), "%Y")) + 35),
        textInput(
          inputId = NS(id, "FWI"), 
          label = "Current FWI Score (Change if necessary)", 
          value = FWI
        ),
      ),
      mainPanel(
        leafletOutput(
          outputId = NS(id, "Map"), 
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
    output[[paste0("Map", x)]] = renderLeaflet({
      if (is.null(getSelectedCrossing(input[[paste0("LX", x)]])) == TRUE){
        m <- leaflet() %>%
          addTiles() %>%
          setView(lng = -5, lat = 55, zoom = 5) %>%
          addMarkers(
            lng = Main$Long[getFilteredCrossings(input[[paste0("LX", x)]], input[[paste0("CrossingTypeFilter", x)]], input[[paste0("ELRFilter", x)]])], 
            lat = Main$Lat[getFilteredCrossings(input[[paste0("LX", x)]], input[[paste0("CrossingTypeFilter", x)]], input[[paste0("ELRFilter", x)]])], 
            popup = Main$Unique.Name[getFilteredCrossings(input[[paste0("LX", x)]], input[[paste0("CrossingTypeFilter", x)]], input[[paste0("ELRFilter", x)]])], 
            clusterOptions = markerClusterOptions()
          )
        m
      }else{
        lng = Main$Long[getSelectedCrossing(input[[paste0("LX", x)]])] 
        lat = Main$Lat[getSelectedCrossing(input[[paste0("LX", x)]])]
        m <- leaflet() %>%
          addTiles() %>%
          setView(lng = lng, lat = lat, zoom = 15) %>%
          addMarkers(
            lng = lng, 
            lat = lat, 
            popup = input[[paste0("LX", x)]], 
            clusterOptions = markerClusterOptions()
          )
        m
      }
    })
  })
}
    
## To be copied in the UI
# mod_level_crossing_map_ui("level_crossing_map_1")
    
## To be copied in the server
# mod_level_crossing_map_server("level_crossing_map_1")
