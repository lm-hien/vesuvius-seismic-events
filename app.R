library(shiny)
library(bslib)
library(dplyr)

# Load the TidyTuesday Vesuvius data
vesuvius <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-13/vesuvius.csv')
# Prepare data for plotting
vesuvius <- vesuvius %>%
  mutate(
    datetime = as.POSIXct(time),
    magnitude = duration_magnitude_md  # Use the correct magnitude column
  ) %>%
  filter(!is.na(magnitude), !is.na(latitude), !is.na(longitude))  # Remove rows with missing key data

# Get year range for slider and fixed axis ranges
year_min <- min(vesuvius$year, na.rm = TRUE)
year_max <- max(vesuvius$year, na.rm = TRUE)
lat_range <- range(vesuvius$latitude, na.rm = TRUE)
lon_range <- range(vesuvius$longitude, na.rm = TRUE)

ui <- page_sidebar(
  title = "Mount Vesuvius Seismic Events - Geographic Distribution",
  sidebar = sidebar(
    h4("Plot Controls"),
    sliderInput("selected_year", "Select Year:", 
                min = year_min, max = year_max, 
                value = year_min,  # Start with the earliest year
                step = 1, sep = "",
                animate = animationOptions(interval = 700, loop = TRUE)),
    br(),
    sliderInput("point_size", "Point Size:", 
                min = 0.5, max = 3, value = 2, step = 0.1),
    br(),
    checkboxInput("color_by_mag", "Color by Magnitude", value = TRUE)
  ),
  layout_columns(
    col_widths = c(8, 4),
    card(
      card_header("Geographic Distribution of Seismic Events"),
      plotOutput("vesuvius_plot", height = "700px")
    ),
    card(
      card_header("Dataset Summary"),
      div(
        h5("Overall Dataset:"),
        p(paste("Total events in dataset:", nrow(vesuvius))),
        p(paste("Year range:", year_min, "to", year_max)),
        p(paste("Latitude range:", round(min(vesuvius$latitude, na.rm = TRUE), 4), "to", round(max(vesuvius$latitude, na.rm = TRUE), 4))),
        p(paste("Longitude range:", round(min(vesuvius$longitude, na.rm = TRUE), 4), "to", round(max(vesuvius$longitude, na.rm = TRUE), 4))),
        p(paste("Magnitude range:", round(min(vesuvius$magnitude, na.rm = TRUE), 2), "to", round(max(vesuvius$magnitude, na.rm = TRUE), 2)))
      ),
      br(),
      div(
        h5("Selected Year:"),
        textOutput("selected_year_display"),
        textOutput("total_events")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive data filtering
  filtered_data <- reactive({
    data <- vesuvius %>%
      filter(year == input$selected_year)
    
    return(data)
  })
  
  # Dynamic summary outputs
  output$total_events <- renderText({
    paste("Events in selected year:", nrow(filtered_data()))
  })
  
  output$selected_year_display <- renderText({
    paste("Selected year:", input$selected_year)
  })
  
  output$vesuvius_plot <- renderPlot({
    
    data <- filtered_data()
    
    # Set up the plot with fixed axes
    plot(lat_range, lon_range, type = "n", 
         xlim = lon_range + c(-0.001, 0.001),
         ylim = lat_range + c(-0.001, 0.001),
         xlab = "Longitude", ylab = "Latitude",
         main = paste("Geographic Distribution of Mount Vesuvius Seismic Events -", input$selected_year),
         cex.main = 1.2)
    
    if (nrow(data) > 0) {
      if (input$color_by_mag) {
        # Color by magnitude using a color palette
        colors <- colorRampPalette(c("#440154", "#31688e", "#35b779", "#fde725"))(100)  # viridis colors
        mag_range <- range(vesuvius$magnitude, na.rm = TRUE)
        color_indices <- round(((data$magnitude - mag_range[1]) / (mag_range[2] - mag_range[1])) * 99) + 1
        point_colors <- colors[color_indices]
      } else {
        point_colors <- "#1f77b4"  # Default blue
      }
      
      # Plot the points
      points(data$longitude, data$latitude, 
             col = point_colors,
             pch = 19,  # Filled circles
             cex = input$point_size)
    }
    
    # Add a color legend if coloring by magnitude
    if (input$color_by_mag && nrow(data) > 0) {
      mag_range <- range(vesuvius$magnitude, na.rm = TRUE)
      legend("topright", 
             legend = c(paste("Min:", round(mag_range[1], 2)), 
                        paste("Max:", round(mag_range[2], 2))),
             col = c("#440154", "#fde725"),
             pch = 19,
             title = "Magnitude",
             cex = 0.8)
    }
  })
}

shinyApp(ui = ui, server = server)
