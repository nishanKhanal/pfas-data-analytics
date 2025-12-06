library(tidyverse)
library(shiny)
library(leaflet)
library(plotly)
library(tigris)

options(tigris_use_cache = TRUE)  
options(tigris_class = "sf")


samples_site_df <- read_csv("data/samples_site.csv")

# Sites and their sample locations
sites_dash <- samples_site_df |>
  mutate(site = nearest_facility,
         lat = latitude.y,
         lon = longitude.y) |>
  group_by(site, lat, lon) |>
  summarize(max_hazard_index = max(hazard_index, na.rm = TRUE)) |>
  select(site, lat, lon, max_hazard_index) |>
  dplyr::distinct()

samples_dash <- samples_site_df |>
  mutate(site = nearest_facility,
         sample_name = system_name,
         lat = latitude.x,
         lon = longitude.x) |>
  select(site, sample_name, lat, lon, pfhxs_result, pfna_result, hfpoda_result, pfbs_result, hazard_index)

mi_counties <- counties(state="MI", cb = TRUE, year = 2024, class = "sf")

#Plots
dist_hazard <- samples_site_df |>
  ggplot( aes(hazard_index)) +
  geom_histogram(color = "black", fill = "#F0E442", bins = 50) +
  scale_x_continuous(limits =  c(0,38)) +
  scale_y_continuous(limits =  c(0,10)) +
  theme_minimal() +
  labs(title = "Distribution of Hazard Index",
       x = "Hazard Index",
       y = "Count",
       caption = "EGLE website: Michigan PFAS data") +
  theme(text = element_text(face = "bold"))

bar_date <- samples_site_df |>
  mutate(month_name = month(sample_date, label = TRUE, abbr = FALSE)) |>
  filter(!is.na(month_name)) |>
  count(month_name) |>
  ggplot(aes(x = month_name, y = n)) +
  geom_col(color = "black",fill = "#56B4E9") + # #56B4E9 is color blind friendly blue
  labs(
    title = "Sampling Effort Over Time",
    x = "Month",
    y = "Number of Samples",
    caption = "EGLE website: Michigan PFAS data"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 8),
    text = element_text(face = "bold")
  )

# UI
ui <- fluidPage(
  titlePanel("Michigan PFAS Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Instructions"),
      p("Click on a site marker to zoom in and view samples for that site."),
      actionButton("reset_view", "Reset Map View", 
                   class = "btn-primary", 
                   style = "margin-top: 10px;"),
      actionButton("view_report", "View Analysis Report",
                   class = "btn-info",
                   style = "margin-top: 10px;"),
      hr(),
      uiOutput("site_info")
    ),
    
    mainPanel(
      width = 8,
      leafletOutput("main_map", height = "400px"),
      hr(),
      fluidRow(
        column(6,
               conditionalPanel(
                 condition = "output.has_nonzero_samples",
                 plotlyOutput("plot1", height = "350px")
               ),
               conditionalPanel(
                 condition = "!output.has_nonzero_samples",
                 div(style = "padding: 50px; text-align: center;",
                     h4("All samples from this site are safe (Hazard Index = 0)"))
               )
        ),
        column(6,
               conditionalPanel(
                 condition = "output.has_nonzero_samples",
                 plotlyOutput("plot2", height = "350px")
               )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive value to store selected site
  selected_site <- reactiveVal(NULL)
  
  sites_hazard_color <- function(hazard_index) {
    sapply(hazard_index, function(hi) {
      if (hi == 0) {
        return("#c0c0c0")  # Blue for zero
      } else if (hi > 0 && hi <= 1) {
        return("#FFE08F")  # Yellow for 0-1
      } else {
        # Gradient from orange to red for values > 1
        max_hi <- max(sites_dash$max_hazard_index, na.rm = TRUE)
        ratio <- min((hi - 1) / (max_hi - 1), 1)
        
        colorRampPalette(c("#FF6C0C", "red"))(100)[round(ratio * 99) + 1]
      }
    })
  }
  
  samples_hazard_color <- function(hazard_index) {
    sapply(hazard_index, function(hi) {
      if (hi == 0) {
        return("#000000")  # Black for zero for visibility
      } else if (hi > 0 && hi <= 1) {
        return("#FFE08F")  # Yellow for 0-1
      } else {
        # Gradient from orange to red for values > 1
        max_hi <- max(samples_dash$hazard_index, na.rm = TRUE)
        ratio <- min((hi - 1) / (max_hi - 1), 1)
        
        colorRampPalette(c("#FF6C0C", "red"))(100)[round(ratio * 99) + 1]
      }
    })
  }
  
  
  # Initial map render
  output$main_map <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      clearControls() |> # Clear old legend first
      addLegend(
        position = "bottomright",
        colors = c("#c0c0c0", "#000000", "#FFE08F", "#FF6C0C", "red"),
        labels = c("HI = 0 (site)", "HI = 0 (sample)", "0 < HI ≤ 1", "HI > 1 (lower)", "HI > 1 (higher)"),
        title = "Hazard Index",
        opacity = 1
      ) |>
      addPolygons(
        data = mi_counties,
        color = "#A2AF9B",
        weight = 1,
        fillOpacity = 0,
        label = ~NAME
      ) |>
      addCircleMarkers(
        data = sites_dash,
        lng = ~lon, 
        lat = ~lat, 
        layerId = ~site,
        radius = 6,
        color = ~sites_hazard_color(max_hazard_index),
        fillColor = ~sites_hazard_color(max_hazard_index),
        fillOpacity = 0.5,
        popup = ~paste("<b>", site, "</b><br>Click to view samples")
      ) |>
      setView(
        lng = mean(c(min(sites_dash$lon), max(sites_dash$lon))),
        lat = mean(c(min(sites_dash$lat), max(sites_dash$lat))),
        zoom = 6  
      )
  })
  
  # Handle report viewing
  observeEvent(input$view_report, {
    showModal(modalDialog(
      title = "Analysis Report",
      size = "l",  # large modal
      easyClose = TRUE,
      footer = modalButton("Close"),
      tags$iframe(
        src = "project_report.html",
        width = "100%",
        height = "600px",
        frameborder = 0
      )
    ))
  })
  
  # Handle site marker clicks
  observeEvent(input$main_map_marker_click, {
    click <- input$main_map_marker_click
    
    if (!is.null(click$id)) {
      site_name <- click$id
      selected_site(site_name)
      
      # Get site and sample data
      selected_site_data <- sites_dash |> filter(site == site_name) # only one
      sample_dash_subset <- samples_dash |> filter(site == site_name)
      
      # Update map: zoom to site and show samples
      map_proxy <- leafletProxy("main_map") |>
        clearMarkers() |>
        clearShapes() |>
        clearControls() |> # Clear old legend first
        addLegend(
          position = "bottomright",
          colors = c("#c0c0c0", "#000000", "#FFE08F", "#FF6C0C", "red"),
          labels = c("HI = 0 (site)", "HI = 0 (sample)", "0 < HI ≤ 1", "HI > 1 (lower)", "HI > 1 (higher)"),
          title = "Hazard Index",
          opacity = 1
        ) |>
        addPolygons(
          data = mi_counties,
          color = "#A2AF9B",
          weight = 1,
          fillOpacity = 0,
          label = ~NAME
        ) |>
        addCircleMarkers(
          data = sites_dash,
          lng = ~lon, 
          lat = ~lat, 
          layerId = ~site,
          radius = 6,
          color = ~sites_hazard_color(max_hazard_index),
          fillColor = ~sites_hazard_color(max_hazard_index),
          fillOpacity = 0.5,
          popup = ~paste("<b>", site, "</b><br>Click to view samples")
        ) |>
        addCircleMarkers(
          data = sample_dash_subset,
          lng = ~lon,
          lat = ~lat,
          radius = 2,
          color = ~samples_hazard_color(hazard_index),
          fillColor = ~samples_hazard_color(hazard_index),
          fillOpacity = 1,
          popup = ~paste("<b>", sample_name, "</b><br>",
                         "Hazard Index:", round(hazard_index, 2))
        )
      # Add lines connecting site to each sample
      for (i in 1:nrow(sample_dash_subset)) {
        map_proxy <- map_proxy |>
          addPolylines(
            lng = c(selected_site_data$lon, sample_dash_subset$lon[i]),
            lat = c(selected_site_data$lat, sample_dash_subset$lat[i]),
            color = "gray",
            weight = 2,
            opacity = 0.5,
            dashArray = "5, 5"
          )
      }
      
      map_proxy |>
        fitBounds(
          lng1 = min(c(sample_dash_subset$lon,selected_site_data$lon), na.rm = TRUE),
          lat1 = min(c(sample_dash_subset$lat,selected_site_data$lat), na.rm = TRUE),
          lng2 = max(c(sample_dash_subset$lon,selected_site_data$lon), na.rm = TRUE),
          lat2 = max(c(sample_dash_subset$lat,selected_site_data$lat), na.rm = TRUE)
        )
    }
  })
  
  # Reset map view
  observeEvent(input$reset_view, {
    selected_site(NULL)
    
    leafletProxy("main_map") |>
      clearMarkers() |>
      addTiles() |>
      clearControls() |> # Clear old legend first
      addLegend(
        position = "bottomright",
        colors = c("#c0c0c0", "#000000", "#FFE08F", "#FF6C0C", "red"),
        labels = c("HI = 0 (site)", "HI = 0 (sample)", "0 < HI ≤ 1", "HI > 1 (lower)", "HI > 1 (higher)"),
        title = "Hazard Index",
        opacity = 1
      ) |>
      addCircleMarkers(
        data = sites_dash,
        lng = ~lon, 
        lat = ~lat, 
        layerId = ~site,
        radius = 6,
        color = ~sites_hazard_color(max_hazard_index),
        fillColor = ~sites_hazard_color(max_hazard_index),
        fillOpacity = 0.5,
        popup = ~paste("<b>", site, "</b><br>Click to view samples")
      ) |>
      setView(
        lng = mean(c(min(sites_dash$lon), max(sites_dash$lon))),
        lat = mean(c(min(sites_dash$lat), max(sites_dash$lat))),
        zoom = 6  
      )
  })
  
  # Site info panel
  output$site_info <- renderUI({
    if (is.null(selected_site())) {
      div(
        h5("No site selected"),
        p("Click on a site marker to view details.")
      )
    } else {
      site_samples <- samples_dash |> filter(site == selected_site())
      div(
        h4(paste("Site:", selected_site())),
        p(strong("Number of samples:"), nrow(site_samples)),
        p(strong("Min hazard index:"), round(min(site_samples$hazard_index, na.rm=TRUE), 2)),
        p(strong("Max hazard index:"), round(max(site_samples$hazard_index, na.rm=TRUE), 2)),
        p(strong("Mean hazard index:"), round(mean(site_samples$hazard_index, na.rm=TRUE), 2)),
        p(strong("Standard deviation hazard index:"), round(sd(site_samples$hazard_index, na.rm=TRUE), 2)),
        p(strong("Median hazard index:"), round(median(site_samples$hazard_index, na.rm=TRUE), 2)),
      )
    }
  })
  
  # Add this reactive to check for non-zero samples
  output$has_nonzero_samples <- reactive({
    if (is.null(selected_site())) {
      return(TRUE)  # Show default plots when no site selected
    }
    
    nonzero_count <- samples_dash %>%
      filter(site == selected_site() & hazard_index > 0) %>%
      nrow()
    
    return(nonzero_count > 0)
  })
  
  outputOptions(output, "has_nonzero_samples", suspendWhenHidden = FALSE)
  
  # First plot
  output$plot1 <- renderPlotly({
    if (is.null(selected_site())) {
      
      first_plot <- dist_hazard
      
    } else {
      first_plot <- samples_dash |> 
        filter(site == selected_site()) |>
        ggplot(aes(y = hazard_index)) + 
        geom_boxplot() +
        labs(title = str_glue("Distribution of Hazard Index for {selected_site()}"),
             y = "Hazard Index")
      
    }
    ggplotly(first_plot) |>
      layout(
        xaxis = list(autorange = TRUE),
        yaxis = list(autorange = TRUE)
      )
  })
  
  # Second Plot
  output$plot2 <- renderPlotly({
    if (is.null(selected_site())) {
      
      second_plot <- bar_date
      
    } else {
      samples_dash_scaled <- samples_dash |>
        filter(site == selected_site() & hazard_index > 0) |>
        mutate(pfhxs_result = pfhxs_result/10,
               pfna_result = pfna_result/10,
               hfpoda_result = hfpoda_result/10,
               pfbs_result = pfbs_result/2000) |>
        pivot_longer(cols = ends_with("result"), 
                     names_to = "factor", 
                     values_to = "value") |>
        group_by(sample_name)
      
      
      # Plot
      second_plot <- ggplot(samples_dash_scaled, aes(x = sample_name, y = value, fill = factor)) +
        geom_col(position = "stack") +
        scale_fill_brewer(palette = "Set2") +
        coord_flip() + 
        labs(title = "Hazard Index Composition by Sample",
             subtitle = "For samples with non-zero hazard index",
             y = "Hazard Index",
             x = "Sample Name",
             fill = "GenX Chemicals") + 
        theme(legend.position = "bottom")
    }
    
    ggplotly(second_plot) |>
      layout(legend = list(
        orientation = "h",  # horizontal
        x = 0.5,            # center horizontally
        xanchor = "center",
        y = -0.4            # position below plot
      ))
  })
}

# Run the app
shinyApp(ui, server)