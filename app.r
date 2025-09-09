# Install required packages (run once)
# install.packages(c("shiny", "DT", "ggplot2", "plotly", "dplyr", "lubridate", "scales", "readr", "networkD3"))

# run the app with 
#  library(shiny); runApp('app.r')


library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(lubridate)
library(scales)
library(readr)
library(networkD3)
library(htmlwidgets)
library(jsonlite)
library(tidyr)
library(googlesheets4)


# Load data from a google sheet
if (file.exists('garden_data.csv')) {
  garden_data <- read_csv('garden_data.csv')
} else {
  gs4_auth('galiwatch.info@gmail.com')
  read_sheet("1WN70BA0_ir_l_pIaaa2McVlJALPbGlkGfi6OqqlRDNc") %>%
    write_csv('garden_data.csv')
}

garden_data = read_csv('garden_data.csv') %>%
    mutate(
      Date = as.Date(Timestamp),
      Time = format(Timestamp, "%H:%M:%S"),
      Flowering = ordered(Flowering, levels = c("Flower buds", "First flowers", ">50% flowering", "Finished flowering", "Onset seed/fruit", ">50% ripe", "nothing")),
      Vegetative = ordered(Vegetative, levels = c("signs of life", "young leaves", "leaves changing colour", ">50% fallen", "nothing"))
    )

# Color match helper and palettes
first_pattern_match <- function(x, patterns, values, default = "#e5e7eb") {
  out <- rep(NA_character_, length(x))
  for (i in seq_along(patterns)) {
    hit <- is.na(out) & grepl(patterns[i], x)
    out[hit] <- values[i]
  }
  out[is.na(out)] <- default
  out
}

flowering_palette <- c(
  "Flower buds" = "#dfc120",
  "First flowers" = "#de8423",
  ">50% flowering" = "#cc5a43",
  "Finished flowering" = "#555d72",
  "Onset seed/fruit" = "#526bb1",
  ">50% ripe" = "#8168e0"
)

vegetative_palette <- c(
  "signs of life" = "#97af4e",
  "young leaves" = "#3f8e6a",
  "leaves changing colour" = "#e7b446",
  ">50% fallen" = "#969eb5"
)

get_flowering_color <- function(stage) {
  first_pattern_match(stage, names(flowering_palette), unname(flowering_palette))
}

get_vegetative_color <- function(stage) {
  first_pattern_match(stage, names(vegetative_palette), unname(vegetative_palette))
}

# Helper function to get month name
get_month_name <- function(date_str) {
  month(dmy(date_str), label = TRUE, abbr = TRUE)
}

# Helper function to get week number 
get_week_number <- function(date_str) {
  date <- dmy(date_str)
  paste0(year(date), "-W", sprintf("%02d", isoweek(date)))
}

# Helper function to get date range subtitle
get_date_range_subtitle <- function(data) {
  # Use the date column we created
  dates <- data$Date
  min_date <- min(dates, na.rm = TRUE)
  max_date <- max(dates, na.rm = TRUE)
  
  min_month <- month(min_date, label = TRUE, abbr = FALSE)
  max_month <- month(max_date, label = TRUE, abbr = FALSE)
  year <- year(min_date)
  
  if (min_month == max_month) {
    subtitle1 <- paste("Phenological monitoring data from", min_month, year)
  } else {
    subtitle1 <- paste("Phenological monitoring data from", min_month, "to", max_month, year)
  }
  
  subtitle2 <- paste("Observations from", format(min_date, "%d/%m/%Y"), "to", format(max_date, "%d/%m/%Y"))
  
  list(subtitle1 = subtitle1, subtitle2 = subtitle2)
}

# Chart.js helper function
create_chartjs_html <- function(data, labels, colors, chart_id) {
  paste0('
    <canvas id="', chart_id, '" width="400" height="400"></canvas>
    <script>
      const ctx = document.getElementById("', chart_id, '").getContext("2d");
      new Chart(ctx, {
        type: "doughnut",
        data: {
          labels: ', jsonlite::toJSON(labels), ',
          datasets: [{
            data: ', jsonlite::toJSON(data), ',
            backgroundColor: ', jsonlite::toJSON(colors), '
          }]
        },
        options: {
          responsive: true,
          maintainAspectRatio: false,
          plugins: {
            legend: {
              position: "right"
            }
          }
        }
      });
    </script>
  ')
}

ui <- fluidPage(
  # Add Bootswatch Minty theme
    tags$head(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/3.9.1/chart.min.js"),
    tags$link(rel = "stylesheet", 
              href = "https://cdn.jsdelivr.net/npm/bootswatch@5.3.0/dist/minty/bootstrap.min.css"),
      tags$style(HTML("
      :root {
        --bs-primary: #376D61;
        --bs-primary-rgb: 111, 66, 193;
      }
      body {
        background: linear-gradient(135deg, #f0f9ff 0%, #e0f2fe 100%);
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        padding: 20px;
      }

      .title-section {
        padding: 3rem 0 2rem 0;
        margin-bottom: 2rem;
      }
      .card {
        border-radius: 12px;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        border: none;
      }
      .card-header {
        border-bottom: none;
        border-radius: 12px 12px 0 0;
        padding: 15px 20px;
        font-weight: 600; 
      }
      .card-header h4 {
        margin: 0;
        font-size: 3rem;
        color:rgb(27, 23, 23);
      }

      .bubble-label {
        display: inline-block;
        padding: 4px 12px;
        border-radius: 20px;
        font-size: 0.8em;
        font-weight: 500;
        margin: 2px;
      }
      .flowering-stage {
        background-color: #f8d7da;
        color: #721c24;
      }
      .leaf-stage {
        background-color: #d4edda;
        color: #155724;
      }
      .location {
        background-color: #d1ecf1;
        color: #0c5460;
      }
      .stat-card {
        background: white;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
        padding: 20px;
        text-align: center;
        margin-bottom: 20px;
      }
       .stat-value {
         font-size: 2.5rem;
         font-weight: bold;
         margin-top: 8px;
       }
       
       /* Custom styling for navigation pills container */
       html body div.container-fluid div.container-fluid div.row.mb-4 div.col-12 div.row div.col-sm-12 ul#main_tabs.nav.nav-pills.nav-stacked.shiny-tab-input.shiny-bound-input {

         border-radius: 10px; 
         padding: 20px; 
         margin: 0 auto; 
         max-width: 900px; 
         gap: 1rem;
         underline: none;
       }

       /* Navigation pills - inactive state */
       #main_tabs li a {
         background-color: white;
         color: #376D61;
         border-radius: 4px;
         text-decoration: none;
         transition: all 0.3s ease;
         font-size: 2rem;
       }
       
       /* Navigation pills - hover state */
       #main_tabs li a:hover {
         background-color: #a8d5c7;
         color: white;
         transform: scale(1.02);
         font-size: 2rem;
       }
       
       /* Navigation pills - active state */
       #main_tabs li.active a {
         background-color: #376D61;
         color: white;
         box-shadow: 0 2px 4px rgba(0,0,0,0.2);
         font-size: 2rem;
        }
      "))
    ),
    
  # Title
  div(class = "container-fluid", style = "max-width: 1400px; margin: 0 auto;",
    div(class = "row title-section",
      div(class = "col-12",
        div(class = "card",
          div(class = "card-body text-center",
            h1("🌿 Garden Observation Dashboard", 
               class = "mb-3", 
               style = "color: #376D61; font-weight: bold;"),
            p(get_date_range_subtitle(garden_data)$subtitle1, 
              style = "color: #666; font-size: 1.5rem; margin-bottom: 0.5rem;"),
            p(get_date_range_subtitle(garden_data)$subtitle2, 
              style = "color: #888; font-size: 1.5rem; margin-bottom: 0;")
          )
        ) 
      )
    ),
    
    # Summary statistics cards
    div(class = "row mb-4",
      div(class = "col-md-2",
        div(class = "stat-card",
          h3("Total Observations", class = "text-primary"),
          div(class = "stat-value text-primary", textOutput("total_obs"))
        )
      ),
      div(class = "col-md-2",
        div(class = "stat-card",
          h3("Unique Species", class = "text-success"),
          div(class = "stat-value text-success", textOutput("unique_species"))
        )
      ),
      div(class = "col-md-2",
        div(class = "stat-card",
          h3("Plant Families", class = "text-info"),
          div(class = "stat-value text-info", textOutput("plant_families"))
        )
      ),
      div(class = "col-md-2",
        div(class = "stat-card",
          h3("Flowering Plants", class = "text-warning"),
          div(class = "stat-value text-warning", textOutput("flowering_plants"))
        )
      ),
      div(class = "col-md-2",
        div(class = "stat-card",
          h3("With Pollinators", class = "text-danger"),
          div(class = "stat-value text-danger", textOutput("pollinator_obs"))
        )
      ),
      div(class = "col-md-2",
        div(class = "stat-card",
          h3("Observation Days", class = "text-secondary"),
          div(class = "stat-value text-secondary", textOutput("observation_days"))
        )
      )
    ),
    
    # Horizontal navigation tabs
    div(class = "row mb-4",
      div(class = "col-12",
        navlistPanel(
          id = "main_tabs",
          widths = c(12, 12),
          well = FALSE,
          tabPanel("📊 Overview", value = "overview"),
          tabPanel("👯 Families", value = "families"),
          tabPanel("🌸 Phenology", value = "phenology"),
          tabPanel("🐝 Pollinators", value = "pollinators"),
          tabPanel("📆 Obervations", value = "timeline")
        )
      )
    ),
    
    # Tab content
    div(id = "tab_content",
      # Overview tab
      conditionalPanel(
        condition = "input.main_tabs == 'overview'",
        div(class = "row",
          div(class = "col-md-6",
            div(class = "card",
                div(class = "card-header", h4("📊 Number of Observations")),
              div(class = "card-body", plotlyOutput("monthly_chart", height = "400px"))
            )
          ),
          div(class = "col-md-6",
            div(class = "card",
              div(class = "card-header", h4("📈 Flowering Timeline")),
              div(class = "card-body", plotlyOutput("flowering_timeline_chart", height = "400px"))
            )
          )
        )
      ),
      
      # Timeline tab
      conditionalPanel(
        condition = "input.main_tabs == 'timeline'",
        div(class = "card",
          div(class = "card-header", 
            div(class = "row",
              div(class = "col-md-8", h4("📅 Observation Timeline")),
              div(class = "col-md-4 text-end", 
                downloadButton("download_timeline_csv", "Export CSV", class = "btn-danger btn-lg")
              )
            )
          ),
          div(class = "card-body",
            div(class = "row mb-3",
              div(class = "col-md-3", textInput("search_filter", "Search plants...", placeholder = "Search plants...")),
              div(class = "col-md-3", selectInput("family_filter", "Family", choices = c("All Families" = ""), selected = "")),
              div(class = "col-md-3", selectInput("location_filter", "Location", choices = c("All Locations" = ""), selected = "")),
              div(class = "col-md-3", numericInput("items_per_page", "Items per page", value = 10, min = 5, max = 50))
            ),
            DT::dataTableOutput("timeline_table")
          )
        )
      ),
      
      # Families tab
      conditionalPanel(
        condition = "input.main_tabs == 'families'",
        div(class = "row",
          div(class = "col-md-6",
            div(class = "card",
              div(class = "card-header", h4("🌱 Family Distribution")),
              div(class = "card-body", plotlyOutput("family_chart", height = "800px"))
            )
          ),
          div(class = "col-md-6",
            div(class = "card",
              div(class = "card-header", h4("📊 Species per Family")),
              div(class = "card-body", plotlyOutput("species_chart", height = "800px"))
            )
          )
        ),
        div(class = "row mt-4",
          div(class = "col-12",
            div(class = "card",
              div(class = "card-header", h4("🗺️ Plant Heatmap by Location")),
              div(class = "card-body", plotlyOutput("location_heatmap", height = "400px"))
            )
          )
        )
      ),
      
      # Phenology tab
      conditionalPanel(
        condition = "input.main_tabs == 'phenology'",
        div(class = "row mt-4",
          div(class = "col-12",
            div(class = "card",
              div(class = "card-header", h4("🌼 Species Flowering Grid by Month")),
              div(class = "card-body", plotlyOutput("species_bubble_grid", height = "1000px"))
            )
          )
        ),
        div(class = "row",
          div(class = "col-md-6",
            div(class = "card",
              div(class = "card-header", h4("🌸 Flowering Stages")),
              div(class = "card-body", plotlyOutput("phenology_chart", height = "400px"))
            )
          ),
          div(class = "col-md-6",
            div(class = "card",
              div(class = "card-header", h4("🌸 Monthly Flower Stages (%)")),
              div(class = "card-body", plotlyOutput("stacked_barplot", height = "400px"))
            )
          )
        ),
        div(class = "row mt-4",
          div(class = "col-md-6",
            div(class = "card",
              div(class = "card-header", h4("🍃 Leaf Stages")),
              div(class = "card-body", plotlyOutput("leaf_phenology_chart", height = "400px"))
            )
          ),
          div(class = "col-md-6",
            div(class = "card",
              div(class = "card-header", h4("🍃 Monthly Leaf Stages (%)")),
              div(class = "card-body", plotlyOutput("stacked_leaf_plot", height = "400px"))
            )
          )
        ),
      ),
      
      # Pollinators tab
      conditionalPanel(
        condition = "input.main_tabs == 'pollinators'",
        div(class = "row",
          div(class = "col-md-6",
            div(class = "card", style = "max-height: 1500px;",
              div(class = "card-header", h4("🐝 Pollinator Activity Flow")),
              div(class = "card-body", sankeyNetworkOutput("sankey_plot", height = "1000px"))
            )
          ),
          div(class = "col-md-6",
            div(class = "card", style = "max-height: 1500px;",
              div(class = "card-header", h4("🗺️ Pollinator Activity by Location")),
              div(class = "card-body", plotlyOutput("pollinator_heatmap", height = "1000px"))
            )
          )
        )
      ),
      
    )
  )
)

server <- function(input, output, session) {

  # Summary value boxes - matching HTML dashboard
  output$total_obs <- renderText({
    nrow(garden_data)
  })
  
  output$unique_species <- renderText({
    length(unique(garden_data$Species))
  })
  
  output$plant_families <- renderText({
    length(unique(garden_data$Family))
  })
  
  output$flowering_plants <- renderText({
    garden_data %>%
      filter(Flowering %in% c("First flowers", ">50% flowering")) %>%
      pull(Plant) %>%
      unique() %>%
      length()
  })
  
  output$pollinator_obs <- renderText({
    garden_data %>%
      filter(!is.na(Pollinator)) %>%
      filter(Pollinator != 'nothing') %>%
      pull(Pollinator) %>%
      unique() %>%
      length()
  })
  
  output$observation_days <- renderText({
    length(unique(garden_data$Timestamp))
  })

  # Update filter inputs
  observe({
    updateSelectInput(
      session, "family_filter",
      choices = c("All Families" = "", sort(unique(garden_data$Family))),
      selected = isolate(input$family_filter)
    )
    updateSelectInput(
      session, "location_filter",
      choices = c("All Locations" = "", sort(unique(garden_data$Location))),
      selected = isolate(input$location_filter)
    )
  })

  # Filtering helper
  filtered_data <- reactive({
    data <- garden_data
    if (!is.null(input$search_filter) && nzchar(input$search_filter)) {
      data <- data %>% filter(grepl(input$search_filter, Plant, ignore.case = TRUE))
    }
    if (!is.null(input$family_filter) && nzchar(input$family_filter)) {
      data <- data %>% filter(Family == input$family_filter)
    }
    if (!is.null(input$location_filter) && nzchar(input$location_filter)) {
      data <- data %>% filter(Location == input$location_filter)
    }
    data
  })
  
  # Overview charts
  output$flowering_timeline_chart <- renderPlotly({
    # Prepare flowering data with categories
    flowering_data <- garden_data %>%
      filter(Flowering != "nothing") %>%
      mutate(
        month = get_month_name(format(Date, "%d/%m/%Y")),
        flower_category = case_when(
          Flowering %in% c("First flowers", ">50% flowering") ~ "Active Flowering",
          Flowering %in% c(">50% ripe", "Onset seed/fruit") ~ "Fruiting/Ripening",
          TRUE ~ "Other"
        )
      ) %>%
      count(month, flower_category) %>%
      arrange(match(month, c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))
    
    # Prepare leaf data with categories
    leaf_data <- garden_data %>%
      filter(Vegetative != "nothing") %>%
      mutate(
        month = get_month_name(format(Date, "%d/%m/%Y")),
        leaf_category = case_when(
          Vegetative %in% c("signs of life", "young leaves") ~ "Active Leaf Growth",
          TRUE ~ "Other"
        )
      ) %>%
      count(month, leaf_category) %>%
      arrange(match(month, c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))
    
    # Combine data for plotting
    plot_data <- bind_rows(
      flowering_data %>% mutate(type = "Flower", category = flower_category),
      leaf_data %>% mutate(type = "Leaf", category = leaf_category)
    ) %>%
      filter(category != "Other")
    
    # Ensure we have data
    if(nrow(plot_data) == 0) {
      return(plotly_empty())
    }
    
    # Define colors
    colors <- c(
      "Active Flowering" = "#ff8c00",  # Orange
      "Fruiting/Ripening" = "#dc143c", # Red
      "Active Leaf Growth" = "#228b22"       # Green
    )
    
    p <- ggplot(
      plot_data,
      aes(
        x = month,
        y = n,
        color = category,
        text = paste0("Month: ", month, "<br>Stage: ", category, "<br>Count: ", n)
      )
    ) +
      geom_line(aes(group = category), linewidth = 2, alpha = 0.7, show.legend = FALSE) +
      geom_point(size = 6, alpha = 0.9, show.legend = TRUE) +
      labs(x = "Month", y = "Count") +
      theme_minimal() +
      scale_color_manual(values = colors)

    ggplotly(p, tooltip = "text")
  })
  
  output$monthly_chart <- renderPlotly({
    monthly_activity <- garden_data %>%
      mutate(month = get_month_name(format(Date, "%d/%m/%Y"))) %>%
      count(month) %>%
      arrange(match(month, c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))
    
    p <- ggplot(monthly_activity, aes(x = month, y = n)) +
      geom_bar(stat = "identity", fill = "#78c2ad", alpha = 0.8) +
      labs(x = "Month", y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Timeline table with filtering
  output$timeline_table <- DT::renderDataTable({
    timeline_data <- filtered_data() %>%
      arrange(Timestamp) %>%
      mutate(
        Flowering_formatted = ifelse(Flowering != "nothing",
          paste0('<span class="bubble-label" style="background-color: ', get_flowering_color(Flowering), '; color: white;">', Flowering, '</span>'),
          '<span class="bubble-label" style="background-color: #e9ecef; color: #6c757d;">No flowering</span>'),
        Vegetative_formatted = paste0('<span class="bubble-label" style="background-color: ', get_vegetative_color(Vegetative), '; color: white;">', Vegetative, '</span>'),
        location_formatted = paste0('<span class="bubble-label location">', Location, '</span>')
      ) %>%
      select(Plant, Species, Family, location_formatted, Vegetative_formatted, 
             Flowering_formatted, Pollinator, Date, Time)
    
    DT::datatable(timeline_data, 
                  options = list(pageLength = input$items_per_page, scrollX = TRUE, lengthChange = FALSE),
                  escape = FALSE,
                  colnames = c('Plant', 'Species', 'Family', 'Location', 'Leaf Stage', 
                             'Flower Stage', 'Pollinator', 'Date', 'Time'))
  })
  
  # Timeline CSV download
  output$download_timeline_csv <- downloadHandler(
    filename = function() {
      paste("garden_timeline_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      export_data <- filtered_data() %>%
        arrange(Timestamp) %>%   
        select(Date, Time, Location, Plant, Species, Family, Vegetative, Flowering, Pollinator)
      
      write.csv(export_data, file, row.names = FALSE)
    }
  )
  
  # Family donut chart (Plotly)
  output$family_chart <- renderPlotly({
    family_counts <- garden_data %>%
      filter(!is.na(Plant), nzchar(Plant)) %>%
      count(Family) %>%
      arrange(desc(n))

    colors <- c('#8884d8', '#82ca9d', '#ffc658', '#ff7c7c', '#8dd1e1', 
                '#d084d0', '#ffb347', '#98fb98', '#dda0dd', '#f0e68c',
                '#ff6b6b', '#4ecdc4', '#45b7d1', '#96ceb4', '#feca57',
                '#ff9ff3', '#54a0ff', '#5f27cd', '#00d2d3', '#ff9f43')

    plot_ly(
      family_counts,
      labels = ~Family,
      values = ~n,
      type = 'pie',
      hole = 0.4,
      marker = list(colors = colors[((seq_len(nrow(family_counts)) - 1) %% length(colors)) + 1])
    ) %>%
      layout(showlegend = TRUE)
  })
  
  output$species_chart <- renderPlotly({
    species_per_family <- garden_data %>%
      filter(!is.na(Species), nzchar(Species)) %>%
      group_by(Family) %>%
      summarise(species_count = n_distinct(Species), .groups = 'drop') %>%
      arrange(desc(species_count))
    
    p <- ggplot(species_per_family, aes(x = reorder(Family, species_count), y = species_count)) +
      geom_bar(stat = "identity", fill = "#78c2ad") +
      coord_flip() +
      labs(x = "Family", y = "Unique Species") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Location heatmap
  output$location_heatmap <- renderPlotly({
    # Create heatmap data: unique species per location
    heatmap_data <- garden_data %>%
      filter(!is.na(Species), nzchar(Species)) %>%
      group_by(Location, Family) %>%
      summarise(species_count = n_distinct(Species), .groups = 'drop') %>%
      tidyr::pivot_wider(names_from = Family, values_from = species_count, values_fill = 0)
    
    # Convert to matrix for heatmap
    locations <- heatmap_data$Location
    families <- setdiff(names(heatmap_data), "Location")
    matrix_data <- as.matrix(heatmap_data[, families])
    rownames(matrix_data) <- locations
    
    # Create heatmap
    p <- plot_ly(
      x = families,
      y = locations,
      z = matrix_data,
      type = "heatmap",
      colorscale = "Viridis",
      showscale = TRUE
    ) %>%
      layout(
        title = "Unique Species Count by Location and Family",
        xaxis = list(title = "Plant Family"),
        yaxis = list(title = "Location")
      )
    
    p
  })
  
  # Phenology charts
  output$phenology_chart <- renderPlotly({
    stage_data <- garden_data %>%
      filter(Flowering != "nothing") %>%
      count(Flowering)

    p <- ggplot(stage_data, aes(x = Flowering, y = n, fill = Flowering)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(x = "Stage", y = "Count") +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_fill_manual(values = setNames(get_flowering_color(stage_data$Flowering), stage_data$Flowering))
    
    ggplotly(p)
  })
  
  # Leaf phenology chart
  output$leaf_phenology_chart <- renderPlotly({
    Vegetative_data <- garden_data %>%
      count(Vegetative) %>%
      arrange(desc(n))
    
    p <- ggplot(Vegetative_data, aes(x = reorder(Vegetative, n), y = n, fill = Vegetative)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(x = "Stage", y = "Count") +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_fill_manual(values = setNames(get_vegetative_color(Vegetative_data$Vegetative), Vegetative_data$Vegetative))
    
    ggplotly(p)
  })
  
  # Interactive stacked barplot for flower stages (monthly)
  output$stacked_barplot <- renderPlotly({
    flower_data <- garden_data %>%
      filter(Flowering != "nothing") %>%
      mutate(
        date_obj = Date,
        month_start = floor_date(date_obj, "month"),
        month_label = format(Date, "%Y-%m")
      ) %>%
      count(month_start, month_label, Flowering) %>%
      group_by(month_start, month_label) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      ungroup() %>%
      arrange(month_start, Flowering)
    
    p <- ggplot(flower_data, aes(x = month_start, y = percentage, fill = Flowering, text = paste0("Month: ", month_label, "<br>Stage: ", Flowering, "<br>Percentage: ", round(percentage, 1), "%"))) +
      geom_bar(stat = "identity", position = "stack") +
      labs(x = "Month", y = "Percentage") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(values = setNames(get_flowering_color(levels(flower_data$Flowering)), levels(flower_data$Flowering))) +
      ylim(0, 100) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 month")
    
    ggplotly(p, tooltip = "text") %>%
      layout(xaxis = list(
        type = "date"
      ))
  })
  
  # Interactive stacked barplot for leaf stages (monthly)
  output$stacked_leaf_plot <- renderPlotly({
    leaf_data <- garden_data %>%
      filter(Vegetative != "nothing") %>%
      mutate(
        date_obj = Date,
        month_start = floor_date(date_obj, "month"),
        month_label = format(Date, "%Y-%m")
      ) %>%
      count(month_start, month_label, Vegetative) %>%
      group_by(month_start, month_label) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      ungroup() %>%
      arrange(month_start, Vegetative)
    
    p <- ggplot(leaf_data, aes(x = month_start, y = percentage, fill = Vegetative, text = paste0("Month: ", month_label, "<br>Stage: ", Vegetative, "<br>Percentage: ", round(percentage, 1), "%"))) +
      geom_bar(stat = "identity", position = "stack") +
      labs(x = "Month", y = "Percentage") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(values = setNames(get_vegetative_color(levels(leaf_data$Vegetative)), levels(leaf_data$Vegetative))) +
      ylim(0, 100) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 month")
    
    ggplotly(p, tooltip = "text") %>%
      layout(xaxis = list(
        type = "date"
      ))
  })

  # Species bubble grid by month
output$species_bubble_grid <- renderPlotly({
  if (nrow(garden_data) == 0) return(plotly_empty())

    monthly_flower <- garden_data %>%
      filter(!is.na(Plant), nzchar(Plant), !is.na(Date)) %>%
      mutate(Month = format(Date, "%m-%Y")) %>%
      filter(Flowering != "nothing") %>%
      group_by(Month, Plant, Flowering) %>%
      summarise(n_flower = n(), .groups = 'drop') %>%
      group_by(Month, Plant) %>%
      slice_max(n_flower, n = 1, with_ties = FALSE) %>%
      ungroup()


    monthly_leaf <- garden_data %>%
      filter(!is.na(Plant), nzchar(Plant), !is.na(Date)) %>%
      mutate(Month = format(Date, "%m-%Y")) %>%
      filter(Vegetative != "nothing") %>%
      group_by(Month, Plant, Vegetative) %>%
      summarise(n_leaf = n(), .groups = 'drop') %>%
      group_by(Month, Plant) %>%
      slice_max(n_leaf, n = 1, with_ties = FALSE) %>%
      ungroup()

    if (nrow(monthly_flower) == 0 && nrow(monthly_leaf) == 0) return(plotly_empty())

    all_species <- garden_data %>%
      filter(!is.na(Plant), nzchar(Plant)) %>%
      group_by(Plant) %>%
      arrange(desc(n())) %>%
      distinct(Plant) %>%
      slice(1:136) %>%
      pull(Plant)

    all_month_labels <- sort(unique(c(monthly_flower$Month, monthly_leaf$Month)))
    if (length(all_month_labels) == 0) return(plotly_empty())
    base_grid <- tidyr::expand_grid(Month = all_month_labels, Plant = all_species)

    # Build combined hover text per month/species (includes both Flower and Leaf)
    hover_map <- base_grid %>%
      left_join(select(monthly_flower, Month, Plant, Flowering, n_flower), by = c("Month", "Plant")) %>%
      left_join(select(monthly_leaf, Month, Plant, Vegetative), by = c("Month", "Plant")) %>%
      mutate(
        hover_text = paste0(
          '<b>', Plant, '</b><br>',
          'Flower: ', ifelse(is.na(Flowering), '—', Flowering), '<br>',
          'Vegetation: ', ifelse(is.na(Vegetative), '—', Vegetative), '<br>',
          'N flowers: ', ifelse(is.na(n_flower), 0, n_flower), '<br>'
        )
      ) %>%
      select(Month, Plant, hover_text)

    flower_data <- base_grid %>%
      left_join(monthly_flower, by = c("Month", "Plant")) %>%
      left_join(hover_map, by = c("Month", "Plant")) %>%
      mutate(
        species_index = match(Plant, all_species),
        x = ((species_index - 1) %% 6) + 1,
        y = ((species_index - 1) %/% 6) + 1,
        n_flower = ifelse(is.na(n_flower), 0, n_flower),
        color_flower = ifelse(is.na(Flowering), "rgba(0,0,0,0)", get_flowering_color(Flowering))
      )

    leaf_data <- base_grid %>%
          left_join(monthly_leaf, by = c("Month", "Plant")) %>%
      left_join(hover_map, by = c("Month", "Plant")) %>%
      mutate(
        species_index = match(Plant, all_species),
        x = ((species_index - 1) %% 6) + 1,
        y = ((species_index - 1) %/% 6) + 1,
        n_leaf = ifelse(is.na(n_leaf), 0, n_leaf),
        color_leaf = ifelse(is.na(Vegetative), "rgba(0,0,0,0)", get_vegetative_color(Vegetative))
      )

    max_n_flower <- max(flower_data$n_flower, na.rm = TRUE)
    sizeref_flower <- if (is.finite(max_n_flower) && max_n_flower > 0) 2 * max_n_flower / (30 ^ 2) else 1
    max_n_leaf <- max(leaf_data$n_leaf, na.rm = TRUE)
    sizeref_leaf <- if (is.finite(max_n_leaf) && max_n_leaf > 0) 2 * max_n_leaf / (40 ^ 2) else 1

    p <- plot_ly(
      leaf_data,
      x = ~x,
      y = ~y,
      frame = ~Month,
      type = 'scatter',
      mode = 'markers',
      marker = list(
        size = ~n_leaf,
        sizemode = 'area',
        sizeref = sizeref_leaf,
        color = ~color_leaf,
        line = list(width = 0)
      ),
      hovertext = ~hover_text,
      hoverinfo = 'text'
    ) %>%
      add_trace(
        data = flower_data,
        x = ~x,
        y = ~y,
        frame = ~Month,
        type = 'scatter',
        mode = 'markers+text',
        marker = list(
          size = ~n_flower,
          sizemode = 'area',
          sizeref = sizeref_flower,
          color = ~color_flower,
          line = list(width = 0)
        ),
        text = ~Plant,
        textposition = 'middle center',
        hovertext = ~hover_text,
        hoverinfo = 'text',
        inherit = FALSE
      ) %>%
      layout(
        xaxis = list(title = "", tickmode = 'array', tickvals = 1:6, range = c(0.5, 6.5), showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE, showline = FALSE),
        yaxis = list(title = "", tickmode = 'array', tickvals = 1:25, range = c(0.5, 25.5), showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE, showline = FALSE),
        showlegend = FALSE
      )

    p
  })

output$species_bubble_grid <- renderPlotly({
    if (nrow(garden_data) == 0) return(plotly_empty())

    monthly_flower <- garden_data %>%
      filter(!is.na(Plant), nzchar(Plant), !is.na(Date)) %>%
      mutate(
        Month = format(Date, "%m-%Y"),
        flower_size = case_when(
          Flowering == "Flower buds" ~ 5,
          Flowering == "First flowers" ~ 8,
          Flowering == ">50% flowering" ~ 10,
          Flowering == "Finished flowering" ~ 2,
          Flowering == "Onset seed/fruit" ~ 5,
          Flowering == ">50% ripe " ~ 10,
          Flowering == "nothing" ~ 0,
          TRUE ~ 0
        ),
        leaf_size = flower_size + 5 + 
          case_when(
            Vegetative == "signs of life" ~ 5,
            Vegetative == "young leaves" ~ 10,
            Vegetative == "leaves changing colour" ~ 10,
            Vegetative == ">50% fallen" ~ 5,
            Vegetative == "nothing" ~ 0,
            TRUE ~ 0
          )
      ) %>%
      group_by(Date, Plant, Flowering, Vegetative) %>%
      mutate(
        flower_size = sum(flower_size),
        leaf_size = sum(leaf_size),
        n= n(), .groups = 'drop'
      ) %>%
      group_by(Date, Plant) %>%
      arrange(n) %>%
      summarise(
        flower_size = sum(flower_size),
        leaf_size = sum(leaf_size),
        n= sum(n()), .groups = 'drop',
        Flowering = first(Flowering),
        Vegetative = first(Vegetative)
      )
    
    species_index <- garden_data %>%
      filter(!is.na(Plant), nzchar(Plant)) %>%
      group_by(Plant) %>%
      arrange(desc(n())) %>%
      distinct(Plant) %>%
      slice(1:136) %>%
      pull(Plant)

    base_grid <- tidyr::expand_grid(
      Date = unique(monthly_flower$Date), 
      Plant = unique(species_index)) 

    flower_data <- base_grid %>%
      left_join(monthly_flower, by = c("Date", "Plant")) %>%
      arrange(Date, Plant) %>%
      mutate(
        species_index = match(Plant, species_index),
        x = ((species_index - 1) %% 6) + 1,
        y = ((species_index - 1) %/% 6) + 1,
        flower_colour = ifelse(is.na(Flowering), "rgba(0,0,0,0)", get_flowering_color(Flowering)),
        leaf_colour = ifelse(is.na(Vegetative), "rgba(0,0,0,0)", get_vegetative_color(Vegetative)),
        hover_text = paste0(
          '<b>', Plant, '</b><br>',
          'Flower: ', Flowering, '<br>',
          'Vegetation: ', Vegetative, '<br>',
          'N: ', ifelse(is.na(n), 0, n), '<br>'
        ),
        Date = format(Date, "%Y-%m-%d")
      ) 

    p <- plot_ly(
      data = flower_data,
      x = ~x, y = ~y, frame = ~Date, type = 'scatter', mode = 'markers', alpha = 0.5,
      marker = list(size = ~leaf_size, sizemode = 'area', sizeref = 0.01, color = ~leaf_colour, line = list(width = 0)),
      hovertext = ~hover_text,
      hoverinfo = 'text'
    ) %>%
    add_trace(
      data = flower_data, x = ~x, y = ~y, frame = ~Date, type = 'scatter', mode = 'markers+text', alpha = 0.5,
      marker = list(size = ~flower_size, sizemode = 'area', sizeref = 0.01, color = ~flower_colour, line = list(width = 0)),
        text = ~Plant,
        textposition = 'middle center',
        hovertext = ~hover_text,
        hoverinfo = 'text',
        inherit = FALSE
      ) %>%
      layout(
        xaxis = list(title = "", tickmode = 'array', tickvals = 1:6, range = c(0.5, 6.5), showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE, showline = FALSE),
        yaxis = list(title = "", tickmode = 'array', tickvals = 1:25, range = c(0.5, 25.5), showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE, showline = FALSE),
        showlegend = FALSE
      )

    p



  })
  
  # Sankey plot for pollinators
  output$sankey_plot <- renderSankeyNetwork({
    # Prepare data for Sankey plot
    pollinator_data <- garden_data %>%
      filter(!is.na(Pollinator), Pollinator != "nothing", nzchar(Pollinator)) %>%
      tidyr::separate_rows(Pollinator, sep = ",\\s*") %>%
      mutate(Pollinator = trimws(Pollinator)) %>%
      filter(nzchar(Pollinator)) %>%
      select(Plant, Pollinator) %>%
      group_by(Pollinator, Plant) %>%
      summarise(value = n(), .groups = 'drop')
    
    if(nrow(pollinator_data) == 0) {
      # Create empty plot with message
      return(plotly_empty() %>% 
             add_annotations(text = "No pollinator data available", 
                            x = 0.5, y = 0.5, 
                            showarrow = FALSE,
                            font = list(size = 25)))
    }
    
    # Create nodes
    pollinators <- unique(pollinator_data$Pollinator)
    plants <- unique(pollinator_data$Plant)
    
    nodes <- data.frame(
      name = c(pollinators, plants),
      group = c(rep("pollinator", length(pollinators)), 
                rep("plant", length(plants)))
    )
    
    # Create links
    links <- pollinator_data %>%
      mutate(
        source = match(Pollinator, nodes$name) - 1,  # 0-indexed
        target = match(Plant, nodes$name) - 1
      ) %>%
      select(source, target, value)
    
    # Create Sankey plot
    sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
                  Target = "target", Value = "value", NodeID = "name",
                  fontSize = 25, nodeWidth = 30)
  })
  
  # Pollinator heatmap
  output$pollinator_heatmap <- renderPlotly({
    # Create heatmap data: pollinator observations per location
    all_locations <- sort(unique(garden_data$Location))
    all_pollinators <- garden_data %>%
      select(Pollinator) %>%
      tidyr::separate_rows(Pollinator, sep = ",\\s*") %>%
      mutate(Pollinator = trimws(Pollinator)) %>%
      filter(!is.na(Pollinator), Pollinator != "nothing", nzchar(Pollinator)) %>%
      distinct(Pollinator) %>%
      arrange(Pollinator) %>%
      pull(Pollinator)

    pollinator_heatmap_data <- garden_data %>%
      tidyr::separate_rows(Pollinator, sep = ",\\s*") %>%
      mutate(Pollinator = trimws(Pollinator)) %>%
      filter(!is.na(Pollinator), Pollinator != "nothing", nzchar(Pollinator)) %>%
      group_by(Location, Pollinator) %>%
      summarise(observation_count = n(), .groups = 'drop') %>%
      tidyr::complete(Location = all_locations, Pollinator = all_pollinators, fill = list(observation_count = 0)) %>%
      tidyr::pivot_wider(names_from = Pollinator, values_from = observation_count, values_fill = 0)
    
    # Convert to matrix for heatmap
    locations <- pollinator_heatmap_data$Location
    pollinators <- setdiff(names(pollinator_heatmap_data), "Location")
    matrix_data <- as.matrix(pollinator_heatmap_data[, pollinators])
    rownames(matrix_data) <- locations
    
    # Create heatmap
    p <- plot_ly(
      x = locations,
      y = pollinators,
      z = t(matrix_data),
      type = "heatmap",
      colorscale = "Viridis",
      showscale = TRUE
    ) %>%
      layout(
        yaxis = list(title = "Pollinator Type"),
        xaxis = list(title = "Location")
      )
    
    p
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)