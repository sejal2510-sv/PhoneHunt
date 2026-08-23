library(shiny)
library(ggplot2)
library(shinythemes)
library(magick)
library(dplyr)
library(stringr)

# --- 1. DATA LOADING AND INITIAL SETUP  ---

# Load primary and top-phone datasets (Assuming CSVs are in the working directory)
all_phones_data <- read.csv("complete_phones_data.csv")
top_phones_data <- read.csv("top_20_phones_2017-2023.csv")

# --- 2. DATA PREPROCESSING AND CLEANING 🧹 ---

# Create a master dataset for cleaning (MD)
MD <- all_phones_data

# Function to extract numeric GB/TB storage and return max GB
# This handles complex strings like "128GB/256GB storage"
extract_max_storage_gb <- function(storage_string) {
  if (is.na(storage_string) || storage_string == "") return(NA_real_)
  
  # Remove extra words and spaces, then split by '/'
  clean_vals <- unlist(strsplit(gsub(" storage.*| ", "", storage_string), "/"))
  
  s_num <- sapply(clean_vals, function(y) { 
    if (grepl("TB", y, ignore.case = TRUE)) {
      # Convert TB to GB (1 TB = 1024 GB)
      return(as.numeric(gsub("TB", "", y, ignore.case = TRUE)) * 1024) 
    } else if (grepl("GB", y, ignore.case = TRUE)) {
      # Extract GB value
      return(as.numeric(gsub("GB", "", y, ignore.case = TRUE)))
    } else {
      # Assume already numeric or raw value (less common)
      return(as.numeric(y))
    }
  })
  
  # Return the maximum storage found in the string
  if (length(s_num) == 0 || all(is.na(s_num))) return(NA_real_)
  return(max(s_num, na.rm = T))
}

# Function to extract max numeric RAM (in GB)
extract_max_ram_gb <- function(ram_string) {
  if (is.na(ram_string) || ram_string == "") return(NA_real_)
  
  # Remove units/words and spaces, then split by '/'
  clean_vals <- unlist(strsplit(gsub("GB RAM.*|GBRAM|GB| ", "", ram_string, ignore.case = T), "/"))
  clean_vals <- gsub("[^0-9\\.]", "", clean_vals)
  r_num <- as.numeric(clean_vals)
  
  # Return the maximum RAM found in the string
  if (length(r_num) == 0 || all(is.na(r_num))) return(NA_real_)
  return(max(r_num, na.rm = T))
}

# Function to extract numeric Camera (in MP)
extract_camera_mp <- function(camera_string) {
  if (is.na(camera_string) || camera_string == "") return(NA_real_)
  x_clean <- gsub("MP.*| ", "", camera_string, ignore.case = TRUE)
  cam_val <- as.numeric(gsub("[^0-9\\.]", "", x_clean))
  return(if (is.na(cam_val)) NA_real_ else cam_val)
}

# Function to extract numeric Battery (in mAh)
extract_battery_mah <- function(battery_string) {
  if (is.na(battery_string) || battery_string == "") return(NA_real_)
  x_clean <- gsub("mAh.*| ", "", battery_string, ignore.case = TRUE)
  bat_val <- as.numeric(gsub("[^0-9\\.]", "", x_clean))
  return(if (is.na(bat_val)) NA_real_ else bat_val)
}


# Apply all cleaning steps using the power of dplyr::mutate
MD <- MD %>% 
  mutate(
    # Categorize OS for easier filtering
    os_category = case_when(
      grepl("iOS", os_type, ignore.case = T) ~ "iOS",
      grepl("Android", os_type, ignore.case = T) ~ "Android",
      grepl("HarmonyOS|EMUI", os_type, ignore.case = T) ~ "HarmonyOS/EMUI",
      TRUE ~ "Other"
    ),
    # Extract only the 4-digit year from the release date
    release_date_numeric = as.numeric(str_extract(release_date, "\\b(20\\d{2})\\b")),
    # Apply custom extraction functions
    storage_numeric = sapply(storage, extract_max_storage_gb),
    display_size_numeric = as.numeric(gsub("[^0-9\\.]", "", display_size)),
    camera_numeric = sapply(camera, extract_camera_mp),
    ram_numeric = sapply(ram, extract_max_ram_gb),
    battery_numeric = sapply(battery, extract_battery_mah)
  )

# Set up variables based on the cleaned data
PD <- MD # Alias for use in the Suggester tab
release_years <- sort(unique(MD$release_date_numeric[!is.na(MD$release_date_numeric)]), decreasing = T)
min_year <- min(MD$release_date_numeric, na.rm = T)
max_year <- max(MD$release_date_numeric, na.rm = T)
# DataTable display options
datatable_options <- list(dom = 't', lengthChange = F, pageLength = 21, rowName = F) 

# Minified CSS (Preserves dark theme look - kept minified for brevity)
# Added comments for the theme color palette
CSS_MINIFIED <- "body{background-color:#0a0e27;color:#e0e0e0;}.well{background-color:#1a1f3a;border:1px solid #2d3561;border-radius:10px;box-shadow:0 4px 12px rgba(0,0,0,0.5);padding:20px;margin-bottom:20px;}.main-title-panel{background:linear-gradient(135deg,#667eea 0%,#764ba2 100%);color:white;padding:25px;margin-bottom:20px;border-radius:0 0 10px 10px;box-shadow:0 4px 12px rgba(0,0,0,0.4);font-weight:700;font-size:30px;}.shiny-input-container label{font-weight:600;color:#b8c1ec;}.selectize-input{background-color:#2d3561!important;border-color:#4a5578!important;color:#e0e0e0!important;}.selectize-dropdown{background-color:#1a1f3a!important;border-color:#4a5578!important;color:#e0e0e0!important;}input[type='number'],input[type='text']{background-color:#2d3561!important;border-color:#4a5578!important;color:#e0e0e0!important;}.irs-bar{background:linear-gradient(to bottom,#667eea 0%,#764ba2 100%);}.irs-from,.irs-to,.irs-single{background:#667eea!important;}.card{border:none;border-radius:10px;overflow:hidden;background-color:#1a1f3a;box-shadow:0 2px 8px rgba(0,0,0,0.6);transition:transform 0.3s ease-in-out,box-shadow 0.3s ease-in-out;height:100%;}.card:hover{transform:translateY(-5px);box-shadow:0 10px 20px rgba(102,126,234,0.4);}.card-body{padding:15px;background-color:#1a1f3a;color:#e0e0e0;}.card-deck{display:flex;flex-wrap:wrap;justify-content:space-between;gap:20px;}.card-title a{font-weight:bold;color:#667eea;text-decoration:none;}.btn-primary{background:linear-gradient(135deg,#667eea 0%,#764ba2 100%);border:none;font-weight:bold;padding:10px 20px;border-radius:5px;color:white;}.nav-tabs>li>a{color:#b8c1ec!important;background-color:#1a1f3a!important;border-color:#2d3561!important;}.nav-tabs>li.active>a,.nav-tabs>li.active>a:hover,.nav-tabs>li.active>a:focus{color:white!important;background-color:#667eea!important;border-color:#667eea!important;}#table thead tr th,#top20_table thead tr th,#compare_table thead tr th{color:transparent!important;height:0;padding:0;border:none;}#table_info,#table_paginate,#top20_table_info,#top20_table_paginate,#compare_table_info,#compare_table_paginate,#table_length,#top20_table_length,#compare_table_length{display:none!important;}table.dataTable{background-color:#1a1f3a!important;color:#e0e0e0!important;}table.dataTable thead th{background-color:#2d3561!important;color:#b8c1ec!important;}table.dataTable tbody tr:hover{background-color:#2d3561!important;}h3,h4,h5{color:#b8c1ec!important;}p{color:#e0e0e0;}"

# --- 3. USER INTERFACE (UI) DEFINITION 📱 ---

ui <- fluidPage(
  theme = shinytheme("cyborg"), 
  tags$head(tags$style(HTML(CSS_MINIFIED))), # Apply custom dark theme
  
  div(class = "main-title-panel", span(icon("mobile-alt"), "PhoneHunt")), # Main Title Banner
  
  tabsetPanel(
    
    # 3.1. Phone Explorer Tab
    tabPanel("Phone Explorer", 
             fluidRow(
               column(3, wellPanel(
                 h4(strong(icon("filter"), "Selection Panel")), 
                 selectInput("brand_all", "Select Brand:", choices = unique(MD$brand), selected = "Samsung"),
                 sliderInput("year_all", "Release Year Range:", min = min_year, max = max_year, value = c(min_year, max_year), step = 1), 
                 selectInput("model", "Select Model:", choices = NULL)
               )),
               column(9, wellPanel(
                 h4(strong(icon("image"), "Model Snapshot")), 
                 plotOutput("image", height = "400px"), 
                 br(), 
                 h4(strong(icon("list-alt"), "Detailed Specifications")), 
                 dataTableOutput("table")
               ))
             )
    ),
    
    # 3.2. Top Phones Tab
    tabPanel("Top Phones", 
             tabsetPanel(
               tabPanel("Phones", 
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("top20_year", "Year: ", choices = unique(top_phones_data$year)), 
                            selectInput("top20_model", "Select Model: ", choices = NULL)
                          ),
                          mainPanel(wellPanel(
                            plotOutput("top20_image", height = "400px"), 
                            br(), br(), 
                            dataTableOutput("top20_table")
                          ))
                        )
               ),
               tabPanel("Visualizing Trends", 
                        sidebarLayout(
                          sidebarPanel(
                            sliderInput("Dominance_year", "Year Range: ", min = min(top_phones_data$year), max = max(top_phones_data$year), value = c(min(top_phones_data$year), max(top_phones_data$year)), step = 1)
                          ),
                          mainPanel(wellPanel(
                            h3(strong("Top Brand Dominance Year by Year")), plotOutput("Dominance_plot"), 
                            h3(strong("SIM Card Trend Over Time")), plotOutput("Sim_plot"), 
                            h3(strong("Phone Weight Trend")), plotOutput("Weights"), 
                            h3(strong("Weight VS Battery Size Correlation")), plotOutput("WBS"), 
                            h3(strong("Average Battery Size Evolution")), plotOutput("BatSize")
                          ))
                        )
               )
             )
    ),
    
    # 3.3. Head-to-Head Compare Tab
    tabPanel("Head-to-Head Compare", 
             fluidRow(
               column(4, wellPanel(
                 h3(strong(icon("mobile"), "Select Phone 1")), 
                 selectInput("compare_brand1", "Brand: ", choices = unique(MD$brand), selected = "Samsung"), 
                 sliderInput("compare_year1", "Release Year: ", min = min_year, max = max_year, value = c(2022, 2024), step = 1), 
                 selectInput("compare_model1", "Model: ", choices = NULL)
               )),
               column(4, wellPanel(
                 h3(strong(icon("mobile"), "Select Phone 2")), 
                 selectInput("compare_brand2", "Brand: ", choices = unique(MD$brand), selected = "Apple"), 
                 sliderInput("compare_year2", "Release Year: ", min = min_year, max = max_year, value = c(2022, 2024), step = 1), 
                 selectInput("compare_model2", "Model: ", choices = NULL)
               )),
               column(4, wellPanel(
                 h3(strong(icon("chart-bar"), "Visual Comparison")), 
                 fluidRow(
                   column(6, plotOutput("compare_image1", height = "350px")), 
                   column(6, plotOutput("compare_image2", height = "350px"))
                 )
               ))
             ),
             fluidRow(
               column(12, wellPanel(
                 h3(strong(icon("table"), "Detailed Comparison Data")), 
                 dataTableOutput("compare_table")
               ))
             )
    ),
    
    # 3.4. Phone Suggester Tab
    tabPanel("Phone Suggester", 
             sidebarLayout(
               sidebarPanel(
                 h3(strong(icon("cogs"), "Filter Specifications")), 
                 selectInput("suggester_brand", "Brand", choices = c("Any", sort(unique(PD$brand))), selected = "Any"), 
                 selectInput("os_type", "Operating System", choices = c("Any", sort(unique(PD$os_category))), selected = "Any"), 
                 numericInput("release_date", "Release Year (From)", value = 2020, min = 2000, max = 2024), 
                 numericInput("storage_min", "Min Storage (GB)", value = 128), 
                 numericInput("storage_max", "Max Storage (GB)", value = 1024), 
                 numericInput("display_min", "Min Display Size (inches)", value = 5.5), 
                 numericInput("display_max", "Max Display Size (inches)", value = 7.0), 
                 numericInput("camera_min", "Min Camera Resolution (MP)", value = 12), 
                 numericInput("camera_max", "Max Camera Resolution (MP)", value = 108), 
                 sliderInput("ram_range", "RAM (GB)", min = 2, max = 16, value = c(4, 12), step = 1), 
                 numericInput("battery_min", "Min Battery Capacity (mAh)", value = 3000), 
                 numericInput("battery_max", "Max Battery Capacity (mAh)", value = 5000), 
                 actionButton("apply_filters", "Apply Filters", class = "btn-primary")
               ),
               mainPanel(wellPanel(
                 h3(strong(icon("search"), "Matching Phones")), 
                 uiOutput("filtered_phones_ui")
               ))
             )
    ),
    
    # 3.5. Same Year Releases Tab
    tabPanel("Same Year Releases", 
             fluidRow(
               column(3, wellPanel(
                 h4(strong(icon("calendar"), "Select Release Year")), 
                 selectInput("year_releases_select", "Year:", choices = release_years, selected = max_year)
               )),
               column(9, wellPanel(
                 h3(strong(icon("list"), "All Models Released in Selected Year")), 
                 p("Explore every phone model released during the selected year."), 
                 dataTableOutput("other")
               ))
             )
    ),
    
    # 3.6. About Project Tab (Icons and Team Roles removed)
    tabPanel("About Project", 
             wellPanel(
               h2(strong("PhoneHunt: Smartphone Trend Analysis")),
               p("Project developed by Team 21 for the MTH208 course using R and the Shiny framework."),
               
               hr(),
               
               h3(strong("Problem Statement & Project Goal")),
               p("The smartphone market is complex and continuously expanding, presenting consumers with an overwhelming number of models and specifications. Our project aims to simplify the selection process by developing an interactive, data-driven tool."),
               tags$ul(
                 tags$li(HTML("<strong>Goal:</strong> To enable users to explore, compare, and analyze key phone specifications and historical trends in features (like battery, camera, and display).")),
                 tags$li(HTML("<strong>Outcome:</strong> Empowering users to make well-informed purchasing decisions."))
               ),
               
               hr(),
               
               h3(strong("Data Sources and Acquisition")),
               p("The data for this project is sourced primarily through web scraping."),
               tags$ul(
                 tags$li(HTML("<strong>Source:</strong> GSM Arena website (<code>https://www.gsmarena.com</code>).")),
                 tags$li(HTML("<strong>Scope:</strong> Covers smartphone models from <strong>2007 to 2024</strong>.")),
                 tags$li(HTML("<strong>Ethical Scraping:</strong> Data collection is limited to publicly available product specifications and strictly rate-limited to avoid overwhelming the source website."))
               ),
               
               hr(),
               
               h3(strong("Proposed Analysis and Research Questions")),
               p("The analysis combines Exploratory Data Analysis (EDA) with visualization of long-term feature trends, focusing on the Top 15/20 Phones from 2017-2023."),
               
               h4(strong("Exploratory Data Analysis (EDA) Topics:")),
               tags$ul(
                 tags$li("Data Cleaning: Standardizing and extracting numerical values for specifications."),
                 tags$li("Distribution Analysis: Examining RAM, Storage, and OS type distributions."),
                 tags$li("Feature Correlation: Investigating the relationship between physical and performance attributes, like battery size vs. phone weight.")
               ),
               
               h4(strong("Key Trend Research Questions:")),
               tags$ul(
                 tags$li("How have key features (average battery size, camera resolution, display size) evolved over time?"),
                 tags$li("How has the market dominance of top phone brands shifted?"),
                 tags$li("What are the long-term trends in phone weight and screen size?")
               )
             )
    )
  )
)


# --- 4. PHONE FILTERING FUNCTION (Simplified for Suggester) ⚙️ ---

# This function applies all filters selected by the user
filter_phones <- function(data, brand_input = NULL, os_type_input = NULL, release_date_input = NULL, 
                          storage_min = NULL, storage_max = NULL, display_min = NULL, display_max = NULL, 
                          camera_min = NULL, camera_max = NULL, ram_min = NULL, ram_max = NULL, 
                          battery_min = NULL, battery_max = NULL) {
  
  data %>% 
    filter(
      # Brand Filter
      (is.null(brand_input) | brand == brand_input) & 
        # OS Filter
        (is.null(os_type_input) | os_category == os_type_input) & 
        # Year Filter
        (is.null(release_date_input) | (!is.na(release_date_numeric) & release_date_numeric >= release_date_input)) & 
        # Storage Filters (using numeric GB)
        (is.null(storage_min) | (!is.na(storage_numeric) & storage_numeric >= storage_min)) & 
        (is.null(storage_max) | (!is.na(storage_numeric) & storage_numeric <= storage_max)) & 
        # Display Filters (using numeric inches)
        (is.null(display_min) | (!is.na(display_size_numeric) & display_size_numeric >= display_min)) & 
        (is.null(display_max) | (!is.na(display_size_numeric) & display_size_numeric <= display_max)) & 
        # Camera Filters (using numeric MP)
        (is.null(camera_min) | (!is.na(camera_numeric) & camera_numeric >= camera_min)) & 
        (is.null(camera_max) | (!is.na(camera_numeric) & camera_numeric <= camera_max)) & 
        # RAM Filters (using numeric GB)
        (is.null(ram_min) | (!is.na(ram_numeric) & ram_numeric >= ram_min)) & 
        (is.null(ram_max) | (!is.na(ram_numeric) & ram_numeric <= ram_max)) & 
        # Battery Filters (using numeric mAh)
        (is.null(battery_min) | (!is.na(battery_numeric) & battery_numeric >= battery_min)) & 
        (is.null(battery_max) | (!is.na(battery_numeric) & battery_numeric <= battery_max))
    ) %>% 
    # Sort results by newest release year first
    arrange(desc(release_date_numeric))
}


# --- 5. SERVER LOGIC 🧠 ---

server <- function(input, output, session) {
  options(shiny.legacy.datatable = TRUE) # Ensures compatibility for DataTable rendering
  
  # --- 5.1. Phone Explorer Logic ---
  
  # Update the model dropdown based on selected brand and year range
  observeEvent(list(input$brand_all, input$year_all), {
    filtered_models <- MD$device_name[
      MD$brand == input$brand_all & 
        !is.na(MD$release_date_numeric) & 
        MD$release_date_numeric >= input$year_all[1] & 
        MD$release_date_numeric <= input$year_all[2]
    ]
    
    updateSelectInput(session, "model", 
                      choices = filtered_models, 
                      selected = if (length(filtered_models) > 0) filtered_models[1] else NULL)
  }, ignoreInit = FALSE)
  
  # Render the phone image
  output$image <- renderPlot({
    req(input$model) # Require a model to be selected
    selected_image_link <- MD$image_link[MD$device_name == input$model]
    
    if (length(selected_image_link) > 0 && !is.na(selected_image_link)) {
      # Use magick to read and display the image from the URL
      return(plot(image_read(selected_image_link)))
    }
    
    # Placeholder plot if no image or model is selected
    plot(1, type = "n", axes = F, xlab = "", ylab = "", main = "Select a phone to view its image.")
  }, bg = "#1a1f3a") # Match plot background to the dark theme
  
  # Render the detailed specs as a transposed data table
  output$table <- renderDataTable({
    req(input$model)
    # Exclude technical/non-display columns
    selected_data <- MD[MD$device_name == input$model, !names(MD) %in% c("phone_url", "device_id", "image_link")]
    
    if (nrow(selected_data) == 0) return(NULL)
    
    # Transpose the data (row to column) for easy reading
    transposed_frame <- as.data.frame(t(selected_data), stringsAsFactors = FALSE)
    
    # Format for display
    specs_table <- data.frame(Features = rownames(transposed_frame), 
                              Values = transposed_frame[, 1], 
                              stringsAsFactors = FALSE)
    colnames(specs_table) <- c("","") # Remove column headers for a cleaner look
    specs_table
  }, options = datatable_options)
  
  # --- 5.2. Same Year Releases Logic ---
  
  output$other <- renderDataTable({
    req(input$year_releases_select)
    select_year <- as.numeric(input$year_releases_select)
    
    # Select relevant columns for the table
    release_table <- MD[MD$release_date_numeric == select_year, 
                        c("brand", "device_name", "camera", "ram", "battery", "release_date")]
    
    if (nrow(release_table) == 0) {
      return(data.frame(Message = paste("No releases found in", select_year)))
    }
    
    # Human-readable column names
    colnames(release_table) <- c("Brand", "Model Name", "Camera", "RAM", "Battery", "Full Release Date")
    release_table
  })
  
  # --- 5.3. Top Phones Logic ---
  
  # Update top model list when year changes
  observeEvent(input$top20_year, { 
    updateSelectInput(session, "top20_model", choices = top_phones_data$phone_name[top_phones_data$year == input$top20_year]) 
  })
  
  # Render Top Phone image
  output$top20_image <- renderPlot({
    selected_image_link <- top_phones_data$image_link[top_phones_data$phone_name == input$top20_model]
    if (length(selected_image_link) > 0) return(plot(image_read(selected_image_link)))
    plot(1, type = "n", axes = F, xlab = "", ylab = "", main = "Select a top phone to view its image.")
  }, bg = "#1a1f3a")
  
  # Render Top Phone specs table
  output$top20_table <- renderDataTable({
    selected_data <- top_phones_data[top_phones_data$phone_name == input$top20_model, 
                                     !names(top_phones_data) %in% c("phone_link", "image_link")]
    if (nrow(selected_data) == 0) return(NULL)
    
    transposed_frame <- as.data.frame(t(selected_data), stringsAsFactors = FALSE)
    t_data <- data.frame(Features = rownames(transposed_frame), 
                         Values = transposed_frame[, 1], 
                         stringsAsFactors = FALSE)
    colnames(t_data) <- c("", "")
    t_data
  }, options = datatable_options)
  
  # Top Phones Plots (Visualizing Trends)
  
  # Filter top phones data based on the year range slider
  top_phones_filtered <- reactive({ 
    top_phones_data %>% 
      filter(year >= input$Dominance_year[1] & year <= input$Dominance_year[2]) 
  })
  
  # Custom ggplot theme for dark background
  dark_plot_theme <- theme(
    plot.background = element_rect(fill = "#1a1f3a", color = NA), 
    panel.background = element_rect(fill = "#1a1f3a", color = NA), 
    panel.grid = element_line(color = "#2d3561"), 
    text = element_text(color = "#e0e0e0"), 
    axis.text = element_text(color = "#b8c1ec"), 
    legend.background = element_rect(fill = "#1a1f3a"), 
    legend.text = element_text(color = "#e0e0e0"), 
    legend.position = "bottom",
    plot.title = element_text(color = "#e0e0e0")
  )
  
  # Brand Dominance Plot
  output$Dominance_plot <- renderPlot({ 
    top_phones_filtered() %>% 
      group_by(year, brand_name) %>% 
      summarise(phone_count = n(), .groups = 'drop') %>% 
      ggplot(aes(x = year, y = phone_count, color = brand_name, group = brand_name)) + 
      geom_line(size = 0.5) + 
      geom_point(size = 2) + 
      labs(x = "Year", y = "Number of Phones", color = "Brand") + 
      theme_minimal() + 
      dark_plot_theme 
  })
  
  # SIM Card Trend Plot
  output$Sim_plot <- renderPlot({ 
    top_phones_filtered() %>% 
      mutate(sim_type = sub(" .*", "", sim)) %>% # Extract primary SIM type
      group_by(year, sim_type) %>% 
      summarise(phone_count = n(), .groups = 'drop') %>% 
      ggplot(aes(x = year, y = phone_count, color = sim_type, group = sim_type)) + 
      geom_line(size = 0.5) + 
      geom_point(size = 2) + 
      labs(x = "Year", y = "Number of Phones", color = "SIM") + 
      theme_minimal() + 
      dark_plot_theme 
  })
  
  # Phone Weight Trend Plot
  output$Weights <- renderPlot({ 
    top_phones_filtered() %>% 
      mutate(weight_g = as.numeric(sub(" .*", "", weight))) %>% # Extract numeric weight
      ggplot(aes(x = year, y = weight_g)) + 
      geom_point(size = 2, color = "#667eea") + 
      labs(x = "Year", y = "Weight (g)") + 
      theme_minimal() + 
      dark_plot_theme + 
      theme(legend.position = "none") 
  })
  
  # Weight vs Battery Size Correlation Plot
  output$WBS <- renderPlot({ 
    top_phones_filtered() %>% 
      mutate(
        weight_g = as.numeric(sub(" .*", "", weight)),
        battery_size_mah = as.numeric(gsub("[^0-9]", "", battery_size....battery_sizes))
      ) %>% 
      ggplot(aes(x = battery_size_mah, y = weight_g)) + 
      geom_point(size = 2, color = "#764ba2") + 
      labs(x = "Battery Size (mAh)", y = "Weight (g)") + 
      theme_minimal() + 
      dark_plot_theme + 
      theme(legend.position = "none") 
  })
  
  # Average Battery Size Evolution Plot
  output$BatSize <- renderPlot({ 
    top_phones_filtered() %>% 
      mutate(battery_size_mah = as.numeric(gsub("[^0-9]", "", battery_size....battery_sizes))) %>% 
      ggplot(aes(x = factor(year), y = battery_size_mah)) + 
      stat_summary(fun = "mean", geom = "bar", fill = "#667eea", color = "#764ba2") + 
      labs(title = "Average Battery Size by Year", x = "Year", y = "Average Battery Size (mAh)") + 
      theme_minimal() + 
      dark_plot_theme 
  })
  
  # --- 5.4. Compare Phones Logic ---
  
  # Update models for Phone 1
  observeEvent(list(input$compare_brand1, input$compare_year1), { 
    f_m1 <- MD$device_name[
      MD$brand == input$compare_brand1 & 
        !is.na(MD$release_date_numeric) & 
        MD$release_date_numeric >= input$compare_year1[1] & 
        MD$release_date_numeric <= input$compare_year1[2]
    ]
    updateSelectInput(session, "compare_model1", choices = f_m1) 
  })
  
  # Update models for Phone 2
  observeEvent(list(input$compare_brand2, input$compare_year2), { 
    f_m2 <- MD$device_name[
      MD$brand == input$compare_brand2 & 
        !is.na(MD$release_date_numeric) & 
        MD$release_date_numeric >= input$compare_year2[1] & 
        MD$release_date_numeric <= input$compare_year2[2]
    ]
    updateSelectInput(session, "compare_model2", choices = f_m2) 
  })
  
  # Render Phone 1 image
  output$compare_image1 <- renderPlot({ 
    selected_image_link <- MD$image_link[MD$device_name == input$compare_model1]
    if (length(selected_image_link) > 0) return(plot(image_read(selected_image_link))) else plot(1, type = "n", axes = F, xlab = "", ylab = "", main = "Select Phone 1 for image.") 
  }, bg = "#1a1f3a")
  
  # Render Phone 2 image
  output$compare_image2 <- renderPlot({ 
    selected_image_link <- MD$image_link[MD$device_name == input$compare_model2]
    if (length(selected_image_link) > 0) return(plot(image_read(selected_image_link))) else plot(1, type = "n", axes = F, xlab = "", ylab = "", main = "Select Phone 2 for image.") 
  }, bg = "#1a1f3a")
  
  # Render comparison table
  output$compare_table <- renderDataTable({
    req(input$compare_model1, input$compare_model2)
    
    # Get cleaned data for selected phones
    comparison_data <- MD[, !names(MD) %in% c("phone_url", "device_id", "image_link")]
    
    phone1_data <- as.data.frame(t(comparison_data[comparison_data$device_name == input$compare_model1, ]), stringsAsFactors = FALSE)
    phone2_data <- as.data.frame(t(comparison_data[comparison_data$device_name == input$compare_model2, ]), stringsAsFactors = FALSE)
    
    if (nrow(phone1_data) == 0 || nrow(phone2_data) == 0) return(NULL)
    
    # Combine transposed data frames
    compare_phones_data <- data.frame(
      Features = rownames(phone1_data),
      Phone1_Value = phone1_data[, 1],
      Phone2_Value = phone2_data[, 1],
      stringsAsFactors = FALSE
    )
    
    # Use selected model names as headers
    colnames(compare_phones_data) <- c("Features", input$compare_model1, input$compare_model2)
    
    # Hide column headers and use datatable_options for styling
    compare_phones_data
  }, options = datatable_options)
  
  # --- 5.5. Phone Suggester Logic ---
  
  # Reactive filter that runs when the "Apply Filters" button is clicked
  filtered_phones <- eventReactive(input$apply_filters, {
    # Call the human-readable filter function
    filter_phones(
      PD, 
      brand_input = if (input$suggester_brand != "Any") input$suggester_brand else NULL, 
      os_type_input = if (input$os_type != "Any") input$os_type else NULL, 
      release_date_input = input$release_date, 
      storage_min = input$storage_min, 
      storage_max = input$storage_max, 
      display_min = input$display_min, 
      display_max = input$display_max, 
      camera_min = input$camera_min, 
      camera_max = input$camera_max, 
      ram_min = input$ram_range[1], 
      ram_max = input$ram_range[2], 
      battery_min = input$battery_min, 
      battery_max = input$battery_max
    )
  })
  
  # Render the phone cards for suggested phones
  output$filtered_phones_ui <- renderUI({
    phones <- filtered_phones()
    
    if (nrow(phones) == 0) {
      return(h4("No phones match the selected criteria.", style = "color:#b8c1ec;"))
    }
    
    # Create a list of UI elements (cards) for each phone
    phone_cards <- lapply(1:nrow(phones), function(i) {
      p <- phones[i, ]
      tags$div(
        class = "card mb-3",
        tags$img(src = p$image_link, style = "width:100%;height:auto;border-radius:10px 10px 0 0;", alt = p$device_name),
        tags$div(
          class = "card-body",
          tags$h5(class = "card-title", tags$a(href = p$phone_url, target = "_blank", p$device_name)),
          tags$p(class = "card-text", HTML(paste0("<strong>Brand:</strong> ", p$brand))),
          tags$p(class = "card-text", HTML(paste0("<strong>OS:</strong> ", p$os_type))),
          tags$p(class = "card-text", HTML(paste0("<strong>Storage:</strong> ", p$storage))),
          tags$p(class = "card-text", HTML(paste0("<strong>Camera:</strong> ", p$camera))),
          tags$p(class = "card-text", HTML(paste0("<strong>RAM:</strong> ", p$ram))),
          tags$p(class = "card-text", HTML(paste0("<strong>Battery:</strong> ", p$battery)))
        )
      )
    })
    
    # Arrange the cards in a grid layout
    tags$div(
      class = "card-deck", 
      style = "display:grid;grid-template-columns:repeat(auto-fill, minmax(280px, 1fr));gap:20px;", 
      do.call(tagList, phone_cards)
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)