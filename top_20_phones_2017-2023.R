# --- 1. LOAD LIBRARIES ---
library(rvest)
library(tidyverse)
library(httr)
library(dplyr)
library(tibble)

# --- 2. INITIAL SETUP ---
# Base URL for GSM Arena website
base_url <- "https://www.gsmarena.com/"

# List of years from 2017-2023
years <- 2017:2023
# URLs of all pages that displayed top 20 smartphones yearwise
urls <- c(
  "the_20_most_popular_phones_of_2017-news-28892.php",
  "the_top_20_most_popular_phones_of_2018-news-34853.php",
  "top_20_phones_of_2019-news-40387.php",
  "top_20_most_popular_phones_in_2020-news-46737.php",
  "top_20_phones_of_the_year-news-52447.php",
  "top_20_phones_of_the_year_2022-news-56761.php",
  "top_20_phones_of_the_year_2023-news-61070.php"
)

# --- 3. THE SCRAPING FUNCTION (WITH FIXES) ---
scrape_phones <- function(year, url_suffix) {
  
  # Get the main list of phones for the year
  page <- read_html(paste0(base_url, url_suffix))
  phone_names <- page %>% html_elements(".phone-name a") %>% html_text()
  phone_links <- page %>% html_elements(".phone-name a") %>% html_attr("href")
  
  # Extract brand names
  brand_vector <- strsplit(phone_names, " ")
  brand_names <- sapply(brand_vector, function(x) x[1])
  
  # Initialize empty vectors to store phone details
  image_links <- vector()
  networks <- vector()
  launch_years <- vector()
  statuses <- vector()
  dimensions <- vector()
  weights <- vector()
  builds <- vector()
  sims <- vector()
  other_body_features <- vector()
  display_types <- vector()
  display_sizes <- vector()
  display_resolutions <- vector()
  display_protections <- vector()
  display_others <- vector()
  oss <- vector()
  chipsets <- vector()
  cpus <- vector()
  gpus <- vector()
  memory_slots <- vector()
  internal_memories <- vector()
  main_cameras <- vector()
  selfie_cameras <- vector()
  sensors <- vector()
  battery_sizes <- vector()
  charging_watts <- vector()
  test_scores <- vector()
  
  # Loop through each phone URL to extract its details
  for (link in phone_links) {
    
    # --- FIX 1: Pause between requests to avoid being blocked ---
    # Pauses for a random time between 2 and 5 seconds.
    pause_duration <- runif(1, min = 2, max = 5)
    cat("Scraping:", link, "... Pausing for", round(pause_duration, 1), "seconds.\n")
    Sys.sleep(pause_duration)
    
    # Define variables as NA before the request
    # This ensures they have a value if the request fails
    network <- NA; launch_year <- NA; status <- NA; dimension <- NA; weight <- NA; build <- NA; sim <- NA;
    other_body <- NA; display_type <- NA; display_size <- NA; display_resolution <- NA;
    display_protection <- NA; display_other <- NA; os <- NA; chipset <- NA; cpu <- NA; gpu <- NA;
    memory_slot <- NA; internal_memory <- NA; main_camera <- NA; selfie_camera <- NA;
    sensor <- NA; battery_size <- NA; charging_watt <- NA; test_score <- NA; image_link <- NA
    
    tryCatch({
      # --- FIX 2: Make the request using httr and a User-Agent ---
      response <- httr::GET(
        url = paste0(base_url, link),
        httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/108.0.0.0 Safari/537.36")
      )
      
      # --- FIX 3: Check if the request was successful ---
      if (httr::status_code(response) == 200) {
        phone_page <- httr::content(response, as = "text") %>% read_html()
        
        # Scrape all details
        network <- phone_page %>% html_element(".link-network-detail.collapse") %>% html_text(trim = TRUE)
        launch_year <- phone_page %>% html_element("[data-spec='year']") %>% html_text(trim = TRUE)
        status <- phone_page %>% html_element("[data-spec='status']") %>% html_text(trim = TRUE)
        dimension <- phone_page %>% html_element("[data-spec='dimensions']") %>% html_text(trim = TRUE)
        weight <- phone_page %>% html_element("[data-spec='weight']") %>% html_text(trim = TRUE)
        build <- phone_page %>% html_element("[data-spec='build']") %>% html_text(trim = TRUE)
        sim <- phone_page %>% html_element("[data-spec='sim']") %>% html_text(trim = TRUE)
        other_body <- phone_page %>% html_element("[data-spec='bodyother']") %>% html_text(trim = TRUE)
        display_type <- phone_page %>% html_element("[data-spec='displaytype']") %>% html_text(trim = TRUE)
        display_size <- phone_page %>% html_element("[data-spec='displaysize']") %>% html_text(trim = TRUE)
        display_resolution <- phone_page %>% html_element("[data-spec='displayresolution']") %>% html_text(trim = TRUE)
        display_protection <- phone_page %>% html_element("[data-spec='displayprotection']") %>% html_text(trim = TRUE)
        display_other <- phone_page %>% html_element("[data-spec='displayother']") %>% html_text(trim = TRUE)
        os <- phone_page %>% html_element("[data-spec='os']") %>% html_text(trim = TRUE)
        chipset <- phone_page %>% html_element("[data-spec='chipset']") %>% html_text(trim = TRUE)
        cpu <- phone_page %>% html_element("[data-spec='cpu']") %>% html_text(trim = TRUE)
        gpu <- phone_page %>% html_element("[data-spec='gpu']") %>% html_text(trim = TRUE)
        memory_slot <- phone_page %>% html_element("[data-spec='memoryslot']") %>% html_text(trim = TRUE)
        internal_memory <- phone_page %>% html_element("[data-spec='internalmemory']") %>% html_text(trim = TRUE)
        main_camera <- phone_page %>% html_element("[data-spec='cam1modules']") %>% html_text(trim = TRUE)
        selfie_camera <- phone_page %>% html_element("[data-spec='cam2modules']") %>% html_text(trim = TRUE)
        sensor <- phone_page %>% html_element("[data-spec='sensors']") %>% html_text(trim = TRUE)
        battery_size <- phone_page %>% html_element("[data-spec='batsize-hl']") %>% html_text(trim = TRUE)
        charging_watt <- phone_page %>% html_element("[data-spec='battype-hl']") %>% html_text(trim = TRUE)
        test_score <- phone_page %>% html_element("[data-spec='tbench']") %>% html_text(trim = TRUE)
        
        # Extract image link from script tag
        script_content <- phone_page %>% html_elements("script") %>% html_text()
        script_image_links <- str_extract(script_content, 'HISTORY_ITEM_IMAGE = "(.*?)"') %>% str_replace_all('HISTORY_ITEM_IMAGE = "|";', "")
        valid_image_link <- script_image_links[!is.na(script_image_links)]
        image_link <- gsub('[\\",/]$', "", valid_image_link)
        
      } else {
        cat("   -> Failed to load page. Status:", httr::status_code(response), "\n")
      }
    }, error = function(e) {
      cat("   -> An error occurred:", e$message, "\n")
    })
    
    # Append the results (either scraped data or NA) to the vectors
    networks <- c(networks, network); launch_years <- c(launch_years, launch_year); statuses <- c(statuses, status)
    dimensions <- c(dimensions, dimension); weights <- c(weights, weight); builds <- c(builds, build); sims <- c(sims, sim)
    other_body_features <- c(other_body_features, other_body); display_types <- c(display_types, display_type)
    display_sizes <- c(display_sizes, display_size); display_resolutions <- c(display_resolutions, display_resolution)
    display_protections <- c(display_protections, display_protection); display_others <- c(display_others, display_other)
    oss <- c(oss, os); chipsets <- c(chipsets, chipset); cpus <- c(cpus, cpu); gpus <- c(gpus, gpu)
    memory_slots <- c(memory_slots, memory_slot); internal_memories <- c(internal_memories, internal_memory)
    main_cameras <- c(main_cameras, main_camera); selfie_cameras <- c(selfie_cameras, selfie_camera)
    sensors <- c(sensors, sensor); battery_sizes <- c(battery_sizes, battery_size); charging_watts <- c(charging_watts, charging_watt)
    test_scores <- c(test_scores, test_score); image_links <- c(image_links, image_link)
  }
  
  # Create a data frame with all the phone details
  data.frame(
    year = year,
    brand_name = brand_names,
    phone_name = phone_names,
    phone_link = paste0(base_url, phone_links),
    image_link = image_links,
    network = networks,
    launch_year = launch_years,
    status = statuses,
    dimensions = dimensions,
    weight = weights,
    build = builds,
    sim = sims,
    other_body = other_body_features,
    display_type = display_types,
    display_size = display_sizes,
    display_resolution = display_resolutions,
    display_protection = display_protections,
    display_other = display_others,
    os = oss,
    chipset = chipsets,
    cpu = cpus,
    gpu = gpus,
    memory_slot = memory_slots,
    internal_memory = internal_memories,
    main_camera = main_cameras,
    selfie_camera = selfie_cameras,
    sensor = sensors,
    battery_size = battery_sizes,
    charging_watt = charging_watts,
    test_score = test_scores,
    stringsAsFactors = FALSE
  )
}

# --- 4. RUN THE SCRAPER ---
# Initialize an empty list to store our data frames
phone_data_list <- list()

# Loop through each year and the corresponding url suffix
for (i in seq_along(years)) {
  cat("\n--- Starting to scrape phones for the year:", years[i], "---\n")
  phone_data <- scrape_phones(years[i], urls[i])
  phone_data_list[[i]] <- phone_data
}

# --- 5. COMBINE AND SAVE THE DATA ---
# Combine all the data frames into one
all_phone_data <- bind_rows(phone_data_list)

# Save all the data into a CSV file
write_csv(all_phone_data, "top_20_phones_2017-2023.csv")

cat("\n--- All done! Data successfully saved to top_20_phones_2017-2023.csv ---\n")