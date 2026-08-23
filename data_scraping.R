library(rvest)
library(tidyverse)
library(dplyr)

url <- read_html("https://www.gsmarena.com/")
brand_links <- url%>%html_elements(".brandmenu-v2.light.l-box.clearfix a")%>%html_attr("href")
brand_names <- url%>%html_elements(".brandmenu-v2.light.l-box.clearfix a")%>%html_text()
head(brand_links)
length(brand_links)
#cleaning brand links
brand_links <- brand_links[-1]
brand_links <- head(brand_links,-2)
brand_links

#creating full links
full_links <- paste0("https://www.gsmarena.com/", brand_links)
head(full_links)
length(full_links)

#cleaning brand names
brand_names <- head(brand_names,-2)
brand_names <- brand_names[-1]
length(brand_names)
brand_names

#extracting details of each model
library(tibble)

###extracting model names with their links and brand names
all_brand_names <- character(0)
all_model_names <- character(0)
all_model_links <- character(0)

for (i in 1:length(full_links)) {
  
  # Use tryCatch to prevent a single error from stopping the entire loop
  tryCatch({
    
    # Let the user know the progress
    print(paste("Scraping brand:", brand_names[i], "(", i, "of", length(full_links), ")"))
    
    # Go to the brand page
    brand_page <- read_html(full_links[i])
    
    # Get all the model names and links for THIS brand
    models_on_page <- brand_page %>% html_elements(".makers li a")
    new_model_names <- models_on_page %>% html_element("strong span") %>% html_text()
    new_model_links <- models_on_page %>% html_attr("href")
    
    # Find out exactly how many models we just found
    num_models_found <- length(new_model_names)
    
    # Create the corresponding brand names vector
    new_brand_names <- rep(brand_names[i], num_models_found)
    
    # Append the new data to our master vectors
    all_brand_names <- c(all_brand_names, new_brand_names)
    all_model_names <- c(all_model_names, new_model_names)
    all_model_links <- c(all_model_links, new_model_links)
    
    # --- THE MOST IMPORTANT PART ---
    # Be polite: Pause for a random time between 1 and 3 seconds
    Sys.sleep(runif(1, min = 1, max = 3))
    
  }, error = function(e) {
    # If an error occurs, print a message and continue to the next iteration
    print(paste("!!! Failed to scrape", brand_names[i], "- Skipping. Error:", e$message))
  })
}

# --- INNER LOOP: Iterate through each MODEL on the page ---
# Assuming 'all_model_links' contains your relative URLs
full_model_links <- paste0("https://www.gsmarena.com/", all_model_links)

# Now 'full_model_links' contains the complete, correct URLs
head(full_model_links)
all_model_names
all_model_links <- full_model_links

###removing watch products
final_phone_list <- tibble(
  brand = all_brand_names,
  model = all_model_names,
  link = all_model_links
)

library(stringr) # For str_detect()

# Filter the tibble to remove rows containing "watch" (case-insensitive)
phones_only <- final_phone_list %>%
  filter(!str_detect(model, regex("watch", ignore_case = TRUE)))

# See the total number of products after filtering
print(paste("Total products after filtering:", nrow(phones_only)))
write.csv(phones_only,"phones_only.csv")

# ###loading csv
# phones_only <- read.csv("phones_only.csv")
# all_model_links <- phones_only$link
# head(all_model_links)
# all_brand_names <- phones_only$brand
# all_model_names <- phones_only$model
# 
# ###getting individual info of each product
# total_number_of_models <- nrow(phones_only)
# battery_mAh <- character(total_number_of_models)
# camera <- character(total_number_of_models)
# display_inch <- character(total_number_of_models)  ##extract in inches
# weight_gm <- character(total_number_of_models)
# thickness_mm <- character(total_number_of_models)
# max_ram_gb <- character(total_number_of_models)
# storage_gb <- character(total_number_of_models)
# os <- character(total_number_of_models)
# release_date <- character(total_number_of_models)
# 
# library(httr) # Make sure this is loaded
# urll <- read_html(all_model_links[1])
# os <- urll%>%html_element("span[data-spec='os-hl']")%>%html_text()
# os


#########


library(httr)

phones_only <- read.csv("phones_only.csv")

# For testing, you can uncomment the line below to run on a smaller sample
#phones_only <- head(phones_only, 20)

# Get total number of models for loop and vector initialization
total_models <- nrow(phones_only)

# --- INITIALIZE VECTORS ---
# Create empty character vectors to store the scraped data.
# The names match your target CSV file.
release_date       <- character(total_models)
image_link         <- character(total_models)
body_detail        <- character(total_models)
os_type            <- character(total_models)
storage            <- character(total_models)
display_size       <- character(total_models)
display_resolution <- character(total_models)
camera             <- character(total_models)
video              <- character(total_models)
ram                <- character(total_models)
chipset            <- character(total_models)
battery            <- character(total_models)
battery_type       <- character(total_models)

# A list of different browser identities to avoid being blocked
user_agents <- c(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/108.0.0.0 Safari/537.36",
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Firefox/115.0",
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/108.0.0.0 Safari/537.36",
  "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/108.0.0.0 Safari/537.36"
)


# --- MAIN SCRAPING LOOP ---
# This loop iterates through each phone link from your CSV
for(i in 1:total_models) {
  
  # Get the current phone's URL and name for the progress message
  current_url <- phones_only$link[i]
  current_name <- phones_only$model[i]
  
  print(paste("Scraping model", i, "of", total_models, ":", current_name))
  
  # Skip if the link is missing or empty
  if (is.na(current_url) || current_url == "") {
    print("   ...Skipping due to invalid link.")
    next 
  }
  
  tryCatch({
    # Randomly select a new User-Agent for each request to look more human
    current_user_agent <- sample(user_agents, 1)
    
    # Make the HTTP GET request to the website
    response <- GET(url = current_url, user_agent(current_user_agent))
    
    # Proceed only if the request was successful (status code 200)
    if (status_code(response) == 200) {
      
      # Parse the HTML content of the page
      page_content <- read_html(response)
      
      # --- EXTRACT ALL SPECIFICATIONS USING THEIR 'data-spec' ATTRIBUTES -
      
      # General Info
      release_date[i] <- page_content %>% html_element("span[data-spec='released-hl']") %>% html_text()
      image_link[i]   <- page_content %>% html_element(".specs-photo-main a img") %>% html_attr("src")
      
      # Body & OS
      body_detail[i] <- page_content %>% html_element("span[data-spec='body-hl']") %>% html_text()
      os_type[i]     <- page_content %>% html_element("span[data-spec='os-hl']") %>% html_text()
      
      # Memory
      storage[i] <- page_content %>% html_element("span[data-spec='storage-hl']") %>% html_text()
      ram[i]     <- page_content %>% html_element("span[data-spec='ramsize-hl']") %>% html_text()
      
      # Display
      display_size[i]       <- page_content %>% html_element("span[data-spec='displaysize-hl']") %>% html_text()
      display_resolution[i] <- page_content %>% html_element("div[data-spec='displayres-hl']") %>% html_text()
      
      # Camera
      camera[i] <- page_content %>% html_element("span[data-spec='camerapixels-hl']") %>% html_text()
      video[i]  <- page_content %>% html_element("div[data-spec='videopixels-hl']") %>% html_text()
      
      # Platform & Battery
      chipset[i]      <- page_content %>% html_element("div[data-spec='chipset-hl']") %>% html_text()
      battery[i]      <- page_content %>% html_element("span[data-spec='batsize-hl']") %>% html_text() # Highlight is easiest for battery size
      battery_type[i] <- page_content %>% html_element(".head-icon.icon-wired-charging") %>% html_text()
      
    } else {
      # Handle cases where the website returns an error (e.g., 404 Not Found)
      print(paste("   ...Skipping due to bad HTTP status:", status_code(response)))
      
      # If we get rate-limited (429), take an extra-long pause
      if (status_code(response) == 429) {
        print("   ...Got a 429 (Too Many Requests). Pausing for 60 seconds...")
        Sys.sleep(60)
      }
    }
    
    # Be polite: Pause between all requests to avoid overwhelming the server
    print("   ...Success. Pausing for 4-8 seconds.")
    Sys.sleep(runif(1, min = 4, max = 8))
    
  }, error = function(e) {
    # If any other error occurs during the scraping, print it and move on
    print(paste("   ...Skipping due to an error:", e$message))
  })
}

print("--- Scraping complete! ---")


# --- COMBINE AND SAVE DATA ---
# Create a final tibble (a modern data frame) with all the data
final_phone_data <- tibble(
  brand = phones_only$brand,
  phone_url = phones_only$link,
  device_name = phones_only$model,
  release_date = release_date,
  image_link = image_link,
  body_detail = body_detail,
  os_type = os_type,
  storage = storage,
  display_size = display_size,
  display_resolution = display_resolution,
  camera = camera,
  video = video,
  ram = ram,
  chipset = chipset,
  battery = battery,
  battery_type = battery_type
)
mutate(
  release_year = str_extract(release_date, "\\d{4}")
)

# Display the first few rows of the final table
print(head(final_phone_data))

# Save the complete dataset to a CSV file
write.csv(final_phone_data, "complete_phones_data.csv", row.names = FALSE)

print("--- Data successfully saved to gsmarena_full_specs.csv ---")