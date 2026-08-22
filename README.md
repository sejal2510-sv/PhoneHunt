# PhoneHunt

PhoneHunt is an R-based smartphone trend analysis project that uses GSMArena data to explore phone specifications, market trends, and comparisons through an interactive Shiny dashboard.

## Features

* Scrapes smartphone specifications from GSMArena.
* Creates comprehensive and Top 20 smartphone datasets.
* Cleans and preprocesses phone specifications for analysis.
* Analyzes trends in battery capacity, phone weight, brand popularity, and SIM support.
* Provides an interactive R Shiny dashboard for:

  * Exploring phone specifications
  * Comparing two phones
  * Finding phones based on requirements
  * Exploring popular phones and yearly trends
  * Viewing phones released in a selected year

## Datasets

The project uses two main datasets:

* `complete_phones_data.csv` — Detailed information for a broad range of smartphone models.
* `top_20_phones_2017-2023.csv` — Top 20 popular phones from each year between 2017 and 2023.

## Technologies Used

* **R**
* **R Shiny**
* **Tidyverse**
* **rvest**
* **dplyr**
* **ggplot2**
* **stringr**
* **httr**
* **shinythemes**
* **magick**

## Key Analysis

The project explores smartphone trends from 2017–2023, including:

* Brand popularity
* Battery capacity
* Phone weight
* Battery-to-weight correlation
* SIM card trends
* Smartphone specification comparisons

The analysis found that average battery capacity increased from 3450 mAh in 2017 to 4910 mAh in 2023, while average phone weight increased from 168 g to 194 g.

## Running the Project

### 1. Data Collection

Install the required R packages and run the data scraping scripts:

* `top_20_phones_2017-2023.R`
* `data_scraping.R`

These scripts generate the required CSV datasets.

### 2. Run the Shiny App

Make sure the following files are in the same working directory:

```text
shiny_app.R
complete_phones_data.csv
top_20_phones_2017-2023.csv
```

Install the required packages:

```r
install.packages(c(
  "shiny",
  "ggplot2",
  "shinythemes",
  "magick",
  "dplyr",
  "stringr"
))
```

Open `shiny_app.R` in RStudio and click **Run App**.

## Data Source

The smartphone data is collected from **GSMArena**, a public source of smartphone specifications.

## Limitations

* The scraper depends on the HTML structure of GSMArena and may require updates if the website changes.
* Collecting the complete dataset can take several hours because of request delays.
* Some uncommon specification formats may not be parsed correctly.

## Project Context

This project was developed for **Data Science Lab 1 (MTH208)** at the **Indian Institute of Technology, Kanpur**.

## License

No specific license is provided in the project report.
