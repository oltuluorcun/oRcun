## This script consist an example of;
## web scrapping with "rvest"
## data visualization with "leaflet"

## This script is created to illustrate covid-19 risks for each city in Turkey
## citywide weekly covid cases per 100k 
## the ones with less than; 
## 100 cases per 100k assigned "low risk" 
## 150 cases per 100k assigned "moderate risk" 
## 200 cases per 100k assigned "high risk" 
## finally, the ones with more than 200 of cases per 100k assigned "very high risk" 

## the data is collected from the official web page of ministry of health in Turkey
## Longitude and lattitude values for each city is taken from a random web page (beycan.net)

############################################################################################

## Load required packages. 

inst_pack_func <- function(list.of.packages){
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  lapply(list.of.packages,function(x){library(x,character.only=TRUE)})
}

list.of.packages <- c("tidyverse","magrittr",
                      "rvest","leaflet")

inst_pack_func(list.of.packages)

## Gather Covid-19 data. 

url_saglikbak <- "https://covid19.saglik.gov.tr/"

webpage_saglikbak <- read_html(url_saglikbak)
covid_data_html <- html_nodes(webpage_saglikbak,'td, th')
covid_matrix_format <- matrix(html_text(covid_data_html), ncol = 2, byrow = T)
colnames(covid_matrix_format) <- covid_matrix_format[1,]
covid_matrix_format <- covid_matrix_format[-1,]
numeric_Value <- gsub("[,]",".",covid_matrix_format[,2]) %>% 
  gsub(" ", "", ., fixed = TRUE) %>% 
  as.numeric()

covid_weekly <- data.frame(City = covid_matrix_format[,1],
                           Value = numeric_Value)

## Getting Long-Latt values for each city in Turkey

url_latlong <- "http://www.beycan.net/1057/illerin-enlem-ve-boylamlari.html"
webpage_latlong <- read_html(url_latlong)
latlong_data_html <- html_nodes(webpage_latlong,'td, th')
latlong_matrix_format <- matrix(html_text(latlong_data_html), ncol = 4, byrow = T)
colnames(latlong_matrix_format) <- latlong_matrix_format[1,]
latlong_matrix_format <- latlong_matrix_format[-1,3:4]

latlong_data <- apply(latlong_matrix_format, 2, 
                      function(x) as.numeric(gsub("[,]",".",x)))

## Data Prep.

covid_data_final <- data.frame(covid_weekly, 
                               Latitude = latlong_data[,1],
                               Longitude = latlong_data[,2])

covid_data_final$City <- gsub(" ", "", covid_data_final$City, fixed = TRUE)

covid_data_final %<>% 
  mutate(Risk = ifelse(Value <= 100, "Low Risk",
                       ifelse(Value <= 150, "Moderate Risk",
                              ifelse(Value <= 200, "High Risk", "Very High Risk"))))

color_vector <- colorFactor(c("Blue", "Yellow", "Dark Orange", "Dark Red"),  
                            domain = covid_data_final$Risk)

## Visualization - Leaflet

covid_data_final %>% 
  leaflet() %>% 
  addProviderTiles("Esri") %>% 
  addCircles(~Longitude, ~Latitude, 
             weight = 15,
             popup = paste0(
               "<b>City: </b>", 
               covid_data_final$City,
               "<br>",
               "<b>cases per 100k: </b>",
               covid_data_final$Value),
             label = ~Risk,
             color = ~color_vector(Risk)) %>% 
  setView(lng = median(covid_data_final$Longitude),
          lat = median(covid_data_final$Latitude),
          zoom = 5)
