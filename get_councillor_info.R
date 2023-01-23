library(httr)
library(XML)
library(dplyr)
library(rvest)
library(leaflet)
library(here)
library(sf)
library(dplyr)
library(tidyr)
library(purrr)
library(mapview)
library(leafem)
library(leaflet.extras)
library(htmlwidgets)
library(htmltools)

get_councillor_info <- function() {
  simple <- read_html("https://www.suffolk.gov.uk/council-and-democracy/councillors-and-elected-representatives/find-your-councillor")
  
  # Find the number of pages
  n_pages <- simple %>%
    html_nodes("button") |> 
    html_text() |> 
    as.integer() 
  
  n_pages[is.na(n_pages)] <- 0
  
  n_pages <- max(n_pages)
  
  # Scrape all paginated pages
  base_link <- "https://www.suffolk.gov.uk/council-and-democracy/councillors-and-elected-representatives/find-your-councillor"
  index_link <- "?pageIndex="
  
  all_links <- paste0(base_link, index_link, 2:n_pages)
  
  all_links <- c(base_link, all_links)
  
  pages <- all_links |>
    map(read_html)
  
  councillors <- pages |> 
    map(html_nodes, "a") |> 
    map(html_attr, "href")
  
  councillors <- unlist(councillors)
  councillors <- as.data.frame(councillors)
  
  councillors <- councillors |> 
    dplyr::filter(stringr::str_detect(councillors, "find-your-councillor/"))
  
  councillors$councillors <- paste0("https://www.suffolk.gov.uk", councillors$councillors)
  
  # Get all information off individual councillor's websites
  
  scc_pages <- councillors$councillors |> 
    map(read_html)
  
  scc_profile <- scc_pages |> 
    map(html_nodes, "div.profile__line") |> 
    map(html_text2)
  
  name <- scc_pages |> 
    map(html_nodes, "div.hero__content") |> 
    map(html_text2)
  
  image <- scc_pages |> 
    map(html_nodes, "img") |> 
    map(html_attr, "src")
  
  councillors$party <- unlist(map(scc_profile, 1))
  councillors$division <- unlist(map(scc_profile, 2))
  councillors$image <- paste0("https://www.suffolk.gov.uk", unlist(map(image, 2)))
  councillors$name <- unlist(name)
  
  # Clean up data and add name
  councillors$party <- gsub("Party", "", councillors$party)
  councillors$division <- gsub("Electoral Division", "", councillors$division)
  
  colnames(councillors)[1] <- "html"
  
  return(councillors)
}
