library(dplyr)
library(rvest)
library(stringr)
library(htmltab)

### creating web crawler for postbank branch locations
### function scrapeCitiesPlz covers the entirety of Germany
rm(list = ls())

url <- "https://www.postbank.de/dienste/gaa_filialsuche/index_layer.html?x=cn&SearchType=BRANCHES_COORD_BASED&MaxRadius=50000&IsoCountryCode=DE&Top=24&Branches=POST&Zip=&City=Berlin&ZipCity=&Street=&SingleSlot=Berlin&SingleSlotGeo=Berlin"
page <- read_html(url)
links <- page %>%
  html_nodes(xpath = '//ul[@class="f3-widget-paginator"]/li/a')

scrapeLargestCities <- function() {
  #scraping postbank locations for the largest cities in Germany
  url_cities <- "https://de.wikipedia.org/wiki/Liste_der_Gro%C3%9Fst%C3%A4dte_in_Deutschland"
  cities <- read_html(url_cities)

  table <- htmltab(doc = url_cities, which = 1)
  list_city <- table[2][[1]]

  list_city <- list_city %>%
    gsub(pattern = "Ã¼", replacement = "ü") %>%
    gsub(pattern = "Ã¶", replacement = "ö") %>%
    gsub(pattern = "Â", replacement = "") %>%
    str_extract("[:alpha:]+")
  list_city_edited <- str_replace_all(list_city, pattern = "ü", replacement = "%C3%BC")

  links <- c()
  for (i in list_city_edited) {
    links <- c(links, paste0("https://www.postbank.de/dienste/gaa_filialsuche/index_layer.html?x=cn&SearchType=BRANCHES_COORD_BASED&MaxRadius=50000&IsoCountryCode=DE&Top=24&Branches=POST&Zip=&City=", i, "&ZipCity=&Street=&SingleSlot=", i, "&SingleSlotGeo=", i))
  }
  links[match("Halle", list_city_all)] <- "https://www.postbank.de/dienste/gaa_filialsuche/index_layer.html?x=xn&filter=ALL&atmsearch=&debug=&BC=&width=918&MapHeight=350&LocX=9.56395&LocY=51.99405&SingleSlotGeo=37620%20Halle"
  links[match("Offenbach", list_city_all)]  <- "https://www.postbank.de/dienste/gaa_filialsuche/index_layer.html?x=xn&filter=ALL&atmsearch=&debug=&BC=&width=918&MapHeight=350&LocX=9.56395&LocY=51.99405&SingleSlotGeo=37620%20Halle"
  streets <- c()
  localities <- c()
  types <- c()
  missing_loc_all <- c()
  for (site in links) {
    page <- try(read_html(site, verbose = TRUE))
    street <- page %>%
      html_nodes(xpath = '//span[@class="ym-Address-Street"]') %>%
      html_text()
    streets <- c(streets, street)
    print(paste("Length Total Streets:", length(streets)))
    locality <- page %>%
      html_nodes(xpath = '//span[@class="ym-Address-Locality"]') %>%
      html_text()
    localities <- c(localities, locality)
    print(paste("Length Total Localities:", length(localities)))
    type <- page %>%
      html_nodes(xpath = '//strong[@class="ym-Address-Name"]') %>%
      html_text()
    types <- c(types, type)
    print(paste("Length Total Types:", length(types)))
    print("Done!")
    if (length(street) < 1 & length(locality) < 1 & length(type) < 1) {
      print("Location missing due to error!")
      missing_loc_all <- c(missing_loc_all, site)
    }
  }

  while (length(missing_loc_all) > 0) {
    print("Debug: currently in while loop")
    for (site in missing_loc_all) {
      page <- try(read_html(site, verbose = TRUE))
      street <- page %>%
        html_nodes(xpath = '//span[@class="ym-Address-Street"]') %>%
        html_text()
      streets <- c(streets, street)
      print(paste("Length Total Streets:", length(streets)))
      locality <- page %>%
        html_nodes(xpath = '//span[@class="ym-Address-Locality"]') %>%
        html_text()
      localities <- c(localities, locality)
      print(paste("Length Total Localities:", length(localities)))
      type <- page %>%
        html_nodes(xpath = '//strong[@class="ym-Address-Name"]') %>%
        html_text()
      types <- c(types, type)
      print(paste("Length Total Types:", length(types)))
      print(paste("Done! No. of missing Loc:", length(missing_loc_all)))
      if (length(street)  > 0 & length(locality) > 0 & length(type) > 0) {
        print("Found location, going forward to remove from missing!")
        missing_loc_all <- missing_loc_all[!missing_loc_all %in% site]
      }
    }
  }
  print(length(missing_loc_all))
  print("Finished scraping all w/ 0 missing!")
  df <- data.frame(streets, localities, types) #create dataframe with only unique and bank branch
  df_bank <- df %>%
    filter(types == "Postbank Filiale") %>%
    unique()
  return(df_bank)
}

scrapeAllCities <- function() {
  #scraping postbank locations for the all cities in Germany
  url_cities_all <- "https://de.wikipedia.org/wiki/Liste_der_Gro%C3%9F-_und_Mittelst%C3%A4dte_in_Deutschland"
  table_all <- htmltab(doc = url_cities_all, which = 2)
  list_city_all <- table_all[2][[1]]

  list_city_all <- list_city_all %>%
    str_extract("[:alpha:]+") %>%
    gsub(pattern = " $", replacement = "")
  list_city_edited <- str_replace_all(list_city_all, pattern = "ü", replacement = "%C3%BC")

  links <- c()
  for (i in list_city_edited) {
    links <- c(links, paste0("https://www.postbank.de/dienste/gaa_filialsuche/index_layer.html?x=cn&SearchType=BRANCHES_COORD_BASED&MaxRadius=50000&IsoCountryCode=DE&Top=24&Branches=POST&Zip=&City=", i, "&ZipCity=&Street=&SingleSlot=", i, "&SingleSlotGeo=", i))
  }
  links[match("Halle", list_city_all)] <- "https://www.postbank.de/dienste/gaa_filialsuche/index_layer.html?x=xn&filter=ALL&atmsearch=&debug=&BC=&width=918&MapHeight=350&LocX=9.56395&LocY=51.99405&SingleSlotGeo=37620%20Halle"
  links[match("Offenbach", list_city_all)] <- "https://www.postbank.de/dienste/gaa_filialsuche/index_layer.html?x=xn&filter=ALL&atmsearch=&debug=&BC=&width=918&MapHeight=350&LocX=9.56395&LocY=51.99405&SingleSlotGeo=37620%20Halle"
  links[match("Ratingen", list_city_all)] <- "https://www.postbank.de/dienste/gaa_filialsuche/index_layer.html?x=xn&filter=ALL&atmsearch=&debug=&BC=&width=918&MapHeight=350&LocX=6.83774&LocY=51.29579&SingleSlotGeo=40878%20Ratingen"
  streets <- c()
  localities <- c()
  types <- c()
  missing_loc_all <- c()
  count <- 0
  for (site in links) {
    count <- count + 1
    print(count)
    page <- read_html(site, verbose = TRUE)
    street <- page %>%
      html_nodes(xpath = '//span[@class="ym-Address-Street"]') %>%
      html_text()
    streets <- c(streets, street)
    print(paste("Length Total Streets:", length(streets)))
    locality <- page %>%
      html_nodes(xpath = '//span[@class="ym-Address-Locality"]') %>%
      html_text()
    localities <- c(localities, locality)
    print(paste("Length Total Localities:", length(localities)))
    type <- page %>%
      html_nodes(xpath = '//strong[@class="ym-Address-Name"]') %>%
      html_text()
    types <- c(types, type)
    print(paste("Length Total Types:", length(types)))
    print("Done!")
    if (length(street) < 1 & length(locality) < 1 & length(type) < 1) {
      print("Location missing due to error!")
      missing_loc_all <- c(missing_loc_all, site)
    }
  }
  while (length(missing_loc_all) > 79) {
    print("Debug: currently in while loop")
    count <- 0
    for (site in missing_loc_all) {
      count <- count + 1
      page <- try(read_html(site, verbose = TRUE))
      street <- page %>%
        html_nodes(xpath = '//span[@class="ym-Address-Street"]') %>%
        html_text()
      streets <- c(streets, street)
      print(paste("Length Total Streets:", length(streets)))
      locality <- page %>%
        html_nodes(xpath = '//span[@class="ym-Address-Locality"]') %>%
        html_text()
      localities <- c(localities, locality)
      print(paste("Length Total Localities:", length(localities)))
      type <- page %>%
        html_nodes(xpath = '//strong[@class="ym-Address-Name"]') %>%
        html_text()
      types <- c(types, type)
      print(paste("Length Total Types:", length(types)))
      print(paste(count, "- Done! No. of missing Loc:", length(missing_loc_all)))
      if (length(street)  > 0 & length(locality) > 0 & length(type) > 0) {
        print("Found location, going forward to remove from missing!")
        missing_loc_all <- missing_loc_all[!missing_loc_all %in% site]
      }
    }
  }
  missing_end <- str_extract(missing_loc_all, "[:alpha:]+$") #extracting all missing places
  list_dash <- c()
  for (i in 1:length(missing_end)) {
    list_dash <- c(list_dash, list_city_all[!is.na(str_extract(list_city_all, paste0(missing_end[i], "-.*")))])
  }
  list_dash_unique <- unique(list_dash)
  missing_sites <- c()
  for (i in list_dash_unique) {
    missing_sites <- c(missing_sites, str_replace_all(missing_loc_all, missing_end, i)[1])
  }
  #scraping places with dashes that were removed earlier
  while (length(missing_sites > 0)) {
    count <- 0
    for (site in missing_sites) {
      count <- count + 1
      print(count)
      page <- read_html(site, verbose = TRUE)
      street <- page %>%
        html_nodes(xpath = '//span[@class="ym-Address-Street"]') %>%
        html_text()
      streets <- c(streets, street)
      print(paste("Length Total Streets:", length(streets)))
      locality <- page %>%
        html_nodes(xpath = '//span[@class="ym-Address-Locality"]') %>%
        html_text()
      localities <- c(localities, locality)
      print(paste("Length Total Localities:", length(localities)))
      type <- page %>%
        html_nodes(xpath = '//strong[@class="ym-Address-Name"]') %>%
        html_text()
      types <- c(types, type)
      print(paste("Length Total Types:", length(types)))
      if (length(street) < 1 & length(locality) < 1 & length(type) < 1) {
        print("Location missing due to error!")
        print(paste(count, "- Done! No. of missing Loc:", length(missing_loc_all)))
      }
      else {
        print("Found location, going forward to remove from missing!")
        missing_sites <- missing_sites[!missing_sites %in% site]
      }
      print(length(missing_sites))
    }
  }
  #missing dash loc.
  missing_dash <- list_city_all[!is.na((str_extract(list_city_all, "[:alpha:]+-.*")))][!(list_city_all[!is.na((str_extract(list_city_all, "[:alpha:]+-.*")))] %in% list_dash_unique)]
  missing_end <- missing_end[!(missing_end %in% str_extract(list_dash_unique, "[:alpha:]+"))]
  missing_loc_all <- missing_loc_all[!(str_extract(missing_loc_all, "[:alpha:]+$") %in% str_extract(list_dash_unique, "[:alpha:]+"))]
  df <- data.frame(streets, localities, types)
  df_bank <- df %>%
    filter(types == "Postbank Filiale") %>%
    unique()
  return(df_bank)
}

scrapeCitiesPlz <- function() {
  ### scrapes all postbank locations by using postal codes and manipulating url
  df_plz <- read.csv("/Users/mmuecke/Downloads/zuordnung_plz_ort.csv", colClasses = c("character", "character", "character"))
  plz_codes <- df_plz$plz
  plz_codes <- unique(plz_codes)
  links <- c()
  for (i in plz_codes) {
    links <- c(links, paste0("https://www.postbank.de/dienste/gaa_filialsuche/index_layer.html?x=cn&SearchType=BRANCHES_COORD_BASED&MaxRadius=50000&IsoCountryCode=DE&Top=24&Branches=POST&Zip=&City=", i, "&ZipCity=&Street=&SingleSlot=", i, "&SingleSlotGeo=", i))
  }
  streets <- c()
  localities <- c()
  types <- c()
  missing_links <- links
  error_links <- c()
  count <- 0
  while (length(missing_links > 2911)) {
    count <- 0
    for (site in missing_links) {
      count <- count + 1
      print(paste("Counter:", count))
      page <- read_html(site, verbose = TRUE)
      street <- page %>%
        html_nodes(xpath = '//span[@class="ym-Address-Street"]') %>%
        html_text()
      streets <- c(streets, street)
      print(paste("Length Total Streets:", length(streets)))
      locality <- page %>%
        html_nodes(xpath = '//span[@class="ym-Address-Locality"]') %>%
        html_text()
      localities <- c(localities, locality)
      print(paste("Length Total Localities:", length(localities)))
      type <- page %>%
        html_nodes(xpath = '//strong[@class="ym-Address-Name"]') %>%
        html_text()
      types <- c(types, type)
      print(paste("Length Total Types:", length(types)))
      if (length(street) < 1 & length(locality) < 1 & length(type) < 1) {
        print("Location missing due to error!")
        error_links <- c(error_links, site)
        missing_links <- missing_links[!missing_links %in% site]
      }
      else {
        print("Found location, going forward to remove from missing!")
        missing_links <- missing_links[!missing_links %in% site]
      }
      print(paste("No. of missing:", length(missing_links)))
    }
  }
  missing_codes <- str_extract(error_links, "[0-9]{5}$") #retrieving all missing codes
  missing_codes <- unique(missing_codes)
  df_missing_codes <- df_plz  %>%
    filter(plz %in% missing_codes)
  df_missing_place <- select(df_missing_codes, ort)
  df_missing_place <- str_extract(string = df_missing_place$ort, "[:graph:]+")

  left_links <- c()
  for (i in df_missing_place) {
    left_links <- c(left_links, paste0("https://www.postbank.de/dienste/gaa_filialsuche/index_layer.html?x=cn&SearchType=BRANCHES_COORD_BASED&MaxRadius=50000&IsoCountryCode=DE&Top=24&Branches=POST&Zip=&City=", i, "&ZipCity=&Street=&SingleSlot=", i, "&SingleSlotGeo=", i))
  }
  count <- 3120 - 1
  for (site in left_links[3120:length(left_links)]) {
    count <- count + 1
    print(paste("Counter:", count))
    page <- read_html(site, verbose = TRUE)
    street <- page %>%
      html_nodes(xpath = '//span[@class="ym-Address-Street"]') %>%
      html_text()
    streets <- c(streets, street)
    print(paste("Length Total Streets:", length(streets)))
    locality <- page %>%
      html_nodes(xpath = '//span[@class="ym-Address-Locality"]') %>%
      html_text()
    localities <- c(localities, locality)
    print(paste("Length Total Localities:", length(localities)))
    type <- page %>%
      html_nodes(xpath = '//strong[@class="ym-Address-Name"]') %>%
      html_text()
    types <- c(types, type)
    print(paste("Length Total Types:", length(types)))
    if (length(street) < 1& length(locality) < 1 & length(type) < 1) {
      print("Location missing due to error!")
    }
    else {
      print("Found location, going forward to remove from missing!")
      left_links <- left_links[!left_links %in% site]
    }
    print(paste("No. of missing:", length(left_links)))
  }
  df <- data.frame(streets, localities, types)
  df_bank <- df %>%
    filter(types == "Postbank Filiale") %>%
    unique()
  return(df_bank)
}

scrapeBranchesDeutsche <- function() {
  ### scrapes all Deutsche Bank locations by using postal codes and manipulating url
  df_plz <- read.csv("/Users/mmuecke/Downloads/zuordnung_plz_ort.csv", colClasses = c("character", "character", "character"))
  plz_codes <- df_plz$plz
  plz_codes <- unique(plz_codes)
  plz_codes <- sort(plz_codes)
  links <- c()
  for (i in plz_codes) {
    links <- c(links, paste0("https://www.deutsche-bank.de/pfb/content/pk-filialsuche.html?country=D&label=BRANCH&lat=53.54896&lng=9.99337&searchTerm=", i))
  }
  street_all <- c()
  place_all <- c()
  missing_links <- links
  error_links <- c()
  count <- 0
  while (length(missing_links > 0)) {
    count <- 0
    for (site in missing_links[1:10]) {
      count <- count + 1
      print(paste("Counter:", count))
      page <- read_html(site, verbose = TRUE)
      result <- page %>%
        html_nodes(xpath = '//article[@class="c9 block"]//div') %>%
        html_text()
      result <- str_replace_all(result, "\r\n            \r\n                Route berechnen\r\n            \r\n        ", "")
      result <- str_replace_all(result, "\r\n            \r\n                Details anzeigen\r\n                \r\n            \r\n\r\n        ", "")
      result <- str_replace_all(result,  "\r\n            \r\n                Agentur-Detailseite aufrufen\r\n                \r\n            \r\n\r\n        ", "")
      result <- result[!(result == "")]
      place <- as.character(str_extract_all(result, "[0-9]{5}.*"))
      place <- place[place != "character(0)"]
      place_all <- c(place_all, place)
      print(paste("No. of locations:", length(place_all)))
      street <- as.character(str_extract_all(result, "[^0-9]{5}.*[:alnum:]+$"))
      street <- street[street != "character(0)"]
      street_all <- c(street_all, street)
      print(paste("No. of streets:", length(street_all)))
      if (length(street) < 1 & length(place)) {
        print("Location missing due to error!")
        error_links <- c(error_links, site)
        missing_links <- missing_links[!missing_links %in% site]
      }
      else {
        print("Found location, going forward to remove from missing!")
        missing_links <- missing_links[!missing_links %in% site]
      }
      print(paste("No. of missing:", length(missing_links)))
    }
  }
  df <- data.frame(streets, localities, types)
  df_bank <- df %>%
    filter(types == "Postbank Filiale") %>%
    unique()
  return(df_bank)
}
