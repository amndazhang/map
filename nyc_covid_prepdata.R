library(tidyverse) #tidy data wrangling
library(vroom) #fast reading/importing of data (csv)
library(sf) #spatial data
library(tigris) #geojoin
library(leaflet) #interactive maps
library(htmlwidgets) #interactive map labels 

setwd("/Users/amanda/Desktop/map")

### IMPORT DATA
#download master repo from NYC DoH Github
download.file(url = "https://github.com/nychealth/coronavirus-data/archive/master.zip", 
              destfile = "coronavirus-data-master.zip")
unzip(zipfile = "coronavirus-data-master.zip")

#read in data
percentpos <- vroom("coronavirus-data-master/trends/percentpositive-by-modzcta.csv")
caserate <- vroom("coronavirus-data-master/trends/caserate-by-modzcta.csv")
testrate <- vroom("coronavirus-data-master/trends/testrate-by-modzcta.csv")

# read_csv = read.csv("coronavirus-data-master/Geography-resources/ZCTA-to-MODZCTA.csv")
# print(read_csv)
#read in modzcta shapefile and zcta conversion table
modzcta <- st_read("coronavirus-data-master/Geography-resources/MODZCTA_2010.shp")
zcta_conv <- vroom("coronavirus-data-master/Geography-resources/ZCTA-to-MODZCTA.csv", delim = ",")

###CLEAN DATA
#clean and reshape caserate data
caserates <- caserate %>% select(-c(2:7))
caserates_long <- caserates %>%
  pivot_longer(2:178, names_to = "modzcta",
               names_prefix = "CASERATE_", values_to = "caserate")

#clean and reshape percentpos data
percentpositives <- percentpos %>% select(-c(2:7))
percentpos_long <- percentpositives %>%
  pivot_longer(2:178, names_to = "modzcta",
               names_prefix = "PCTPOS_", values_to = "pctpos")

#clean and reshape testrate data
testrates <- testrate %>% select(-c(2:7))
testrates_long <- testrate %>%
  pivot_longer(2:178, names_to = "modzcta",
               names_prefix = "TESTRATE_", values_to = "testrate")

### MERGE IN GEOGRAPHY DATA
#combine all 3 long data frames into 1
all <- caserates_long %>%
  left_join(percentpos_long, by = c("week_ending", "modzcta")) %>%
  left_join(testrates_long, by = c("week_ending", "modzcta"))

#merge all covid data w zcta shapefile
all_modzcta <- geo_join(modzcta, all, 'MODZCTA', 'modzcta', how = "inner")

#NOTE: modzcta and zcta are not the same, as modzctas encompass several small zctas
#code below would switch between, but multiple zctas can map one modzcta, so leaving lookup table is helpful

#convert week_ending from a character to a date
all_modzcta$week_ending <-as.Date(all_modzcta$week_ending, format = "%m/%d/%Y")

#save df for Shiny app
saveRDS(all_modzcta, "all_modzcta.RDS")

### DATA INSPECTION
#check distribution of caserate data
all_modzcta %>%
  ggplot(aes(x=as.numeric(caserate))) + 
  geom_histogram(bins=20, fill='#69b3a2', color='white')

### MAKE INTERACTIVE MAP OF CASERATE
labels <- sprintf(
  "<strong>%s</strong><br/>%g cases per 100,000 people",
  all_modzcta$MODZCTA, all_modzcta$caserate) %>%
  lapply(htmltools::HTML)

pal <- colorBin(palette = "OrRd", 9, domain = all_modzcta$caserate)
# pal <- colorNumeric(palette = "OrRd", 9, domain = all_zcta$caserate)

map_interactive <- all_modzcta %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet() %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(label = labels,
              stroke = FALSE,
              smootherFactor = .5,
              opacity = 1,
              fillOpacity = 0.7,
              fillColor = ~ pal(caserate),
              highlightOptions = highlightOptions(weight = 5,
                                                  fillOpacity = 1,
                                                  color = "black",
                                                  opacity = 1,
                                                  bringToFront = TRUE)) %>%
  addLegend("bottomright",
            pal = pal,
            values = ~ caserate,
            title = "Cases Per 100,000,
            opacity = 0.7")

saveWidget(l, "nyc_covid_caserate_map.html")

if (interactive() && Sys.getenv("TERM_PROGRAM") == "vscode") {
  if ("httpgd" %in% .packages(all.available = TRUE)) {
    options(vsc.plot = FALSE)
    options(device = function(...) {
      httpgd::hgd(silent = TRUE)
      .vsc.browser(httpgd::hgd_url(history = FALSE), viewer = "Beside")
    })
  }
}