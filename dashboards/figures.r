library(highcharter)
library(dplyr)
library(viridisLite)
library(flexdashboard)
library(data.table)
library(ggplot2)
library(lgrdata)
library(ggsci)
library(leaflet)
library(sf)
library(htmltools)
library(VHRscope)

data("oil")

figuur <- list()

figuur$A <-
oil %>% 
  hchart(
    'column', hcaes(x = 'year', y = 'production', group = 'country'),
    stacking = "normal"
  ) %>%
  hc_colors(pal_jco()(8)) %>% 
  hc_title(
    text = "Olieproductie (1971 - 2017) ",
    margin = 20,
    align = "center",
    style = list(color = "gray30", useHTML = TRUE)
  ) %>% 
  # hc_subtitle(text = "binnen NNZ",
  #             margin = 20,
  #             align = "center",
  #             style = list(color = "#0B6623", useHTML = TRUE)) %>%
  hc_xAxis(
    title = list(text = "jaar")
  ) %>% 
  hc_yAxis(
    title = list(text = "ton olie-equivalent (toe)")
  )

figuur$B <-
  oil %>% 
  hchart(
    'line', hcaes(x = 'year', y = 'production', group = 'country'),
    stacking = "normal"
  ) %>%
  hc_colors(pal_jco()(8)) %>% 
  hc_title(
    text = "Olieproductie (1971 - 2017) ",
    margin = 20,
    align = "center",
    style = list(color = "gray30", useHTML = TRUE)
  ) %>% 
  # hc_subtitle(text = "binnen NNZ",
  #             margin = 20,
  #             align = "center",
  #             style = list(color = "#0B6623", useHTML = TRUE)) %>%
  hc_xAxis(
    title = list(text = "jaar")
  ) %>% 
  hc_yAxis(
    title = list(text = "ton olie-equivalent (toe)")
  )


figuur$C <-

oil %>% filter(year == 2017) %>%
  hchart(
    'bar', hcaes(x = 'country', y = 'production', group = 'country'),
    stacking = "normal"
  ) %>%
  hc_colors(pal_jco()(8)) %>% 
  hc_title(
    text = "Olieproductie (2017) ",
    margin = 20,
    align = "center",
    style = list(color = "gray30", useHTML = TRUE)
  ) %>% 
  # hc_subtitle(text = "binnen NNZ",
  #             margin = 20,
  #             align = "center",
  #             style = list(color = "#0B6623", useHTML = TRUE)) %>%
  hc_xAxis(
    title = list(text = "land")
  ) %>% 
  hc_yAxis(
    title = list(text = "ton olie-equivalent (toe)")
  )

figuur$D <-
gauge(42, min = 0, max = 100, symbol = '%', label = 'geslaagd',
      gaugeSectors(
  success = c(80, 100), warning = c(40, 79), danger = c(0, 39)
))

figuur$E <-
gauge(80, min = 0, max = 100, symbol = '%', label = 'deelgenomen',
      gaugeSectors(
  success = c(80, 100), warning = c(40, 79), danger = c(0, 39)
))

figuur$F <-
  gauge(12, min = 0, max = 100, symbol = '%', label = 'vragen',
        gaugeSectors(
    success = c(80, 100), warning = c(40, 79), danger = c(0, 39)
  ))

waardenbox <- list()

waardenbox$A <- valueBox(sample(size = 1,1000), caption = "Fietsen", icon="ion-android-bicycle")
waardenbox$B <-valueBox(sample(size = 1,1000), caption = "Auto's", icon="ion-android-car")
waardenbox$C <-valueBox(sample(size = 1,1000), caption = "Voetgangers", icon="ion-eye")
waardenbox$D <-valueBox(sample(size = 1,1000), caption = "Vliegtuigen", icon="fa-random")
waardenbox$E <-valueBox(sample(size = 1,1000), caption = "Treinen", icon="ion-coffee")
waardenbox$F <-valueBox(sample(size = 1,1000), caption = "Schepen", icon="ion-android-boat")


##
N2000 <- st_read_N2000NL()

N2000 <- N2000 %>% group_by(gebiedNummer, gebiedNaam) %>% summarise() %>%
  mutate(label = paste0(gebiedNaam,' (', gebiedNummer,')')) %>%
  st_transform(crs = 'EPSG:4326') 

N2000$gebiedNaam %>% unique

N2000 %>% filter(gebiedNaam == "Kop van Schouwen"  )

kaart <- list()

kaart$A <-
leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = N2000 %>% filter(gebiedNaam == "Kop van Schouwen"  ) ,
              group = 'Kop van Schouwen',
              color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.3,
              fillColor = 'red', popup = ~htmlEscape(label),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addPolygons(data =N2000 %>% filter(gebiedNaam == "Manteling van Walcheren"   ),
              group = 'Manteling van Walcheren',
              color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.3,
              fillColor = 'red', popup = ~htmlEscape(label),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addPolygons(data = N2000 %>% filter(gebiedNaam ==  "Yerseke en Kapelse Moer"  ) ,
              group = 'Yerseke en Kapelse Moer',
              color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.3,
              fillColor = 'red', popup = ~htmlEscape(label),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addPolygons(data = N2000 %>% filter(gebiedNaam == "Zwin & Kievittepolder" ) ,
              group = 'Zwin & Kievittepolder',
              color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.3,
              fillColor = 'red', popup = ~htmlEscape(label),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addPolygons(data = N2000 %>% filter(gebiedNaam == "Groote Gat"  ) ,
              group = 'Groote Gat',
              color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.3,
              fillColor = 'red', popup = ~htmlEscape(label),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addPolygons(data = N2000 %>% filter(gebiedNaam ==  "Canisvliet" ) ,
              group = 'Canisvliet',
              color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.3,
              fillColor = 'red', popup = ~htmlEscape(label),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addPolygons(data = N2000 %>% filter(gebiedNaam == "Vogelkreek"  ) ,
              group = 'Vogelkreek',
              color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.3,
              fillColor = 'red', popup = ~htmlEscape(label),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addControl(html = HTML('<h4>N2000-gebieden</h4>'), position = "topright", className="map-title") %>%
  addLayersControl(baseGroups  = c( "Kop van Schouwen" ,
                        "Manteling van Walcheren",
                        "Yerseke en Kapelse Moer",
                        "Zwin & Kievittepolder",
                        "Groote Gat",
                        "Canisvliet",
                        "Vogelkreek"),
    options = layersControlOptions(collapsed = F)
   )  # %>% 
  # hideGroup("Yerseke en Kapelse Moer") %>% 
  # hideGroup("Vogelkreek") 
