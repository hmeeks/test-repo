#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# For mapping
library(dplyr)
library(lubridate)
library(maptools)

library(shiny)
library(spdep)
library(leaflet)
library(RColorBrewer)

# turn off scientific notation
options(scipen = 999)

setwd("X:/PPG/Projects/Smith - ELCS/Enumeration District/ED Shape File")


#============Census 1940============
c1940 <- read.table("Census1940.txt",
                    header = TRUE,
                    sep = "\t",
                    stringsAsFactors = FALSE)

# There were initially 106352 people
# Quality control

# - Remove 1 with unknown sex and 8 with "N" sex
c1940 <- subset(c1940, c1940$Sex %in% c("F", "M"))

c1940$SexCode <- ifelse(c1940$Sex == "F", 0, 1)

# If missing Birth Date then given the date of 07/01- There were 80 missing BDate and 0 missing BYr
c1940$BDate1 <- ifelse(is.na(c1940$BYr), NA,
                       ifelse(is.na(c1940$BDate), 
                              paste(c1940$BYr, "-07-01 LMT", sep = ""),
                              as.character(c1940$BDate)))
c1940$BDate1 <- as.Date(c1940$BDate1)

# If missing death date then given the date of 07/01- There were 80840 missing DDate and 80292 missing DYr   
c1940$DDate1 <- ifelse(is.na(c1940$DYr), NA,
                       ifelse(is.na(c1940$DDate), 
                              paste(c1940$DYr, "-07-01 LMT", sep = ""),
                              as.character(c1940$DDate)))
c1940$DDate1 <- as.Date(c1940$DDate1)

c1940$LastLivingDate <- as.Date(c1940$LastLivingDate)
c1940$LastResUtahDate <- as.Date(c1940$LastResUtahDate)

c1940$LastLivingYear <- year(c1940$LastLivingDate)
c1940$LastResUtahYear <- year(c1940$LastResUtahDate)

c1940$DaysAlive <- ifelse(!is.na(c1940$DYr), 
                          as.Date(c1940$DDate1) - as.Date(c1940$BDate1), 
                          as.Date(c1940$LastLivingDate) - as.Date(c1940$BDate1))

# There were 31 people with DaysAlive < 0
# We take the average DaysAlive of children died within the first month, which is 27.37
mean(c1940[c1940$DaysAlive <= 365.25 & c1940$DaysAlive >= 0, ]$DaysAlive, na.rm = TRUE)
sd(c1940[c1940$DaysAlive <= 365.25 & c1940$DaysAlive >= 0, ]$DaysAlive, na.rm = TRUE)

set.seed(1)
c1940$DaysAlive <- ifelse(c1940$DaysAlive < 0, 
                          rnorm(1, 
                                mean = mean(c1940[c1940$DaysAlive <= 365.25 & c1940$DaysAlive >= 0, ]$DaysAlive,
                                            na.rm = TRUE),
                                sd = sd(c1940[c1940$DaysAlive <= 365.25 & c1940$DaysAlive >= 0, ]$DaysAlive,
                                        na.rm = TRUE)), 
                          c1940$DaysAlive)
c1940$DaysAlive <- ifelse(c1940$DaysAlive < 0, 1, c1940$DaysAlive)
c1940$YearsAlive <- c1940$DaysAlive/365.25

c1940$DiedBef1 <- ifelse(c1940$YearsAlive < 1, 1, 0)
c1940$DiedBef5 <- ifelse(c1940$YearsAlive < 5, 1, 0)
c1940$DiedBef18 <- ifelse(c1940$YearsAlive < 18, 1, 0)


c1940.LiveTil1 <- subset(c1940, c1940$YearsAlive >= 1)
c1940.LiveTil5 <- subset(c1940, c1940$YearsAlive >= 5)

# Get mortality risk by enumeration district in Utah in 1940
# Only keep the ED with at least 10 people
c1940_ed <- c1940 %>%
  group_by(Enumdist) %>%
  summarise(tot = length(Enumdist),
            diedbef1 = sum(DiedBef1),
            pctdiedbef1 = diedbef1*100/tot) %>%
  filter(tot >= 10)

c1940_livetil1_ed <- c1940.LiveTil1 %>%
  group_by(Enumdist) %>%
  summarise(tot = length(Enumdist),
            diedbef5 = sum(DiedBef5),
            pctdiedbef5 = diedbef5*100/tot) %>%
  filter(tot >= 10)

c1940_livetil5_ed <- c1940.LiveTil5 %>%
  group_by(Enumdist) %>%
  summarise(tot = length(Enumdist),
            diedbef18 = sum(DiedBef18),
            pctdiedbef18 = diedbef18*100/tot) %>%
  filter(tot >= 10)

# Get geocode information of counties in utah
counties <- readShapePoly("Counties.shp",
                          proj4string = CRS("++proj=utm +zone=12"))

counties <- spTransform(counties, CRS("+proj=longlat"))
# Get geocode information of enumeration district in utah from census 1940
utah_ed <- readShapeSpatial("Edpoints_UtahState",
                            proj4string = CRS("++proj=utm +zone=12"))

utah_ed <- spTransform(utah_ed, CRS("+proj=longlat"))

# Match each enumeration district with mortality countes row.
c1940_ed_o <- c1940_ed[match(utah_ed@data$ED_ID, c1940_ed$Enumdist), ]

# Remove enumeration district with NAs
c1940_ed_o1 <- na.omit(c1940_ed_o)
utah_ed_o1 <- utah_ed[utah_ed$ED_ID %in% c1940_ed_o1$Enumdist, ]

# The interp() function from the akima package takes a vector of x-coords, y-coords, and then 
#  a final value varible, z
df <- cbind(ed.longitude = utah_ed_o1@coords[, "coords.x1"],
           ed.latitude = utah_ed_o1@coords[, "coords.x2"],
           ed.id = utah_ed_o1@data$ED_ID,
           ed.county = as.character(utah_ed_o1@data$COUNTY),
           pop = utah_ed_o1@data$POP_1940,
           borninutah = c1940_ed_o1$tot,
           pctdiedbef1 = c1940_ed_o1$pctdiedbef1)

df <- as.data.frame(df, stringsAsFactors = FALSE)
df$ed.longitude <- as.numeric(df$ed.longitude)
df$ed.latitude <- as.numeric(df$ed.latitude)
df$pop <- as.numeric(df$pop)
df$borninutah <- as.numeric(df$borninutah)
df$pctdiedbef1 <- round(as.numeric(df$pctdiedbef1), 2)


# Set up maps

# Define UI for application that 
ui <- fluidPage(
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         radioButtons("radio", label = "Select a neighborhood type:",
                      choices = list("K-nearest neighbors" = 1,
                                     "Distance" = 2),
                      selected = 1),
         conditionalPanel(
           condition = "input.radio == 1",
           sliderInput("knn_slider", "Select number of neighbors",
                       min = 1, max = 400, value = 10)
         ),
         conditionalPanel(
           condition = "input.radio == 2",
           sliderInput("dist_slider", "Select a distance threshold in km",
                       min = 100, max = 500, step = 10, value = 1)
         )
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("ED Geography", leafletOutput("map", width = "100%", height = 800)),
          tabPanel("Infant deaths", leafletOutput("gimap", width = "100%", height = 800))
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
   output$map <- renderLeaflet({
     cols <- rev(brewer.pal(7, "RdGy"))
     pal <- colorBin(palette = cols,
                      domain = df$borninutah,
                      bins = c(min(df$borninutah), 50, 100, 150,
                               200, 250, 300, max(df$borninutah)))     
     popup <- paste0("<strong>ED ID</strong><br>", df$ed.id,
                     "<br><br><strong>County</strong><br>", df$ed.county,
                     "<br><br><strong>Population</strong><br>", df$pop,
                     "<br><br><strong>Born in Utah</strong><br>", df$borninutah,
                     "<br><br><strong>% Died before 1 yo</strong><br>",
                     df$pctdiedbef1)
     map <- leaflet() %>%
       addProviderTiles("CartoDB.Positron") %>%
       addCircleMarkers(data = df,
                        lng = ~ed.longitude,
                        lat = ~ed.latitude,
                        radius = 3.5,
                        color = "black",
                        fillColor = ~pal(borninutah),
                        weight = 0.9,
                        popup = popup) %>%
       addLegend(pal = pal,
                 values = df$borninutah,
                 title = "Total Births")
     
     map
   })

   # to select neighbors
   
   click_tract <- eventReactive(input$map_marker_click, {
     return(input$map_marker_click$ed.id)
   })
   
   
  
  # Getis Ord Map
  tract_weights <- reactive({
    if(input$radio == 1) {
      k <- knearneigh(coordinates(utah_ed_o1), k = input$knn_slider, longlat = TRUE)
      return(nb2listw(include.self(knn2nb(k))))
    } else if (input$radio == 2) {
      d <- dnearneigh(coordinates(utah_ed_o1), 0, input$dist_slider, longlat = TRUE)
      return(nb2listw(include.self(d)))
    }
  })
  
  gi_data <- reactive({
    g <- localG(df$pctdiedbef1, tract_weights())
    df$g <- g
    return(df)
  })
  
  output$gimap <- renderLeaflet({
    cols <- rev(brewer.pal(7, "RdBu"))
    pal <- colorBin(palette = cols,
                    domain = gi_data()$g,
                    bins = c(min(gi_data()$g), -2.58, -1.96, -1.65, 
                             1.65, 1.96, 2.58, max(gi_data()$g)))
    popup <- paste0("<strong>ED ID</strong><br>", gi_data()$ed.id,
                    "<br><br><strong>County</strong><br>", gi_data()$ed.county,
                    "<br><br><strong>Population</strong><br>", gi_data()$pop,
                    "<br><br><strong>Born in Utah</strong><br>", gi_data()$borninutah,
                    "<br><br><strong>% Died before 1 yo</strong><br>", 
                    gi_data()$pctdiedbef1,
                    "<br><br><strong>% Gi* z-score</strong><br>", 
                    round(gi_data()$g, 2))
    gimap <- leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(data = gi_data(),
                       lng = ~ed.longitude,
                       lat = ~ed.latitude,
                       color = "black",
                       fillColor = ~pal(g),
                       fillOpacity = 0.6,
                       weight = 0.7,
                       radius = 2.5, 
                       popup = popup) %>%
      addLegend(pal = pal,
                values = gi_data()$g,
                title = "Gi* z-score")
    
    gimap
  
  })
}
  

# Run the application 
shinyApp(ui = ui, server = server)

