library(shiny)

##User input panel
station_list <- list()
list_item <- function(city_name, station_id){
  station_list <- c(station_list, city_name = station_id)
}

###########################################################################
## HOW TO ADD A WEATHER STATION FROM NOAA US SUMMARIES DB
##
## find a station: https://www.ncdc.noaa.gov/cdo-web/datatools/findstation
## enter city and/or state: e.g. 'Hiawatha, KS, USA' or 'Missouri, USA'
## select data set: 'Daily Summaries'
## select data range: recommended to select from previous month
## data categories: check 'Air Temperature' and 'Precipitation'
## from 'Network:ID	GHCND:US1INTP0028' 
##       add ID (e.g., 'US1INTP0028') to stations list below
##
## note: none of the Lafayette stations have temperature data available
############################################################################

stations <- list(
  'Maize' = list('Crawfordsville, IN' = 'USC00121873', # West of Lebanon
                 'Columbus, IN' = 'USC00121747',       # West of Hope
                 'Chalmers, IN' = 'USC00121417',      # North of Lafayette
                 'Francesville, IN' = 'USC00123078',
                 'Frankfort, IN' = 'USC00123082',      # East of Lafayette    
                 'Franklin, IN' = 'USC00122825',
                 'Hopkins, MO' = 'USC00131533', # Clarinda, IA
                 'Hiawatha, KS' = 'USC00143634',
                 'Kansas City, MO' = 'USW00013988', # North of Missouri, City
                 'Kokomo, IN' = 'USC00124662',
                 'Monon, IN' = 'USC00123078', 
                 'Muncie, IN' = 'USC00122825',
                 'St. Joseph, MO' = 'USW00013993' # NE of Halls/Rushville
                 ),  
  'Pulse' = list('Pullman, WA' = 'USC00350265', 
                 'Yuma, AZ' = 'USW00003145'
                 )

)

fluidPage(
  titlePanel("Historical Weather Analysis", 
             windowTitle = "Weather"),

#input column
  column(4, 
    wellPanel( #user input options
    selectInput('stationID',
                label = 'Select a location:',
                choices = c(Location='',
                            stations[names(stations)]),
                selected = NULL,
                multiple = FALSE,
                selectize = TRUE),
    dateRangeInput('dateRange',
                   label = 'Date range:',
                   start = as.Date("2022-06-01"), 
                   end = Sys.Date()),
    radioButtons("whatUnits", 
                       label = 'Select units:', 
                       choices = list("Standard" = 1, "Metric" = 2),
                       selected = 1),
    radioButtons("GDDcrop", 
                 label = "Calculate GDD for:", 
                 choices = list("Maize" = 1, "Pulse" = 2), 
                 selected = 1), 
    dateInput("plantDate", "Planting Date:", 
                value = '2022-06-01'),
    splitLayout(
          numericInput(inputId = "VE", label = textInput('VE_label','','VE'), value = "100"), 
          numericInput(inputId = "R1", label = textInput('R1_label','','R1'), value = '1300'), 
          numericInput(inputId = 'R6', label = textInput('R6_label','','R6'), value = '2700')
        ),
    htmlOutput("viewStation")
         ),
    
    wellPanel( #data pull transparency
      actionButton('scrapeNow', "Submit")
   ), 
   wellPanel( #download data table
     downloadButton("downloadWthr", "Save Data Table")
     )## close panel
   ), ## close column


#output column
  column(8, #width
         
   wellPanel( # troubleshooting
     tableOutput("text")
     ), 

   wellPanel(
    plotOutput("prcpPlot"),
    downloadButton("downloadPrcp", "Save Plot")
         ), 
  
  wellPanel(
    plotOutput("gddPlot"),
    downloadButton("downloadGDD", "Save Plot")
      ), 
  
  wellPanel(
    tableOutput("summary_table"), 
    downloadButton("downloadSummary", "Save Table")
  )
  
  ) #close column
) ##close page