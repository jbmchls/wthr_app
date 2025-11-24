library(shiny)

##User input panel
station_list <- list()
list_item <- function(city_name, station_id){
  station_list <- c(station_list, city_name = station_id)
}

###########################################################################
## HOW TO ADD A WEATHER STATION FROM NOAA US SUMMARIES DB
##
## find a station: https://www.ncdc.noaa.gov/cdo-web/search
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
  'Midwestern corn belt' = list(
    'Franklin, IN' = 'USC00123091', 
    'Bowling Green, MO' = 'USC00230856'),  
  'Northern wheat belt and basin' = list(
    'Arlington, OR' = 'USC00350265',
    'Davenport, WA' = 'USC00452007'
  )
)

fluidPage(
  titlePanel("Historical Weather Analysis", 
             windowTitle = "Weather"),

#input column
  column(5, 
    wellPanel( #user input options
    selectInput('stationSelect',
                label = 'Quick Access Stations',
                choices = c("Type station ID below" = '',
                            stations[names(stations)]),
                selected = NULL,
                multiple = FALSE,
                selectize = TRUE),
    htmlOutput("SelectStationLink"),
    textInput('stationText',
              label = 'Station ID (e.g., USC00114355)'),
    dateRangeInput('dateRange',
                   label = 'Date range:',
                   start =as.Date(paste0(format(Sys.Date(), "%Y"), "-06-01")), 
                   end = Sys.Date()),
    radioButtons("whatUnits", 
                       label = 'Select units:', 
                       choices = list("Standard" = 1, "Metric" = 2),
                       selected = 1),
    radioButtons("GDDcrop", 
                 label = "Calculate GDD for:", 
                 choices = list("Corn" = 1, "Peas" = 2), 
                 selected = 1), 
    dateInput("plantDate", "Planting Date:", 
                value = as.Date(paste0(format(Sys.Date(), "%Y"), "-06-01"))),
    actionButton('scrapeNow', "Submit"), 
    htmlOutput("ViewStationLink")
         ),
    
    wellPanel(
      htmlOutput("GDDheader"),
      splitLayout(
        numericInput(inputId = "VE", label = textInput('VE_label','','VE'), value = "100"), 
        numericInput(inputId = "R1", label = textInput('R1_label','','R1'), value = '1300'), 
        numericInput(inputId = 'R6', label = textInput('R6_label','','R6'), value = '2700')
      ),
      htmlOutput("grower_milestones_blurb")
    ) ## close panel
   ), ## close column


#output column
  column(7, #width
         
   # Agricultural regions map      
   wellPanel( 
     uiOutput("imageDisplay")
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
    downloadButton("downloadWthr", "Save Data Table"),
    tableOutput("summary_table")
  )
  
  ) #close column
) ##close page