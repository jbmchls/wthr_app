library(dplyr)
library(tidyr)
library(data.table)
library(curl)
library(naniar)
library(ggplot2)
library(ggthemes)

## This app was written by Jessie Browne-Michaels
## last update by original author: 2022-09-13

function(input, output) {
  
  stationID <- reactive({
    if(input$stationText != "") {input$stationText
      } else if(input$stationSelect != "") {input$stationSelect
        } else {NULL}
    })
  
  startMonth <- reactive({
    paste0(format(input$dateRange[[1]],'%Y'),format(input$dateRange[[1]],'%m'))
  })
  endMonth <- reactive({
    paste0(format(input$dateRange[[2]],'%Y'),format(input$dateRange[[2]],'%m'))
  })  
  stationURL <- eventReactive(input$scrapeNow, {
    paste0('https://www.ncdc.noaa.gov/cdo-web/datasets/GHCND/stations/GHCND:',stationID(),'/detail')
  })
  
  output$SelectStationLink <- renderUI({
    HTML("
    <div style='padding-bottom: 10px;'>
    <a href='https://www.ncdc.noaa.gov/cdo-web/search' target='_new'>Find a station</a><br>
    <i>Selected station must have air temperature and precipitation data available.</i><br>
    </div>
    ")
  })
  
  output$ViewStationLink <- renderUI({
    HTML("
    <div style='padding-top: 10px;'>
    <a href='", stationURL(), "' target='_new'>View selected station details</a><br>
         ")
  })
  
  stationdata <- eventReactive(input$scrapeNow, {
  
      # scrape pulic weather data from NOAA
      base <- 'https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/'
      stationDly <- paste0(base,'all/',stationID(),'.dly')
      h = new_handle()
      con = curl(stationDly, "", h)
      raw = readLines(con)
      close(con)
      
      # break text lines into dataframe columns
      tbl <- as.data.frame(raw) %>%
        separate(1, into =c(NA, 'yyyymmATTR'), sep=11) %>%
        separate(1, into = c('yyyymm','ATTR'), sep=6) %>%
        separate(2, into = c('ATTR','dly'), sep=4)
      
      counter <- 1
      repeat {
        tbl <- separate(tbl, 'dly', into = c(counter,'dly'), sep=5) %>% 
          separate('dly', into = c(NA,'dly'), sep=3)
        counter <- counter + 1
        if (counter > 30)
          break
      }
      tbl <- separate(tbl, 'dly', into = c('31',NA), sep=5)
      return(tbl)
    })
  
  # filter station data down to query
  querydata <- reactive({
    wthr <- c('PRCP','TMIN','TMAX')
    filter(stationdata(), ATTR %in% wthr & yyyymm %in% startMonth():endMonth())
  })
  
  prcp <- reactive({
    filter(querydata(), ATTR == 'PRCP') %>% select(-ATTR)
  })
  tmin <- reactive({
    filter(querydata(), ATTR == 'TMIN') %>% select(-ATTR)
  })
  tmax <- reactive({
    filter(querydata(), ATTR == 'TMAX') %>% select(-ATTR)
  })
  
  # transpose so each day is an observation with temp and prcp attributes
  flip_data <- function(indivdf){
    transpose(indivdf, make.names="yyyymm", keep.names="dd") %>%
      tidyr::pivot_longer(!dd,
        names_to = "yyyymm", 
        values_to = "value") %>%
      mutate(date = as.character(paste0(yyyymm, dd)), 
             .keep = "unused", .before = "value") %>%
      replace_with_na(replace = list(value = '-9999')) %>%
      mutate(value = as.numeric(value)) %>%
      mutate(value = value/10)
  }
  
  output$GDDheader <- renderUI({
    HTML("
    <b>Growing Degree Days</b><br>
    Thermal time to selected growth stages in cumulative growing degree units (GDU). 
    Update the numbers below to reflect field observations and hybrid characteristics.
    ")
  })
  
  observe({
    if (input$whatUnits == 2){
      updateNumericInput(session = getDefaultReactiveDomain(), "VE", value = "55")
      updateNumericInput(session = getDefaultReactiveDomain(), "R1", value = "720")
      updateNumericInput(session = getDefaultReactiveDomain(), "R6", value = "1500")
    } else if (input$whatUnits == 1){
      updateNumericInput(session = getDefaultReactiveDomain(), "VE", value = "100")
      updateNumericInput(session = getDefaultReactiveDomain(), "R1", value = "1300")
      updateNumericInput(session = getDefaultReactiveDomain(), "R6", value = "2700")
    }
  })
  
  # Planting and harvest milestones in growing degrees; add documentation to finished app
  output$grower_milestones_blurb <- renderUI({
    HTML("
    <b>Growth stages of corn</b>
    <ul>
      <li>VE: seedling emergence</li>
      <li>R1: average silking</li>
      <li>R6: black layer; maturity</li>
    </ul>
    <b>Growth stages of field peas</b>
    <ul>
      <li>VE: seedling emergence</li>
      <li>R1: average flowering</li>
      <li>R6: mid-maturity</li>
    </ul>
     <b>Sources:</b>
    <ul>
      <li>Purdue University Extension<br><a href='https://www.agry.purdue.edu/ext/corn/news/timeless/VStagePrediction.html' target='_blank'>
          VS Stage Prediction</a>, <a href='https://www.agry.purdue.edu/ext/corn/news/timeless/HeatUnits.html' target='_blank'>
          Heat Units</a></li>
      <li>Manitoba Pulse and Soybean Growers<br><a href='https://manitobapulse.ca/2018/10/field-pea-growth-staging-guide/' target='_blank'>
          Staging Guide</a></li>
    </ul>
  ")
  })

  
  th <- theme(
    plot.background = element_rect(fill = "white"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())

  # data pull transparency
  output$viewStation <- renderUI({
    tags$a(
      href= paste0('https://www.ncdc.noaa.gov/cdo-web/datasets/GHCND/stations/GHCND:',input$stationID,'/detail'),
      "View Station Information")
  })
  
  wthr_data_mm <- reactive({
    data.frame(
      date = flip_data(tmax())[[1]],
      precip = flip_data(prcp())[[2]],
      hiTemp = flip_data(tmax())[[2]], 
      loTemp = flip_data(tmin())[[2]]
    )
  })
      
  wthr_data <- reactive({
    if(input$whatUnits == 1){
      wthr_data_mm() %>%
        mutate(precip = precip /25.4) %>%
        mutate(hiTemp = hiTemp *9/5 + 32) %>%
        mutate(loTemp = loTemp *9/5 + 32)
    } else if(input$whatUnits == 2){
      wthr_data_mm()
    }
  })

  gdd_tbl <- reactive({
    if(input$whatUnits == 1){ # standard units
      cap <- 86
      base <- ifelse(input$GDDcrop == 1, 50, 40)
    } else if(input$whatUnits == 2){ # metric units
      cap <- 30
      base <- ifelse(input$GDDcrop == 1, 10, 4.44)
    }
    date_range <- seq.Date(as.Date(input$plantDate), as.Date(input$dateRange[[2]]), by = "day")
  
    wthr_data() %>%
      mutate(date=as.Date(date, format='%Y%m%d')) %>% arrange(date) %>%
      mutate(adjHi = ifelse(hiTemp > cap, cap, hiTemp)) %>%
      mutate(adjHi = ifelse(hiTemp < base, base, adjHi)) %>%
      mutate(adjLo = ifelse(loTemp < base, base, loTemp)) %>%
      mutate(adjLo = ifelse(loTemp > cap, cap, adjLo)) %>%
      mutate(gdu = (ifelse(date %in% date_range, ((adjHi + adjLo) / 2 - base), 0))) %>% 
      mutate(gdd = cumsum(replace_na(gdu,0))) %>%
      mutate(cumPrec = cumsum(replace_na(precip,0)), .after = precip) 
    })
  
###############################################
    # server snippet
  output$tbl <- renderTable({
    #table <- gdd_tbl()
    #table
  })
  
  output$txt <- renderText({
    stationID()
  })
################################################

    # download gdd_tbl as csv file
  output$downloadWthr <- downloadHandler(
    filename = function(){
      paste0(input$stationID, '_', Sys.Date(),'.csv')},
    content = function(file) {
      write.csv(gdd_tbl(), file, row.names = FALSE)
      }
    )

  #render precipitation plot
  precipPlotInput <- function() {
    if(input$whatUnits == 1){
      units <- "inches"
    } else if(input$whatUnits == 2){
      units <- "mm"
    }
    gdd_tbl() %>%
      ggplot(aes(x=date, y=precip)) +
      geom_bar(stat="identity", color = "#273691", fill = "#5887DA") +
      scale_x_date(date_breaks = '1 month', date_labels = '%B') + 
      ylim(0, NA) + 
      labs(title = input$stationID, 
           subtitle = paste0("Precipitation (", units,")"),
           caption = paste("Historical weather data from NOAA: ",stationURL())) +
      theme_stata() + 
      th
  }
  
  output$prcpPlot <- renderPlot({
    precipPlotInput()
  })
  
  #save precipitation plot
  output$downloadPrcp <- downloadHandler(
    filename = function(){
      paste0(input$stationID, '_', Sys.Date(), '_prcp.png')},
    content = function(file) {
      ggsave(file, plot = precipPlotInput(), device = "png")
    }
  )

  gddPlotInput <- function() {
    plot_data <- gdd_tbl()
    
    # Find dates where GDD crosses each threshold
    find_stage_date <- function(gdd_value) {
      if (any(plot_data$gdd >= gdd_value)) {
        plot_data$date[min(which(plot_data$gdd >= gdd_value))]
      } else {
        NA
      }
    }
    
    ve_date <- find_stage_date(input$VE)
    r1_date <- find_stage_date(input$R1)
    r6_date <- find_stage_date(input$R6)
    
    ggplot(plot_data, aes(x=date, y=gdd)) + 
      geom_line(stat="identity", color="red") +
      # Add points for growth stages
      {if(!is.na(ve_date)) geom_point(aes(x = ve_date, y = input$VE), size = 3)} +
      {if(!is.na(r1_date)) geom_point(aes(x = r1_date, y = input$R1), size = 3)} +
      {if(!is.na(r6_date)) geom_point(aes(x = r6_date, y = input$R6), size = 3)} +
      # Add labels with dates
      {if(!is.na(ve_date)) geom_label(aes(x = ve_date, y = input$VE, label = paste("VE\n", ve_date)), 
                                      color = "black", vjust = -0.5)} +
      {if(!is.na(r1_date)) geom_label(aes(x = r1_date, y = input$R1, label = paste("R1\n", r1_date)), 
                                      color = "black", vjust = -0.5)} +
      {if(!is.na(r6_date)) geom_label(aes(x = r6_date, y = input$R6, label = paste("R6\n", r6_date)),
                                      vjust = -0.5)} +
      labs(title = input$stationID, subtitle = "Cumulative GDUs") +
      expand_limits(y = max(plot_data$gdd, input$R6) * 1.15) +
      theme_stata() + 
      th
  }
  
  
  output$gddPlot <- renderPlot({
    gddPlotInput()
  })
  
  #save growing degrees plot
  output$downloadGDD <- downloadHandler(
    filename = function(){
      paste0(input$stationID, '_', Sys.Date(), '_gdd.png')},
    content = function(file) {
      ggsave(file, plot = gddPlotInput(), device = 'png')
    }
  )

  #render data table
  output$summary_table <- renderTable({
    data_summary <- gdd_tbl() %>% mutate(date = as.character(date))
    })
  
  # US Agricultural Regions map
  output$imageDisplay <- renderUI({
    tags$img(src = "US_ag_regions.png", 
             width = "100%", 
             height = "auto")
  })
  
}

#source is the user input (dateRange, loc, units, plot selector)
#expression (conductor) is the database pull and data cleaning
#endpoint is the two plots and any other output