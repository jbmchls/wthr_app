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
  
  startMonth <- reactive({
    paste0(format(input$dateRange[[1]],'%Y'),format(input$dateRange[[1]],'%m'))
  })
  endMonth <- reactive({
    paste0(format(input$dateRange[[2]],'%Y'),format(input$dateRange[[2]],'%m'))
  })  
  stationURL <- eventReactive(input$scrapeNow, {
    paste0('https://www.ncdc.noaa.gov/cdo-web/datasets/GHCND/stations/GHCND:',input$stationID,'/detail')
  })
  stationdata <- eventReactive(input$scrapeNow, {
  
      base <- 'https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/'
      stationDly <- paste0(base,'all/',input$stationID,'.dly')
      h = new_handle()
      con = curl(stationDly, "", h)
      raw = readLines(con)
      close(con)
      
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
  
  flip_data <- function(indivdf){
    transpose(indivdf, make.names="yyyymm", keep.names="dd") %>%
      gather("yyyymm", "value", -dd) %>%
      mutate(date = paste0(yyyymm, dd), .keep = "unused", .before = "value") %>%
      mutate(date = as.Date(date, format = '%Y%m%d')) %>%
      replace_with_na(replace = list(value = '-9999')) %>%
      mutate(value = as.numeric(value)) %>%
      mutate(value = value/10)
  }
  
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
  
    wthr_data() %>%
      mutate(adjHi = ifelse(hiTemp > cap, cap, hiTemp)) %>%
      mutate(adjHi = ifelse(hiTemp < base, base, adjHi)) %>%
      mutate(adjLo = ifelse(loTemp < base, base, loTemp)) %>%
      mutate(adjLo = ifelse(loTemp > cap, cap, adjLo)) %>%
      mutate(gdu = (ifelse(date %in% input$plantDate:input$dateRange[[2]], ((adjHi + adjLo) / 2 - base), 0))) %>% 
      mutate(gdd = cumsum(replace_na(gdu,0))) %>%
      mutate(cumPrec = cumsum(replace_na(precip,0)), .after = precip)
    })
  
###############################################
    # troubleshooting
  #output$text <- renderTable({
   # table <- flip_data(tmax())
   # table
  #})
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
           caption = paste("Historical weather data from NOAA: ",stationURL()),
           tag = "Ingredion") +
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



  #render growing degrees plot
  gddPlotInput <- function() {
    gdd_tbl() %>%
      ggplot(aes(x=date, y=gdd)) + 
      geom_line(stat="identity", color="red") +
      labs(title = input$stationID, subtitle = "Cumulative GDUs") +
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
  


}
#source is the user input (dateRange, loc, units, plot selector)
#expression (conductor) is the database pull and data cleaning
#endpoint is the two plots and any other output