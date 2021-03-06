## app.R ##

library(shinydashboard)
library(leaflet)
library(rgdal)
library(mapedit)
library(mapview)
library(sf)
library(shiny)
library(rlist)
library(ggplot2)
library(DT)
library(leaflet.extras)
library(ggplot2)
library(lubridate)

library(plotly)

# interactive time series
library(dygraphs)
# create time series objects (class xs)
library(xts)


#read & visualize aot data
library(rgdal) #spatial data wrangling
library(rgeos) #spatial data wrangling & analytics
library(tidyverse) # data wrangling
library(tmap) #modern data visualizations
library(data.table)
library(RCurl)



popupstring = "STREET"
BoxPlotIndexAttributes ="SYSTEMSTOP"

#read aot data ---------------------------------

ReadAotData<-function(ExtraDate,ThisWorkPath)
{
  DataType = c("complete","public")
  #DateRangType <- c("daily","monthly") #the monthly and weekly data is difference
  if(is.Date(ExtraDate))
  {
    #get the name
    
    DateText <- format(ExtraDate, format = "%Y-%m-%d"); 
    
    FileNameWithoutExt<-paste0("chicago-",DataType[1],".daily.",DateText)
    FileName<-paste0("chicago-",DataType[1],".daily.",DateText,".tar")
    AotDateUrl <- paste0("https://s3.amazonaws.com/aot-tarballs/",FileName)
    SaveUrl <-paste0(ThisWorkPath,FileName) #Aot Chicago Public Daily
    file.path <- c(AotDateUrl)
    file.dest <- c(SaveUrl)
    if(!file.exists(file.dest)){
      download.file(file.path, file.dest,method="curl")
      if(file.info(file.dest)$size<1500)
      {
        pc <<- list(DWS = FALSE)
        return(pc)
      }
      untar(file.dest,exdir=ThisWorkPath)
    }
    if(file.info(file.dest)$size<1500)
    {
      pc <<- list(DWS = FALSE)
      return(pc)
    }
    outupt.data <- read.csv(paste0(ThisWorkPath,FileNameWithoutExt,"/data.csv.gz"))
    outupt.provenance <<- read.csv(paste0(ThisWorkPath,FileNameWithoutExt,"/provenance.csv"))
    outupt.info <- read.csv(paste0(ThisWorkPath,FileNameWithoutExt,"/sensors.csv"))
    outupt.nodes <- read.csv(paste0(ThisWorkPath,FileNameWithoutExt,"/nodes.csv"))
    outupt.chiCA <- st_read(paste0(ThisWorkPath,"ChiComArea.shp"))
    outupt.nodes.spt <- SpatialPointsDataFrame(outupt.nodes[,c('lon','lat')],outupt.nodes)
    outupt.prjN <- proj4string(outupt.nodes.spt) <- CRS("+init=epsg:4326")
    
  
    pc <- list ( DWS = TRUE, data = outupt.data, provenance = outupt.provenance,info= outupt.info,nodes = outupt.nodes, spatialpolygon = outupt.chiCA, spt = outupt.nodes.spt, prjN = outupt.prjN)
    setDT(pc$data)
    setDT(pc$nodes)
  }
  return(pc)
}



#basic ------------------------

#input data
nc <- st_read("F://Dianaprince//Bloomberg//CBSWithTestField.shp")
nc$OBJID <- seq.int(nrow(nc))
SRAot <-list(DWS = FALSE)# the default Search Result of Aot Data
#SelectedList
IndexofSelectRect = 0;
Sell<-list()
vi=1;

#the drawned and intersected selection area. this might be infeasible for a multilayer case
Drawned <-1 
SearchResult <- c(1,1)
RenderTab<-FALSE

#set ui -----------------
ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    ## Sidebar content
    dashboardSidebar(
      sidebarMenu(
        menuItem("Attributes1", tabName = "dashboard", icon = icon("headphones")),
        menuItem("Attributes2", tabName = "widgets", icon = icon("cutlery")),
        selectInput("column", "Select Field A", "placeholder1"),
        selectInput("CXC", "Select Field B", "placeholder1"),
        checkboxInput("TypedLine","Group",value=TRUE),
        selectInput("TypedLineName", "Select Field B", "placeholder1"),
        selectInput("timetag", "Select Field Time Tag", "placeholder1")
      )
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(width = 12,
        editModUI("editor")
      ),
      box(
        
        collapsible = TRUE,
        plotlyOutput("plot1", height = 250)
       ),
      box(
        collapsible = TRUE,
        width = 5,
        plotlyOutput("selectstat",height = 250)
        
      ),
      box(
        title = "Smooth line Field A B - Selected",
        witdth = 5,
        plotlyOutput("SC",height = 250)
      ),
      box(
        title = "Smooth line Field A B - All",
        witdth = 5,
        plotlyOutput("CX",height = 250)
        
      ),
      # box(
      #   width = 2,
      #   title = "Controls",
      #   sliderInput("slider", "Number of observations:", 1, 100, c(25,75))
      # ),
      box(
        title = "Time series of all data",
        width = 10,
        dygraphOutput('TS',height = 150)
      ),
      box(
        title = "Time series of selected data",
        width = 10,
        dygraphOutput('TSS',height = 150)
      ),
      box(
        title = "datatable",
        width = 12,
        DT::dataTableOutput("table")
      ),
      box(
        verbatimTextOutput("SelectedPointsFromPlotly")
      ),
      box(
        leafletOutput("TMAP_TEST")
      ),
      box(
        actionButton("DWLDAot","Download Aot Data"),
        dateInput('DownloadDate',
                  label = 'Date input: yyyy-mm-dd',
                  value = Sys.Date()
        ),
        selectInput("AotField","AotField",c("Node_id","Value")),
        actionButton("VisAot","Visualize Aot Data"),
        DT::dataTableOutput("visAot_Text")
      )
      
      
    )
  )
)

#set server -----------------

server <- function(input, output,session) {
  set.seed(122)
  observe({
    isNumericField<-sapply(nc,is.numeric)
    isDateField<-sapply(nc,is.Date)
    FieldNames<-names(nc)
    updateSelectInput(session,"CXC", choices = FieldNames[isNumericField])
    updateSelectInput(session,"column", choices = names(nc))
    updateSelectInput(session,"TypedLineName", choices = FieldNames[!isNumericField])
    updateSelectInput(session,"timetag", choices = FieldNames[isDateField])

  })
  
 
 
  ns <- NS("editor")
  base_map <- leaflet(ns("map")) %>% 
    addTiles() %>% 
    addDrawToolbar(
      # targetLayerId = "draw",
      targetGroup = "draw",
      # polygonOptions = FALSE,
      circleOptions = FALSE,
      # rectangleOptions = FALSE,
      polylineOptions = FALSE,
      markerOptions = FALSE,
      circleMarkerOptions = FALSE,
      singleFeature = TRUE,
      editOptions = editToolbarOptions()
    )%>% 
    addFeatures(nc)%>%
    addLayersControl(overlayGroups = c("draw"), options = layersControlOptions(collapsed = FALSE))
  
  drawn <- callModule(editMod, "editor", base_map)
  
  testdrawn <-function(){
    # f1<-callModule(editMod, "editor", base_map)
    req(drawn()$finished)
    # {
      if(Drawned <= length(drawn()$finished$X_leaflet_id))
      {
        qk_intersect_test <<- st_intersection(drawn()$finished[length(drawn()$finished$X_leaflet_id),], nc)
        Drawned <<- Drawned +1
        # print("1")
      }
      else
      {
        # print("2")
      }
      return(qk_intersect_test) 
    
  }
 
  output$TMAP_TEST = renderLeaflet({
    
    req(is.numeric(nc[[input$column]]))
    tm <- tm_basemap(leaflet::providers$Stamen.Watercolor) + tm_shape(nc) + tm_dots(input$column) +
      tm_layout(
      legend.title.size=1,
      legend.text.size = 0.6,
      legend.bg.color = "white",
      legend.bg.alpha = 1)+
      tm_view(view.legend.position = c("left", "bottom"))
    tmap_leaflet(tm)
  })
  
  output$selectstat<-renderPlotly({

    FT<-testdrawn()
    if(!is.null(FT))
      qk_intersect <-FT

    req(nrow(qk_intersect) > 0)

    req(is.numeric(nc[[input$column]]))
    # boxplot(
    #   list(
    #     All = as.numeric(nc[[input$column]]),
    #     selected = as.numeric(qk_intersect[[input$column]])
    #   ),
    #   xlab = "depth"
    # )
    plot_ly()%>%
      add_boxplot( y= ~nc[[input$column]],name = "All",boxpoints = 'outliers') %>%
      add_boxplot(y = ~qk_intersect[[input$column]], name = "Selected",boxpoints = 'outliers') %>%
      layout(barmode = "stack",dragmode =  "select")
  })
  
 
  observeEvent(input$column, {
    req(input$column %in% names(nc))
    popupstring<<-input$column
    
    leafletProxy(ns("map")) %>%
      clearMarkers()%>%
      addCircleMarkers(
         color = "navy",
         weight = 2,
         opacity = 0.8,
         radius = 3,
         fill=FALSE,
         data=nc,
         popup =~as.character(get(input$column))
         )
  })

  output$table = DT::renderDataTable({ DT::datatable(nc,options = list(lengthMenu = c(10, 50, 100)))

  })
 
  output$SC <- renderPlotly({
    
    FT<-testdrawn()
    if(!is.null(FT))
      qk_intersect <-FT

    req(length(drawn()$finished$X_leaflet_id)>=1)
    qk_intersect <- st_intersection(drawn()$finished[length(drawn()$finished$X_leaflet_id),], nc)

    req(nrow(qk_intersect) > 0)
    req(is.numeric(nc[[input$column]]))
    ggplot() + 
      geom_smooth(mapping = aes(x = as.numeric(qk_intersect[[input$column]]), y = as.numeric(qk_intersect[[input$CXC]])))
    
  })
  output$CX <- renderPlotly({
    # print("1111")
    # req(drawn()$finished)
    # req(length(drawn()$finished$X_leaflet_id)>=1)
    # 
    # qk_intersect <- st_intersection(drawn()$finished[length(drawn()$finished$X_leaflet_id),], nc)
    # req(nrow(qk_intersect) > 0)
    # print(nrow(qk_intersect))
    # print(nrow(nc))
    req(is.numeric(nc[[input$column]]))
    # if(!input$TypedLine)
    # {
    #   typeline = "2"
    # }
    # else
    # {
    #   typeline=nc[[input$TypedLineName]]
    # }
    key <- row.names(nc)
    plot_ly(source = "PointSelected") %>%
      add_trace(data = nc, x = ~get(input$column), y = ~get(input$CXC), key = ~get("OBJID")) %>%
      layout(dragmode = "select")
    # ggplot()+
    #   geom_smooth(mapping = aes(x = as.numeric(nc[[input$column]]), y = as.numeric(nc[[input$CXC]]), linetype = typeline
    #                               ))
    
  })
  output$plot1 <- renderPlotly({
    
    FT<-testdrawn()
    if(!is.null(FT))
      qk_intersect <-FT
    # req(drawn()$finished)
    # req(length(drawn()$finished$X_leaflet_id)>=1)
    # qk_intersect <- st_intersection(drawn()$finished[length(drawn()$finished$X_leaflet_id),], nc)
    req(nrow(qk_intersect) > 0)
    req(is.numeric(nc[[input$column]]))
    # pda<-qk_intersect
    # pdf<-as.data.frame(qk_intersect)
    # x = ~as.numeric(qk_intersect[[input$column]]) , type = "histogram"
    plot_ly(source="SelectFromHist")%>%
      add_histogram(nc[[input$column]],opacity = 1, name ="All")%>%
      add_histogram(qk_intersect[[input$column]],opacity = 1, name ="Selected")%>%
    layout(barmode = "overlay")
    #plot_ly(as.data.frame(qk_intersect),x = input$column, type = "histogram")
    # plot_ly(x = ~rnorm(50), type = "histogram")
    # hist( 
    #   as.numeric(qk_intersect[[input$column]]),
    #   main = paste("Histogram of" , input$column),
    #   xlab = input$column
    #  
    # )
    
  })
  output$TS <- renderDygraph({
    # req(drawn()$finished)
    # req(length(drawn()$finished$X_leaflet_id)>=1)
    # qk_intersect <- st_intersection(drawn()$finished[length(drawn()$finished$X_leaflet_id),], nc)
    FT<-testdrawn()
    if(!is.null(FT))
      qk_intersect <-FT
    req(nrow(qk_intersect) > 0)
    req(is.numeric(nc[[input$column]]))
    req(is.Date(nc[[input$timetag]]))
    discharge_timeSeries <- xts(x = qk_intersect[[input$column]],
                                order.by = qk_intersect[[input$timetag]])
    dygraph(discharge_timeSeries)%>% dyRangeSelector()
    
  })
  output$TSS <- renderDygraph({

    req(is.numeric(nc[[input$column]]))
    req(is.Date(nc[[input$timetag]]))
    discharge_timeSeries <- xts(x = nc[[input$column]],
                                order.by = nc[[input$timetag]])
    dygraph(discharge_timeSeries)%>% dyRangeSelector()
    
  })
  
  observeEvent(input$DWLDAot,{
    #download data at given date
    SRAot <<- ReadAotData(input$DownloadDate,"F:/Dianaprince/Bloomberg/")
    req(SRAot$DWS)
    print(paste0("Data loaded",as.character(input$DownloadDate),as.character(SRAot$DWS)))
    vq <- SRAot$data[,.(.N),by=.(parameter)]
    updateSelectInput(session,"AotField", choices = as.character(vq$parameter))
  })
  
  UpdateSelectedResult<-eventReactive(input$VisAot, {
    
    req(SRAot$DWS)
    SearchCode<<-paste0("SRAot$data[parameter == '",input$AotField,"' & !is.na(value_raw),.(median(as.numeric(as.character(value_hrf)))),by = .(node_id)]")
    SearchResult<<-eval(parse(text = SearchCode))
    SearchResult

  })
  
  
  output$visAot_Text <- DT::renderDataTable(UpdateSelectedResult())
  
  
  
  output$SelectedPointsFromPlotly <- renderPrint({
    NowSelected <<- event_data("plotly_selected", source = "PointSelected")
    if(!is.null(NowSelected)){
      leafletProxy(ns("map")) %>%
        clearMarkers()%>%
        addCircleMarkers(
          color = "navy",
          weight = 2,
          opacity = 0.8,
          radius = 3,
          fill=FALSE,
          data=nc,
          popup =~as.character(get(input$column))
        )%>%
        addCircleMarkers(
          color = "yellow",
          weight = 2,
          opacity = 0.8,
          radius = 3,
          fill=FALSE,
          data= nc[as.numeric(NowSelected$key),]
      )
      NowSelected
     
    }
  })
}

shinyApp(ui, server)