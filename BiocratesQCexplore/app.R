## app.R ##
library(DBI) #for RSQLite...
library(pool)
library(shiny)
library(DT)
library(tidyverse)

pool <- dbPool(
  drv = RSQLite::SQLite(),
  dbname = "biocr_data_demo.db"
)

startData <- as_tibble(dbGetQuery(pool, 'select CBMplates.name as CBMplate, plateBarcodes.name as plateBarcode,  analytes.analyte_class as analyte_class,  count(badQCs.id) as bad_QCs
from measurements join
CBMplates on CBMplates.id=measurements.CBMplate_id join
plateBarcodes on plateBarcodes.id=measurements.plateBarcode_id join
analytes on measurements.analyte_id=analytes.id left join
badQCs on badQCs.id=measurements.id
group by CBMplates.name, plateBarcodes.name,  analytes.analyte_class'))


startData <- startData %>% mutate(analyte_class =  
  if_else(analyte_class %in% c("aminoacids", "biogenic amines"),
          "LC",
          if_else(analyte_class %in% c("glycerophospholipids","sphingolipids"),
                "phospholipids",
                analyte_class)
    ) ) 

startData1 <- startData %>% 
  group_by(CBMplate,plateBarcode,analyte_class) %>% 
  summarize(bad_QCs = sum(bad_QCs, na.rm = TRUE)) %>%
  arrange(plateBarcode)

startData2 <- startData %>% 
  mutate(plateBarcode = "Total", analyte_class="Total") %>%
  group_by(CBMplate,plateBarcode, analyte_class) %>% 
  summarize(bad_QCs = sum(bad_QCs, na.rm = TRUE)) %>%
  arrange(plateBarcode) 

finalData <- as_tibble(rbind(startData1,startData2) %>% mutate(outlier=NA))

qData <- finalData %>% group_by(analyte_class) %>%
  do(data.frame(t(quantile(.$bad_QCs)))) %>% 
  mutate(IQR = X75. - X25.) %>%
  mutate(lowQ = X25.-1.5*IQR, higQ = X75. + 1.5*IQR)

finalData <- cbind(finalData, 
                lowQ=apply(finalData, 1, function(x) {
                  unlist(qData[qData$analyte_class==x["analyte_class"],"lowQ"])
                }), 
                highQ = apply(finalData, 1, function(x) {
                  unlist(qData[qData$analyte_class==x["analyte_class"],"higQ"])
                })
 ) %>% mutate(outlier = !(bad_QCs >= lowQ & bad_QCs <= highQ))

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("analClass", "Analyte classes to show:",
                         choices=unique(finalData$analyte_class),
                         selected=unique(finalData$analyte_class)
                         ),
      checkboxInput("outliers", "Mark outliers", value=FALSE),
      width = 3
    ),
    mainPanel(
      plotOutput("qcPlot", click="plot_click"), 
      DTOutput("selPlate"),
      br(),
      DTOutput('plateDetails'),
      width = 9)
  )
)


server <- function(input, output) {
  
  filteredData <- reactive({
    finalData %>% filter(analyte_class %in% input$analClass)
  })
  
  filteredOutliers <- reactive({
    if(input$outliers) {
      filteredData() %>% filter(outlier == TRUE)
    } 
  })
  
  output$qcPlot <- renderPlot({
    if(input$outliers) {
      ggplot(filteredData(), aes(x=CBMplate, y=bad_QCs, group=analyte_class, colour=analyte_class) ) +
        geom_line() +
        geom_point(mapping=aes(x=CBMplate, y=bad_QCs), data=filteredOutliers()) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    } else {
      ggplot(filteredData(), aes(x=CBMplate, y=bad_QCs, group=analyte_class, colour=analyte_class) ) +
        geom_line() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }
  })
  
  allPlates <- eventReactive(input$plot_click, { 
    plates <- nearPoints(isolate(filteredData()), input$plot_click, threshold = 10, maxpoints = 1,
                         addDist = FALSE) %>% distinct(CBMplate)
    finalData %>% filter(CBMplate %in% plates$CBMplate) 
  })
  
  rowSelected <- eventReactive(input$selPlate_rows_selected, {
    plBarcodes <- allPlates()[input$selPlate_rows_selected,] %>% ungroup %>% 
                  distinct(plateBarcode) %>% filter(plateBarcode!="Total")
    
    plBarcodesStr=paste0(unlist(plBarcodes),collapse=',')
    sql_str <- paste0("select  well,  sampleTypes.name as samplType, count(badQCs.id) as bad_QCs
                from measurements join
                sampleTypes on sampleTypes.id=measurements.samplType_id join
                plateBarcodes on plateBarcodes.id=measurements.plateBarcode_id join
                badQCs on badQCs.id=measurements.id
                where  plateBarcodes.name in (",
                      plBarcodesStr,
                ") and sampleTypes.name !='Sample'
                group by well, sampleTypes.name
                order by well ")
    plateDetails <- dbGetQuery(pool, sql_str)
  })
  
  output$selPlate <- renderDT({
      allPlates() %>% select(CBMplate, plateBarcode, analyte_class, bad_QCs, outlier)
    }, 
    #selection = "single",
    options = list(paging = FALSE, searching = FALSE),
    caption = "Summary statistics",
    server = TRUE
  )
  
  output$plateDetails <- renderDT({
    rowSelected()},
    options = list(paging = FALSE, searching = FALSE),
    caption = "Detailed statistics",
    selection = "none"
  )
  
}


shinyApp(ui = ui, server = server)