# |----------------------------------------------------------------------------------|
# | Project: ICD-9 Shiny App                                                         |
# | Script: ICD-9 Shiny App                                                          |
# | Authors: Davit Sargsyan                                                          |   
# | Created: 03/31/2018                                                              |
# | Modified: 04/03/2018, DS: replaced text boxes wit DT table. Download only        |
# |                           SELECTED rows (all selected by default)                |
# |                           Output a map file, i.e. R list with mapped diagnoses   |
# |           04/27/2018, DS: Added ICD-9 procedure codes. NOTE: 'major' category is |
# |                           just a copy of 'sub-chapter', too many labels to create|
# |                           by hand. Find a full table online and use it.          |
# |           05/24/2018, DS: Switched to icd Version 3.2.0 (developer) and icd.data |
# |                           version 1.0.1. Added functions to merge different      |
# |                           versions of ICD data (currently, V23-V32).             |
# | ToDo: Keep selected diagnoses after switching to the next category               |
# |----------------------------------------------------------------------------------|
options(stringsAsFactors = FALSE)

# devtools::install_github("jackwasey/icd")
# devtools::install_github("jackwasey/icd.data")
require(shiny)
require(icd)
require(DT)
source("icd9_dx_get_data_v1.R")
source("icd9_sg_get_data_v1.R")

# # TEST: bypass user interface!
# source("source/icd9_dx_get_data_v1.R")
# source("source/icd9_sg_get_data_v1.R")
# input <- list()
# dt1 <- icd9cm_merge_version_dx(32)
# input$chapter = unique(as.character(dt1$chapter))[4]
# input$subchapter = unique(as.character(dt1$sub_chapter[dt1$chapter == input$chapter]))[1]
# input$major = unique(as.character(dt1$major[dt1$sub_chapter == input$subchapter]))[2]
# input$dx = unique(as.character(dt1$long_desc[dt1$major == input$major]))[1]

ui <- fluidPage(
  titlePanel("ICD-9 Clinical Modification Codes & Diagnoses"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = "dataset",
                   label = "Select List",
                   choices = c("Diagnoses",
                               "Procedures"),
                   selected = "Diagnoses"),
      selectInput(inputId = "icd9_version",
                   label = "ICD-9 Version",
                   choices = available_icd9_versions()),
      uiOutput(outputId = "chapterIn"),
      uiOutput(outputId = "subchapterIn"),
      uiOutput(outputId = "majorIn"),
      uiOutput(outputId = "dxIn"),
      checkboxInput("selectAll", "Select All")
    ),
    mainPanel(
      DT:: dataTableOutput("tbl"),
      br(),
      actionButton(inputId = "do",
                   label = "Save Selection"),
      br(),
      DT:: dataTableOutput("tbl2"),
      br(),
      downloadLink(outputId = "downloadData",
                   label = "Download Selected Rows"),
      br(),
      downloadLink(outputId = "downloadMap",
                   label = "Download Map of Selected Rows")
    )
  )
)

server <- function(input, output, session) {
  dt0f <- reactive({
    if(input$dataset == "Diagnoses"){
      dt0 <- icd9cm_merge_version_dx(input$icd9_version)
      # TEMPORARY PATCH: see Issue4: https://github.com/CVIRU/shiny.icd/issues/4
      # Why dose this work?
      dt0$long_desc[dt0$code == "0413"] <- "Friedländer's bacillus infection in conditions classified elsewhere and of unspecified site"
    } else {
      dt0 <- icd9cm_merge_version_pcs(input$icd9_version)
    }
    dt0$short_desc <- NULL
    dt0
  })
  
  dt1f <- reactive({
    # Display ICD codes along with labels----
    dt1 <- dt0f()
    dt1$long_desc <- paste(dt1$code,
                           dt1$long_desc,
                           sep = ":")
    dt1$major <- paste(dt1$major_code,
                       dt1$major,
                       sep = ":")
    dt1
  })
  
  output$chapterIn <- renderUI({
    dt1 <- dt1f()
    selectInput(inputId = "chapter", 
                label = "Chapter", 
                choices = unique(as.character(dt1$chapter)))
  })
  
  output$subchapterIn <- renderUI({
    dt1 <- dt1f()
    selectInput(inputId = "subchapter",
                label = "Sub-chapter",
                choices = unique(as.character(dt1$sub_chapter[dt1$chapter == input$chapter])))
  })

  output$majorIn <- renderUI({
    dt1 <- dt1f()
    selectInput(inputId = "major",
                label = "Major",
                choices = unique(as.character(dt1$major[dt1$sub_chapter == input$subchapter])))
  })

  output$dxIn <- renderUI({
    dt1 <- dt1f()
    selectInput(inputId = "dx",
                label = "Diagnosis",
                choices = unique(as.character(dt1$long_desc[dt1$major == input$major])),
                multiple = TRUE)
  })
  
  observeEvent(input$selectAll,{
    dt1 <- dt1f()
    updateSelectInput(session = session, 
                      inputId = "dx",
                      selected = if(input$selectAll){
                        unique(as.character(dt1$long_desc))
                      } else {
                        ""
                      })
  })

  # Source: https://yihui.shinyapps.io/DT-rows/
  output$tbl <- DT::renderDT({
    dt0 <- dt0f()
    dt1 <- dt1f()
    DT::datatable(unique(dt0[dt1$long_desc %in% input$dx, ]),
                  options = list(pageLength = 10),
                  selection = list(mode = "multiple"),
                  rownames = FALSE)
  })

  # Source: http://shiny.rstudio.com/articles/action-buttons.html
  observeEvent(input$do, {
    dt0 <- dt0f()
    dt1 <- dt1f()
    if (!exists("dtt")) {
      dtt <<- unique(dt0[dt1$long_desc %in% input$dx, ])
    } else {
      dtt <<- unique(rbind.data.frame(dtt,
                                      dt0[dt1$long_desc %in% input$dx, ]))
    }
    dtt <<- dtt[order(as.numeric(rownames(dtt))), ]
    output$tbl2 <- DT::renderDT({
      DT::datatable(dtt,
                    options = list(pageLength = 10),
                    selection = list(mode = "multiple",
                                     selected = 1:nrow(dtt),
                                     target = "row"),
                    rownames = FALSE)
    })
  })

  # Source: https://shiny.rstudio.com/articles/download.html
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("icd9_codes_",
            Sys.Date(),
            ".csv",
            sep = "")
    },
    content = function(file) {
      tmp <- dtt[input$tbl2_rows_selected, ]
      write.csv(tmp,
                file,
                row.names = FALSE)
    }
  )

  # New comorbidity map
  output$downloadMap <- downloadHandler(
    filename = function() {
      paste("icd9_map_",
            Sys.Date(),
            ".RData",
            sep = "")
    },
    content = function(file) {
      tmp <- dtt[input$tbl2_rows_selected, ]
      l1 <- list()
      for (i in 1:length(unique(tmp$major))) {
        l1[[i]] <- unique(c(tmp$code[tmp$major == unique(tmp$major)[i]]))
      }
      names(l1) <- unique(tmp$major)
      l1 <- as.icd_comorbidity_map(l1)
      save(l1,
           file = file)
    }
  )
}

shinyApp(ui, server)