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
# | ToDo: Keep selected diagnoses after switching to the next category               |
# |----------------------------------------------------------------------------------|
options(stringsAsFactors = FALSE)
require(icd)
require(DT)
# dt0 <- icd9cm_hierarchy
# dt0$short_desc <- NULL
# 
# # Display ICD codes along with labels----
# dt1 <- dt0
# dt1$long_desc <- paste(dt1$code,
#                        dt1$long_desc,
#                        sep = ":")
# dt1$major <- paste(dt1$sub_code,
#                        dt1$major,
#                        sep = ":")

# # TEST: bypass user interface!
# input <- list()
# input$chapter = unique(as.character(dt1$chapter))[1]
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
      uiOutput(outputId = "chapterIn"),
      uiOutput(outputId = "subchapterIn"),
      uiOutput(outputId = "majorIn"),
      uiOutput(outputId = "dxIn")
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
                   label = "Download Map of Selected Rows"),
      br(),
      actionButton(inputId = "reset",
                   label = "Reset Table",
                   style = "background-color: #FF0000")
    )
  )
)

server <- function(input, output) {
  dt0f <- reactive({
    print(input$dataset)
    if(input$dataset == "Diagnoses"){
      dt0 <- icd9cm_hierarchy
      names(dt0)[names(dt0) == "three_digit"] <- "sub_code"
    } else {
      # Data source: icd_package_procedure_icd9_v1.R
      load("icd9cm_sg_hierarchy.RData")
      dt0 <- icd9cm_sg_hierarchy
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
    dt1$major <- paste(dt1$sub_code,
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

  # Source: https://yihui.shinyapps.io/DT-rows/
  output$tbl <- DT::renderDT({
    dt0 <- dt0f()
    dt1 <- dt1f()
    DT::datatable(unique(dt0[dt1$long_desc %in% input$dx, ]),
                  options = list(pageLength = 10),
                  selection = list(mode = "multiple"))
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
                                     target = "row"))
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

  # Reset diagnoses table
  observeEvent(input$reset, {
    if (exists("dtt")) {
      rm(dtt,
         envir = .GlobalEnv)
    }
  })
}

shinyApp(ui, server)