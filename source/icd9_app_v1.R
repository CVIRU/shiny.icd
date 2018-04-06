options(stringsAsFactors = FALSE)

require(icd)
dt0 <- icd9_chapters
dt0 <- data.frame(do.call("rbind", dt0))
dt0
# Correction: no V99, stops at V91
dt0$end[dt0$start == "V01"] <- "V91"

dt1 <- icd9_sub_chapters
dt1 <- data.frame(do.call("rbind", dt1))
dt1

dt2 <- icd9cm_hierarchy
dt2$major <- as.character(dt2$major)

# TEST!
# input <- list()
# input$chapter = rownames(dt0)[1]
# input$subchapter = rownames(dt1)[(which(dt1$start == dt0$start[rownames(dt0) == input$chapter])):(which(dt1$end == dt0$end[rownames(dt0) == input$chapter]))][1]
# input$major = unique(dt2$major[min(which(dt2$three_digit == dt1$start[rownames(dt1) == input$subchapter])):max(which(dt2$three_digit == dt1$end[rownames(dt1) == input$subchapter]))])[1]

ui <- fluidPage(
  titlePanel("ICD-9 Clinical Modification Codes & Diagnoses"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "chapter",
                  label = "Chapter",
                  choices = rownames(dt0)),
      uiOutput("subchapterIn"),
      uiOutput("majorIn")
    ),
    mainPanel(
      textOutput(outputId = "majorICD9")
    )
  )
)

server <- function(input, output) {
  output$subchapterIn <- renderUI({
    lbls1 <- rownames(dt1)[(which(dt1$start == dt0$start[rownames(dt0) == input$chapter])):(which(dt1$end == dt0$end[rownames(dt0) == input$chapter]))]
    selectInput(inputId = "subchapter", 
                label = "Sub-chapter", 
                choices = lbls1)
  })
  
  output$majorIn <- renderUI({
    lbls2 <- unique(dt2$major[min(which(dt2$three_digit == dt1$start[rownames(dt1) == input$subchapter])):max(which(dt2$three_digit == dt1$end[rownames(dt1) == input$subchapter]))])
    selectInput(inputId = "major", 
                       label = "Major", 
                       choices = lbls2)
  })
  output$majorICD9 <- renderText({
    paste("Major ICD-9 Code:",
          unique(dt2$three_digit[dt2$major == input$major]))
  })
  
  output$majorIn <- renderUI({
    lbls2 <- unique(dt2$major[min(which(dt2$three_digit == dt1$start[rownames(dt1) == input$subchapter])):max(which(dt2$three_digit == dt1$end[rownames(dt1) == input$subchapter]))])
    selectInput(inputId = "major", 
                label = "Major", 
                choices = lbls2)
  })
}

shinyApp(ui, server)