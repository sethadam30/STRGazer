##############################################
## STRGazer App 
## For viewing STR NGS data
## S. Faith 
## NC State University
## 26 Apr 17
## safaith@ncsu.edu, www.genomicidlab.com
#################################################
# Input file must have headers for the following:
# "locus" - containing any autosomal or Y or Amelogenin locus name(s)
# "class" - containing the fields "AUTOSOMAL", "Y", or "AMEL"
# "coreSize" - the fragment size of the repeat, equivalent to CE typing
# "seqcount" - the number of reads per allele
# All other columns and headers will be displayed in the table,
#     but only the four above are used for plotting data
# Ensure package libraries listed below are installed
###################################################
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)


ui <- fluidPage(
  # Style for HTML/CSS
  tags$head(
    tags$style(type="text/css")
    ),
  titlePanel("Welcome to STRGazer"),
  
  fluidRow(
      #upload selector box with .txt and .csv compatability, default to space
      sidebarPanel(
        fileInput('file1', 'Choose File',
                  accept=c('text/csv', 
                           'text/comma-separated-values,text/plain', 
                           '.csv',
                           '.txt')),
        tags$hr(),
        checkboxInput('header', 'Header', TRUE),
        radioButtons('sep', 'Separator',
                     c(Space = " ",
                       Comma=',',
                       Semicolon=';',
                       Tab='\t'),
                     ' ')
		
    ),
    # radio selctor buttons to switch betwen auto and y and AMEL
      column(width = 4, wellPanel(
        radioButtons("plot_type", "Plot type",
                     c("Auto", "Y", "AMEL")
        )
    ),
      # text display of read counts  
      hr(),
        verbatimTextOutput("AllCount"),
        verbatimTextOutput("AutoCount"),
        verbatimTextOutput("YCount")
    ),
    #boxes for input of thresholds and downlad
      column(width = 4, wellPanel(
        textInput("at", "Read Detection Filter", value = "50"),
        textInput("st", "Read Threshold (Ratio of total)", value = "0.005"),
        downloadButton('x3', 'Download Filtered Data')
      )
    )
  ),
  #the pane with the plots
  fluidRow(
    column(width = 12,
           plotOutput("plot1", height = 700)
    )
  ),
  # the table with the DT package, better than just "Table"
  fluidRow(
    column(width = 12,
           DT::dataTableOutput("table")
    )
  )
)

# server.R functions
server <- function(input, output) {
  datasetInput <- reactive({
  inFile <- input$file1
  #file1 is passed from upload
  # input$file1 will be NULL initially. After the user selects
  # and uploads a file, it will be a data frame with 'name',
  # 'size', 'type', and 'datapath' columns. The 'datapath'
  # column will contain the local filenames where the data can
  # be found.
  if (is.null(inFile))
      return(NULL)
  read.csv(inFile$datapath, header=input$header, sep=input$sep)
  })
  
  #create reactive objects for filters
  rdt <- reactive({ 
     rdt <- as.numeric(input$at)
  })

  intf <- reactive({ 
    intf <- as.numeric(input$st)
  })
 
  #parse data to an Auto table and convert coreSize allele sizes from vector to double prec.
  datasetInputAuto <- reactive({
    datasetInput() %>%
      filter(class == "AUTOSOMAL" & locus != "AMEL")  %>%
      mutate_at(vars(matches("coreSize")), funs(as.character)) %>%
      mutate_at(vars(matches("coreSize")), funs(as.double))
  })
  
  #filter to remove Auto noise and build table in sorted order (low to high) of 'coreSize', 
  #then vectorize 'coreSize' for discrete plotting
  datasetInputAutoFilt <- reactive({
    datasetInputAuto() %>%
      filter(seqcount > rdt()) %>%
      arrange(coreSize) %>%
      droplevels() %>%
      mutate_at(vars(matches("coreSize")), funs(as.factor))
  })
  
  #parse data to Y Table and convert coreSize allele sizes from vector to double prec.
  datasetInputY <- reactive({
    datasetInput() %>%
      filter(class =="Y") %>%
      mutate_at(vars(matches("coreSize")), funs(as.character)) %>%
      mutate_at(vars(matches("coreSize")), funs(as.double))
  })
  
  #filter to remove Y noise and build table in sorted order (low to high) of 'coreSize', 
  #then vectorize 'coreSize' for discrete plotting
  datasetInputYFilt <- reactive({
    datasetInputY() %>%
      filter(seqcount > rdt()) %>%
      arrange(coreSize) %>%
      droplevels() %>%
      mutate_at(vars(matches("coreSize")), funs(as.factor))
  })
  
  #parse data to AMEL Table 
  datasetInputAMEL <- reactive({
    datasetInput() %>%
      filter(locus == "AMEL") 
  })
  
  #filter to remove AMEL noise and build table 
  datasetInputAMELFilt <- reactive({
    datasetInputAMEL() %>%
      filter(seqcount > rdt())
  })
  
  
  #sum up All counts and make object to pass into other reactives and text
  sumAllCount <- reactive({
    sumALLCount <- sum(datasetInput()$seqcount)
  })
  output$AllCount <- renderText({
    if (!is.null(datasetInput())){
      paste("Total Reads in File = ", sumAllCount())
    }
  })
  
  #sum up counts in Auto and make object to pass into other reactives and text
  sumAutoCount <- reactive({
	  sumAutoCount <- sum(datasetInputAuto()$seqcount)
  })
  output$AutoCount <- renderText({
	  if (!is.null(datasetInput())){
		  paste("Total Autosomal Reads = ", sumAutoCount())
	  }
  })
  
  #sum up counts in Y and make object to pass into other reactives and text
  sumYCount <- reactive({
    sumYCount <- sum(datasetInputY()$seqcount)
  })
  output$YCount <- renderText({
    if (!is.null(datasetInput())){
      paste("Total Y Reads = ", sumYCount())
    }
  })
  
  # the plots, default with no data selcted blank, with radio button selection select df
  # note scales = "free_x" releases Y to normalize across all panels
  output$plot1 <- renderPlot({
    if (is.null(datasetInput())){
      ggplot() + geom_blank() +
      ggtitle("Please upload a file")
    } else if (input$plot_type == "Auto") {
      ggplot(datasetInputAutoFilt(), aes(x = (coreSize), y = seqcount, group = locus, fill = locus)) + 
        geom_bar(position = "stack", stat = "identity", colour = "grey1", show.legend =FALSE)+ 
        scale_x_discrete("coreSize") +
        theme(axis.text.x=element_text(hjust = 0)) + 
        facet_wrap(~locus, scales = "free", nrow =3) +
        geom_hline(yintercept = (intf() * sumAutoCount()), colour = "blue") +
        theme(axis.line=element_line()) +
        xlab("Allele Size") +
        theme_grey(base_size = 18)
    } else if (input$plot_type == "Y") {
      ggplot(datasetInputYFilt(), aes(x = (coreSize), y = seqcount, group = locus, fill = locus)) + 
        geom_bar(position = "stack", stat = "identity", colour = "grey1", show.legend =FALSE) + 
        scale_x_discrete("coreSize") +
        theme(axis.text.x=element_text(hjust = 0)) + 
        facet_wrap(~locus, scales = "free", nrow =3) +
        geom_hline(yintercept = (intf() * sumYCount()), colour = "blue") +
        theme(axis.line=element_line()) +
        xlab("Allele Size") +
        theme_grey(base_size = 18)
    } else if (input$plot_type == "AMEL") {
        ggplot(datasetInputAMELFilt(), aes(x = (coreSize), y = seqcount, fill = coreSize)) + 
        geom_bar(position = "stack", stat = "identity", colour = "grey1", show.legend =FALSE) + 
        geom_hline(yintercept = (intf() * sumAutoCount()), colour = "blue") +
        theme(axis.line=element_line()) +
        theme_grey(base_size = 18)
    }
     
  })
  #render data table, selected by radio button
  output$table <- DT::renderDataTable({
    if (is.null(datasetInput())){
      datasetInput() 
    } else if (input$plot_type == "Auto") {
      datasetInputAutoFilt() 
    } else if (input$plot_type == "Y") {
      datasetInputYFilt()
    } else if (input$plot_type == "AMEL") {
      datasetInputAMELFilt()
    }
  })
  
  #create a df to contain auto and Y data filtered by rdt and interpreation (intf)
  #handling filtering by each categotry, Auto and Y, not all
  filtereddf <- reactive({
    bind_rows(filter(datasetInputAutoFilt(), seqcount >= (intf() * sumAutoCount())),
      filter(datasetInputYFilt(), seqcount >= (intf() * sumYCount())),
      filter(datasetInputAMELFilt(), seqcount >= (intf() * sumAutoCount()))
    )
  })
  
  #create character object to store name of input file, edited to pass to DL handler
  #gsub Regex is taking input file name and removing .txt or .csv
  #paste is adding "_filtered.csv" to file name
  
  noff <- reactive({
    noff <- paste(gsub(pattern = "\\.csv$|\\.txt$", "", input$file1$name), "_filtered.csv", sep = "")
  })
  
  #send output of new filtered df to ui as .csv
  output$x3 = downloadHandler(noff, content = function(file) {
    write.table(filtereddf(), file, sep = ",", row.names = FALSE)
  })
  
  
}

shinyApp(ui, server)


