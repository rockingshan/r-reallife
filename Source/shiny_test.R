# Load required libraries
library(shiny)
library(dplyr)

options(shiny.maxRequestSize = 100*1024^2)
# Define UI
ui <- fluidPage(
  # Add a file input widget
  fileInput("file", "Choose a file"),
  
  # Add a submit button
  actionButton("submit", "Submit"),
  
  # Add a placeholder for the output
  textOutput("output")
)

# Define server logic
server <- function(input, output) {
  
  # When the submit button is clicked...
  observeEvent(input$submit, {
    
    # Read the selected file
    df <- read.csv(input$file$datapath)
    
    # Perform some tasks on the file
    cardtype = c('Smart Card','ABV Smart Card','Sumavision SC','Gospell SC','Safeview SC','Nagra Cardless STB')
    df <- df %>% filter(!(ITEM_DESCR %in% cardtype)) %>% 
      select(SERIAL_NUMBER,TYPE,ITEM_DESCR,LOCATION_DESCR,ENTITY_CODE,CUSTOMER_NBR) %>% unique()
    
    # Get the file name and directory of the input file
    file_name <- input$file$name
    file_dir <- dirname(input$file$datapath)
    
    # Output the modified file to the same directory as the input file,
    # with the input file name as a prefix
    write.csv(df, file = file.path(file_dir, paste0(file_name, "_STB_SERIALS.csv")), row.names = F)
    
    # Update the output text
    output$output <- renderText({
      paste("File modified and saved to", input$file$datapath)
    })
  })
}

# Run the app

shinyApp(ui = ui, server = server)
