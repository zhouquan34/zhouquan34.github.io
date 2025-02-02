### HOW TO USE THIS FILE
# Step 1. Install shiny package in RStudio
#   This can be done by running the command 
#     install.packages('shiny') 
#   in RStudio console.
# Step 2. Restart RStudio and open this file 
# Step 3. Click "Run App" in the menu bar of the top left panel


library(shiny)

ui <- fluidPage( 
  titlePanel("Blood Type Sampler"),
  sidebarLayout(
    sidebarPanel(
      numericInput("n",
                "How many unrelated U.S. individuals to draw?",
                "100"), 
      br(), # adds a blank line
      actionButton("do", "Generate their blood types!"),
      width = 4
    ),  
    
    mainPanel( 
      verbatimTextOutput("res"), 
    )
  )
  
)
 
 

server <- function(input, output) {
  res <- eventReactive(input$do, {  
    n <- input$n 
    prob <- c(0.44, 0.42, 0.10, 0.04) # according to https://web.archive.org/web/20110719200400/http://bloodcenter.stanford.edu/about_blood/blood_types.html 
    ct <- as.numeric(rmultinom(1, size=n, prob=prob))
    bt <- c("O", "A", "B", "AB") 
    out <- ''
    for (i in 1:4){
      out = c(out, "Blood type ", bt[i], ": ", ct[i],  "\n")      
    } 
    return(out)
  })
 
  output$res <- renderText( 
    res(), sep=""
  )  
}


# RUN APPLICATION ----------------------------------
shinyApp(ui = ui, server = server)