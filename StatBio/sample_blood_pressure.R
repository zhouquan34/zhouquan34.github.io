### HOW TO USE THIS FILE
# Step 1. Install shiny package in RStudio
#   This can be done by running the command 
#     install.packages('shiny') 
#   in RStudio console.
# Step 2. Restart RStudio and open this file 
# Step 3. Click "Run App" in the menu bar of the top left panel


library(shiny)

ui <- fluidPage( 
  titlePanel("Systolic Blood Pressure Sampler"),
  sidebarLayout(
    sidebarPanel(
      numericInput("n",
                "How many unrelated U.S. adults to draw?",
                "100"), 
      br(), # adds a blank line
      actionButton("do", "Generate their systolic blood pressure!"),
      width = 4
    ),  
    
    mainPanel( 
      h4("Statistics"), 
      verbatimTextOutput("res"), 
      h4("Histogram"),
      plotOutput("histogram")
    )
  )
  
)


sample_sbp_healthy <- function(n){
  m0 = 112.8 
  s0 = 9 
  low = 50
  up = 145
  y = rt(n, df=10) * s0 
  y[which(y > 0)] = y[which(y > 0)] * 1.15
  x = y + m0
  outlier = which(x < low | x > up)
  k = length(outlier)
  while (k > 0){
    y = rt(k, df=10) * s0  
    y[which(y > 0)] = y[which(y > 0)] * 1.15
    x[outlier] = y + m0 
    outlier = which(x < low | x > up)
    k = length(outlier)
  }
  return(x)
}

sample_sbp_hyper <- function(n){
  m1 = 133.2
  s1 = 13.5
  low = 90
  up = 250
  y = rt(n, df=10) * s1
  y[which(y > 0)] = y[which(y > 0)] * 1.15
  x = y + m1
  outlier = which(x < low | x > up)
  k = length(outlier)
  while (k > 0){
    y = rt(k, df=10) * s1 
    y[which(y > 0)] = y[which(y > 0)] * 1.15
    x[outlier] = y + m1 
    outlier = which(x < low | x > up)
    k = length(outlier)
  }
  return(x)
}

# mean and sd estimated using the data from https://pmc.ncbi.nlm.nih.gov/articles/PMC10424908/#F3 
# the choice of the distributions is somewhat arbitrary and does not reflect the truth 
sample_sbp <- function(n){
  status <- as.numeric(runif(n) < 0.447) # 1: hypertension; 0: no hypertension 
  n1 = sum(status)
  n0 = n - n1
  x0 = numeric(0)
  x1 = numeric(0)
  if (n0 > 0){
    x0 = sample_sbp_healthy(n0)
  }
  if (n1 > 0){
    x1 = sample_sbp_hyper(n1)
  }
  return(c(x0, x1))
}
 
 

server <- function(input, output) {
  samples <- reactiveVal()
  observeEvent(input$do, {
    samples(sample_sbp(input$n))  
  })
  
  output$res <- renderPrint({ 
    x <- samples()
    req(x)
    out <- c("Mean SBP: ", signif(mean(x),5), "\n")
    out <- c(out, "SBP < 60: ", sum(x < 60), "\n")
    for (i in seq(70, 160, by = 10)){
      out = c(out, i - 10, " <= SBP < ", i, ": ", sum(x>=i-10 & x<i),  "\n")      
    } 
    out = c(out, "160 <= SBP < 180: ", sum(x>=160 & x<180),  "\n")      
    out = c(out, "SBP > 180: ", sum(x>180),  "\n") 
    out_text = paste0(out, collapse="")
    cat(out_text) 
  })  
  
  output$histogram <- renderPlot({
    x <- samples()
    req(x)
    hist(x, main="",  xlab = "SBP", col = "blue", border = "white")
  })
}


# RUN APPLICATION ----------------------------------
shinyApp(ui = ui, server = server)