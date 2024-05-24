### HOW TO USE THIS FILE
# Step 1. Install shiny package in RStudio
#   This can be done by running the command 
#     install.packages('shiny') 
#   in RStudio console.
# Step 2. Restart RStudio and open this file 
# Step 3. Click "Run App" in the menu bar of the top left panel



library(shiny)

ui <- fluidPage( 
  titlePanel("Blood Type Simulator"),
  sidebarLayout(
    sidebarPanel(
      p("This app simulates the ABO blood type and gender of kids of the same parents.
        We assume the frequencies of 3 alleles iA, iB, i are 27%, 6%, 67%
        (these are estimates for North Americans)."),
      numericInput("n",
                "How many kids?",
                2), 
      br(), # adds a blank line
      actionButton("do", "Generate blood types of the family!"),
      br(),
      br(), 
      p("If you keep repeating this simulation, you will see that the chance of the two
        parents having the same blood type is about 40%, 
        while the chance of the first two kids having the same blood type
        is about 2/3."),
      width = 4
    ),  
    
    mainPanel( 
      verbatimTextOutput("parents"), 
    )
  )
)

# estimated allele frequences if O/A/B in US
fA <- 0.27
fB <- 0.06
fi <- 0.67
gA <- fA*fA + 2*fA*fi
gB <- fB*fB + 2*fB*fi
gAB <- 2*fA*fB
gO <- fi*fi  
gs <- c(gA, gB, gAB, gO)

geno <- function(p){
  g <- c()
  if (p == 'A'){
    p_hom = fA*fA
    p_het = 2*fA*fi
    if (runif(1) < p_het/(p_het + p_hom)){ g = c("A","O")}
    else{g = c("A", "A")}
  }
  if (p == 'B'){
    p_hom = fB*fB
    p_het = 2*fB*fi
    if (runif(1) < p_het/(p_het + p_hom)){ g = c("B","O")}
    else{g = c("B", "B")}
  }
  if (p == 'AB'){ 
    g = c("A", "B")
  }
  if (p == 'O'){
    g = c("O", "O")
  }
  return(g)
} 

sample_blood_type <- function(g1, g2){
  M = sample(g1, 1)
  D = sample(g2, 1)
  if (M == 'A'){
    if (D == 'B'){return("AB")}
    else{return("A")}
  }
  if (M == 'B'){
    if (D == 'A'){return("AB")}
    else{return("B")}
  }
  if (M == 'O'){
    if (D == 'A'){return("A")}
    if (D == 'B'){return("B")}
    if (D == 'O'){return("O")} 
  }
  return("")
}

server <- function(input, output) {
  res <- eventReactive(input$do, {  
    M <- sample(c('A','B','AB','O'),1,prob=gs)
    D <- sample(c('A','B','AB','O'),1,prob=gs)
    Ms <- geno(M)
    Ds <- geno(D)  
    out = c("Mother's blood type: ", M, "\tFather's blood type: ", D, "\n")
    out = c(out, "Mother's genotype: ", Ms, "\tFather's genotype: ", Ds, "\n") 
    for (i in 1:input$n){
      bt <- sample_blood_type(Ms, Ds)
      gender <- sample(c("Female", "Male  "), 1)
      out = c(out, "Kid ", i, ": ", gender, ", ", bt, "\n")      
    } 
    return(out)
  })

  output$parents <- renderText( 
    res(), sep=""
  )  
}


# RUN APPLICATION ----------------------------------
shinyApp(ui = ui, server = server)