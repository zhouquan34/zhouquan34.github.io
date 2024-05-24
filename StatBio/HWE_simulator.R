### HOW TO USE THIS FILE
# Step 1. Install shiny package in RStudio
#   This can be done by running the command 
#     install.packages('shiny') 
#   in RStudio console.
# Step 2. Restart RStudio and open this file 
# Step 3. Click "Run App" in the menu bar of the top left panel


library(shiny)


N <- 200 
K <- 10
n_gen <- 5
settings <- c('Random mating',
              'Assortative mating (same genotypes preferred)',
              'Disassortative mating (different genotypes preferred)')

ui <- fluidPage( 
  titlePanel("HWE Simulator"),
  sidebarLayout(
    sidebarPanel(
      p("This app simulates how the distribution of 
      three genotypes (AA, Aa, aa) changes over 5 generations.
      We assume each generation has 200 individuals, 
      and the first generation is under Hardy-Weinberg equilibrium."), 
      numericInput("freq",
                  "Relative frequency of major allele A in 1st generation",
                  0.5, min=0.01, max=0.99, step=0.01), 
      radioButtons("setting",
                   "Choose simulation setting",
                   settings), 
      numericInput('coef',
                   "How strong is the preference? (not effective for random mating)",
                   5, min=1, max=20, step=1), 
      br(), # adds a blank line
      actionButton("do", "Simulate generations!"),
      width = 4
    ),  
    
    mainPanel( 
      verbatimTextOutput("out"),
    )
  )
  
)



server <- function(input, output) {
  res <- eventReactive(input$do, {  
    p <- input$freq
    set <- input$setting 
    mode <- which(settings == set)
    kappa <- input$coef 
    q <- 1 - p
    pAA <- p*p
    paa <- q*q
    pAa <- 2*p*q
    pop <- sample(c(2,1,0), size=N, replace=T, prob=c(pAA, pAa, paa))
    out = c("Generation 1: ", "n_AA = ", sum(pop==2), '; n_Aa = ', sum(pop==1), '; n_aa = ', sum(pop==0), "\n", sep='') 
    for (k in 2:n_gen){
      pop_new <- numeric(N)
      for (s in 1:N){
        par1 <- sample(pop, 1) # parent 1 
        cdds <- sample(pop, K) # suppose everyone needs to choose their mate from K candidates
        prefer <- rep(1, K)
        if (mode == 2){
           prefer[which(par1 == cdds)] = kappa^2 + 1
           prefer[which(abs(par1 - cdds) == 1)] = kappa + 1
        }
        if (mode == 3){
          prefer[which(abs(par1 - cdds) == 2)] = kappa^2 + 1          
          prefer[which(abs(par1 - cdds) == 1)] = kappa + 1
        }
        par2 <- sample(cdds, 1, prob=prefer) # sample parent 2 
        a1 <- 0 # allele from parent 1
        a2 <- 0 # allele from parent 2
        if (runif(1) <= par1/2){a1 <- 1}
        if (runif(1) <= par2/2){a2 <- 1}
        pop_new[s] = a1 + a2
      }
      pop <- pop_new 
      out = c(out, "Generation ", k, ": ", "n_AA = ", sum(pop==2), '; n_Aa = ', sum(pop==1), '; n_aa = ', sum(pop==0), "\n", sep='') 
    }
    return(out)
  })
  
  output$out <- renderText( 
    res(), sep=""
  )  
}

# RUN APPLICATION ----------------------------------
shinyApp(ui = ui, server = server)