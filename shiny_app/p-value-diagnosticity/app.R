# dependencies ----
library(shiny)
library(dplyr)
library(knitr)
library(kableExtra)
library(janitor)

# ui ----
ui <- fluidPage(
   
   # Application title
   titlePanel("False Discovery Rates given alpha level, statistical power, and the baserate of true hypotheses"),
   
   # Sidebar with a slider input  
   sidebarLayout(
      sidebarPanel(
         sliderInput("alpha",
                     "Alpha",
                     min   = 0.005,
                     max   = 0.300,
                     value = 0.05,
                     step  = 0.005),
         
         sliderInput("power",
                     "Power (1-beta)",
                     min   = 0.00,
                     max   = 1.00,
                     value = 0.80,
                     step  = 0.01),
         
         sliderInput("true_hypotheses",
                     "Percent of alternative hypotheses made that are correct",
                     min   = 0.00,
                     max   = 1.00,
                     value = 0.50,
                     step  = 0.01)
      ),
      
      # Show a table of the results
      mainPanel(
        
        p(textOutput("diagnosticity_text")),
        p("Note that factors such as underestimated power, poor measurement, p-hacking, publication bias, etc. will worsen this rate further."),
        
        tableOutput("diagnosticity"),
        
        p("Code by Ian Hussey, available on", a(href = "https://github.com/ianhussey/p-value-diagnosticity", "GitHub")) 
        
      )
   )
)

# server ----
server <- function(input, output) {
  
  p_diagnosticity <- function(alpha = 0.05, power = 0.80, true_hypotheses = 0.50, total_sample = 100) {
    
    TP <- round_half_up(true_hypotheses     * power     * total_sample, 0)
    FN <- round_half_up(true_hypotheses     * (1-power) * total_sample, 0)
    TN <- round_half_up((1-true_hypotheses) * (1-alpha) * total_sample, 0)
    FP <- round_half_up((1-true_hypotheses) * alpha     * total_sample, 0)
    
    P <- TP + FN
    N <- TN + FP
    
    PP <- TP + FP
    PN <- TN + FN
    
    PPV <- round_half_up(TP / PP, 2)
    NPV <- round_half_up(TN / PN, 2)
    
    FDR <- round_half_up(1 - PPV, 2)
    FOR <- round_half_up(1 - NPV, 2)
  
    res <- 
      tibble(`Experiments run`      = as.character(total_sample),
             `Total real effects`   = as.character(P),
             `Total null effects`   = as.character(N),
             `Real effects found`   = as.character(TP),
             `Real effects missed`  = as.character(FN),
             `Null effects found`   = as.character(TN),
             `Null effects missed`  = as.character(FP),
             `False discovery rate` = paste0(FDR*100, "%"),
             `False omission rate`  = paste0(FOR*100, "%"),
             `Diagnosticity of p-value for a true-real effect` = paste0(PPV*100, "%"),
             `Diagnositicity of p-value for a true-null effect` = paste0(NPV*100, "%"))
    
    return(res)
    
  } 
  
  output$diagnosticity <- function() {
    
    p_diagnosticity(alpha = input$alpha, 
                    power = input$power, 
                    true_hypotheses = input$true_hypotheses) %>%
      pivot_longer(cols = everything(), names_to = "Metric", values_to = "Result") %>%
      knitr::kable("html") %>%
      kable_styling("striped", full_width = FALSE, position = "left")
    
  }
  
  output$diagnosticity_text <- function() {

    result <- p_diagnosticity(alpha = input$alpha,
                              power = input$power,
                              true_hypotheses = input$true_hypotheses)

    result_FDR <- result %>%
      pull(`False discovery rate`)

    return(paste0("Under these conditions, ", result_FDR, " of significant p-values represent false positives."))
    
  }
  
}

# Run the application 
shinyApp(ui = ui, server = server)

