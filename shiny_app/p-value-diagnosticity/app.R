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
   
   # Sidebar with a slider input for number of bins 
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
      
      # Show a plot of the generated distribution
      mainPanel(
        
        #p("Alpha value and power (1-beta) are often thought of separately, with alpha value serving to limit false positives and power serving to limit false negatives. However, in practice, the two are inextricably linked. This app demonstrates that the diagnostic value of any given p value is co-dependant on alpha, power (1-beta), and the percentage of hypotheses that are ultimately true."),
        #p("Specifically, diagnosticity refers to probability that the test result (significant vs. non-significant) is congruent with the truth (true vs. null effect)."),
        #p("E.g., when significant p value is produced by a test that examines a true effect, or a non-significant p value is produced by a test that examines a true null effect."),
        #p("While the proportion of ultimately true hypotheses is ultimately unknowable, it is possible, useful, or even necessary to specify a range for this value for a given area of research in order to understand the diagnosticity of any given p value."),
        
        #h2("Results"),
        p(textOutput("diagnosticity_text")),
        p("Note that factors such as underestimated power, poor measurement, p-hacking, publication bias, etc. will worsen this rate further."),
        
        tableOutput("diagnosticity"),
        
        # h2("Power examples"),
        # p("In case you don't have a power calculator at hand, below are some example power values for some common sample and effect sizes. All represent an independent t-test using alpha = 0.05 (two-tailed). Percent of hypotheses is subjective in all cases."),
        # tableOutput("examples"),
        
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
      tibble(`Experiments run`      = total_sample,
             `Total real effects`   = P,
             `Total null effects`   = N,
             `Real effects found`   = TP,
             `Real effects missed`  = FN,
             `Null effects found`   = TN,
             `Null effects missed`  = FP,
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
      gather(Metric, Result) %>%
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
  
  # output$examples <- function() {
  #   
  #   data.frame(N = c(50, 50, 50, 100, 100, 100),
  #              Size = c("small", "medium", "large", "small", "medium", "large"),
  #              Power = c(0.11, 0.41, 0.79, 0.18, 0.70, 0.98)) %>%
  #     knitr::kable("html") %>%
  #     kable_styling("striped", full_width = FALSE)
  #   
  # }
  
}

# Run the application 
shinyApp(ui = ui, server = server)

