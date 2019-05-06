# dependencies ----
library(shiny)
library(dplyr)
library(knitr)
library(kableExtra)

# ui ----
ui <- fluidPage(
   
   # Application title
   titlePanel("Diagnosticity of a p value"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("alpha",
                     "Alpha",
                     min   = 0.001,
                     max   = 0.100,
                     value = 0.05,
                     step  = 0.001),
         
         sliderInput("power",
                     "Power (1-beta)",
                     min = 0.01,
                     max = 0.99,
                     value = 0.80,
                     step  = 0.01),
         
         sliderInput("perc_true_hypotheses",
                     "Percent of alternative hypotheses made that are correct",
                     min = 0.01,
                     max = 0.99,
                     value = 0.50,
                     step  = 0.01)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        
        p("Alpha value and power (1-beta) are often thought of separately, with alpha value serving to limit false positives and power serving to limit false negatives. However, in practice, the two are inextricably linked. This app demonstrates that the diagnostic value of any given p value is co-dependant on alpha, power (1-beta), and the percentage of hypotheses that are ultimately true."),
        p("Specifically, diagnosticity refers to probability that the test result (significant vs. non-significant) is congruent with the truth (true vs. null effect)."),
        p("E.g., when significant p value is produced by a test that examines a true effect, or a non-significant p value is produced by a test that examines a true null effect."),
        p("While the proportion of ultimately true hypotheses is ultimately unknowable, it is possible, useful, or even necessary to specify a range for this value for a given area of research in order to understand the diagnosticity of any given p value."),
        
        h2("Results"),
        tableOutput("diagnosticity"),
        p(textOutput("diagnosticity_text")),
        p("Other factors such as underestimated power, p-hacking, publication bias, etc. may of course distort conclusions further."),
        
        h2("Examples"),
        p("In case you don't have a power calculator at hand, below are some example power values for some common sample and effect sizes. All represent an independent t-test using alpha = 0.05 (two-tailed). Percent of hypotheses is subjective in all cases."),
        tableOutput("examples")
        
      )
   )
)

# server ----
server <- function(input, output) {
  
  p_diagnosticity <- function(alpha = 0.05, power = 0.80, perc_true_hypotheses = 0.50) {
    
    h1_true <- rep(c(rep(0, (1-power)*100), 
                     rep(1, power*100)), perc_true_hypotheses*100)
    
    h0_true <- rep(c(rep(0, (1-alpha)*100), 
                     rep(1, alpha*100)), (1-perc_true_hypotheses)*100)
    
    all_tests <- append(h1_true, h0_true)
    
    sig    <- round(length(h1_true[h1_true == 1]) / length(all_tests[all_tests == 1]), 2)
    nonsig <- round(length(h0_true[h0_true == 0]) / length(all_tests[all_tests == 0]), 2)
    both   <- round((length(h1_true[h1_true == 1]) + length(h0_true[h0_true == 0])) / length(all_tests), 2)
    
    return(data.frame(Result = c("Significant", "Non-significant", "All"),
                      Diagnosticity = c(sig, nonsig, both)))
    
  } 
  
  output$diagnosticity <- function() {
    
    p_diagnosticity(alpha = input$alpha, 
                    power = input$power, 
                    perc_true_hypotheses = input$perc_true_hypotheses) %>%
      knitr::kable("html") %>%
      kable_styling("striped", full_width = FALSE)
    
  }
  
  output$diagnosticity_text <- function() {
    
    result <- p_diagnosticity(alpha = input$alpha, 
                              power = input$power, 
                              perc_true_hypotheses = input$perc_true_hypotheses)
    
    result_sig <- result %>%
      filter(Result == "Significant") %>%
      pull(Diagnosticity)
    
    result_nonsig <- result %>%
      filter(Result == "Non-significant") %>%
      pull(Diagnosticity)
    
    result_all <- result %>%
      filter(Result == "All") %>%
      pull(Diagnosticity)
    
    return(paste0("Under these conditions, ", round((1-result_sig)*100, 0), "% of all significant p values represent false positives, and ",
                  round((1-result_nonsig)*100, 0), "% of all non-signficiant p values represent false negatives. Together, ", 
                  round((1-result_all)*100, 0), "% of all p values return incorrect conclusions simply due to error variance."))
    
  }
  
  output$examples <- function() {
    
    data.frame(N = c(50, 50, 50, 100, 100, 100),
               Size = c("small", "medium", "large", "small", "medium", "large"),
               Power = c(0.11, 0.41, 0.79, 0.18, 0.70, 0.98)) %>%
      knitr::kable("html") %>%
      kable_styling("striped", full_width = FALSE)
    
  }
  
}

# Run the application 
shinyApp(ui = ui, server = server)

