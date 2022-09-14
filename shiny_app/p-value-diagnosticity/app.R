# dependencies ----
library(shiny)
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)
library(janitor)
library(DiagrammeR)

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
                     max   = 0.100,
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
        
        grVizOutput('diagnosticity_plot', width = "60%", height = "30%"),
        
        tableOutput("diagnosticity_table"),
        
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
  
  output$diagnosticity_table <- function() {
    
    result <- p_diagnosticity(alpha = input$alpha,
                              power = input$power,
                              true_hypotheses = input$true_hypotheses)
    
    result %>%
      select(`False discovery rate`,
             `False omission rate`,
             `Diagnosticity of p-value for a true-real effect`,
             `Diagnositicity of p-value for a true-null effect`) %>%
      pivot_longer(cols = everything()) %>%
      knitr::kable("html", col.names = NULL) %>%
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
  
  output$diagnosticity_plot <- renderGrViz({
    
    result <- p_diagnosticity(alpha = input$alpha,
                              power = input$power,
                              true_hypotheses = input$true_hypotheses)
    
    #fixedsize = TRUE, 
    #fontsize = 4] 
    grViz(diagram = "digraph flowchart {
        # define node aesthetics
        node [fontname = Arial, 
              shape = square, 
              color = Lavender, 
              style = filled]
        tab1 [label = '@@1-1 experiments']
        tab2 [label = '@@1-2 real effects']
        tab3 [label = '@@1-3 null effects']
        tab4 [label = '@@1-4 real effects found']
        tab5 [label = '@@1-5 false negatives']
        tab6 [label = '@@1-6 null effects found']
        tab7 [label = '@@1-7 false positives']
        # set up node layout
        tab1 -> tab2;
        tab1 -> tab3;
        tab2 -> tab4;
        tab2 -> tab5;
        tab3 -> tab6;
        tab3 -> tab7
        }
        [1]: result
        "
    )
    
  })
  
  # output$diagnosticity_plot2 <- renderGrViz({
  #   
  # })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

