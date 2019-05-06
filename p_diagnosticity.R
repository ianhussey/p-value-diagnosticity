#' Diagnosticity of p values
#'
#' Given alpha, power, and the percentage of true hypotheses, returns the probability that a given p value represents a true positive or true negative. 
#' @param alpha alpha value. Defaults to .05
#' @param power Power (1-beta). Defaults to 0.80.
#' @param perc_true_hypotheses The percentage of true hypotheses among those tested. While this is ultimately unknowable, it is possible, useful, or even necessary to specify a range for this value for a given area of research in order to understand the diagnosticity of any given p value.
#' @examples
#' p_diagnosticity(alpha = 0.05,
#'                 power = 0.80,
#'                 perc_true_hypotheses = 0.5)

# function
p_diagnosticity <- function(alpha = 0.05, power = 0.80, perc_true_hypotheses = 0.50) {
  
  require(dplyr)
  
  h1_true <- rep(c(rep(0, (1-power)*100), 
                   rep(1, power*100)), perc_true_hypotheses*100)
  
  h0_true <- rep(c(rep(0, (1-alpha)*100), 
                   rep(1, alpha*100)), (1-perc_true_hypotheses)*100)
  
  all_tests <- append(h1_true, h0_true)
  
  sig    <- round(length(h1_true[h1_true == 1]) / length(all_tests[all_tests == 1]), 4)
  nonsig <- round(length(h0_true[h0_true == 0]) / length(all_tests[all_tests == 0]), 4)
  both   <- round((length(h1_true[h1_true == 1]) + length(h0_true[h0_true == 0])) / length(all_tests), 4)
  
  return(data.frame(result = c("significant", "non-significant", "both"),
                    diagnosticity = c(sig, nonsig, both)))
  
} 
