#' Diagnosticity of p values
#'
#' Given alpha, power, and the percentage of true hypotheses, returns the probability that a given p value represents a true positive or true negative. 
#' For a primer on classification statistics see Tharwat (2018) Classification assessment methods, Applied Computing and Informatics, doi 10.1016/j.aci.2018.08.003
#' @param alpha alpha value. Defaults to .05
#' @param power Power (1-beta). Defaults to 0.80.
#' @param baserate_of_true_hypotheses The percentage of hypotheses that are tested that are actually true in reality. While this is ultimately unknowable, it is possible, useful, or even necessary to specify a range for this value for a given area of research in order to understand the diagnosticity of any given p value.
#' @examples
#' p_diagnosticity(alpha = 0.05,
#'                 power = 0.80,
#'                 baserate_of_true_hypotheses = 0.5)

p_diagnosticity <- function(alpha = 0.05, power = 0.80, baserate_of_true_hypotheses = 0.50) {
  
  require(dplyr)
  
  # true and false positives
  h1_true <- rep(c(rep(0, (1-power)*100), rep(1, power*100)), 
                 times = baserate_of_true_hypotheses*100)
  
  # true and false negatives
  h0_true <- rep(c(rep(0, (1-alpha)*100), rep(1, alpha*100)), 
                 times = (1-baserate_of_true_hypotheses)*100)
  
  # all findings
  all_tests <- append(h1_true, h0_true)
  
  # NB alpha value is effectively specificity, and power is sensitivity
  # Diagnositic odds ratio could therefore be calculated from these two alone.
  
  # PPV = TP/(TP + FP)
  # nb false discovery rate = 1 - PPV
  positive_predictive_value <- round(length(h1_true[h1_true == 1]) / length(all_tests[all_tests == 1]), 4)
  
  # FOR = FN/(TN + FN). 
  # NB negative predictive value = 1 - FOR 
  false_omission_rate       <- round(length(h1_true[h1_true == 0]) / length(all_tests[all_tests == 0]), 4)
  
  # accuracy
  accuracy   <- round((length(h1_true[h1_true == 1]) + length(h0_true[h0_true == 0])) / length(all_tests), 4)

  return(data.frame(positive_predictive_value = positive_predictive_value, 
                    false_omission_rate       = false_omission_rate,
                    accuracy                  = accuracy,
                    markedness                = markedness))
  
} 


