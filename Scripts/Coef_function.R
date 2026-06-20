library(boot)

# Create a function to extract useful coef for the logistic regression

logistic_effects <- function(coef, int, avg) {
  library(boot)
  odds_ratio <- exp(coef)
  p_avg <- inv.logit(int + coef*avg)
  avg_slope <- coef*p_avg * (1 - p_avg)
  print(list(OR = odds_ratio, P = p_avg, Slope = avg_slope))
}

logistic_effects(0.053, -1.822, 6.2)
  