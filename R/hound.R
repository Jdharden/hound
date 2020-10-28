#' @title
#'
#' @description
#'
#' @param
#'
#' @return
#'
#' @examples
#'
#' @export

## a simple outcome based analysis for determinig how likely an outcome is for one group compared to another
relative_risk <- function(x_impacted, x_pop, y_impacted, y_pop) {
  risk <- (x_impacted/x_pop) / (y_impacted/y_pop)

  if(risk > 0){
    print(paste("The outcome based analysis shows that group A's rate is", round(risk),"times the rate of group B's"))
  }
}

## a percent change calculator
percent_change <- function(new, old) {
  change <- ((old - new)/old)*100
  if(change > 0) {
    print(paste("There was a", change, "percent increase"))
  }
  if(change < 0) {
    print(paste("There was a", change, "percent decrease"))
  }
}

## creates a common pivot table for calculating the number of times a variable appears in a column
sniff <- function(x){
  group_by(x) %>%
    tally() %>%
    print()
}

## creates a table to sum columns
dig <- function(x, y){
  group_by(x) %>%
    summarize(
      total_sum = sum(y, na.rm = TRUE)
    ) %>%
    print()
}




