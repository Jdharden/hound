#' @title hound
#'
#' @description Contains basic calculations journalists often use to analyze data
#'
#' @param dataframe
#'
#' @return dataframe
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
sniff <- function(data, col){
  search <- data %>%
  group_by(data[, col]) %>%
    tally()

  print(search)

}

#example  sniff(test, "NAMELSAD")


## creates a table to sum columns
dig <- function(data, a, b){

dig_total <- group_by(data[, a]) %>%
    summarize(
      total_sum = sum(data[, b], na.rm = TRUE)
    )

print(dig_total)

}

# example: dig(test, "NAMELSAD", "total")








