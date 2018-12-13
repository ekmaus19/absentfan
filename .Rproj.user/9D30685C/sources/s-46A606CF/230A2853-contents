#' @title Summarize one-way ANOVA
#' 
#' @description 
#' \code{summary.oneway} summarizes the results of the one-way ANOVA.
#' 
#' @details
#' This function print the ANOVA table from the linear model.
#' 
#' @param object an object of class \code{oneway}
#' @param ... additional arguments passed to the function.
#' 
#' @export
#' 
#' @return the input is returned silently
#' 
#' @author Rob Kabacoff <rkabacoff@@wesleyan.edu>
#' 
#' @examples
#' mileage <- oneway(hwy ~ class, cars)
#' summary(mileage)

summary.oneway <- function(object, ...){
  if(!inherits(object, "oneway")) stop("object must  be class 'oneway'")
  print(anova(object$anova), ...)
}
