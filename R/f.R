#' f Function
#'
#' @param x x-values
#'
#' @return sinus plot
#' @export
#'
#' @examples f(x=50)
#' @examples f(x=50)
f <- function(x){
  
  # x-vals
  x_val <- seq(0, x, by=0.01)
  
  # Function
  shift <- sample(x=1:5, size=2, replace=TRUE)
  f_val <- sin(shift[1]*x_val) + shift[2]
  
  # Plot
  plot(x=x_val, y=f_val, type="l", xlab="x-Werte", ylab="y-Werte",
       main=paste("Sin(",shift[1],"*x)+",shift[2]))
}
