
##########################################

#' Transform probabilities into logit values
#' @description This function transform probabilities into logit values
#' @param prob probability value(s) in [0, 1]
#' @return Logit value
#' @export
prob2logit = function(prob=NA){
  return(log(prob/(1-prob)))
}

##########################################

#' Transform logit values into probabilities
#' @description  This function transform logit values into probabilities
#' @param logit logit value(s)
#' @return Probability
#' @export
logit2prob = function(logit=NA){
  odds = exp(logit)
  return(odds / (1 + odds))
}

##########################################

#' Try different seeds for a particular chunk of code
#' @description (Re)runs a chunk of code with a different seed every time you press ENTER. Useful if the code includes random number generation. Press ESC to exit
#' @param expr Chunk of code to try, among {} brackets
#' @param maxrun The maximum number of times it reruns the code
#'
#' @return Whatever the code chunk produce (include print for text/numbers or plot for figures) plus the seed used
#' @export
try_seed = function(expr, maxrun = 10) {
  expr = substitute(expr)
  for (i in 1:maxrun) {
    seed <- round(runif(1, 0, 99999))
    set.seed(seed)
    eval(expr, envir = parent.frame())
    readline(sprintf("seed = %s, Press enter to continue!", seed))
  }
}

##########################################


