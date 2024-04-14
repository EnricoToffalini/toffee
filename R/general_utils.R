
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
