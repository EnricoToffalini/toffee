
##########################################

#' Transform z-score(s) into Sum Score(s) 
#' @description Transform z-score(s) into plausible Sum Score(s) that could be obtained from a test or questionnaire with a finite number of items with binomial or ordinal responses
#' @param z A single z-score or a vector of z-scores, plausibly reflecting the "true" latent scores. Must be numeric
#' @param nItems The number of items composing the test or questionnaire
#' @param minResp The minimum observable response to a single item (for binomial responses 0 is recommended; for a Likert scale in 1-4, should be 1). Must be an integer number
#' @param maxResp The maximum observable response to a single item (for binomial responses 1 is recommended; for a Likert scale in 1-4, should be 4). Must be an integer number
#' @param itemZmin The item difficulty of the easiest item, on a z-score scale. All other item difficulties will be computed on equal intervals between this and itemZmax
#' @param itemZmax The item difficulty of the most difficult item, on a z-score scale
#'
#' @return The "z" vector transformed in a vector of plausible sum scores
#' @export
z2SumScore = function(z=NA,nItems=10,minResp=1,maxResp=4,itemZmin=-2,itemZmax=+2){
  require(scales)
  levels = min(c(minResp,maxResp)):max(c(minResp,maxResp))
  if(minResp %% 1 !=0 | maxResp %% 1 !=0) stop("both minResp and maxResp must be integer numbers")
  itemDiff = seq(itemZmin,itemZmax,length.out=nItems)
  resp = matrix(rep(z,length(nItems)),nrow=length(z),ncol=nItems)
  resp = t(t(resp) - itemDiff)
  resp = matrix(resp, nrow=length(z),ncol=nItems)
  resp = pnorm(resp)*(max(levels)-min(levels))+min(levels)
  resp = round(rowSums(resp))
  return(resp)
}

##########################################

#' Calculate McDonald's Omega for latent factor reliability
#' @description Calculate McDonald's Omega for latent factor reliability from a fitted CFA ("cfa" object) in lavaan, or from a single vector of standardized factor loadings
#' @param fit An object fitted with the "cfa" function of the "lavaan" package
#' @param stdLoadings Alternatively, a vector of standardized factor loadings (provide a single vector)
#' @param absoluteValues Logical value, TRUE if loading signs must be ignored because observed variables can indifferently be positive or negative indicators of the latent factor (default)
#'
#' @return A list of omega coefficients (if a "cfa" object is provided) or a single numerical value representing the omega coefficient (if a vector of standardized factor loadings is provided)
#' @export
mcOmega = function(fit=NULL,stdLoadings=NULL,absoluteValues=TRUE){
  if((is.null(fit)&&is.null(stdLoadings))||(!is.null(fit)&&!is.null(stdLoadings))) stop("Provide either a lavaan model with latent factors or a vector of standardized loadings, but not both")
  if(!is.null(fit)){
    sts = standardizedsolution(fit)
    sts = sts[sts$op=="=~",]
    fctrs = levels(as.factor(sts$lhs))
    omega = list()
    for(i in 1:length(fctrs)){
      x = sts[sts$lhs == fctrs[i],]
      if(absoluteValues) ldngs = abs(ldngs)
      ldngs = x$est.std
      omega[[i]] = sum(ldngs)^2 / (sum(ldngs)^2 + sum(1-ldngs^2))
      names(omega)[[i]] = fctrs[i]
    }
  }
  if(!is.null(stdLoadings)){
    ldngs = stdLoadings
    if(sum(ldngs>1)+sum(ldngs<(-1))>0) stop("Loadings are not standardized, as one or more of them exceed [-1, 1]. Please provide standardized loadings")
    if(absoluteValues) ldngs = abs(ldngs)
    omega = sum(ldngs)^2 / (sum(ldngs)^2 + sum(1-ldngs^2))
  }
  return(omega)
}

##########################################

#' Minimize Transactions for Bill Settlement
#' @description A function that efficiently settle shared expenses by minimizing the number of transactions between contributors, similar to the approach used by Splitwise 
#' @param balances A named vector representing each person's contribution to the total expense. People who have contributed no money must have values equal to zero
#' @param digits An integer specifying the number of decimal places for rounding transaction amounts. Default is 2
#'
#' @return A list of transactions that minimizes the number of payments needed to settle the shared expense, showing who pays whom and how much
#' @examples
#' # Define contributions
#' balances = c(Amber = 120, Julie = 20, Phil = 0, Tommy=0)
#' # Calculate the minimum transactions to settle
#' transactions = minimize_transactions(balances)
#' print(transactions)
#' @export
minimize_transactions = function(balances,digits=2) {
  if(is.null(names(balances))) names(balances) = paste0("x",1:length(balances))
  for(i in 1:length(balances)) if(names(balances)[i]=="") names(balances)[i] = paste0("x",i)
  sumtotal = sum(balances)
  balances = balances - sumtotal/length(balances)
  creditors = balances[balances > 0]
  debtors = balances[balances < 0]
  # Initialize the list to hold transactions
  transactions = list()
  # Sort creditors and debtors in descending order
  creditors = sort(creditors, decreasing = TRUE)
  debtors = sort(debtors)
  # Initialize transaction counter
  transaction_count = 1
  while (length(creditors) > 0 && length(debtors) > 0) {
    # Take the largest debtor and largest creditor
    creditor_name = names(creditors)[1]
    debtor_name = names(debtors)[1]
    creditor = creditors[1]
    debtor = debtors[1]
    # The transaction amount is the minimum of the two balances
    amount = min(creditor, abs(debtor))
    # Store the transaction with names
    transactions[[transaction_count]] = paste0(debtor_name, " pays ", creditor_name, " ", round(amount,digits))
    transaction_count = transaction_count + 1
    # Update balances
    creditors[1] = creditor - amount
    debtors[1] = debtor + amount
    # Remove settled accounts
    if (creditors[1] == 0) {
      creditors = creditors[-1]
    }
    if (debtors[1] == 0) {
      debtors = debtors[-1]
    }
  }
  # Return the list of transactions
  return(transactions)
}

##########################################

#' Calculate Standard Error (SE) associated with a desired level of statistical power
#' @description Calculate the Standard Error (SE) that is associated with a given power and a given alpha level for a given value of a model parameter B
#' @param B The model parameter B (practically, the effect size)
#' @param power The desired level of statistical power
#' @param alpha The alpha level (rate of type I errors)
#' @param alternative Whether the statistical test is two sided or one sided. Must be one of "two.sided" (default) or "one.sided"
#'
#' @return The numerical value of the Standard Error (SE) associated with the above arguments
#' @export
SE4power = function(B = NA, power = 0.80, alpha = 0.05, alternative = c("two.sided","one.sided")[1]){
  if(!alternative %in% c("two.sided","one.sided")) stop("alternative can be only 'two.sided' or 'one.sided'")
  if(alternative == "two.sided") alpha = alpha/2
  SE = abs(B) / (qnorm(1-alpha)+qnorm(power))
  return(SE)
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
    seed = round(runif(1, 0, 99999))
    set.seed(seed)
    eval(expr, envir = parent.frame())
    readline(sprintf("seed = %s, Press enter to continue!", seed))
  }
}

##########################################

#' Normal Quantile Transformation (Rank-based Inverse Normal Transformation)
#' @description Transforms a numeric vector into an approximately standard normal distribution using rank-based quantile transformation (also called inverse normal transformation).
#' @param x A numeric vector to be transformed
#' @param naIgnore Logical. If TRUE (default), missing values (NA) are ignored during transformation. If FALSE, any presence of NA will return a vector of NAs.
#'
#' @return A numeric vector of the same length as \code{x}, where non-missing values are transformed to standard normal quantiles. Missing values are preserved.
#' @examples
#' x = c(3, 1, 4, NA, 2)
#' normalize(x)
#' normalize(x, naIgnore = FALSE)
#' @export
normalize = function(x, naIgnore = TRUE){
  q = rep(NA, length(x))
  if(!naIgnore && any(is.na(x))) return(q)
  p = rank(x[!is.na(x)]) / (length(x[!is.na(x)]) + 1)
  q[!is.na(x)] = qnorm(p)
  return(q)
}

##########################################
