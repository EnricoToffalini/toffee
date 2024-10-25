
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
    seed = round(runif(1, 0, 99999))
    set.seed(seed)
    eval(expr, envir = parent.frame())
    readline(sprintf("seed = %s, Press enter to continue!", seed))
  }
}

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

#' Minimize transactions among people to settle bills
#' @description Splitwise-like function for minimizing transactions for settling bills
#' @param balances A named vector with the amount contributed by each person to the total expense
#' @param digits The number of digits to round the amounts of money
#'
#' @return The shortest possible list indicating the amounts owed by whom to whom to settle the bill
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
