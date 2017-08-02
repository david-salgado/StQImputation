#' @title Set value of component \code{VarNames} of an object \linkS4class{ImputationParam}
#'
#' @description \code{setVarNames} assigns a character value to the component \code{VarNames}  of
#' the input object \linkS4class{ImputationParam}.
#'
#' @param object Object \linkS4class{ImputationParam} whose component \code{VarNames}
#' is to be assigned.
#'
#' @param value character vector to be assigned to the component \code{VarNames}.
#'
#' @return Object \linkS4class{ImputationParam} with the component \code{VarNames} updated.
#'
#' @export
setGeneric("setVarNames<-", function(object, value){standardGeneric("setVarNames<-")})
#'
#' @rdname setVarNames
#'
#' @export
setReplaceMethod(
  f = "setVarNames",
  signature = c("ImputationParam","character"),
  function(object, value){
    object@VarNames <- value
    return(object)
  }
)
