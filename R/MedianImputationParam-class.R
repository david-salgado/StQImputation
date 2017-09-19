#' @title S4 class for the parameters of the median imputation method
#'
#' @description Definition of the S4 class named \code{MedianImputationParam} for the parameters for
#' the median imputation method of missing values.
#'
#' @slot VarNames character vector with the names of the variables to impute.
#'
#' @slot DomainNames  character vector with the names of the variables determining the domains over
#' which to compute the median.
#'
#' @examples
#' # An empty MedianImputationParam object:
#' new(Class = 'MedianImputationParam')
#'
#' @export
setClass(Class = "MedianImputationParam",
         slots = c(VarNames = 'character',
                   DomainNames = 'character'),
         prototype = list(VarNames = character(0),
                          DomainNames = character(0)),
         validity = function(object){
           
           return(TRUE)
         }
)