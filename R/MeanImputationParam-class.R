#' @title S4 class for the parameters of the mean imputation method
#'
#' @description Definition of the S4 class named \code{MeanImputationParam} for the parameters for
#' the mean imputation method of missing values.
#'
#' @slot VarNames character vector with the names of the variables to impute.
#'
#' @slot DomainNames  character vector with the names of the variables determining the domains over
#' which to compute the mean.
#'
#' @examples
#' # An empty MeanImputationParam object:
#' new(Class = 'MeanImputationParam')
#'
#' @export
setClass(Class = "MeanImputationParam",
         slots = c(VarNames = 'character',
                   DomainNames = 'character'),
         prototype = list(VarNames = character(0),
                          DomainNames = character(0)),
         validity = function(object){

           return(TRUE)
         }
)
