#' @title S4 class for the parameters of the benchmark variable(s) imputation method.
#'
#' @description Definition of the S4 class named \code{BenchImputationParam} for the parameters for
#' the benchmark variable(s) imputation method of missing values.
#'
#' @slot VarNames character vector with the names of the variables to impute.
#'
#' @slot DomainNames  character vector with the names of the variables determining the domains over
#' which to compute the benchmark values quantiles.
#'
#' @slot BenchValues \linkS4class{data.table} with the values of the benchmark variable used to
#' compute the imputation values.
#' @examples
#' # An empty BenchImputationParam object:
#' new(Class = 'BenchImputationParam')
#'
#' \dontrun{
#'
#' FF.StQList <- readRDS('../E30183.FF.StQList.rds')
#' FF <- FF.StQList[["MM072016"]]
#'
#' ValToImpute <- dcast_StQ(FD, ExtractNames(c('CifraNeg_13.___', 'ActivEcono_35._4._2.1.4._0')))
#' ValToImpute[sample(1:16941, floor(16941 / 5)), CifraNeg_13.___ := NA_real_]
#'
#' BenchValues <- dcast_StQ(FF, ExtractNames(c('CifraNeg_13.___', 'ActivEcono_35._4._2.1.4._0')))
#' BenchValues[is.na(CifraNeg_13.___), CifraNeg_13.___ := mean(CifraNeg_13.___), by = 'ActivEcono_35._6._2.1.4._0']
#' BenchValues <- BenchValues[, c('NOrden', 'CifraNeg_13.___'), with = FALSE]
#'
#' BenchImpParam <- new(Class = 'BenchImputationParam',
#'                      VarNames = 'CifraNeg_13.___',
#'                      DomainNames = 'ActivEcono_35._4._2.1.4._0',
#'                      BenchValues = BenchValues)
#' }
#'
#' @export
setClass(Class = "BenchImputationParam",
         slots = c(VarNames = 'character',
                   DomainNames = 'character',
                   BenchValues = 'data.table'),
         prototype = list(VarNames = character(0),
                          DomainNames = character(0),
                          BenchValues = 'data.table'),
         validity = function(object){

           return(TRUE)
         }
)
