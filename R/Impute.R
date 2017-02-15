#' Apply the imputation method to a data set
#'
#' @param object Object containing the the data upon which the imputation is to be computed.
#'
#' @param Param Object of virtual class \linkS4class{ImputationParam} with the parameters specifying
#' the method of imputation to be applied.
#'
#' @return Object of the same class as the input \code{object} with the missing values imputed
#' according to the method specified in the input argument \code{Param}
#'
#' @examples
#' \dontrun{}
#'
#' @export
setGeneric("Impute", function(object, Param) {standardGeneric("Impute")})

#' @rdname Impute
#'
#' @include MeanImputationParam-class.R
#'
#' @import data.table StQ StQT
#'
#' @export
setMethod(f = "Impute",
          signature = c("data.table", "MeanImputationParam"),
          function(object, Param){

            ImputationVars <- Param@VarNames
            byVars <- Param@DomainNames
            output <- copy(object)
            for (Var in ImputationVars){

              if (length(byVars) != 0) {

                output[, (paste0('Mean_', Var)) := mean(get(Var), na.rm = TRUE), by = byVars]

              } else {

                output[, (paste0('Mean_', Var)) := mean(get(Var), na.rm = TRUE)]

              }
                output[is.na(get(Var)), (Var) := get(paste0('Mean_', Var))]

                if (all(is.na(output[[Var]]))) {

                  stop(paste0('[ImputationParam:: Impute] The variable ', Var, ' has all missing values. It is impossible to compute its mean value.\n'))

                }
                output[, (paste0('Mean_', Var)) := NULL]
                if (length(byVars) != 0 && any(is.na(output[[Var]]))) {

                  NewParam <- new(Class = 'MeanImputationParam',
                                  VarNames = ImputationVars,
                                  DomainNames = byVars[-length(byVars)])
                  output <- Impute(output, NewParam)
                }

            }

            return(output[])
          }
)
