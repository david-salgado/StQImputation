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
#' \dontrun{
#'
#' FF.StQList <- readRDS('../E30183.FF.StQList.rds')
#' FF <- FF.StQList[["MM072016"]]
#'
#' ValToImpute <- dcast_StQ(FD, ExtractNames(c('CifraNeg_13.___', 'ActivEcono_35._4._2.1.4._0')))
#' ValToImpute[sample(1:16941, floor(16941 / 5)), CifraNeg_13.___ := NA_real_]
#' Units1 <- ValToImpute[, 'NOrden', with = FALSE]
#'
#' BenchValues <- dcast_StQ(FF, ExtractNames(c('CifraNeg_13.___', 'ActivEcono_35._4._2.1.4._0')))
#' Units2 <- BenchValues[, 'NOrden', with = FALSE]
#' Units <- merge(Units1, Units2)
#' setkeyv(ValToImpute, 'NOrden')
#' ValToImpute <- ValToImpute[NOrden %chin% Units[['NOrden']]]
#' BenchValues <- BenchValues[, c('NOrden', 'CifraNeg_13.___', 'ActivEcono_35._4._2.1.4._0'), with = FALSE]
#' setkeyv(BenchValues, 'NOrden')
#' BenchValues <- BenchValues[NOrden %chin% Units[['NOrden']]]
#' BenchValues[is.na(CifraNeg_13.___), CifraNeg_13.___ := mean(CifraNeg_13.___, na.rm = TRUE), by = 'ActivEcono_35._4._2.1.4._0']
#' BenchValues[is.na(CifraNeg_13.___), CifraNeg_13.___ := mean(CifraNeg_13.___, na.rm = TRUE)]
#' BenchValues <- BenchValues[, c('NOrden', 'CifraNeg_13.___'), with = F]
#' BenchImpParam <- new(Class = 'BenchImputationParam',
#'                      VarNames = 'CifraNeg_13.___',
#'                      DomainNames = 'ActivEcono_35._4._2.1.4._0',
#'                      BenchValues = BenchValues)
#' Impute(ValToImpute, BenchImpParam)
#' }
#'
#' @include MeanImputationParam-class.R
#'
#' @import data.table StQ
#'
#' @export
setGeneric("Impute", function(object, Param) {standardGeneric("Impute")})

#' @rdname Impute
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


#' @rdname Impute
#'
#' @export
setMethod(f = "Impute",
          signature = c("data.table", "BenchImputationParam"),
          function(object, Param){

            BenchImp <- function(x, y){

              if (all(is.na(x))) return(rep(NA_real_, length(x)))
              output <- quantile(x, probs = ecdf(y)(y), na.rm = TRUE)
              return(output)
            }

            ImputationVars <- Param@VarNames
            byVars <- Param@DomainNames
            UnitVars <- Param@UnitNames
            output <- copy(object)
            BenchValues <- copy(Param@BenchValues)
            OrigBenchVar <- setdiff(names(BenchValues), c(UnitVars, ImputationVars))

            if (length(OrigBenchVar) > 1) stop('[StQImputation::Impute] There exist more than one benchmark variable.')
            if (any(is.na(BenchValues[[OrigBenchVar]]))) stop(paste0('[StQImputation::Impute] The slot BenchValues has missing values in the variable ', OrigBenchVar, '.\n'))

            setnames(BenchValues, OrigBenchVar, paste0('Bench_', OrigBenchVar))
            BenchVar <- paste0('Bench_', OrigBenchVar)
            output <- merge(output, BenchValues, all.x = TRUE)

            for (Var in ImputationVars) {

              if (length(byVars) != 0) {

                output[, (paste0('Imp_', Var)) := BenchImp(get(Var), get(BenchVar)), by = byVars]

              } else {

                output[, (paste0('Imp_', Var)) := BenchImp(get(Var), get(BenchVar))]

              }
              output[is.na(get(Var)), (Var) := get(paste0('Imp_', Var))]

              if (all(is.na(output[[Var]]))) {

                stop(paste0('[ImputationParam:: Impute] The variable ', Var, ' has all missing values. It is impossible to compute its benchmark ecdf.\n'))

              }

              output[, (paste0('Imp_', Var)) := NULL]

              if (length(byVars) != 0 && any(is.na(output[[Var]]))) {

                output[, (BenchVar) := NULL]
                if (BenchVar %in% names(BenchValues)) setnames(BenchValues, BenchVar, OrigBenchVar)
                NewParam <- new(Class = 'BenchImputationParam',
                                VarNames = Var,
                                DomainNames = byVars[-length(byVars)],
                                UnitNames = UnitVars,
                                BenchValues = BenchValues)

                output <- Impute(output, NewParam)
              }
            }

            return(output[])
          }
)
