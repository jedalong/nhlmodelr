# ---- roxygen documentation ----
#
#' @title Locally weighted regression smoothing
#'
#' @description
#'  Wrapper function for computing locally weigthed regression smoothing of NHL shot data, for comparison with the output NHL model values.
#'
#' @details
#'  Will be expanded later on.
#'
#' @param model a dataframe, specifically, ouput from the function \code{NHLmodel}.
#' @param alpha the smoothing parameter; see \code{?loess}
#'
#' @return
#'  A NHLmodel dataframe with an additional column - \code{loess} the local regression values.
#'
# @references
# @keywords
# @examples
#'
#' @export
#
# ---- End of roxygen documentation ----

NHLloess <- function(model,alpha=0.05){
  rawp <- sapply(model$g/model$n,max,0,na.rm=T)
  lowdf <- data.frame(rawp=rawp,x=model$newx,y=model$newy)
  aa <- loess(rawp ~ x + y,data=lowdf, weights=model$n,span=alpha)  
  model$loess <- aa$fitted
  return(model)
}