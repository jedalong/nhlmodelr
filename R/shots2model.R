# ---- roxygen documentation ----
#
#' @title Format shot data for model 
#'
#' @description
#'  Format shot location data for spatial model for NHL goal scoring probability
#'
#' @details
#'  Will be expanded later on.
#'  - Automatically removes penalty shots and empty net situations.
#'  - Currently only considers reg. season games, could be updated.
#'
#' @param shots a dataframe, specifically, a shots data frame downloaded from the package \code{nhlmodelr}.
#'
#' @return
#'  The function returns a dataframe with four columns: x and y locations, and number of shots and goals from each location.
#'
# @references
# @keywords
# @examples
#'
#' @export
#
# ---- End of roxygen documentation ----

shots2model <- function(shots){
  
  #create
  xx <- -89:-25
  yy <- -42:42
  xy <- expand.grid(x=xx,y=yy)
  xy$xi <- 0
  xy$ni <- 0
  
  ## Spatial Pool shots to model format.
  for (i in 1:dim(xy)[1]){
    loc <- shots[shots$jX==xy$x[i] & shots$jY==xy$y[i],]
    
    n.g <- length(which(loc$event == 'GOAL'))
    n.s <- length(which(loc$event == 'SHOT')) 
    n.m <- length(which(loc$event == 'MISS')) 

    xy$xi[i] <- n.g
    xy$ni[i] <- n.g + n.s #+ n.m   
  }
  return(xy)
}

