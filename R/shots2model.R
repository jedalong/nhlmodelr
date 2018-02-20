# ---- roxygen documentation ----
#
#' @title Format shot data for model 
#'
#' @description
#'  Format shot location data for spatial model for NHL goal scoring probability
#'
#' @details
#'  Will be expanded later on.
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
  #------------------------------------------------------------------
  gls <- subset(shots,PLAYID == 505)
  svs <- subset(shots,PLAYID == 506)
  mis <- subset(shots,PLAYID == 507)
  blk <- subset(shots,PLAYID == 508)       #should be no blks if using Goalie = 901
  #------------------------------------------------------------------
  xx <- -89:-25
  yy <- -42:42
  xy <- expand.grid(x=xx,y=yy)
  xy$xi <- 0
  xy$ni <- 0
  
  ## Spatial Pool shots to model format.
  for (i in 1:dim(xy)[1]){
    xi <- xy$x[i]
    yi <- xy$y[i]
    
    n.g <- length(which(gls$x==xi & gls$y==yi))
    n.s <- length(which(svs$x==xi & svs$y==yi)) 
    n.m <- length(which(mis$x==xi & mis$y==yi))
    n.b <- length(which(blk$x==xi & blk$y==yi))
    
    xy$xi[i] <- n.g
    xy$ni[i] <- n.g + n.s #+ n.m     + n.b?
  }
  return(xy)
}

