# ---- roxygen documentation ----
#
#' @title Team shooting analysis
#'
#' @description
#'  Calculate spatially adjusted team-based shooting  statistics using spatially explicit model of NHL scoring probability.
#'
#' @details
#'  Will be expanded later on.
#'
#' @param model a dataframe, specifically, ouput from the function \code{NHLmodel}.
#' @param shots the raw data shots file that was used as input into the function \code{shots2model}.
#'
#' @return
#'  The function returns a dataframe with spatially adjusted team shooting statistics.
#'
# @references
# @keywords
# @examples
#'
#' @export
#
# ---- End of roxygen documentation ----

#Function to Process Team CGP Statistics
teams <- function(model,shots){
  
  data(teamInfo)
  teamInfo$Goals <- 0
  teamInfo$Shots <- 0
  teamInfo$Eg <-0
  teamInfo $GD <- 0
  
  xx <- -89:-25
  yy <- -42:42
  for (i in 1:30){                       #Vegas will be 31    
    shts <- subset(shots,TeamID == i)
    shts <- subset(shts, Goalie == 901) #No EN or Penalty Shots
    gls <- subset(shts,PLAYID == 505)
    svs <- subset(shts,PLAYID == 506)
    mis <- subset(shts,PLAYID == 507)
    blk <- subset(shts,PLAYID == 508)
    
    xy <- expand.grid(x=xx,y=yy)
    xy$xi <- 0
    xy$ni <- 0
    xy$pi <- model$p_postmean
    ## Spatial Pool shots to model format.
    for (j in 1:dim(xy)[1]){
      xi <- xy$x[j]
      yi <- xy$y[j]
      n.g <- length(which(gls$x==xy$x[j] & gls$y==xy$y[j]))
      n.s <- length(which(svs$x==xy$x[j] & svs$y==xy$y[j]))
      n.m <- length(which(mis$x==xy$x[j] & mis$y==xy$y[j]))
      n.b <- length(which(blk$x==xy$x[j] & blk$y==xy$y[j]))
      xy$xi[j] <- n.g
      xy$ni[j] <- n.g + n.s #+ n.m + n.b
    }
    
    teamInfo$Goals[i] <- sum(xy$xi)
    teamInfo$Shots[i] <- sum(xy$ni)
    teamInfo$Eg[i] <- sum(xy$ni * xy$pi)
    
  }
  
  teamInfo$GD<- teamInfo$Goals - teamInfo$Eg
  return(teamInfo)
}