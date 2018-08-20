# ---- roxygen documentation ----
#
#' @title Goalie save percentage analysis
#'
#' @description
#'  Calculate spatially adjusted goalie save percentage statistics using spatially explicit model of NHL scoring probability.
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
#Function to Process Goalie CGP Statistics
goalies <- function(model,shots){
  #Goalie Database
  goalID <- unique(shots$GOALIE.ID,na.rm=T)
  
  #Goalie Analysis
  goalCGP <- data.frame(ID=goalID,NAME=NA,POS=NA,TEAMID=NA,TEAMABBR=NA,Shots=0,Goals=0,Saves=0,Eg=0,Es=0,stringsAsFactors=FALSE)
  xx <- -89:-25
  yy <- -42:42
  xy <- expand.grid(x=xx,y=yy)
  xy$xi <- 0
  xy$ni <- 0
  xy$pi <- model$p_postmean
  
  for (i in 1:length(goalID)){
    id <- goalID[i]
    #Compute CGP & dCGP
    shts <- subset(shots, GOALIE.ID==id & Goalie==901)  #No EN or Penalty Shots
    gls <- subset(shts,PLAYID == 505)
    svs <- subset(shts,PLAYID == 506)
    goalCGP$NAME[i] <- shts$GoalieName[1]
    goalCGP$POS[i] <- 'G'
    goalCGP$TEAMID[i] <- shts$gTeamID[1]   #only gets team during first shot of season
    
    for (j in 1:dim(xy)[1]){
      n.g <- length(which(gls$x==xy$x[j] & gls$y==xy$y[j]))
      n.s <- length(which(svs$x==xy$x[j] & svs$y==xy$y[j]))
      #n.m <- length(which(mis$x==xy$x[j] & mis$y==xy$y[j]))
      #n.b <- length(which(blk$x==xy$x[j] & blk$y==xy$y[j])
      xy$xi[j] <- n.g
      xy$ni[j] <- n.g + n.s #+n.m + n.b
      xy$si[j] <- n.s
    }
    goalCGP$Shots[i] <- sum(xy$ni)
    goalCGP$Goals[i] <- sum(xy$xi)
    goalCGP$Saves[i] <- sum(xy$si)
    goalCGP$Eg[i] <- sum(xy$ni * xy$pi)
    goalCGP$Es[i] <- sum((1-xy$pi)*xy$ni)
    
  }
  
  #Add Team Abbreviations
  data(teamInfo)
  for (i in 1:30){                  #Vegas will be 31
    ind <- which(goalCGP$TEAMID==i)
    goalCGP$TEAMABBR[ind] <- teamInfo$Abbr[i] #Get Team Name Abbreviation
  }

  
  goalCGP$SD <- goalCGP$Saves - goalCGP$Es
  goalCGP$SvP <- goalCGP$Saves / goalCGP$Shots * 100
  goalCGP$ESvP <- goalCGP$Es / goalCGP$Shots * 100
  goalCGP$dSvP <- goalCGP$SvP - goalCGP$ESvP
  goalCGP$SP <- (goalCGP$Saves - goalCGP$Es) / goalCGP$Es
  return(goalCGP)
}

