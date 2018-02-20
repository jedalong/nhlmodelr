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
#' @param games the raw game data associated with \code{shots} (see details).
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
goalies <- function(model,shots,games){
  #Goalie Database
  goalID <- unique(shots$GOALIE.ID,na.rm=T)
  
  #Goalie Analysis
  goalCGP <- data.frame(ID=goalID,NAME=NA,POS=NA,TEAMID=NA,TEAMABBR=NA,GOALS=0,SAVES=0,SHOTS=0,Eg=0,Es=0,GD=0,stringsAsFactors=FALSE)
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
    
    goalCGP$GOALS[i] <- sum(xy$xi)
    goalCGP$SAVES[i] <- sum(xy$si)
    goalCGP$SHOTS[i] <- sum(xy$ni)
    goalCGP$Eg[i] <- sum(xy$ni * xy$pi)
    goalCGP$Es[i] <- sum((1-xy$pi)*xy$ni)
    
  }
  #Add Team Abbreviations - can we simplify this and do this earlier?
  for (i in 1:30){
    ind <- which(goalCGP$TEAMID==i)
    goalCGP$TEAMABBR[ind] <- as.character(games$V25[which(games$V22 == i)[1]]) #Get Team Name
  }
  
  goalCGP$GD <- goalCGP$GOALS - goalCGP$Eg
  goalCGP$CHI2 <- (goalCGP$GOALS-goalCGP$Eg)^2/goalCGP$Eg * sign(goalCGP$GOALS-goalCGP$Eg)
  goalCGP$SPCT <- goalCGP$SAVES / goalCGP$SHOTS
  goalCGP$eSPCT <- goalCGP$Es/goalCGP$SHOTS
  goalCGP$dSPCT <- goalCGP$SPCT - goalCGP$eSPCT
  goalCGP$AdjSPCT <- goalCGP$SPCT + (goalCGP$SPCT - goalCGP$eSPCT)
  return(goalCGP)
}

