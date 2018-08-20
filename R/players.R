# ---- roxygen documentation ----
#
#' @title Player shooting analysis
#'
#' @description
#'  Calculate spatially adjusted player-based shooting  statistics using spatially explicit model of NHL scoring probability.
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
#Function to Process Player CGP Statistics
players <- function(model,shots){
  #Player Database
  plyrID <- unique(shots$PLYR1.ID)
  
  #Player Analysis
  plyrCGP <- data.frame(ID=plyrID,NAME=NA,POS=NA,TEAMID=NA,TEAMABBR=NA,Goals=0,Shots=0,Eg=0,stringsAsFactors=FALSE)
  xx <- -89:-25
  yy <- -42:42
  xy <- expand.grid(x=xx,y=yy)
  xy$xi <- 0
  xy$ni <- 0
  xy$pi <- model$p_postmean
  
  for (i in 1:length(plyrID)){
    id <- plyrID[i]
    #Compute CGP & dCGP
    shts <- subset(shots, PLYR1.ID==id & Goalie==901)  #No EN or Penalty Shots
    gls <- subset(shts,PLAYID == 505)
    svs <- subset(shts,PLAYID == 506)
    plyrCGP$NAME[i] <- shts$PlyrName[1]
    plyrCGP$POS[i] <- shts$PlyrPos[1]
    plyrCGP$TEAMID[i] <- shts$TeamID[1]   #only gets team during first shot of season
    
    for (j in 1:dim(xy)[1]){
      n.g <- length(which(gls$x==xy$x[j] & gls$y==xy$y[j]))
      n.s <- length(which(svs$x==xy$x[j] & svs$y==xy$y[j]))
      #n.m <- length(which(mis$x==xy$x[j] & mis$y==xy$y[j]))
      #n.b <- length(which(blk$x==xy$x[j] & blk$y==xy$y[j])
      xy$xi[j] <- n.g
      xy$ni[j] <- n.g + n.s #+n.m + n.b
    }
    plyrCGP$Goals[i] <- sum(xy$xi)
    plyrCGP$Shots[i] <- sum(xy$ni)
    plyrCGP$Eg[i] <- sum(xy$ni * xy$pi)
  }
  #Add Team Abbreviations
  data(teamInfo)
  for (i in 1:30){
    ind <- which(plyrCGP$TEAMID==i)
    plyrCGP$TEAMABBR[ind] <- teamInfo$Abbr[i] #Get Team Name Abbreviation
  }
  
  plyrCGP$GD <- plyrCGP$Goals - plyrCGP$Eg
  plyrCGP$ShP <- (plyrCGP$Goals / plyrCGP$Shots) * 100
  plyrCGP$dShP <- plyrCGP$GD / plyrCGP$Shots * 100
  plyrCGP$GP <- plyrCGP$GD / plyrCGP$Eg
  
  return(plyrCGP)
}
