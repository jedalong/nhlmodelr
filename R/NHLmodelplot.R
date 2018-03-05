# ---- roxygen documentation ----
#
#' @title Plot NHL model results
#'
#' @description
#'  Very basic plotting function for outputs from the spatial model of NHL goal scoring probabilities.
#'
#' @details
#'  Will be expanded later on.
#'
#' @param model a dataframe, specifically, ouput from the function \code{NHLmodel}.
#' @param plot character, what to plot; one of (\code{'shots','goals','postmean','postvar','none'}). If none is given just the rink outline is plotted.
#'
#' @return
#'  A half-rink plot of shots or goals (as points) or posterior mean or variance (as a surface).
#'
# @references
# @keywords
# @examples
#'
#' @export
#
# ---- End of roxygen documentation ----
NHLmodelplot <- function(model,plot='none'){
  ##############################################################################
  #plotting elements for the rink proper.
  
  faceoff.circle = function (x,y) {
    
    theta = seq(0,2*pi,length=300)
    #outer.
    polygon (x + 15*cos(theta),
             y + 15*sin(theta),
             lwd=2,
             border=2)
    polygon (x + 1*cos(theta),
             y + 1*sin(theta),
             col=2,
             border=2)
    segments (c(x-0.75,x-0.75, x+0.75,x+0.75, x-0.75,x-0.75, x+0.75,x+0.75),
              c(y-2,y-2, y-2,y-2, y+2,y+2,y+2,y+2),
              c(x-0.75,x-3.75, x+0.75,x+3.75, x-0.75,x-3.75, x+0.75,x+3.75),
              c(y-6,y-2, y-6,y-2, y+6,y+2,y+6,y+2),
              col=2, lwd=2)
    dd <- (5+7/12)/2
    segments (c(x-15, x-15, x+15, x+15),
              c(y-dd, y+dd, y-dd, y+dd),
              c(x-17, x-17, x+17, x+17),
              c(y-dd, y+dd, y-dd, y+dd),
              col=2, lwd=2)
  }
  
  goal.crease = function (flip=1) {
    xseq = seq(-4,4,length=100)
    polygon (c(-4, xseq, 4),
             flip*c(89, 83+xseq^2/4^2*1.5, 89),
             col="lightblue", border="red")
  }
  
  half.rink = function () {
    
    theta = seq(0,2*pi,length=300)
    #par(mar=c(0,0,0,0))
    plot(c(-42.6, 42.6), c(-101,0), ty="n", ylim=c(-101,0), xlim=c(-42.6, 42.6), ylab="", xlab="", axes=FALSE,asp=1)
    
    #Center Ice Circle
    polygon (15*cos(theta), 15*sin(theta), lwd=2, border=4)
    
    #Referee semi circle at penalty box
    theta2 = seq (pi/2, 3*pi/2, length=300)
    polygon (42.5 + 10*cos(theta2), 10*sin(theta2), lwd=2, border=2)
    
    #Center Ice
    rect(-42.5, -25, 42.5, -26, col=4, border=4)
    #Blue line
    rect(-42.5, -0.5, 42.5, 0.5, col=2, border=2)
    
    #Outside boards
    x <- 18
    lines (c(-42.5, 
             -42.5 + x - x*cos(seq(0,pi/2,length=20)),
             42.5 - x + x*cos(seq(pi/2,0,length=20)),
             42.5),
           c(15,
             -82 - x*sin(seq(0,pi/2,length=20)),
             -82 - x*sin(seq(pi/2,0,length=20)),
             15),
           col=1, lwd=2)
    
    
    goal.line.extreme = 42.5 - x + sqrt(x^2 - (x-11)^2)
    
    #the goal line & crese
    lines(goal.line.extreme*c(-1, 1), rep(-89,2), col=2,lwd=2)        #the goal line.
    lines(c(-3,-3,3,3), -(c(90,92,92,90)-1), col=1, lwd=3)    #the goal net.
    goal.crease(-1)
    ## traps.
    segments(c(-11, 11, -11, 11), c(89,89,-89,-89),
             c(-14,14,-14,14), c(100,100, -100,-100), col=2, lwd=2)
    
    faceoff.circle (22, -69)
    faceoff.circle (-22, -69)
    
    faceoff.dot = function (x,y) {
      polygon (x + 1*cos(theta),
               y + 1*sin(theta),
               col=2,
               border=2)
    }
    faceoff.dot (-22,-20)
    
    faceoff.dot (22,-20)
    
  }
  
  jitter <- function(x,y,nj,cs=0.5){
    jdf <- data.frame(x=runif(nj,min=x-cs,max=x+cs)) 
    jdf$y <- runif(nj, min=y-cs,max=y+cs)
    jdf
  }
  
  #Rotates x and y so that net is the bottom of plot, adjusted accordingly below.
  half.rink()
  
  #Plotting options
  # plotting all shots
  if (plot =='shots'){
    for (i in 1:dim(model)[1]){
      temp <- jitter(-1*model$oldy[i],model$oldx[i],model$n[i],0.5)   
      points(temp$x,temp$y,cex=0.05,pch=19)
    }
  }
  
  #Plotting goals
  if (plot == 'goals') {
    for (i in 1:dim(model)[1]){
      temp <- jitter(-1*model$oldy[i],model$oldx[i],model$g[i],0.5)   
      points(temp$x,temp$y,cex=0.05,pch=19)
    }
  }
  
  #plotting raster surface
  xx <- unique(model$oldx)
  yy <- unique(model$oldy)
  #col=rev(heat.colors(100,alpha=0.7))
  col=topo.colors(100,alpha=0.7)
  
  #Plotting Posterior Mean
  if (plot == 'postmean'){
    mat <- matrix(data=model$p_postmean,nrow=length(xx),ncol=length(yy),byrow=F)
    mlim <- c(0,max(mat,na.rm=T))
    mat <- t(mat)  #Transpose to rotate plot
    image(yy,xx,mat,zlim=mlim,col=col,add=T)
    fields::image.plot(legend.only=TRUE,zlim=mlim,col=col,legend.shrink=0.4,horizontal=F)
    contour(yy,xx,mat,levels=c(0.05,0.1,0.15,0.2),add=TRUE)
  }
  
  #Plotting Posterior variance
  if (plot == 'postvar'){
    mat <- matrix(data=model$p_postvar,nrow=length(xx),ncol=length(yy),byrow=F)
    mlim <- c(0,max(mat,na.rm=T))
    mat <- t(mat)
    image(yy,xx,mat,zlim=mlim,col=col,add=T)
    fields::image.plot(legend.only=TRUE,zlim=mlim,col=col,legend.shrink=0.4,horizontal=F)
    contour(yy,xx,mat,levels=c(0.05,0.1,0.15,0.2),add=TRUE)
  }
  
}
