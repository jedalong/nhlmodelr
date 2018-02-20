# ---- roxygen documentation ----
#
#' @title Spatial model for NHL goal scoring probability
#'
#' @description
#'  Calculate spatial probability of NHL goal scoring based on shot location data.
#'
#' @details
#'  Will be expanded later on.
#'
#' @param x a dataframe, specifically, output from the function \code{shots2model}.
#' @param ngibbs numeric, number of Gibbs samples to run the model for.
#' @param burnin numeric, number of Gibbs samples to discard as burn-in.
#' @param p.max numeric, tuning parameter indicating the maximum probability value at the goal (should be between 0 and 1).
#' @param inits a dataframe containing values to initiate the Gibbs sampler (see details).
#' @param outfile output path and filename (.RData) for storing the model results (8 objects; see details).
#'
#' @return
#'  The function returns a dataframe with the posterior mean and variance of the model which can be used to map spatial variation in NHL goal scoring probabilities.
#'
# @references
# @keywords
# @examples
#'
#' @export
#
# ---- End of roxygen documentation ----
#Core model function that can be easily called with a suite of key inputs.
# This model should be adjusted to allow for testing of alterntive models.
NHLmodel <- function(x,ngibbs,burnin,p.max,inits,outfile){
  #-----------------------------------------------
  # Format the data - needs to be simplified
  #-----------------------------------------------
  mx = 89-25+1    #number of locations in x dimension
  my = 1+42+42    #number of locations in y dimension
  m = mx*my
  df <- expand.grid(x=1:mx,y=1:my)
  df$oldx <- x[,1]
  df$oldy <- x[,2]
  df$newx <- df$oldx + 89
  df$newy <- df$oldy
  df$rad <- sqrt((df$newx)^2+(df$newy)^2)
  df$theta <- acos(df$newx/df$rad)
  df$theta[mx*(my-1)/2+1] = 0          #theta down the center = 0 (removes an NA)
  #convert theta to be equal left or right of centre
  for (i in 1:mx) {
    for (j in 1:my) {
      index = (j-1)*mx+i
      if ( df$theta[index] > pi ) { df$theta[index] = 2*pi - df$theta[index] }
    }
  }
  df$index <- (df$y-1)*mx+df$x
  df$n <- x[,4]
  df$g <- x[,3]
  #------------------------------------------

  #Other parameters
  p <- inits  #Initial values
  w <- 3   #Size of spatial neighbourhood, not sure why we would ever change this..

  #variables for watching the draws from the posterior
  post.p <- matrix(nrow=m,ncol=ngibbs)
  post.upp <- matrix(nrow=m,ncol=ngibbs)
  post.low <- matrix(nrow=m,ncol=ngibbs)
  post.upp.ind <- post.upp
  post.low.ind <- post.low
  p. <- p
  upper <- p
  lower <- p
  upper.ind <- upper
  lower.ind <- lower


  #====================================================================
  # Main Functions
  #====================================================================
  #### Function cbeta that generates from a constrained beta.
  cbeta <- function(p_lower,p_upper,x,n) {
    if (p_upper < p_lower){
      ptemp <- p_upper
      p_upper <- p_lower
      p_lower <- ptemp
    }
    #is this necessary?
    if (p_upper ==  p_lower){
      p_upper <- p_upper + 0.0001
    }
    a = n+1
    u = runif(1)
    #if no goals are scored
    if ( x == 0 ){ ccbeta = 1-((1-p_upper)**a*u+(1-u)*(1-p_lower)**a)**(1/a) }
    #if num goals equals num of shots
    if ( (x != 0) && (x == n) ){ ccbeta = ((p_upper**a-p_lower**a)*u+p_lower**a)**(1/a) }
    #if num goals < num shots
    if ( (x != 0) && (x != n) ){
      temp <- x/n
      if ( temp < p_lower ){ temp <- p_lower }
      if ( temp > p_upper ){ temp <- p_upper }
      u2 <- runif(1)
      ratio <- -1
      count <- 0
      while ( u2 > ratio ){
        count <- count + 1
        u1 <- runif(1)
        u2 <- runif(1)
        ccbeta <- p_lower + u1*(p_upper-p_lower)
        ratio <- (ccbeta/temp)^x * ((1-ccbeta)/(1-temp))^(n-x)
      }
    }
    return(ccbeta)
  }
  #--------------------------------------------------------------------
  #### Function deriving the index list of a wxw (i.e. 3x3) neighbourhood around each locaiton.
  # This is just to save processing time later on.
  bounds <- function(index,w,xydf){
    w<- trunc(w/2)
    x <- xydf$x[index]
    y <- xydf$y[index]
    x.range <- (x-w):(x+w)
    y.range <- (y-w):(y+w)

    indo <- which(xydf$x %in% x.range & xydf$y %in% y.range)
    ind.k <- xydf$index[indo]
    ind.i <- which(ind.k == index)
    ind.k <- ind.k[-ind.i]

    return(ind.k)
  }
  index.list <- sapply(df$index,bounds,w=w, xydf=df[,c('index','x','y')])
  #---------------------------------------------------------------------
  #=====================================================================

  # Gibbs sampling: the current shooting location corresponds to index=1,...,m.
  for (igibbs in 1:ngibbs) {
    # Loop to derive upper and lower limits (p_U and p_L).
    for (jj in df$index){
      win <- unlist(index.list[jj]) #Get the indices of neighbours for location i
      #Constrain based only on rad (distance)
      iU <- which(df$rad[win] < df$rad[jj])
      iL <- which(df$rad[win] > df$rad[jj])
      #Constrain based on rad (distance) and theta (shot angle)
      #iU <- which(df$rad[win] < df$rad[jj] & df$theta[win] <= df$theta[jj])
      #iL <- which(df$rad[win] > df$rad[jj] & df$theta[win] >= df$theta[jj])

      #------- This is the area where we are having some confusion -----------------
      #Below, I keep switching the min and max, min for upper max for lower is how Tim envisioned it
      # Having max for upper and min for lower seems to produce the more realistic shapes, but may
      #    not be as appropriate.
      #     if(length(iU) > 0){
      #       upper[jj] <- min(p[win[iU]])
      #       #store the index
      #       upper.ind[jj] <- win[iU[which.min(p[win[iU]])]]
      #       }
      #     if(length(iL) > 0){
      #       lower[jj] <- max(p[win[iL]])
      #       #store the index
      #       lower.ind[jj] <- win[iL[which.max(p[win[iL]])]]
      #       }
      if(length(iU) > 0){
        upper[jj] <- max(p[win[iU]])
        #store the index
        upper.ind[jj] <- win[which.max(p[win[iU]])]
      }
      if(length(iL) > 0){
        lower[jj] <- min(p[win[iL]])
        #store the index
        lower.ind[jj] <- win[which.min(p[win[iL]])]
      }

      #-----------------------------------------------------------------------------

      #constrain p_lower and p_upper in certain locations, manually set p_L = 0 for certain locations
      if (df$newy[jj] == -42){lower[jj] <- 0}   #p_L along bottom boards = 0
      if (df$newy[jj] == 42){lower[jj] <- 0}    #p_L along top boards = 0
      if (df$newx[jj] == 64){lower[jj] <- 0}    #p_L along blueline = 0
      if (df$newx[jj] == 0 & abs(df$newy[jj]) >= 5){lower[jj] <- 0} #p_L along goalline = 0
      if (df$newx[jj] == 0 & df$newy[jj] == 0){upper[jj] <- p.max} #pmax in the center of the goal
    }
    #-----------------------------------------------------------------------------------------------
    # Note: Below I use an 'apply' function which essentially parralellizes the Gibbs sampling across space. This makes the current round of Gibbs sampling dependent only on the previous round, and not partially dependent on the current round. I'm not sure if this is appropriate or not, but it is faster computationally.
    p. <- mapply(cbeta, lower, upper, df$g, df$n)
    #----------------------------------------------------------------------------------------------

    #keep the draws and upper and lower constraints for later viewing
    post.p[,igibbs] <- p.
    post.upp[,igibbs] <- upper
    post.low[,igibbs] <- lower
    post.upp.ind[,igibbs] <- upper.ind
    post.low.ind[,igibbs] <- lower.ind
    #update p
    p <- p.
  }

  ##Summarizing
  p.mat <- post.p[,(burnin+1):ngibbs]
  df$p_postmean <- apply(p.mat,1,sum)/(ngibbs-burnin)
  df$p_postvar <- apply(p.mat,1,var)

  #save results
  save.list <- c('df','post.p','post.upp','post.low','post.low.ind','post.upp.ind','burnin','ngibbs')
  save(list=save.list,file=outfile)

  return(df)
}

