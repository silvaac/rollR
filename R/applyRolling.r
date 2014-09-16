#' Similar to usual apply function but applies a function on a sliding window
#' 
#' Very powerfull function that applies a given function over a sliding window and
#' uses snowfall package to run in parallel. Checks shows considerable performance
#' increase (linear on the number of cores). If sfInit is not called it will run 
#' the serial "applyRolling". Similar but faster that rollapplyr from xts.
#' The main difference is that the user has to explicitly handle input data with
#' more than one column (see example).
#' @param x input xts,matrix or data.frame
#' @param fn function to be applied 
#' @param lookback is the number of periods we slide over the ts x
#' @param step is the number of period we move the lookback window
#' @param expand if true the window starts at lookback and increases at every step
#' @param parallel if true it will try to run in parallel in multiple cores or cluster.
#'        Set up needs to be performed before calling applyRolling. 
#'        Default is parallel=FALSE. See snowfall package for set up.
#' @param fill TRUE (default) will return a series of dim equal to x filling the
#'        values within the lookback with NA. FALSE returns data < dim(x)
#' @param ... additional parameters for function fn
#' @return time series (xts) of dim equal to x
#' @examples
#' x1 <- xts(1:100,Sys.Date()+1:100)
#' x2 <- merge(x1,xts(101:200,Sys.Date()+1:100))
#' # Using multiple CPUs with snowfall
#' \dontrun{  
#' library(snowfall)
#' sfInit(parallel=TRUE,cpus=4)
#' sfLibrary(xts)
#' applyRolling(x1,fn=mean,lookback=2,parallel=TRUE)
#' applyRolling(x2,fn=function(x) colMeans(x),lookback=2,parallel=TRUE)
#' sfStop()
#' }
#' # Not parallel
#' applyRolling(x1,fn=mean,lookback=2)
#' applyRolling(x2,fn=function(x) colMeans(x),lookback=2)
#' @export
applyRolling <- function(x,fn,lookback=100,step=1,expand=FALSE,parallel=FALSE,fill=TRUE,...){
  
  n <- dim(x)[1]
  if(is.null(n)) stop('data input x has NULL dim')   

  if(expand){
    seqq <- lookback:n
    f_t<-function(n,y,fn,lookback2,...){
      xx <- y[1:n,]
      z<-fn(xx,...)
      return(z)
    }
  }else{
    seqq <-seq(lookback,n,by=step)
    if(seqq[length(seqq)]!=n) seqq <- c(seqq,n)  # make sure we always have the last point in
    f_t<-function(n,y,fn,lookback2,...){
      xx <- y[{n-lookback2+1}:n,]
      z<-fn(xx,...)
      return(z)
    }
  }
  if(!is.xts(x)){
	  if(parallel){
	    return(snowfall::sfSapply(seqq,f_t,y=x,fn=fn,lookback2=lookback,...))
	  }else{
	    return(sapply(seqq,f_t,y=x,fn=fn,lookback2=lookback,...))
	  }
  }else{
	  if(parallel){
            xx<- snowfall::sfSapply(seqq,f_t,y=coredata(x),fn=fn,lookback2=lookback,...)
	  }else{
            xx<- sapply(seqq,f_t,y=coredata(x),fn=fn,lookback2=lookback,...)
	  }
  }
    xx <- as.matrix(xx)
    if(dim(xx)[1]!=length(index(x[seqq,]))) xx <- t(xx)
    
    if(!fill) return(xts(xx,order.by=index(x[seqq,])))
    
    xx1 = xts(xx,order.by=index(x[seqq,]))
    xx2 = merge(x[,1],xx1)[,-1]
    colnames(xx2) <- colnames(xx1)
    return(xx2)
  
}
weekDay <- function(dates,dayNum,type="week"){
		
	if(type=="week" && !any(dayNum==(1:7))) stop("ERROR: one week has 7 days, dayNum must go from 1 to 7.")
	if(type=="month" && !any(dayNum==(1:31))) stop("ERROR: one month has a max of 31 days, dayNum must go from 1 to 31.")

	a = switch(type,
	           week = which(lubridate::wday(dates)==dayNum),
	           month= which(lubridate::mday(dates)==dayNum)
	)
	if(length(a)==0) stop("dayNum selected does not exist in sample")
	return(a)	
}
#' Apply a function from one calendar day to the next
#'
#' One can apply a function from one day of the week to the next. So for
#' example, from one Monday to the next Monday. Or from the 3rd of the month to
#' the next 3rd.  Notice that if your data does not have a 3rd every month (for
#' instance it is a holiday), then apply will skip to the next 3rd available.
#' This results into a gap of at least 2 months. The same goes for weekly data.
#' @param x input xts
#' @param fn function to be applied 
#' @param dayNum is the number that matches to the day wanted. For type "week",
#' the numbers go from 1 (sunday) to 7 (saturday). For type "month" they go from
#' 1 to 31.
#' @param type week or month, where default is week. For example , from Monday
#' (dayNum=2) to Monday for "week" or from the 2rd to the next 2rd for "month".
#' @param parallel default is FALSE. If TRUE use snowfall package to run in
#' parallel.
#' @param ... additional parameters for function fn
#' @examples
#' x1 <- xts(1:100,Sys.Date()+1:100)
#' x2 <- merge(x1,xts(101:200,Sys.Date()+1:100))
#' # Using multiple CPUs with snowfal
#' \dontrun{ 
#' library(snowfall)
#' sfInit(parallel=TRUE,cpus=4)
#' sfLibrary(xts)
#' applyDToD(x1,fn=mean,dayNum=2,parallel=TRUE)
#' applyDToD(x2,fn=colMeans,dayNum=2,parallel=TRUE)
#' sfStop()
#' }
#' # Not parallel
#' applyDToD(x1,fn=mean,dayNum=3)
#' applyDToD(x2,fn=colMeans,dayNum=3)
#' @export
applyDToD <- function(x,fn,dayNum=2,type="week",parallel=FALSE,...){
	if(!is.xts(x)) stop("Need xts obj")

	seqq = weekDay(index(x),dayNum,type)
	nseq = 2:length(seqq)
    	f_t<-function(n,y,fn,tseqq,...){
      		xx <- y[tseqq[n-1]:tseqq[n],]
      		z<-fn(xx,...)
      		return(z)
    	}

	if(parallel){
          xx<- snowfall::sfSapply(nseq,f_t,y=coredata(x),fn=fn,tseqq=seqq,...)
	}else{
          xx<- sapply(nseq,f_t,y=coredata(x),fn=fn,tseqq=seqq,...)
	}
	xx <- as.matrix(xx)
	if(dim(xx)[1]!=length(index(x[seqq[-1],]))) xx <- t(xx)
        return(xts(xx,order.by=index(x[seqq[-1],])))
}
