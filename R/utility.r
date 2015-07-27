#' Buissines calendar using RQuantLib
#'
#' Using QuantLib calendars as default. However we found some errors comparing
#' to NYSE. Therefore we have an option to use the S&P500 index from yahoo to
#' extract the valid buissines days for NYSE calendar. Yahoo has data
#' after 1950. For data before 1950 we use QuantLib. QuantLib NYSE calendar after
#' 1980 is corrected manually on 2014-09. That means that Yahoo and NYSE dates
#' are going to match, but since it is manual process (I give it a list of days)
#' it might not be updated fast enough, therefore there could be errors!
#' Notice that you
#' will need internet to download data from yahoo. It
#' is also slower to download data from yahoo. Finally, Yahoo data might also
#' have errors (missing business days or other)...  
#' According to RQuantLib documentation there are lots of other calendars. See
#' ?isBusinessDay for other calendars (bond, energy and other countries)
#' @param from starting day
#' @param to   end of period
#' @param by   step to update days
#' @param calendar name of calendar. See ?isBusinessDay in RQuantLib for
#' 	  possible names. Default is "UnitedStates/NYSE".
#' @param useYahoo do you want to download the NYSE calendar from Yahoo? It
#' 	  works only from days after 1950. Default is FALSE.
#' @return array of buisness days 
#' @examples
#' b <- bDays()
#' @export
bDays <- function(from="1980-01-01",to=Sys.Date(),by=1,calendar="UnitedStates/NYSE",useYahoo=FALSE){
	
	yahooStartDate = as.Date("1950-01-03")
	if(calendar=="UnitedStates/NYSE" &&
	   as.Date(to)>yahooStartDate && useYahoo){
	# Double check with yahoo data --- sometimes QuantLib misses some days
	# only works for data after 1950
	  freq.url <- substr("daily",1,1)
	  if(as.Date(from)>=yahooStartDate){
	    beg <- as.POSIXlt(from)
	  }else{
	    beg <- as.POSIXlt(yahooStartDate)
	  }
	  end <- as.POSIXlt(to)
    	  url <- paste( "http://ichart.finance.yahoo.com/table.csv?s=","^GSPC",
                  "&a=", beg$mon, "&b=", beg$mday, "&c=", beg$year+1900,
                  "&d=", end$mon, "&e=", end$mday, "&f=", end$year+1900,
                  "&g=", freq.url, "&ignore=.csv", sep="" )
    	  ohlc    <- read.table(url, header=TRUE, sep=",")
	  bdates0 <- sort(as.Date(as.character(ohlc[,1])))
	  if(as.Date(from)>=yahooStartDate){
		bdates <- bdates0
	  }else{
            		  
	    if(!require(RQuantLib)) stop("Install RQuantLib")
	    dates   = seq(as.Date(from),yahooStartDate-1,by=by)
	    bdates1 = dates[isBusinessDay(dates,calendar=calendar)]
	    bdates  = sort(union(as.character(bdates1),as.character(bdates0)))	    
	  }
	  if(by!=1){
	  	seqq <- seq(1,length(bdates),by=by)
	  	bdates <- bdates[seqq]
	  }
	  return(bdates)
	}
	if(!require(RQuantLib)) stop("Install RQuantLib")
	dates = seq(as.Date(from),as.Date(to),by=by)
	isB <- isBusinessDay(dates,calendar=calendar)
	if(calendar=="UnitedStates/NYSE"){
		# Manual fix of known problems after 1980: need to rm 3 days if NYSE
		rmDays <- c("1985-09-27","2012-10-29","2012-10-30")
		rmIt<- rmDays[!is.na(isB[rmDays])]
		if(length(rmIt)>0) isB[rmIt] <- FALSE
	}
	bdates = dates[isB]
	return(bdates)
}
#' Resample a xts object into a matrix every N time periods.
#'
#' We can take a time series and separate the time series into observation every
#' N periods. For example if we have daily data and want to have data every 5
#' days, we can use this function.  This function will return (in the case of 5
#' periods)  xts matrix of 5 cols, one for data from 1 and 5, other from 2 to 6
#' and so forth. If one wants to reseample weekly or monthly one can use
#' xts build in functions such as to.weekly(x,OHLC=F), or apply.weely(x,last). 
#' @param R input xts time series.
#' @param period the number of periods to split the data. Default is 5.
#' @param na.rm if true we backfill with previus values and rm any NA that comes
#' from merging the xts time series together. Default = FALSE
#' @return xts matrix with resampled time series
#' @examples 
#' resamplePeriodically(xts(1:100,Sys.Date()+1:100))
#' @export
resamplePeriodically <- function(R,period=5,na.rm=FALSE){
  
  if(!is.xts(R)) stop('Input needs to be xts')
  n <- dim(R)[1]
  for(i in 1:period){
    p<-seq(i,n,by=period)
    if(i==1) pr <- R[p,]
    if(i!=1) pr <- merge(pr,R[p,])
  }
  # Notice that data have dim smaller because na.omit
  if(na.rm) return(na.omit(na.locf(pr)))
  # No fill: NA on dates we do not have data
  return(pr)
}
#' Simple Moving average faster version
#'
#' Uses RcppRoll function to implement MA. 
#' Basically MA(2) = x(1)+x(2)/2; MA(3) = x(3)+x(2)/2 for window = 2
#' @param x matrix or xts
#' @param n lookback window to average over
#' @return matrix with MA calculated.
#'         The first n values are set to NA.
#' @examples
#' ma(xts(1:10,Sys.Date()+1:10),n=2)
#' @export  
ma <- function(x,n){
   
    N = c(0,0)   	
    if(is.matrix(x) || is.xts(x) || is.data.frame(x)){		
    	N  = dim(x)
    }else{
	if(is.numeric(x)){
		N[1] = length(x)
		N[2] = 1
	}else{
		stop("Not supported data type")
	}
    }
    ma = RcppRoll::roll_mean(as.matrix(x),n=n) # from RcppRoll
    M  = N[1]-n+1
    fill = matrix(NA,nrow =(N[1]-M),ncol=N[2])
    if (!is.xts(x)){
	    return(rbind(fill,ma))
    }
    xx = rbind(fill,ma)
    return(xts(xx,order.by=index(x)))    
}
#' Resample a xts object into at the end of the month(EOM)
#'
#' This function should have basically the same functionality to xts to.monthly with OHLC=FALSE
#' We have found occasional errors with xts::to.monthly most probably due to the date time class. 
#' Therefore this code uses lubridate instead and works only for data without time info. We include the last
#' point of the time series even if it is not the end of the month... 
#' @param R input xts time series.
#' @return xts matrix with resampled time series
#' @examples 
#' sampleEOM(xts(1:100,Sys.Date()+1:100))
#' @export
sampleEOM <- function(R){
  dates = index(R)
  eom = abs(c(diff(lubridate::month(dates)),1))>0
  return(R[eom,])
}
#' Resample a xts object into weekday
#'
#' This function is approx xts::to.weekly but makes sure to use only mondays, ....
#' @param R input xts time series.
#' @param weekday weekday to be as a number form 1:sunday to 7
#' @return xts matrix with resampled time series
#' @examples 
#' sampleWeekday(xts(1:100,Sys.Date()+1:100))
#' @export
sampleWeekday <- function(R,weekday=2){
  dates = index(R)
  whatDay = lubridate::wday(dates)==weekday
  return(R[whatDay,])
}

