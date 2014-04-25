## R functions to clean up daylight savings time changes in ibutton source data
## Dylan Schwilk
##  2011-06-06

# find_time_change finds the first point at which the times are out of ascending order.
# this indicates the tme change point if the record interval is less than 1
# hour. If the record interval is longer than 1 hour, then this function
# returns a vector of TRUEs and the time change point is determiend entirely by
# the hard-coded start and end points of daylight time.
find.time.change<- function(x) {
  n <- length(x)
  ii <- index(x)
  for(i in ii) {
    if(i+1 > n){
      break
    }
    if(x[i+1] < x[i]){
      return(ii < i+1)
    }
  }
  return(rep(TRUE,n))
}



find.time.change2 <- function(dt) {
  n <-  length(dt)
  return(dt[2:n]-dt[1:n-1])
  
}
  
  

dst.2010 <- function(dt) {
  start <-  strptime("03/14/2010 02:00:00 AM", "%m/%d/%Y %I:%M:%S %p", tz="CST")
  # not correct, but what the windows machine did! :
  end <- strptime("10/31/2010 02:00:00 AM", "%m/%d/%Y %I:%M:%S %p",tz="CST")
  dst <- dt > start & dt < end
  before_change <- find.time.change(dt)
  return(dst & before_change)
#  index(thez)[dst & !dupes] <-  index(thez)[dst & !dupes] - hour1
}

dst.2011 <- function(dt) {
  start <-  strptime("03/13/2011 02:00:00 AM", "%m/%d/%Y %I:%M:%S %p", tz="CST")
  end <- strptime("11/06/2011 02:00:00 AM", "%m/%d/%Y %I:%M:%S %p",tz="CST")
  dst <- dt > start & dt < end
  before_change <- find.time.change(dt)
  return(dst & before_change)
#  index(thez)[dst & !dupes] <-  index(thez)[dst & !dupes] - hour1
}

filter_dst <- function(dt) {
  return(dst.2010(dt))
#  return(dst.2010(dt) | dst.2011(dt) )
}
