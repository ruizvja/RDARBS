#' Cumulative record
#'
#' Plot cumulative responses through consecutive seconds (or any other unit)  of a session. 
#' @param time vector that contains moments in which some type of event occurred.
#' @param events vector that contains the sequence of numeric values corresponding to registered events.
#' @param t_res number to indicate the resolution of time. By defult \code{t_res=1}, for a resolution of time in seconds.
#' @param code vector with values used to identify the correspondence between numbers and the type of events they refer to, for example: (1) responses and (2) reinforcers. By default \code{code=c(1,2)}.
#' @param ymax value to establish the maximum in Y axis.  
#' @param xlab label for x axis.
#' @param ylab label for y axis
#' @return Returns a graphic that shows cumulative responses over time of observation.
#' @author omitted for blind review
#' @examples
#' cumRecord(operants$time,operants$events,t_res=.2,code=c(1,7),ymax=100,xlab="Time in seconds",ylab="Cumulative responses")
#'  
#' @export
#' 




cumRecord <-
function(time,events,t_res=1,code=c(1,2),ymax=100,xlab="Time in seconds", ylab = "Cumulative responses"){
  
  R<-NA;SR<-NA
  
  for (i in 1:length(events)){
    if (events[i]==code[1]){R[i]<-1}else{R[i]<-0}
    if (events[i]==code[2]){SR[i]<-1}else{SR[i]<-NA}
  }

  R.v2<-R[1];suma<-R[1]
  for (i in 2:length(R)){
    suma[i]<- R[i]+R.v2[i-1]; R.v2[i]<-suma[i];if (suma[i]==(ymax+1)) {
      R.v2[i]=R[i]
    } 
  }
  
  plot(time*t_res,R.v2,type="l",ylim = c(0,ymax),xlim=c(0,max(time*t_res)),
       xlab=xlab,frame.plot = T, ylab = ylab)
  
  SR_marks<-(SR*R.v2)
  segments(time*t_res,SR_marks,(time*t_res)+3,SR_marks-2, lwd=2, col="black")
  
}
