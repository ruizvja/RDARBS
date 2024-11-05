#' Plot data from a changing-criterion design
#'
#' Plot data through consecutive days or sessions, indicating which was the current criteria in each phase and when each phase change occurred. 
#' @param time vector that contains the number of day or session.
#' @param dimension vector that contains the values to be plotted for each day or session.
#' @param criteria vector with the names of criteria for each day or session.
#' @param phase vector with the labels of corresponding phase for each day or session.
#' @param symbol symbol to use for plotting data. By default \code{pch=18},but it can be changed by the user.  
#' @param type character that indicates to plot both, points and line. By default, \code{type="o"} (overplotted), but it can be changed for \code{type="b"} (with breaks between each point and the line). 
#' @param xlab label for x axis.
#' @param ylab label for y axis
#' @param legend label to identify the subject.
#' @param xdiv value to indicate the number of intervals into which the x-axis is divided. By default, value 0 results in dividing the axis into n-1 intervals, where n is the total number of sessions.
#' @param ylim numeric value, giving the y-axis range. By default, \code{ylim=NULL} sets the limit in the next ten with respect to the maximum in \code{dimension} variable. 
#' @return Returns a graphic that shows the registered value for each day, indicating the current criteria in each phase, as well as the change from one phase to another.
#' @author omitted for blind review
#' @examples
#' plot_CC(changing$Session,changing$Measure,changing$Criteria,changing$Phase,xdiv=6,ylim=NULL)
#'  
#' @export
#' 


plot_CC<-function(time,dimension,criteria,phase,symbol=18,type="o",xlab="x",ylab="y",legend="Subject",xdiv=0,ylim=NULL){
  if(is.null(ylim)){
    next_ten <-  10*ceiling(max(dimension/10))    
  } else if(!is.null(ylim)){
    next_ten <-ylim }
  

  crit<-unique(criteria)
  phases<-unique(phase)
  sessions<-as.data.frame(table(phase))
  colnames(sessions)<-c("ph","Freq")
  ses.p.phase<-NA;j<-1
  for (i in phases){
    ses.p.phase[j]<-sessions$Freq[sessions$ph==i];j<-j+1
  }
  inis<-1
  for(i in 1:(length(ses.p.phase)-1)){
    inis[i+1]<-inis[i]+ses.p.phase[i]
  }
  mid<-ses.p.phase[1]/2;endis<-ses.p.phase[1]
  for (i in 2:length(ses.p.phase)){
    mid[i]<-endis[i-1]+(ses.p.phase[i]/2); endis[i]<-ses.p.phase[i]+endis[i-1]
  }
  
  if(length(ses.p.phase)>2){
  v.lines<-ses.p.phase[1]+.5
  for (i in 2:(length(ses.p.phase)-1)){
    v.lines[i]<-v.lines[i-1]+ses.p.phase[i]
  } }
  if(length(ses.p.phase)==2){
    
    v.lines<-ses.p.phase[1]+.5
  } 
  if(length(ses.p.phase)==1){
    
    v.lines<-NA
  }
  
  #x11()
  
  par(xaxs='i',yaxs='i',pin=c(3,2))
  plot(1,pch=symbol,xlim=c(0,max(time)+1),ylim=c(0,next_ten),
       xlab=xlab,ylab = ylab,type="n", axes=F,bty="n")  
  for(i in phases){
    points(time[phase==i],dimension[phase==i],pch=symbol,type=type)}
  if(xdiv==0){  axis(1,xaxp=c(0,max(time),length(time)),cex=0.8)}else if(xdiv!=0){
    axis(1,xaxp=c(0,max(time),xdiv),cex=0.8) 
  }
  
 # if(next_ten>50){
  #  axis(2,at=10*(0:next_ten),gap.axis = 1/4) 
  #} else if (next_ten<=50){
   # axis(2,at=5*(0:next_ten),gap.axis = 1/4) }
  
 axis(2,at=seq(0,next_ten,length.out=6)) 
#  axis(2,at=seq(0,next_ten,by=next_ten/5))
 # axis(2)#,xaxp=c(min(time),max(time),length(time)-1),cex=0.8)
  abline(v=v.lines, col="black", lwd=1, lty=1)
  segments(inis,crit,endis, col="black", lwd=1.5, lty=2)
  #text(x=mid,y=max(dimension)+(max(dimension)/10),labels=phases, adj=0)
  mtext(phases,side=3,line = 0,at=mid)
   mtext(legend,side=3,line=1,adj=1)
}
