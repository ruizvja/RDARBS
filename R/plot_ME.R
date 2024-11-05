#' Plot data from a multielement design
#'
#' Plot data through consecutive days or sessions, indicating the label of each phase, the series for each alternating treatments, and when changes of conditions occurred. 
#' @param time vector that contains the number of day or session.
#' @param dimension vector that contains the values to be plotted for each day or session.
#' @param treatment vector with labels indicating the corresponding treatment for each day.
#' @param phase vector with the labels of corresponding phase for each day or session.
#' @param symbol symbol to use for plotting data. By default \code{pch=18},but it can be changed by the user.  
#' @param type character that indicates to plot both, points and line. By default, \code{type="o"} (overplotted), but it can be changed for \code{type="b"} (with breaks between each point and the line).
#' @param xlab label for x axis.
#' @param ylab label for y axis
#' @param legend label to identify the subject.
#' @param xdiv value to indicate the number of intervals into which the x-axis is divided. By default, value 0 results in dividing the axis into n-1 intervals, where n is the total number of sessions.
#' @param ylim numeric value, giving the y-axis range. By default, \code{ylim=NULL} sets the limit in the next ten with respect to the maximum in \code{dimension} variable. 
#' @return Returns a graphic that shows the registered value for each day, indicating the current phase and treatment, as well as the change from one phase to another.
#' @author omitted for blind review
#' @examples
#' plot_ME(multielement$Session,multielement$Measure,multielement$Treatment,multielement$Phase,xdiv=5,ylim=NULL)
#' 
#' 
#' @export
#'

plot_ME <-
function(time,dimension,treatment,phase,symbol=c(0,15,19),type="o",xlab="x",ylab="y",legend="Subject",xdiv=0,ylim=NULL){
  if(is.null(ylim)){
    next_ten <-  10*ceiling(max(dimension/10))    
  } else if(!is.null(ylim)){
    next_ten <-ylim }
  
 treatments<-unique(treatment)
  phases<-unique(phase)
  sessions<-as.data.frame(table(phase))
  colnames(sessions)<-c("ph","Freq")
  next_ten <-  10*ceiling(max(dimension/10))
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
  
  par(xaxs='i',yaxs='i',pin=c(3,2))
  plot(1,xlim=c(0,max(time)+1),ylim=c(0,next_ten),
       xlab=xlab,ylab = ylab,type="n", axes=F,bty="l")  
  for(i in phases){
    for(n in treatments){
     
    points(time[phase==i&treatment==n],dimension[phase==i&treatment==n],pch=symbol[which(n==treatments)],type=type)}}
  #  segments(inis,criteria,endis, col="black", lwd=1.5, lty=2)
  #  axis(1,at=c(1,endis),labels=c(1,endis))
  #axis(1,xaxp=c(min(time),max(time),length(time)-1),cex=0.8)
  if(xdiv==0){  axis(1,xaxp=c(0,max(time),length(time)),cex=0.8)}else if(xdiv!=0){
    axis(1,xaxp=c(0,max(time),xdiv),cex=0.8) 
  }
  
  axis(2,at=seq(0,next_ten,length.out=6)) 
#  axis(2,at=seq(0,next_ten,by=next_ten/5))
  abline(v=v.lines, col="black", lwd=1, lty=1)
  #segments(inis,crit,endis, col="black", lwd=1.5, lty=2)
  #text(x=mid,y=max(dimension)+5,labels=phases, adj=0.5)
  #text(x=max(time),y=min(dimension),legend,adj=1)
  mtext(phases,side=3,line = 0,at=mid)
  mtext(legend,side=3,line=1,adj=1)
  
  #legend("bottomleft", legend=treatments,horiz=T,bty="n",
  #       pch=symbol,xpd=T, inset=c(0,.85), lty=1:2, cex=0.8) 
}
