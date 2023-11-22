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
#' @return Returns a graphic that shows the registered value for each day, indicating the current phase and treatment, as well as the change from one phase to another.
#' @author omitted for blind review
#' @examples
#' plot_ME(multielement$Session,multielement$Measure,multielement$Treatment,multielement$Phase)
#' 
#' 
#' @export
#'

plot_ME <-
function(time,dimension,treatment,phase,symbol=c(0,15,19),type="o",xlab="x",ylab="y",legend="Subject"){
  treatments<-unique(treatment)
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
  

  plot(1,xlim=c(1,max(time)),ylim=c(0,(max(dimension)+(max(dimension)/5))),
       xlab=xlab,ylab = ylab,type="n", xaxt = 'n',bty="l")  
  for(i in phases){
    for(n in treatments){
     
    points(time[phase==i&treatment==n],dimension[phase==i&treatment==n],pch=symbol[which(n==treatments)],type=type)}}
  #  segments(inis,criteria,endis, col="black", lwd=1.5, lty=2)
  #  axis(1,at=c(1,endis),labels=c(1,endis))
  axis(1,xaxp=c(min(time),max(time),length(time)-1),cex=0.8)
  abline(v=v.lines, col="black", lwd=1, lty=4)
  #segments(inis,crit,endis, col="black", lwd=1.5, lty=2)
  text(x=mid,y=max(dimension)+(max(dimension)/10),labels=phases, adj=0)
  #text(x=max(time),y=min(dimension),legend,adj=1)
  mtext(legend,side=3,adj=1)
  
  legend("bottomleft", legend=treatments,horiz=T,bty="n",
         pch=symbol,xpd=T, inset=c(0,.85), lty=1:2, cex=0.8) 
}
