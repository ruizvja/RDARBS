#' Plot data from a reversal design
#'
#' Plot data through consecutive days or sessions, indicating the label of each phase and when changes of conditions occurred. 
#' @param time vector that contains the number of day or session.
#' @param dimension vector that contains the values to be plotted for each day or session.
#' @param phase vector with the labels of corresponding phase for each day or session.
#' @param symbol symbol to use for plotting data. By default \code{pch=18},but it can be changed by the user.  
#' @param type character that indicates to plot both, points and line. By default, \code{type="o"} (overplotted), but it can be changed for \code{type="b"} (with breaks between each point and the line).
#' @param xlab label for x axis.
#' @param ylab label for y axis
#' @param legend label to identify the subject.
#' @return Returns a graphic that shows the registered value for each day, indicating the current phase, as well as the change from one phase to another.
#' @author omitted for blind review
#' @examples
#' plot_Reversal(reversal$Session,reversal$Measure,reversal$Phase)
#' 
#' 
#' @export
#'

plot_Reversal <-
function(time,dimension,phase,symbol=18,type="o",xlab="x",ylab="y",legend="Subject"){
  phases<-unique(phase)
  sessions<-as.data.frame(table(phase))
  colnames(sessions)<-c("ph","Freq")
  ses.p.phase<-NA;j<-1
  for (i in phases){
    ses.p.phase[j]<-sessions$Freq[sessions$ph==i];j<-j+1
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
  

  plot(1,pch=symbol,xlim=c(1,max(time)),ylim=c(0,(max(dimension)+(max(dimension)/5))),
       xlab=xlab,ylab = ylab,type="n", xaxt = 'n',bty="l")  
  for(i in phases){
  points(time[phase==i],dimension[phase==i],pch=symbol,type=type)}

 axis(1,xaxp=c(min(time),max(time),length(time)-1))
   abline(v=v.lines, col="black", lwd=1, lty=4)
  text(x=mid,y=max(dimension)+(max(dimension)/10),labels=phases, adj=0)
  mtext(legend,side=3,adj=1)
  }
