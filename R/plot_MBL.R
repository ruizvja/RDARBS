#' Plot data from a multiple baseline design
#'
#' Plot data through consecutive days or sessions, indicating the label of each phase, the case to whom each registered data belongs, and when changes of conditions occurred for each case. 
#' @param time vector that contains the number of day or session.
#' @param dimension vector that contains the values to be plotted for each day or session.
#' @param case vector with labels indicating each case.
#' @param phase vector with the labels of corresponding phase for each day or session.
#' @param symbol symbol to use for plotting data. By default \code{pch=11},but it can be changed by the user.  
#' @param type character that indicates to plot both, points and line. By default, \code{type="o"} (overplotted), but it can be changed for \code{type="b"} (with breaks between each point and the line).
#' @param xlab label for x axis.
#' @param ylab label for y axis
#' @return Returns a graphic that shows the registered value for each day, indicating the current phase, as well as the change from one phase to another for each case.
#' @author omitted for blind review
#' @examples
#' plot_MBL(mbl$session,mbl$measure,mbl$case,mbl$phase) 
#' 
#' 
#' @export
#'

plot_MBL <-
function(time,dimension,case,phase,symbol=11,type="o",xlab="x",ylab="y"){
  
cases<-unique(case)

par(mfrow=c(length(cases),1),mai = c(0.1, .1, .1, .1),mar = c(3, 4, 1.5, 1.5))

for(n in cases){
    
    phases<-unique(phase[case==n])
    sessions<-as.data.frame(table(phase[case==n]))
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
    

    plot(1,xlim=c(1,max(time[case==n])),ylim=c(0,(max(dimension[case==n])+(max(dimension[case==n])/10))),
         xlab="",ylab = ylab,type="n", xaxt = 'n',bty="l")  
    for(i in phases){
      points(time[phase==i&case==n],dimension[phase==i&case==n],pch=symbol,type=type)}
    
    abline(v=v.lines, col="black", lwd=1, lty=4)
    text(x=mid,y=max(dimension)+(max(dimension[case==n])/20),labels=phases, adj=0)
    mtext(n,side=3,adj=1)
   
    if(n==tail(cases,1)){ 
    title(xlab=xlab)
    axis(1,xaxp=c(min(time[case==n]),max(time[case==n]),length(time[case==n])-1))}
     
  }}
