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
#' @param ylab label for y axis.
#' @param xdiv value to indicate the number of intervals into which the x-axis is divided. By default, value 0 results in dividing the axis into n-1 intervals, where n is the total number of sessions.
#' @param ylim numeric value, giving the y-axis range. By default, \code{ylim=NULL} sets the limit in the next ten with respect to the maximum in \code{dimension} variable. 
#' @return Returns a graphic that shows the registered value for each day, indicating the current phase, as well as the change from one phase to another for each case.
#' @author omitted for blind review
#' @examples
#' plot_MBL(mbl$session,mbl$measure,mbl$case,mbl$phase,xdiv=5,ylim=NULL) 
#' 
#' 
#' @export
#'


plot_MBL <-
  function(time,dimension,case,phase,symbol=11,type="o",xlab="x",ylab="Y-axis",xdiv=0,ylim=NULL){
    require(grid)  
    if(is.null(ylim)){
      next_ten <-  10*ceiling(max(dimension/10))    
    } else if(!is.null(ylim)){
      next_ten <-ylim }
    
    cases<-unique(case)
    #x11()
    #dev.new(width = 4, height = 7, noRStudioGD = T)
    #par(mfrow=c(length(cases),1),omi=c(.2,2,.116667,1.616667))#,mai = c(0.1, .1, .1, .1),mar = c(4, 4, 2, 2))
    lay.mat<-matrix(c(1:length(cases)),nrow=length(cases),ncol=1,byrow = T)
    layout(mat=lay.mat,
           heights = rep(2,length(cases)),
           widths = rep(3,length(cases)))
    layout.show(length(cases))
    
    v.mark<-NA;counter<-0
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
      par(xaxs='i',yaxs='i',mai = c( 0.45, 2.108334, 0.055556,2.108334))#,pin=c(3.3,2.2))#,mai = c(.1, .4, .1, .1))
      
      plot(1,xlim=c(0,max(time[case==n])+1),ylim=c(0,next_ten),
           xlab="",ylab = "",type="n", axes=F,bty="l")  
      for(i in phases){
        points(time[phase==i&case==n],dimension[phase==i&case==n],pch=symbol,type=type)}
      
      #abline(v=v.lines, col="black", lwd=1, lty=4)
      counter<-counter+1
      v.mark[counter]<-v.lines
      mtext(n,side=3,adj=1,line=-1)
      
      if(n==head(cases,1)){text(x=mid,y=max(dimension)-1,labels=phases, adj=0.5)}
      
      if(n!=tail(cases,1)){ 
        #axis(1,labels=F)
        if(xdiv==0){  axis(1,xaxp=c(0,max(time),length(time)),cex=0.8,labels = F)}else if(xdiv!=0){
          axis(1,xaxp=c(0,max(time),xdiv),cex=0.8,labels = F) 
        }
        axis(2,at=seq(0,next_ten,length.out=6))}
      if(n==tail(cases,1)){ 
        title(xlab=xlab,line=2)
        if(xdiv==0){  axis(1,xaxp=c(0,max(time),length(time)),cex=0.8)}else if(xdiv!=0){
          axis(1,xaxp=c(0,max(time),xdiv),cex=0.8) 
        }
        axis(2,at=seq(0,next_ten,length.out=6))
       # mtext(xlab,side=1,line = 3)
        #axis(1,xaxp=c(min(time[case==n]),max(time[case==n]),length(time[case==n])-1))
      }
      
    }
    
    (X <- grconvertX(-4, "user", "ndc"))
    
    pushViewport(viewport())
    
    grid.text(label=ylab, x = X, y = 0.5,
              just = "centre", hjust = NULL, vjust = NULL, rot = 90,
              check.overlap = FALSE, default.units = "npc",
              name = NULL, gp = gpar(), draw = TRUE, vp = NULL)
    
    (Y0 <- grconvertY(0, "user", "ndc"))
    (Y1 <- grconvertY(33, "user", "ndc"))
    
    (X0 <- grconvertX(0, "user", "ndc"))
    (X1 <- grconvertX(v.mark[1], "user", "ndc"))
    (X2 <- grconvertX(v.mark[2], "user", "ndc"))
    (X3 <- grconvertX(v.mark[3], "user", "ndc"))
    
    library(grid)
    pushViewport(viewport())
    # grid.lines(x = c(X0,X1), y = (.33)*3, gp = gpar(col = "red"))
    grid.lines(x = c(X1), y = c((.33*2),(.33*3)), gp = gpar(col = "black"))
    grid.lines(x = c(X1,X2), y = .33*2, gp = gpar(col = "black"))
    grid.lines(x = X2, y = c((.33),(.33*2)), gp = gpar(col = "black"))
    grid.lines(x = c(X2,X3), y = c(.33), gp = gpar(col = "black"))
    grid.lines(x = X3, y = c((Y0),(.33)), gp = gpar(col = "black"))
    
    
    
  }
