#' Plot routes per cycle (or trial)
#'
#' Plot the routes corresponding to the several cycles or trials within the session time. 
#' @param x vector that contains coordinates in x axis, frame by frame.
#' @param y vector that contains coordinates in y axis, frame by frame.
#' @param cycle vector with values indicating the corresponding number of cycle or trial for each frame.
#' @param select value or vector with values indicating the number of cycle or trial to be plotted. By default, \code{select=0} produce a plot for each temporal segment.
#' @param xlim vector with min and max values for x axis.
#' @param ylim vector with min and max values for y axis.
#' @param xlab label for x axis.
#' @param ylab label for y axis.
#' @param roi vector with a pair of values to indicate coordinates of a region of interest (assuming a circular shape).
#' @param r value to indicate the radius of \code{roi}.
#' @return Returns a graphic that shows routes of movement through the experimental arena, by cycle or by trial.
#' @author omitted for blind review
#' @examples
#' library(interp)  #install interp package if needed
#' x11()
#' par(pty="s")
#' par(mfrow=c(3,3))
#' plot_dir(xy_test1$x,xy_test1$y,xy_test1$cycle,select=c(1,2,3,43,44,45,70,71,72),xlim=c(0,92),ylim=c(0,92),roi=c(0,46))
#' 
#'  
#' @export
#' 

plot_dir <-
function(x,y,cycle,select=0,xlim,ylim,xlab="X",ylab="Y",
                   roi=c(xlim[2]/2,ylim[2]/2),r=ylim[2]/10){
  require(interp)
  #x11()
#par(mfrow=c(layout[1],layout[2]))
  par(pty="s")
if(length(select)==1 & select[1]==0){
  cycles<-unique(cycle)  
} else if(length(select)>=1 & select[1]!=0 ){cycles<-select} 
  

for(i in cycles){
ex<-x[cycle==i]
ye<-y[cycle==i]
exN<-tail(na.omit(ex),1)
yeN<-tail(na.omit(ye),1)

plot(ex,ye,type="l",xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,
     main =paste0("Cycle ",i))
points(ex[1],ye[1],pch=19)
points(exN,yeN,pch=25)
circles(roi[1],roi[2],r=r)

    }
      }
