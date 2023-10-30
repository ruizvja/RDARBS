#' Plot routes per cycle
#'
#' Plot the route correspondiente a cada uno de los ciclos en los que se divide el tiempo de registro. 
#' @param x vector that contains coordinates in x axis.
#' @param y vector that contains coordinates in y axis.
#' @param n.cycles value used to indicate the number of cycles.
#' @param len.cycles value used to indicate the length of each cycle.
#' @param xlim vector with min and max values for x axis.
#' @param ylim vector with min and max values for y axis.
#' @param xlab label for x axis.
#' @param ylab label for y axis.
#' @param roi vector with values to indicate coordinates of a region of interest.
#' @param r value to indicate the radius of ROI.
#' @return Returns a graphic that shows categories of behavior over time of observation.
#' @author Jorge A. Ruiz <ruizvja@yahoo.com>
#' @examples
#' plot_categories(categories$Serie,categories$Inicio,categories$Final) 
#' 
#' plot_categories(categories$Serie,categories$Inicio,categories$Final,lev=c("O","e","E","h","L","s"),col=c(categories$Serie,categories$Serie))
#'  
#' @export
#' 

plot_dir <-
function(x,y,n.cycles,len.cycles,xlim,ylim,xlab="X",ylab="Y",
                   roi=c(xlim[2]/2,ylim[2]/2),r=ylim[2]/10){
  
  
  fin<- seq(len.cycles,(len.cycles*n.cycles),by=len.cycles) 
  ini<-fin-(len.cycles-1)
  
#x11()
#par(mfrow=c(layout[1],layout[2]))
  par(pty="s")
  
for(i in 1:n.cycles){
j<-ini[i]
k<-fin[i]

plot(x[j:k],y[j:k],type="l",xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,
     main =paste0("Ciclo ",i))
points(x[j],y[j],pch=19)
points(x[k],y[k],pch=25)
circles(roi[1],roi[2],r=r)

    }
      }
