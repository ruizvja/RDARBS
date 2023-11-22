#' Behavior categories in continuous time
#'
#' Plot the occurrence of behavior categories through consecutive time (in any unit) of a period of observation. 
#' @param categories vector that contains the sequence of numeric or character values corresponding to registered categories.
#' @param start vector with values to indicate time of beginning of each category.
#' @param end vector with values to indicate the time in which each category ends.
#' @param lev vector with values used to indicate an order of to indicate the order of appearance of the categories on the Y axis. By default, this argument is NULL, so the categories are placed in ascending order.
#' @param xlab label for x axis.
#' @param ylab label for y axis.
#' @param title text to show as title of plot.
#' @param col value or vector to indicate color for categories in the plot. By default \code{col=I("black")}.
#' @return Returns a graphic that shows categories of behavior over time of observation.
#' @author omitted for blind review
#' @examples
#' plot_categories(categories$Serie,categories$Inicio,categories$Final) 
#' 
#' plot_categories(categories$Serie,categories$Inicio,categories$Final,lev=c("O","e","E","h","L","s"),col=c(categories$Serie,categories$Serie))
#'  
#' @export
#' 




plot_categories <-
function(categories,start,end,lev=NULL,xlab="Time of observation",
                     ylab=NULL,title="title",col=I("black")){

if(!is.null(lev)){
  events<-factor(c(categories,categories),levels=lev)} else{
    events<-c(categories,categories)
  }

len.series<-length(categories)

id<-rep(c(1:len.series),2)

cat.TS<-data.frame(id,events,c(start,end))
colnames(cat.TS)<-c("id","events","time")

categorical.TS<- ggplot(cat.TS, aes(time, events,color=col, group=id)) +
  geom_line(size = 10) +
  labs(x=xlab, y=ylab, title=title)
categorical.TS 

}
