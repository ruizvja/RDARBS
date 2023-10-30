



closeness <-
function(x,y,n.cycles,len.cycles,x_roi,y_roi){
  d<-NA
  
  fin<- seq(len.cycles,(len.cycles*n.cycles),by=len.cycles) 
  ini<-fin-(len.cycles-1)
  
  for(i in 1:n.cycles){
    j<-ini[i]
    k<-fin[i]
    d[i]<- (((x[j:k]-x_roi)^2  + (y[j:k]-y_roi)^2)^0.5)}
  print(d)
}
