add3 <- function(x,y){
  x+y
}
abv10<-function(x){
  x[x>10]
}
columnmean<-function(x)
{ vec<-numeric(ncol(x))
for(i in seq_len(ncol(x)))
{
  suma<-0
  for (j in seq_len(nrow(x)))
  {
    suma<-suma+x[j,i]
    
  }
  vec[i]<-suma/nrow(x)
}
vec
}
cmean<-function(x)
{
  vc<-numeric(ncol(x))
  for (i in 1:ncol(x)){
    vc[i]<-mean(x[,i],na.rm = T)
  }
  
  vc
  
}