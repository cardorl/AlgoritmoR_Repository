mu.til.f<-function(alfa.til,beta.til,mu.x.til){
  result<-c(alfa.til+(beta.til*mu.x.til),mu.x.til);
  result<-matrix(data=result,ncol=1);
  return (result); 
  
}