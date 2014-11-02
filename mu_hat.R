mu.hat.f<-function(alfa.hat,beta.hat,mu.x.hat){  
  result<-cbind(alfa.hat+(beta.hat*mu.x.hat),mu.x.hat);
  return (result);  
}