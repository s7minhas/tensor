yX1_r1r<-function(y,X,tol=1e-5,i1=1)
{
  p1<-dim(X)[1] ; p2<-dim(X)[2] ; n<-dim(X)[3]

  Xm<-mat(X,3)
  B<-matrix( solve( t(Xm)%*%Xm + diag(p1*p2)/length(y) ) %*%t(Xm)%*%y,p1,p2)  
  #mean( ( y-Xm%*%c(B) )^2 )  
  sB<-svd(B)  
  a<-matrix(sB$u[,1]*sqrt(sB$d[1]),1,p1)  
  b<-matrix(sB$v[,1]*sqrt(sB$d[1]),1,p2) 

  ss0<-Inf
  aBx<-c( tprod(X,list(a,b))) ; ss1<-mean( (y- aBx)^2 )
  if(sum(b)!=0 ) 
  {
  while( (ss0-ss1)/(ss1+tol)  > tol & ss1>10*tol  )
  {  
    ss0<-ss1

    Xb<-t(amprod(X,b,2)[,1,] )
    a<- t(solve( t(Xb)%*%Xb ) %*% t(Xb)%*%y)
    
    aX<-t(amprod(X,a,1)[1,,] )
    b<- t(solve( t(aX)%*%aX ) %*% t(aX)%*%y)

    aBx<-c( tprod(X,list(a,b)))
    ss1<-mean( (y- aBx)^2 )          
  }
  }
  list(a=a,b=b,ss=ss1) 
}

####
yX_r1r <-function(y,X,tol=1e-5)
{
  p1<-dim(X)[1] ; p2<-dim(X)[2] ; n<-dim(X)[3]

  a<-matrix( lm( y~ -1+ apply(X,c(3,1),mean)  )$coef,1,p1) 
  b<-matrix (lm( y~ -1+ apply(X,c(3,2),mean)  )$coef,1,p2) 

  ss0<-Inf
  aBx<-c( tprod(X,list(a,b))) ; ss1<-mean( (y- aBx)^2 )
  if(sum(a*b)!=0 ) 
  {
  while( (ss0-ss1)/(ss1+tol)  > tol & ss1>10*tol  )
  {  
    ss0<-ss1
    Xb<-t(amprod(X,b,2)[,1,] )
    a<- t(solve( t(Xb)%*%Xb ) %*% t(Xb)%*%y) 
 
    aX<-t(amprod(X,a,1)[1,,] )
    b<- t(solve( t(aX)%*%aX ) %*% t(aX)%*%y)

    aBx<-c( tprod(X,list(a,b)))
    ss1<-mean( (y- aBx)^2 )          
  }
  }
  list(a=a,b=b,ss=ss1) 
}
#####

