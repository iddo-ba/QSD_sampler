# Weierstranns
f=abs
phi_n  = function (x,n) 
 { 
   phi = function (x) (1-x^2)^n
   c=integrate (phi,-1,1)$value
   return(phi(x)/c)
}
approx = function(x)
{ 
  g1= function(x) f(x)-0.5*((1-x)*f(-1)+(1+x)*f(1))
  g2 = function(x) {ifelse(-0.25<=x & x<=0.25,g1(4*x),0)}
  my=function(y) phi_n(x/4-y,n)*g2(y)
  Rn = integrate (my,-0.5,0.5)$value
  Rn = Rn+0.5*((1-x)*f(-1)+(1+x)*f(1))
return(Rn)
}


