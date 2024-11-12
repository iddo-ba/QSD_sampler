#Idetifying the decay of the extinction for the Subcritical Branching Process
library(ggplot2)

a = c(1/2, 1/4, 1/8, 1/16, 1/32, 1/64, 1/64) #Offspring Distirbution
sum(a)


b = a[2:length(a)]*c(1:(length(a)-1))
sum(b)
#Initial size of the population
Z_0 = 1


birth = sample(c(0:(length(a)-1)), size = 1, prob=a) #Birth per individual
#according to the distribution

#trying to came up with a functional recursive function  here that represents 
#the size of the genenations
Z_1 = sum(sample(c(0:(length(a)-1)), size = Z_0, replace = TRUE, prob=a))
Z_2 = sum(sample(c(0:(length(a)-1)), size = Z_1, replace = TRUE, prob=a))
Z_3 = sum(sample(c(0:(length(a)-1)), size = Z_2, replace = TRUE, prob=a))

#Size of the n-th generation according to the distritution a
size_gen = function(n) {
  birth = sample(c(0:(length(a)-1)), size = 1, prob=a) #
  Z_n = Z_0
  g = 0
  while(Z_n != 0 && g <= n) {
    Z_n = sum(sample(c(0:(length(a)-1)), size = Z_n, replace = TRUE, prob=a))
    g = g + 1
    return(Z_n)
  }
}

size_gen(4)


#time until extinction according to a
extinction = function() {
  
  birth = sample(c(0:(length(a)-1)), size = 1, prob=a) #
  Z_n = Z_0
  g = 0
  while(Z_n != 0) {
    Z_n = sum(sample(c(0:(length(a)-1)), size = Z_n, replace = TRUE, prob=a))
    g = g + 1
  }
  return(g)
}

T = 10^5 #number of trials
data = replicate(T, extinction()) #replicating it T times
df = data.frame(table(data))
df$Freq = df$Freq/T #rescaling

#The graphs
p <- ggplot(data = df, mapping = aes(x = data, y = Freq)) +
  geom_point()
p
p <- ggplot(data = df, mapping = aes(x = data, y = Freq)) +
  geom_point() + coord_trans(y ='log', x='log')
p

#lim P(tau > n) ~ C*mu^n
k = 70*log(df[70,2]) - log(mu)
exp(k)

L = length(df$data)
log_c = c((1:L)*log(df[(1:L),2]) - log(mu))
vec_c = exp(log_c)
plot(vec_c)

############## Trying Bin(n,p)
#Here we need E(X) = np < 1 to have a subcritical process, then p < 1/n.
#i.e., p = 1/(n+q), q>0
extinction = function(n,q) {
  Z_n = rbinom(Z_0,n, 1/(n+q))
  g = 1
  
  while(Z_n != 0) {
    Z_n = sum(rbinom(Z_n,n, 1/(n+q)))
    g = g + 1
  }
  return(g)
}



T = 10^7
data = replicate(T, extinction(10,0.1)) #replicating it T times
df = data.frame(table(data))
df$Freq = df$Freq/T #rescaling

#The graphs
p <- ggplot(data = df, mapping = aes(x = data, y = Freq)) +
  geom_point()
p
p <- ggplot(data = df, mapping = aes(x = data, y = Freq)) +
  geom_point() + coord_trans(y ='log', x='log')
p

#lim P(tau > n) ~ C*mu^n
k = 70*log(df[70,2]) - log(10/(10+0.1))
exp(k)
L = length(df$data)
log_c = c((1:L)*log(df[(1:L),2]) - log(10/(10+0.1)))
vec_c = exp(log_c)
vec_c
plot(vec_c)


#Trying a lower number of offspring and a low q
T = 10^7
data = replicate(T, extinction(5,0.1)) #replicating it T times
df = data.frame(table(data))
df$Freq = df$Freq/T #rescaling

#The graphs
p <- ggplot(data = df, mapping = aes(x = data, y = Freq)) +
  geom_point()
p
p <- ggplot(data = df, mapping = aes(x = data, y = Freq)) +
  geom_point() + coord_trans(y ='log', x='log')
p

#lim P(tau > n) ~ C*mu^n
L = length(df$data)
log_c = c((1:L)*log(df[(1:L),2]) - log(5/(5+0.1)))
vec_c = exp(log_c)
vec_c
plot(vec_c)