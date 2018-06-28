binwidth = 0.005
theta = seq(from = binwidth/2, to =1-(binwidth/2), by = binwidth)
a = 5
b = 2
ptheta = dbeta(theta, a, b)
data=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
y=sum(data==1)
N=length(data)
pdatagiventheta=theta^y*(1-theta)^(N-y)
pthetagivendata = dbeta(theta, a+y, N+b-y)
windows(10,10)
layout(matrix(c(1,2,3), nrow = 3, ncol = 1, byrow = FALSE))
maxy=max(c(ptheta, pthetagivendata))
plot(theta, ptheta, type = "l", lwd = 3, main="Prior")
plot(theta, pdatagiventheta, type = "l", lwd = 3, main="Likelihood")
plot(theta, pthetagivendata, type = "l", lwd = 3, main="Posterior")