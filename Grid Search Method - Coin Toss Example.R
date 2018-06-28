data=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0
       ,0,0)
N=40
z=25
theta = seq(from =0.4, to= 0.7, by = 0.01)
ptheta1 = dunif(theta, 0, 1) #not sure why this vector gives out all 1s
ptheta = ptheta1/sum(ptheta1)
nheads = sum(data ==1)
ntails = sum(data ==0)
pdatagiventheta = theta^nheads *(1-theta)^(ntails) #likelihood function
pdata=sum(pdatagiventheta *ptheta) #marginal pdf of discrete X
pthetagivendata = pdatagiventheta*ptheta/pdata #posterior distribution
par(mfrow = c(3, 1))
plot(theta, ptheta, type = "h", lwd = 3, main = "Prior")
plot(theta, pdatagiventheta, type = "h", lwd = 3, main = "likelihood")
plot(theta, pthetagivendata, type = "h", lwd = 3, main = "posterior")