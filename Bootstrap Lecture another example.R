#another example
waittimes<-c(11.17,14.88,58.74,1.3,132.68,70.06,2.78,80.87,61.04,32.89,84.02,2.25,113.2,20.48,.28)
shapiro.test(waittimes)
bs_mean =rep(0, 10000)
bs_sd=rep(0, 10000)
sample_mean=mean(waittimes)
sample_sd=sd(waittimes)
n=length(waittimes)
bs_t = rep(0, 1000)
for(i in 1:1000)
{
  bs=sample(waittimes, 15, replace = T)
  bs_mean[i]=mean(bs)
  bs_sd[i]=sd(bs)
  bs_t[i]=(bs_mean[i]-sample_mean)/(bs_sd[i]/sqrt(n))
  mean_mean = sum(bs_mean[])/1000
}
t_actual = (sample_mean-50)/(sample_sd/sqrt(n))
pval1= mean(bs_t>t_actual)
pval2=mean(bs_t<t_actual)
pval = 2*(min(pval1, pval2))
pval

#Compare with a t-test
t.test(waittimes, mu=50)