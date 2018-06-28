#beta random sample
set.seed(120)
betadata<-rbeta(18,2,5)
shapiro.test(betadata)
bs_mean =rep(0, 5000)
bs_sd=rep(0, 5000)
sample_mean=mean(betadata)
sample_sd=sd(betadata)
n=length(betadata)
bs_t = rep(0, 5000)
for(i in 1:1000)
{
  bs=sample(betadata, 18, replace = T)
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
t.test(betadata)