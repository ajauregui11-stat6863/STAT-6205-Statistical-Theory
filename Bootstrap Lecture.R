comp_scores = c(60, 76,
                  26, 90, 81, 75, 95, 98, 38, 73,
                  90, 46, 41, 83, 100, 85, 76,
                  69, 91, 78)
mean(comp_scores)
qqnorm(comp_scores)
qqline(comp_scores)
shapiro.test(comp_scores)

N= length(comp_scores) #here it is 20
sample_mean = mean(comp_scores) #storing the sample mean
B = 1000 #number of bootstrap samples we are interested in
bss = sample(comp_scores, B*N, replace = T) #choosing the samples
bss_matrix = matrix(bss, nrow = B) #storing in a matrix
bs_mean=rowMeans(bss_matrix) #computing the bootstrap mean for each of the samples, the y*
hist(bs_mean)
bci = quantile(bs_mean, c(0.025, 0.975)) # computing a simple bootstrap confidence interval
bci

#Compare with a t-test
t.test(comp_scores) 

#Hypothesis testing#
#Suppose we want to test Ha: mu < 80
sample_sd = sd(comp_scores)
bs_sd = apply(bss_matrix, 1, sd) # to obtain the sd of each boot strap sample and store in bs_sd
bs_t = (bs_mean-sample_mean)/(bs_sd/sqrt(N)) #to compute the test statistics t for each bootstrap sample
t_actual = (sample_mean-80)/(sample_sd/sqrt(N))
t_value = mean(bs_t < t_actual)
t_value

#Compare with a p-value from a left-tailed t-test.
t.test(comp_scores, mu=80, alternative = "less")