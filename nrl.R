min.RSS <- function(data, par) {
  pars=matrix(par,ncol = 5)
  K=nrow(pars)
  
  mixF=function(x,pl,m1,s1,m2,s2) {
    pl*plnorm(x,meanlog=log(m1/sqrt(1+s1/m1^2)),sdlog=sqrt(log(1+s1/m1^2)))+
      (1-pl)*plnorm(x,meanlog=log(m2/sqrt(1+s2/m2^2)),sdlog=sqrt(log(1+s2/m2^2)))}
  
  nals_un=unique(data$s_un)
  rss_s=rep(NA,length(nals_un))
  for (s in 1:length(nals_un)) {
    nals_s=data[data$s_un==nals_un[s],c("bins_h","cdf_obs")]
    rss_sk=rep(NA,K);
    for (k in 1:K) {
      rss_sk[k]=sum((nals_s$cdf_obs-mixF(nals_s$bins_h,pars[k,1],pars[k,2],pars[k,3],pars[k,4],pars[k,5]))^2)
    }
    rss_s[s]=min(rss_sk)
  }
  rss=sum(rss_s)
  return(RSS=rss)
}

pars_in=c(runif(3,.3,.7),runif(3,8,15),runif(3,8,15),runif(3,27,35),runif(3,20,25))

pars_l=c(rep(0,3),rep(-Inf,3),rep(0,3),rep(-Inf,3),rep(0,3))
pars_u=c(rep(1,3),rep(Inf,12))

result <- optim(par = pars_in, fn = min.RSS, data = nalsg,lower = pars_l,upper = pars_u,
                control = list(trace = 5), method = "L-BFGS-B")


po=c(0.252928,0.600592,1,15.1523,12.8596,5.92314,9.41973,13.1422,8.67204,31.9466,25.7971,32.0475,31.2264,29.4702,34.8326)
