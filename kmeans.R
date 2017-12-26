
ATs=VCAT_ALL_mod[VCAT_ALL_mod$AXLE_GROUP==1,]

ATs=VCAT_ALL_mod

unimod=((SA$pl_hat<.15 | SA$pl_hat>.85) |
          (abs((SA$m1_hat-SA$m2_hat)/SA$m1_hat)<.3))

sum(unimod,na.rm = T)/length(unimod[!is.na(unimod)])


library(NbClust)
ALLm=ATs[!is.na(ATs$pl_hat),c("pl_hat","m1_hat","s1_hat","m2_hat","s2_hat")]
df <- scale(ALLm)  
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
table(nc$Best.n[1,])

barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")


fit.km <- kmeans(df, 3, nstart=25)
fit.km$size
fit.km$centers

aggregate(ALLm, by=list(cluster=fit.km$cluster), mean)



mln=function(x,p,m1,s1,m2,s2) {
  lm1=log(m1/sqrt(1+s1/m1^2))
  lm2=log(m2/sqrt(1+s2/m2^2))
  ls1=sqrt(log(1+s1/m1^2))
  ls2=sqrt(log(1+s2/m2^2))
  return(p*dlnorm(x,lm1,ls1)+(1-p)*dlnorm(x,lm2,ls2))}


x=seq(0,80,length.out = 500)
mco=aggregate(ALLm, by=list(cluster=fit.km$cluster), mean)[-1]
TQmco=mco
mco=TAmco
yln=mln(x,mco[1,1],mco[1,2],mco[1,3],mco[1,4],mco[1,5]);plot(x,yln,col='black',type='l',ylim=c(0,.12))
yln=mln(x,mco[2,1],mco[2,2],mco[2,3],mco[2,4],mco[2,5]);lines(x,yln,col='blue')
yln=mln(x,mco[3,1],mco[3,2],mco[3,3],mco[3,4],mco[3,5]);lines(x,yln,col='red')

yln=mln(x,mco[4,1],mco[4,2],mco[4,3],mco[4,4],mco[4,5]);lines(x,yln,col='magenta')
yln=mln(x,mco[5,1],mco[5,2],mco[5,3],mco[5,4],mco[5,5]);lines(x,yln,col='cyan')

yln=mln(x,mco[6,1],mco[6,2],mco[6,3],mco[6,4],mco[6,5]);lines(x,yln,col='green')
yln=mln(x,mco[7,1],mco[7,2],mco[7,3],mco[7,4],mco[7,5]);lines(x,yln,col='darkblue')

yln=mln(x,mco[8,1],mco[8,2],mco[8,3],mco[8,4],mco[8,5]);lines(x,yln,col='darkgreen')
yln=mln(x,mco[9,1],mco[9,2],mco[9,3],mco[9,4],mco[9,5]);lines(x,yln,col='darkblue')



x=seq(0,40,length.out = 500)
mco=SAmco
yln=mln(x,mco[1,1],mco[1,2],mco[1,3],mco[1,4],mco[1,5]);plot(x,yln,col='black',type='l',ylim=c(0,.12))
yln=mln(x,mco[2,1],mco[2,2],mco[2,3],mco[2,4],mco[2,5]);lines(x,yln,col='blue')
yln=mln(x,mco[3,1],mco[3,2],mco[3,3],mco[3,4],mco[3,5]);lines(x,yln,col='red')


x=seq(0,80,length.out = 500)
mco=TAmco
yln=mln(x,mco[1,1],mco[1,2],mco[1,3],mco[1,4],mco[1,5]);plot(x,yln,col='black',type='l',ylim=c(0,.12))
yln=mln(x,mco[2,1],mco[2,2],mco[2,3],mco[2,4],mco[2,5]);lines(x,yln,col='blue')
yln=mln(x,mco[3,1],mco[3,2],mco[3,3],mco[3,4],mco[3,5]);lines(x,yln,col='red')

x=seq(0,140,length.out = 500)
mco=TQmco
yln=mln(x,mco[1,1],mco[1,2],mco[1,3],mco[1,4],mco[1,5]);plot(x,yln,col='black',type='l',ylim=c(0,.12))
yln=mln(x,mco[2,1],mco[2,2],mco[2,3],mco[2,4],mco[2,5]);lines(x,yln,col='blue')
yln=mln(x,mco[3,1],mco[3,2],mco[3,3],mco[3,4],mco[3,5]);lines(x,yln,col='red')


