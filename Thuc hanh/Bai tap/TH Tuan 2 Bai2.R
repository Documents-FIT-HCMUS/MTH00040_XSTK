bai2<-function(){
radius= c(3:20)
volume=c()
for(i in seq(3:20))
volume[i]=4*pi*radius[i] ^ 3 / 3
df=data.frame('Radius' = radius, 'Volume'=volume)
df
}
