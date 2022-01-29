sunspot.year
write.csv2(sunspot.year, file="data.csv", row.names=T)

n1=sample(1:289, 50, replace=FALSE)
n2=sample(1:289, 20, replace=FALSE)

N1=double(50)
N2=double(20)

for (i in 1:50)
{
N1[i]=sunspot.year[n1[i]]
}

for (i in 1:20)
{
N2[i]=sunspot.year[n2[i]]
}

N1=sort(N1)
N2=sort(N2)
SS=sort(sunspot.year)

d=ceiling((max(sunspot.year)-min(sunspot.year))/10)
A=integer(10)
D=double(10)
D[1]=d-0.5
for(i in 2:10)
{ D[i]=D[i-1]+d }

Y=double(10)
Y[1]=0
for (i in 2:10) {Y[i]=D[i-1]}

B=paste(Y,D,sep="-")

for(i in 1:289) { if (sunspot.year[i]<D[1]) {A[1]=A[1]+1}
else if (sunspot.year[i]<D[2]) {A[2]=A[2]+1}
else if (sunspot.year[i]<D[3]) {A[3]=A[3]+1}
else if (sunspot.year[i]<D[4]) {A[4]=A[4]+1}
else if (sunspot.year[i]<D[5]) {A[5]=A[5]+1}
else if (sunspot.year[i]<D[6]) {A[6]=A[6]+1}
else if (sunspot.year[i]<D[7]) {A[7]=A[7]+1}
else if (sunspot.year[i]<D[8]) {A[8]=A[8]+1}
else if (sunspot.year[i]<D[9]) {A[9]=A[9]+1}
else if (sunspot.year[i]<D[10]) {A[10]=A[10]+1} 
}

names(A)=B
A

#функция распределения
Fun <- function(arg1) {
n=double(1)
for (i in 1:289) 
{ if (sunspot.year[i]<=arg1) {n=n+1} }
return (n/289)}
Fn=ecdf(sunspot.year)

#плотность
Fun1 <- function(arg1) {
n=double(1)
if (arg1<D[1]) {n=A[1]/289}
else if (arg1<D[2]) {n=A[2]/289}
else if (arg1<D[3]) {n=A[3]/289}
else if (arg1<D[4]) {n=A[4]/289}
else if (arg1<D[5]) {n=A[5]/289}
else if (arg1<D[6]) {n=A[6]/289}
else if (arg1<D[7]) {n=A[7]/289}
else if (arg1<D[8]) {n=A[8]/289}
else if (arg1<D[9]) {n=A[9]/289}
else if (arg1<D[10]) {n=A[10]/289}
n=n/d
return (n) } 

split.screen(c(1,2))
screen(1)
plot(Fn, xlab="x", ylab="Fn(x)", main="Distribution Function", frame=FALSE)
screen(2)
hist(sunspot.year, breaks=10,freq=FALSE, main="Density Function", xlab="x", ylab="fn(x)", plot=TRUE, labels=FALSE)


#мат ожидание
M1=sum(N1)/50
M2=sum(N2)/20
M=sum(SS)/289

#смещенная дисперсия
d1=double(50)
d2=double(20)
d=double(289)
for (i in 1:50)
{
d1[i]=(N1[i]-M1)^2
}
for (i in 1:20)
{
d2[i]=(N2[i]-M2)^2
}
for (i in 1:289)
{
d[i]=(SS[i]-M)^2
}
D1=sum(d1)/50
D2=sum(d2)/20
D=sum(d)/289

#несмещенная дисперсия
D01=sum(d1)/49
D02=sum(d2)/19
D0=sum(d)/289

#отклонение
S1=sqrt(D1)
S2=sqrt(D2)
S=sqrt(D)
S01=sqrt(D01)
S02=sqrt(D02)
S0=sqrt(D0)

#асимметрия
Me1=(N1[25]+N1[26])/2
Me2=(N2[10]+N2[11])/2
Me=SS[145]
Sk1=(M1-Me1)/S1
Sk2=(M2-Me2)/S2
Sk=(M-Me)/S

#экцесс
s1=double(1)
s2=double(1)
s1=sum(d1^2)
s2=sum(d2^2)
s=double(1)
s=sum(d^2)
M41=s1/50
M42=s2/20
M4=s/289
G1=M41/(S01^4)
G2=M41/(S02^4)
G=M4/(S0^4)

#доверительная вероятность=0,9
#доверительный интервал для мат ожидания, alpha=0.9
z=1.645
E1=z*S/sqrt(50)
E1=paste(M1-E1, M1+E1, sep="; ")
E1
M
t=1.729
E2=t*S02/sqrt(20)
E2=paste(M2-E2, M2+E2, sep="; ")
E2
M

#доверительный интервал дисперсии, alpha=0.1
xl=33.93
xr=66.339
E11=(49)*D01/xr
E12=(49)*D01/xl
E10=paste(E11, E12, sep="; ")
E10
D0
xl=10.118
xr=30.144
E21=(19)*D02/xr
E22=(19)*D02/xl
E20=paste(E21, E22, sep="; ")
E20
D0

e=0.001
n=(z*S0/e)^2
e=0.0001
n=(z*S0/e)^2


A

X=integer(10)
X[1]=9.5
for (i in 2:10)
{ X[i]=X[i-1]+20 }
F=A
FX=F*X
x=sum(FX)/sum(F)
l=1/x

#теоретическое
P=double(10)
P[1]=1-exp(-l*(X[1]+10))
for (i in 2:9)
{ P[i]=exp(-l*(X[i]-10))-exp(-l*(X[i]+10)) }
P[10]=exp(-l*(X[10]+10))
PN=double(10)
for (i in 1:10)
{ PN[i]=P[i]*sum(F) }
dif=double(10)
for (i in 1:10)
{ dif[i]=PN[i]-F[i] }
xi=double(10)
for (i in 1:10)
{ xi[i]=dif[i]^2/PN[i] }
Xi=sum(xi)
Xi
#alpha=0.05, df=8
XI=16.91898
if (Xi<XI) {print("exp")} else {print("not exp")}

N1
f<- function(y) {return(1-exp(-y/x))}
f(N1)
fn=double(50)
for (i in 1:50)
{ fn[i]=i/50 }
fn
D=abs(fn-f(N1))
D
la=max(D)*sqrt(50)
la
laa=1.358
if (la<laa) {print("exp")} else {print("not exp")}


#Колмогоров-Смирнов
N1
N2
fn <- function (x) {k=double(1); for (i in 1:50) {if (x>=N1[i]) {k=k+1/50}}; return(k)}
gn <- function (x) {k=double(1); for (i in 1:20) {if (x>=N2[i]) {k=k+1/20}}; return(k)}

Fn=double(20)
for (i in 1:20)
{
Fn[i]=i/20-fn(N2[i])
}
Fn

Gn=double(50)
for (i in 1:50)
{
Gn[i]=i/50-gn(N1[i])
}
Gn

D1=max(Fn)
D2=max(Gn)
D=max(D1,D2)
la=1.358
if (D<la) {print("the same")} else {print("not the same")}


#гипотеза о мат ожидании: =45
#нормальное распределение, а=0,001; 1-а=0,99; z-знач=2,33
Z=(M-45)*sqrt(289)/S0
Z
if (Z<2.33 && Z>=-2.33) {print ("yes")} else {print ("no")}


#гипотеза о величине дисперсии: =1600
#хи распределение а=0,1; 1-а=0,9; 319,16 и 257,7
Z=288*S0^2/1600
Z
if (Z<319.16 && Z>257.7) {print ("yes")} else {print ("no")}


#равенство мат ожиданий и дисперсий SS b N1
#мат ожидание, нормальное распределение, a=0.05; 1-a/2=0.975; z=1.96
Z=(M-M1)/sqrt(D0/289+D01/50)
Z
if (Z<1.96 && Z>=-1.96) {print ("yes")} else {print ("no")}

#дисперсии, критерий фишера,1,399
Z=D01/D0
Z
if (Z<1.399) {print ("yes")} else {print ("no")}

