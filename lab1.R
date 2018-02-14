#1.
	#(a)
1:20
	#(b)
20:1
	#(c)
c(1:20,19:1)
	#(d)
tmp = c(4, 6, 3); tmp
	#(e)
rep(tmp,10) 
	#(f)
c(rep(tmp,10),4)
	#(g)
c(c(rep(4,10),rep(6,20)),rep(3,30))
	#(h)
h = c(c(2^4,2^6),rep(2^3,4))
length(grep(6,h))

#2.
x = seq(3,6,0.1)
y=exp(x)*cos(x); y

#3
	#(a)
x = seq(3,36,3); y = seq(1,34,3)
a = (0.1^x)*(0.2^y); a
	#(b)
n = 1:25
b = (2^n)/n; b

#4
	#(a)
s=0; for (i in 10:100) {s = s+i*i*i+4*i*i}; s
	#(b)
s=0; for (i in 1:25) {s = s+((2^i)+(3^i)/i)/i}; s

#5
	#(a)
a = character(30);for (n in 1:30){a[[n]] = paste('label',as.character(n))}; a
	#(b)
a = character(30);for (n in 1:30){a[[n]] = paste('fn',as.character(n),sep="")}; a

#6
xVec = sample(0:999,250,replace=TRUE); yVec = sample(0:999,250,replace=TRUE); xVec; yVec 
	#(a)
a=NULL; for (n in 1:249) {a[n] = yVec[n+1]-xVec[n]}; a 
	#(b)
b=NULL; for (n in 1:249) {b[n] = sin(yVec[n])/cos(xVec[n+1])}; b
	#(c)
c=NULL; for (n in 1:248) {c[n] = xVec[n]+2*xVec[n+1]-xVec[n+2]}; c
	#(d)
d=0; for (n in 1:249) {d = d+exp(-xVec[n+1])/(xVec[n]+10)}; d
#7
	#(a)
for (n in 1:250) {if(yVec[n]>600) print(yVec[n])}
	#(b)
c=NULL; for (n in 1:250) {if(yVec[n]>600) {print(n); c = append(c,n)}}
	#(c)
for (n in 1:length(c)) {if(xVec[c[n]]>600) print(c[n])}
	#(d)
s=0; for (n in 1:250) {s = s+xVec[n]}; x = s/250
d=NULL; for (n in 1:250) {d[n] = Mod(xVec[n]-x)^(1/2)}; d
	#(e)
e=0; max=yVec[which.max(yVec)]; for (n in 1:250) {if(max-yVec[n]<=200) e++1}; e
	#(f)
chet=0; nechet=0; x7=0
for (n in 1:250){ if((xVec[n]%%2)==0) {chet=chet+1} else {nechet=nechet+1; if(xVec[n]%%7==0) x7=x7+1} }; chet; nechet; x7
	#(g)
X = integer(250);t=rank(yVec, ties='first'); for (i in 1:length(t)){X[t[i]] = xVec[i]};X;t
	#(h)
h=seq(1,250,3); for (n in 1:length(h)) print(yVec[h[n]])
	#(i)
X=xVec; for (n in 1:10) {print(X[which.max(X)]); X[which.max(X)]=0}
	#(j)
t=xVec[duplicated(xVec)]; t
//t=duplicated(xVec); p=NULL; for(n in 1:250) {if(t[n]) p=append(p,xVec[n])}; p
j = unique(xVec);j

#8
e=1; sum=1; for(i in seq(2,38,2)) {e=e*i/(i+1); sum = sum+e}; sum


