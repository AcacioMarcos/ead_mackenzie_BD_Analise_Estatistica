set.seed(123)

n <- 16

sequencia <- sample.int(100, size=n)

sequencia.ord <-sort(sequencia)

i = seq(1, n)

pi = (i - 1)/(n - 1)

data.frame(Indice = i, Seq = sequencia, Seq.ord = sequencia.ord, Quantis = round(pi * 100,2))

####Primeiro Quartil

i = round(n * (1/4))

c(pi[i], pi[i + 1])

c(sequencia.ord[i], sequencia.ord[i + 1])

q = 0.25

f = (q - pi[i])/(pi[i+1]-pi[i])

f

Q25 = (1 - f) *sequencia.ord[i] + f * sequencia.ord[i + 1]

Q25

####Segundo Quartil

if(all.equal(n%%2,0)){
  i = n/2} else {
    i = round(n/2,0)}

c(pi[i], pi[i+1])

c(sequencia.ord[i], sequencia.ord[i + 1])

q=0.5

f = (q - pi[i])/(pi[i + 1] - pi[i])

f

Q50 = (1 - f) * sequencia.ord[i] + f * sequencia.ord[i + 1]

Q50

quantile(sequencia)

######Assimetria

for (N in c(5, 10, 20, 30, 50, 75, 100, 125, 150, 200, 250)) {print (round(c(N, sqrt(N*(N-1))/(N-2)),3))}
