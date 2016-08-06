x=c(1, 3)
y=c(12, 14)

approx(x, n=10)

m = lm(y ~ x)$coeff[2] 
b = lm(y ~ x)$coeff[1]

# if clay is at .2 (De) then what is the proportion (y)

x1 = 0.2

m*x1 + b

