# Define X Distance
X = 100 # 100 in used as default

# Define Y Distance
Y = 100 # 100 in used as default

gridID = X*Y; gridID
# Define N (number of samples)
N = 15 # 15 is default

# Create a Matrix to Sample from 
samplespace = matrix(data=1:gridID, nrow=X, ncol=Y, byrow=TRUE); head(samplespace)
set.seed(111)
gridsample = sample(gridID, N); gridsample

results=NULL
for (i in 1:N){
coord = which(samplespace==gridsample[i], arr.ind=TRUE)
print(coord)
results = rbind(results, c(coord))
}

str(results)
c(results)

Xord = results[, 1]
Yord = results[, 2]

Location = data.frame(Xordinate = Xord, Yordinate=Yord, GridID=gridsample)
order(Location)

LocationOrdered = Location[with(Location, order(Xordinate, Yordinate)), ]

