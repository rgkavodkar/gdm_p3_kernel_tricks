library("kernlab")
set.seed(2)

# data generation
x1 = rnorm(50, mean = 50, sd = 3)
y1 = rnorm(50, mean = 50, sd = 3)

radius_1 = 20
radius_2 = 40

x2 = rep(NA, 100)
y2 = rep(NA, 100)

x3 = rep(NA, 100)
y3 = rep(NA, 100)

for(i in 1:100) {
  radius_delta = runif(1, min = 0, max = 5)
  radius = radius_1 + radius_delta
  
  degree = runif(1, min = 0, max = 360)
  
  x2[i] = radius * cos(degree) + 50
  y2[i] = radius * sin(degree) + 50
  
  radius_delta = runif(1, min = 0, max = 5)
  radius = radius_2 + radius_delta
  
  degree = runif(1, min = 0, max = 360)
  
  x3[i] = radius * cos(degree) + 50
  y3[i] = radius * sin(degree) + 50
  
}

X = c(x1, x2, x3)
Y = c(y1, y2, y3)
id = c(rep(1, length(x1)), rep(2, length(x2)), rep(3, length(x3)))

data = data.frame(X, Y, id)

x11()
plot(data[, 1], data[, 2], xlim = c(0, 100), ylim = c(0, 100), col = data[, 3])
 

# Regular KMeans
km = kmeans(data, center = 3)

km$cluster
x11()
plot(data[, -3], xlim = c(0, 100), ylim = c(0, 100), col = km$cluster)


# RBF Kernel KMeans
data2 = as.matrix(data[, -3])
kkm = specc(data2, centers = 3, kernel = "rbfdot")
kkm

x11()
plot(data2, col = kkm)

# Polynomial Kernel KMeans
data2 = as.matrix(data[, -3])
kkm = specc(data2, centers = 3, kernel = "polydot")
kkm

x11()
plot(data2, col = kkm)


