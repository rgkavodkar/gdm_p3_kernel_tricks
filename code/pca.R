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
id
data = data.frame(X, Y, id)
x11()
plot(data[, 1], data[, 2], xlim = c(0, 100), ylim = c(0, 100), col = data[, 3])

pca = princomp (data[, -3], center=TRUE)

x11()
plot (pca)

# eigen vectors
pca_vector = loadings(pca)
pca_vector
summary = summary (pca)
summary
pca_data = pca$scores
ev_slope_1 = pca_vector[1, 1] / pca_vector[2, 1]
ev_slope_2 = pca_vector[1, 2] / pca_vector[2, 2]

x11()
plot(pca_data[ ,1], pca_data[ ,2], col = data[, 3])
abline(0, ev_slope_1, col = "red")
abline(0, ev_slope_2, col = "blue")

# RBF Kernelizing
data = as.matrix(data)
kpca_data = kpca(data[, -3], kpar = list(sigma = 0.15), kernel = "rbfdot", features = 2)

hist(eig(kpca_data))

kpca_vectors = pcv(kpca_data)
x11()
plot(kpca_vectors[, 1], kpca_vectors[, 2], col = data[, 3])

x11()
hist(kpca_vectors[, 1])
x11()
hist(kpca_vectors[, 2])

# Polydot Kernelizing
data = as.matrix(data)
kpca_data = kpca(data[, -3], kpar = list(scale=1, offset=0, degree=2), kernel = "polydot", features = 2)
kpca_data

x11()
hist(eig(kpca_data))

p = pcv(kpca_data)
p

x11()
plot(p[, 1], p[, 2], col = data[, 3])

kpca_vectors = pcv(kpca_data)

x11()
hist(kpca_vectors[, 1])
x11()
hist(kpca_vectors[, 2])
