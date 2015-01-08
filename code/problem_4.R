library("kernlab")

generate_n_dim_sphere = function(dim, num) {
  mat = matrix(0, nrow = num, ncol = dim)
  
  for(i in 1:num) {
    random_numbers = rnorm(dim)
    squared_sum = 0
    for(k in 1: dim) {
      squared_sum = squared_sum + random_numbers[k]**2
    }
    denom = sqrt(squared_sum)
    for(j in 1:dim) {
      mat[i, j] = random_numbers[j] / denom
    }
  }
  mat
}

no_cluster_1 = 200
no_cluster_2 = 200
dimensions = 10
radius_1 = 5
radius_1 = 20

# data generation
mat1 = generate_n_dim_sphere(dimensions,no_cluster_1)
mat1 = radius_1 * mat1
mat1 = as.data.frame(mat1)
id = rep(1, no_cluster_1)
mat1 = data.frame(mat1, id)
mat1

id = NA

mat2 = generate_n_dim_sphere(dimensions,no_cluster_2)
mat2 = radius_2 * mat2
mat2 = as.data.frame(mat2)
id = rep(2, no_cluster_2)
mat2 = data.frame(mat2, id)
mat2

points = rbind(mat1, mat2)

# Regular KMeans
km = kmeans(points, centers = 2)
km$cluster

dat = as.matrix(points[, -6])

# Kernelizing the data
kpca_data = kpca(dat, kpar = list(scale=1, offset=0, degree=2), kernel = "polydot")

components = eig(kpca_data)
sum_components = sum(components)
cumulative_components = matrix(0, nrow = length(components), ncol = 2)
for(i in 1:length(components)) {
  prev_comp = 0
  if(i > 1) {
    prev_comp = cumulative_components[i-1, 2]
  }
  cumulative_components[i, 1] = i
  cumulative_components[i, 2] = components[i] / sum_components + prev_comp
  
}


x11()
plot(cumulative_components)

sum(components[1:45]) / sum_components

rot = rotated(kpca_data)
rot = rot[, 1:45]

# Kmeans on Kernel data
km2 = kmeans(rot, centers = 2)
km2$cluster

data2 = as.matrix(dat[, -3])
kkm = specc(dat, centers = 2, kernel = "rbfdot")

data2 = as.matrix(dat[, -3])
kkm = specc(dat, centers = 2, kernel = "polynomial")
  
