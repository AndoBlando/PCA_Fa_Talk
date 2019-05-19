# Introductory example of PCA applied to high-dimensional data
# A.B.
# data from:
# https://archive.ics.uci.edu/ml/datasets/Parkinson%27s+Disease+Classification
# read data
pd_speech_data <- read_csv("Downloads/pd_speech_features.csv", skip = 1)
# inspect types
apply(pd_speech_data,2,class)

# keep identifier columns
pd_speech_ids = pd_speech_data[c("id","gender",'class')]
data_columns = setdiff(names(pd_speech_data),c("id","gender",'class'))
data_matrix = as.matrix(pd_speech_data[data_columns])

# work with re-scaled data
std_data_matrix = scale(data_matrix)
std_cov = cov(std_data_matrix)
std_eigen = eigen(std_cov)
std_eigen_values = std_eigen$values
std_eigen_vectors = std_eigen$vectors

# use first two PC's for display purposes
# first two PC's capture around 22% of the variation
head( cumsum(std_eigen_values)/sum(std_eigen_values) ) 
pca_data = std_data_matrix %*% std_eigen_vectors[,1:2]

# 1 = PD
# 0 = Control
pd_inds = which(pd_speech_ids$class == 1)
control_inds = which(pd_speech_ids$class == 0)

# 1 = Male
# 0 = Female
M_inds = which(pd_speech_ids$gender == 1)
F_inds = which(pd_speech_ids$gender == 0)

# 4 intersections PD & Gender
M_pd_inds = which((pd_speech_ids$gender == 1) & ( pd_speech_ids$class == 1) )
F_pd_inds = which((pd_speech_ids$gender == 0) & ( pd_speech_ids$class == 1) )
M_control_inds = which((pd_speech_ids$gender == 1) & ( pd_speech_ids$class == 0) )
F_control_inds = which((pd_speech_ids$gender == 0) & ( pd_speech_ids$class == 0) )

# obtain plots used in slides #

# pca data as-is
plot(pca_data, pch=20, xlab = 'PC1', ylab = 'PC2', main = "Dimension Reduced data by group")
 
# bivariate with control / PD 
# Control/PD, control broken down by gender
plot(pca_data[pd_inds,],pch=20,col='red', xlim = range(pca_data[,1]),ylim = range(pca_data[,2]), xlab = 'PC1', ylab = 'PC2', main = "Dimension Reduced data by group")
points(pca_data[control_inds,],pch=20,col='blue')
legend('bottomright', pch=20, col=c('red','blue'), legend=c('PD','Control'))

points(pca_data[M_control_inds,],pch=20,col='cyan')
points(pca_data[F_control_inds,],pch=20,col='darkblue')
legend('bottomright', pch=20, col=c('red','cyan','darkblue'), legend=c('PD','Control & M', 'Control & F'))

# Control/PD, PD broken down by gender
plot(pca_data[pd_inds,],pch=20,col='red', xlim = range(pca_data[,1]),ylim = range(pca_data[,2]), xlab = 'PC1', ylab = 'PC2', main = "Dimension Reduced data by group")
points(pca_data[control_inds,],pch=20,col='blue')
points(pca_data[M_pd_inds,],pch=20,col='darkred')
points(pca_data[F_pd_inds,],pch=20,col='pink')
legend('bottomright', pch=20, col=c('blue','darkred','pink'), legend=c('Control','PD & M', 'PD & F'))






