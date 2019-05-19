# projection code
# A.B.

set.seed(3) # used to get same plots in slides
n=40
X = rnorm(n,sd=5)
Y = rnorm(n,sd=3)

## export as : 840 x 655 ##
par(oma=c(0,0,0,0),mar=c(4.5,4.5,1.5,1))

# par(mfrow=c(1,2),oma=c(0,0,0,0),mar=c(4.5,4.5,1.5,1))

plot(X,Y, pch=20, main='Data cloud',asp=1); points(0,0,pch=3,col='black')

# project onto line
a = c(2,1)
a_dir = a / sqrt(sum(a^2))

# add projection line
abline(0,a[2]/a[1])

# compute projected data
projected_coefs =  cbind(X,Y) %*% a_dir 
temp=lapply(projected_coefs, function(x){ x * a_dir } )
projected_data = matrix(unlist(temp),n,2,byrow=TRUE)


# color data before projection
points( X, Y, pch=20, col='blue')
for (j in 1:n) {
  # projection arrows
  arrows(X[j],Y[j],projected_data[j,1],projected_data[j,2],col='red',code=0)
}
# resulting data
points( projected_data, pch=20, col='red' )


# univariate plot of projected data
stripchart(projected_coefs, pch=20,col='red', asp=1, main='Projected data'
           ,xlab='aX + bY')
# add origin
stripchart(0,pch=3,col='black',add=TRUE)
# histogram of projected data, with std. dev
hist( projected_coefs , main=c('Projected data'), xlab = 'a X + b Y' )
text( x=6,y=10, labels = paste0('sd = ', round(sd(projected_coefs),3) ))
sd_dir_1 = sd(projected_coefs)


# same idea,new direction

# graphical setting
par(oma=c(0,0,0,0),mar=c(4.5,4.5,1.5,1))
# data cloud with origin, again
plot(X,Y, pch=20, main='Data cloud',asp=1); points(0,0,pch=3,col='black')

# project onto different line
a = c(1,-.5)
a_dir = a / sqrt(sum(a^2))

# add projection line
abline(0,a[2]/a[1]) 

# focus on an invidiual point (outlier in X)
i=which.min(X) 
points(X[i],Y[i],pch=20,col='blue')

# calculate projected data
projected_coefs =  cbind(X,Y) %*% a_dir 
temp=lapply(projected_coefs, function(x){ x * a_dir } )
projected_data = matrix(unlist(temp),n,2,byrow=TRUE)

# plot projection for indvidiual point
arrows(X[i],Y[i],projected_data[i,1],projected_data[i,2],col='red',code=0)
points(projected_data[i,1],projected_data[i,2],pch=20,col='red')

# plot projection for second point
points(X[2],Y[2],pch=20,col='blue')
arrows(X[2],Y[2],projected_data[2,1],projected_data[2,2],col='red',code=0)
points(projected_data[2,1],projected_data[2,2],pch=20,col='red')



# plot all the projected data, with projections
points( X, Y, pch=20, col='blue')
for (j in 1:n) {
  arrows(X[j],Y[j],projected_data[j,1],projected_data[j,2],col='red',code=0)
}
points( projected_data, pch=20, col='red' )


# plot univariate data
stripchart(projected_coefs, pch=20,col='red', asp=1, main='Projected data'
           ,xlab = 'a X + b Y' )
stripchart(0,pch=3,col='black',add=TRUE)

# graphical setting for side by side
par(mfrow=c(1,2),oma=c(0,0,0,0),mar=c(4.5,4.5,1.5,1))


# plot histogram of projected data
hist( projected_coefs, main=c('Projected data'), xlab = 'a X + b Y' )
text(x=6,y=8, labels = paste0('sd = ', round(sd(projected_coefs),3) ))
sd_dir_2 = sd(projected_coefs)

par(oma=c(0,0,0,0),mar=c(4.5,4.5,1.5,1))
# eigenplot #
# plot data
plot(X,Y, pch=20, main='Data cloud',asp=1)
points(0,0,pch=3,col='black') # add origin


# a = c(2,1)
# a_dir_1 = a / sqrt(sum(a^2))
# 
# sd_dir_1 = sd(cbind(X,Y) %*% a_dir_1 )
# 
# a = c(1,-.5)
# a_dir_2 = a / sqrt(sum(a^2))
# sd_dir_2 = sd(cbind(X,Y) %*% a_dir_2 )
# 

# calculate cov-matrix of data
cov_mat = cov( cbind(X,Y) )
cov_inv = solve(cov_mat)
cov_eigen = eigen(cov_mat)
cov_values = cov_eigen$values
# principal axis of ellipse
first_vec = cov_eigen$vectors[,1]
second_vec = cov_eigen$vectors[,2]

# 
# 
# 
# arrows(-sd_dir_1*a_dir_1[1],-sd_dir_1*a_dir_1[2],sd_dir_1*a_dir_1[1],sd_dir_1*a_dir_1[2],code=0)
# arrows(-sd_dir_2*a_dir_2[1],-sd_dir_2*a_dir_2[2],sd_dir_2*a_dir_2[1],sd_dir_2*a_dir_2[2],code=0)
# points(-sd_dir_1*a_dir_1[1],-sd_dir_1*a_dir_1[2],pch=20,col='green')
# points(sd_dir_1*a_dir_1[1],sd_dir_1*a_dir_1[2],pch=20,col='green')
# points(-sd_dir_2*a_dir_2[1],-sd_dir_2*a_dir_2[2],pch=20,col='green')
# points(sd_dir_2*a_dir_2[1],sd_dir_2*a_dir_2[2],pch=20,col='green')


# plotting ellipse axis
arrows(-sqrt(cov_values[1])*first_vec[1],-sqrt(cov_values[1])*first_vec[2],sqrt(cov_values[1])*first_vec[1],sqrt(cov_values[1])*first_vec[2],code=0)
arrows(-sqrt(cov_values[2])*second_vec[1],-sqrt(cov_values[2])*second_vec[2],sqrt(cov_values[2])*second_vec[1],sqrt(cov_values[2])*second_vec[2],code=0)

# plotting ellipse itself using inverse cov-mat
ellispe_function = function(x,y){
  sqrt(cov_inv[1,1]*x^2 + 2 * cov_inv[1,2] *x * y + cov_inv[2,2] * y^2 ) - 1 
}

x<-seq(min(X),max(X),length=500)
y<-seq(min(Y),max(Y),length=500)
z<-outer(x,y, function(x,y){ ellispe_function(x,y) } )
contour(x,y,z,levels=0,drawlabels = FALSE,asp=1,add=TRUE)


# new PC univariate data
PC_data = cbind(X,Y) %*% cov_eigen$vectors
colnames(PC_data) = c('PC1','PC2')

par(mfrow=c(1,2),oma=c(0,0,0,0),mar=c(4.5,4.5,1.5,1))
stripchart(PC_data[,1],pch=20,col='red',main='max Projected data',xlab='PC1')
stripchart(PC_data[,2],pch=20,col='red',main='min Projected data',xlab='PC2')


# bi-plot
par(mfrow=c(1,2),oma=c(0,0,0,0),mar=c(4.5,4.5,1.5,1))
plot( PC_data, pch=20, col='black', main='Projected data cloud' )
plot(X,Y,pch=20, col='black',main='Original data cloud')

# correlations
cor(cbind(X,Y))

cor(PC_data)
