# Demonstration of PCA in R
# A.B.
library(MASS)

# Normally we begin by reading in a dataset
# For this example we'll generate one
n=30; p=5;

Lambda = diag( c(5,3,2,1,0.75)  )
vec1 = c(1,1,0,0,0)/sqrt(2)
vec2 = c(1,-1,0,0,0)/sqrt(2)
vec3 = c(0,0,1,0,0)
vec4 = c(0,0,0,1,1)/sqrt(2)
vec5 = c(0,0,0,1,-1)/sqrt(2)
V_mat = cbind(vec1,vec2,vec3,vec4,vec5)
Sigma = t(V_mat) %*% Lambda %*% V_mat


# generate data
X = mvrnorm( n, mu=rep(0,p), Sigma = Sigma )
colnames(X) = paste0( 'X', 1:5 )
rownames(X) = as.character(1:n)

X_pca = princomp(X)
print(X_pca$loadings, covmat=TRUE, cutoff = 0)
pca_loadings = X_pca$loadings
loadings_mat = pca_loadings[]

# Large std. dev problem
n=30
set.seed(5)
# generate data
X1 = rnorm(n,sd=1)
X2 = rnorm(n,sd=20)
# look at sample sd
c( sd(X1), sd(X2) ) # factor of 20

X = cbind(X1,X2)
X_pca = princomp(X)
 # pca essentially selects the original coordinates
print(X_pca$loadings,cutoff = 0)

# lets look at ellipse
cov_inv = solve(cov(X))
ellispe_function = function(x,y){
  sqrt(cov_inv[1,1]*x^2 + 2 * cov_inv[1,2] *x * y + cov_inv[2,2] * y^2 ) - 1 
}

x<-seq(min(X1),max(X1),length=500)
y<-seq(min(X2),max(X2),length=500)
z<-outer(x,y, function(x,y){ ellispe_function(x,y) } )

par(oma=c(0,0,0,0),mar=c(4.5,4.5,1.5,1))
lims=range(c(X1,X2))
plot(X1,X2,pch=20,cex=0.7,asp=1,xlim=lims,ylim=lims,main='Data with ellipse from PCs')
contour(x,y,z, col='red',levels=0,drawlabels = FALSE,asp=1,add=TRUE)

