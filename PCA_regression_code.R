# PCA regression 
# dataset used
# https://archive.ics.uci.edu/ml/datasets/Superconductivty+Data

train <- read_csv("superconduct/train.csv")
names(train)
ncol(train)
X = as.matrix(train[,-82])
Y = as.matrix(train[,82])
XX = cov(X) # large scale differences

X_std = scale(X)
eigens = eigen( cov(X_std) )

X_pca = X_std %*% eigens$vectors
colnames(X_pca) = paste0('PC',seq(1:81))

full_pca_fit = lm( Y ~ X_pca )
summary(full_pca_fit)

# which PC's to use?
# one way
# top 10 with most variation

cumsum( eigens$values )/sum(eigens$values) # about top 10 gives ~ 88%
X_mat = X_pca[,1:10]

first_10_df = data.frame( Y, X_pca[,1:10] )

top10_var_fit = lm( critical_temp ~ ., data = top10_df)
top10_var_fit_sum = summary(top10_var_fit)

xtable::xtable(top10_var_fit_sum$coefficients )

# top 10 with largest t-stat in regression
temp=summary(full_pca_fit)
p_vals=temp$coefficients[-1,4]
smallest_p_val_inds = order(p_vals)
cov(X_pca[,smallest_p_val_inds[1:10] ])
smallest_p_10_df = data.frame( Y, X_pca[,smallest_p_val_inds[1:10] ] )
smallest_p_10_fit = lm( critical_temp ~., data = smallest_p_10_df)
smallest_p_10_fit_sum = summary(smallest_p_10_fit)

xtable::xtable( smallest_p_10_fit_sum$coefficients )
