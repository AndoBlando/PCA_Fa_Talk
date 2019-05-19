# PCA Interpetation code
# A.B.
# data used:
# https://archive.ics.uci.edu/ml/datasets/Abalone
# description
# Name		  Data     Type	  Meas.	Description
# ----		---------	-----	-----------
# Sex		    nominal			M, F, and I (infant)
# Length		continuous	mm	Longest shell measurement
# Diameter	continuous	mm	perpendicular to length
# Height		continuous	mm	with meat in shell
# Whole     weight	   continuous	grams	whole abalone
# Shucked   weight	   continuous	grams	weight of meat
# Viscera   weight  	continuous	grams	gut weight (after bleeding)
# Shell     weight	  continuous	grams	after being dried
# Rings		  integer			+1.5 gives the age in years

setwd('Documents/Spring_2019/RTG/')
abalone_data = read.table('abalone.data',sep=',' )
names(abalone_data) = c('sex','length','diameter','height','whole weight',
                        'shucked weight','viscera weight','shell weight','rings count')
M_abalone = abalone_data[which(abalone_data$sex=='M'),-c(1,9)]
F_abalone = abalone_data[which(abalone_data$sex=='F'),-c(1,9)]


apply(M_abalone,2,sd) # can see the problem of different standard deviations!
apply(F_abalone,2,sd)

# PCA on original vars (cov mat)
M_cov_data = cov(M_abalone)
F_cov_data = cov(F_abalone)

M_cov_eigen = eigen(M_cov_data)
F_cov_eigen = eigen(F_cov_data)

# first two
M_cov_cumprop = cumsum(M_cov_eigen$values)/sum(M_cov_eigen$values)
F_cov_cumprop = cumsum(F_cov_eigen$values)/sum(F_cov_eigen$values)

F_cov_cumprop[1:2]

M_cov_PC_table = M_cov_eigen$vectors[,1:2]
colnames(M_cov_PC_table) = c('PC1','PC2')
rownames(M_cov_PC_table) = names(M_abalone)
M_cov_PC_table = rbind(M_cov_PC_table,M_cov_cumprop[1:2])

F_cov_PC_table = F_cov_eigen$vectors[,1:2]
colnames(F_cov_PC_table) = c('PC1','PC2')
rownames(F_cov_PC_table) = names(F_abalone)
F_cov_PC_table = rbind(F_cov_PC_table,F_cov_cumprop[1:2])

xtable::xtable(cbind(M_cov_PC_table,F_cov_PC_table))

std_dev_factors = rbind( apply(M_abalone,2,sd)/min(apply(M_abalone,2,sd)), apply(F_abalone,2,sd)/min(apply(F_abalone,2,sd))  )
rownames(std_dev_factors) = c('M','F')
xtable::xtable(std_dev_factors)

# PCA on standardized vars (cor mat)
M_cor_data = cor(M_abalone)
F_cor_data = cor(F_abalone)

M_cor_eigen = eigen(M_cor_data)
F_cor_eigen = eigen(F_cor_data)

# first two
M_cor_cumprop = cumsum(M_cor_eigen$values)/sum(M_cor_eigen$values)
F_cor_cumprop = cumsum(F_cor_eigen$values)/sum(F_cor_eigen$values)

M_cor_eigen$vectors[,1:2]
F_cor_eigen$vectors[,1:2]

M_cor_PC_table = M_cor_eigen$vectors[,1:2]
colnames(M_cor_PC_table) = c('PC1','PC2')
rownames(M_cor_PC_table) = names(M_abalone)
M_cor_PC_table = rbind(M_cor_PC_table,M_cor_cumprop[1:2])

F_cor_PC_table = F_cor_eigen$vectors[,1:2]
colnames(F_cor_PC_table) = c('PC1','PC2')
rownames(F_cor_PC_table) = names(F_abalone)
F_cor_PC_table = rbind(F_cor_PC_table,F_cor_cumprop[1:2])

xtable::xtable(cbind(M_cor_PC_table,F_cor_PC_table))

