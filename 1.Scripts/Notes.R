########################### Clustering

# Some clustering methods can use the raw vegetation data, instead of the CCA.
# This should probably be better, since the CCA axes explain relatively little of the variation.
# Though maybe if the non-metric MDS works better at explaining the variance, its axis can be used.

# https://youtu.be/7xHsRkOdVwo?si=lrpQew6NPLB13hli
# Hierarchical clustering and dendrograms: start by comparing individual samples with each other,
# and slowly builds bigger and bigger clusters. How these clusters got formed is depicted in a 
# dendrogram. All methods below are 'flat' clustering methods.

# https://youtu.be/HVXime0nQeI?si=OK8J6_aOJdBTy50m
# K-nearest neighbor classification: not applicable for the data, because it requires a lot of data
# of which categories/clusters are already known.

# https://youtu.be/4b5d3muPQmA?si=2p3mjdckOs3hliAr
# Another option is to do K-means clustering: plot the reduction of variation against the number of 
# clusters to see where the elbow point is, and use that as K. Raw data can be used.

# https://youtu.be/RDZUdRSDOok?si=aRRp0ssyc4LZN6jf
# When the clusters are more complex than 'circular-shaped', a standard method such as K-means might
# have difficulty to cluster everything correctly. 

# Additionally, with data that has more than two/three axes, it's not possible to visually identify 
# whether clusters are nested. There are clustering algorithms that can do this. One of these 
# methods is the DBSCAN.

# DBSCAN has two user defined parameters: radius of "close" circle, and number of close points for a
# core point. Core points extend a cluster with all points in close proximity. Non-core points can 
# only join a cluster, not extend it further.






########################### NMDS

# Polar ordination, DCA, CCA, PCA, RDA and MDS all fit together in the group of linear/gradient-
# based models. t-SNE and UMAP are non-linear/distance-based models.


# https://youtu.be/GEn-_dAyYME?si=4_CIHXwCMFN8L-bd
# Multidimensional scaling (MDS)

# A PCA converts correlations (or lack thereof) among samples into a 2d-plot. Highly correlated
# samples form clusters. MDS is similar, but instead of correlations, it converts the distances
# among the sampes into a 2d-plot.

# There are two versions of multi-dimensional scaling: metric/classical or non-metric. The former is
# also called the principal coordinate analysis (PCoA). It uses the actual distances instead of just
# the "rank order" that the non-metric version uses. The non-metric MDS is therefore uses ordinal
# data instead of scale.

# A metric MDS/PCoA that uses the Euclidian distance produces the exact same graph as a PCA. But 
# there are more ways to measure distances, such as the average of the absolute values of the log
# fold change, the manhatten distance, hamming distance, great circle distance, etc. Selecting the
# "best" distance is part of the art of data science.

# Once again, coming back to the comparison with a PCA: the only difference with the MDS is what's
# initially calculated: the correlations between samples, or the distances. The math behind it, and
# the types of results (coordinates for a graph, percent of variation each axis accounts for, and 
# the loading scores) are exactly the same.

# Non-metric multidimensional scaling is quite appropriate for data with many zeroes.


# https://youtu.be/eN0wFzBA4Sc?si=VtLB7BGhtdYUgh2B
# The problem with a PCA or similar analyses is that it only works well if the first 2 axes account
# most of the variation in the data. I.e., with a complicated dataset, a PCA may not work very well. 
# A UMAP (uniform manifold approximation and projection) is one alternative.

# https://youtu.be/NEaUSP4YerM?si=aisltXBbikrqtjRo
# t-SNE is very similar to a UMAP, apart from two big differences: t-SNE always starts with a random
# initialization of the low-dimensional graph; every time it is ran on the same data, it starts out
# with a different graph. This is not the case for UMAP, which does exactly the same every time. The
# second difference, is that UMAP scales better with big datasets.