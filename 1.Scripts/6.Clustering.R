# Some clustering methods can use the raw vegetation data, instead of the CCA.
# This should probably be better, since the CCA axes explain relatively little of the variation.
# Though maybe if the non-metric MDS works better at explaining the variance, its axis can be used.



# https://youtu.be/7xHsRkOdVwo?si=lrpQew6NPLB13hli
# Hierarchical clustering and dendrograms: start by comparing individual samples with each other,
# and slowly builds bigger and bigger clusters. How these clusters got formed is depicted in a 
# dendrogram. If there is logically no such thing as nested clusters in your type of data, this 
# method doesn't make a lot of sense to use. All methods below are 'flat' clustering methods.

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