

################################################################################
#
# PREAMBLE
#

# This is example R code for using pvclust together with opm to calculate
# clusterings with branch support values. Assessing the uncertainty of 
# clustering results should be done for all serious interpretations of such
# clusterings. This is one approach to assessing uncertainty; see the pvclust
# documentation for details.
#
# (C) 2013 by Markus Goeker (markus [DOT] goeker [AT] dsmz [DOT] de)
#
# This file is distributed under the terms of the GPL.

library(pvclust)
library(opm)


################################################################################


# The matrix must be transposed: pvclust expects the objects to be clustered
# in the columns. See pvclust::pvclust and base::t for details. If you need
# more space for the clustering, generate shorter labels from the metadata.
#
x <- t(extract(vaas_4, list("Species", "Strain")))

# The default distance method is based on correlations, which makes not much
# sense for the usual application of PM data, at least not for vaas_4. Several
# other distance measures could be applicable, however, as well as clustering
# algorithms other than the default average-linkage clustering.
#
x.pvc <- pvclust(x, method.dist = "euclidean")

# The two kinds of support values are visible on the branches. We see that
# E. coli is well differentiated from P. aeruginosa.
#
pdf("vaas_clustering_with_pvalues.pdf")
plot(x.pvc, hang = -1)
dev.off()

