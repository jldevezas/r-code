#
# Cluster evaluation metrics.
#
# This was adapted and generalized from some code I had done before. It should work
# without a problem, but if it doesn't feel free to report it.
#
# José Devezas (joseluisdevezas@gmail.com)
#

f.partition.to.membership <- function(partition) {
  as.integer(as.factor(apply(partition, 1, function(e) e[2])))
}

f.membership.df.to.cluster.list <- function(clue) {
  lapply(levels(clue$MembershipId), function(l) clue$InstanceId[which(clue$MembershipId == l)])
}

f.find.largest.intersection <- function(community, partition) {
  len <- lapply(partition, function(cluster) length(intersect(community, cluster)))
  partition[[which.max(len)]]
}

f.calculate.tp.tn.fp.fn <- function(ground.truth.membership, clue.membership) {
  if (length(ground.truth.membership) != length(clue.membership)) {
    cat("Vector sizes differ.")
    return()
  }
  
  gt.M <- ground.truth.membership
  cl.M <- clue.membership
  
  contingency <- matrix(0, 2, 2)
  row.names(contingency) <- c("Same Class", "Different Class")
  colnames(contingency) <- c("Same Cluster", "Different Clusters")
  
  result <- data.frame(tp=0, tn=0, fp=0, fn=0)
  
  idxcomb <- combn(1:length(gt.M), 2)
  for (i in 1:ncol(idxcomb)) {
    id1 <- idxcomb[1,i]
    id2 <- idxcomb[2,i]
    if (gt.M[id1] == gt.M[id2] && cl.M[id1] == cl.M[id2]) {
      # TP
      contingency[1,1] = contingency[1,1] + 1
      result$tp <- contingency[1,1]
    } else if (gt.M[id1] != gt.M[id2] && cl.M[id1] != cl.M[id2]) {
      # TN
      contingency[2,2] = contingency[2,2] + 1
      result$tn <- contingency[2,2]
    } else if (gt.M[id1] == gt.M[id2] && cl.M[id1] != cl.M[id2]) {
      # FN
      contingency[1,2] = contingency[1,2] + 1
      result$fn <- contingency[1,2]
    } else if (gt.M[id1] != gt.M[id2] && cl.M[id1] == cl.M[id2]) {
      # FP
      contingency[2,1] = contingency[2,1] + 1
      result$fp <- contingency[2,1]
    }
  }
  
  print(contingency)
  return(result)
}

f.fscore <- function(contingency.values, beta=1) {
  val <- contingency.values
  precision <- val$tp / (val$tp + val$fp)
  recall <- val$tp / (val$tp + val$fn)
  ((beta^2 + 1) * precision * recall) / (beta^2 * precision + recall)
}

f.mutual.information <- function(clue, ground.truth) {
  cl <- f.membership.df.to.cluster.list(clue)
  gt <- f.membership.df.to.cluster.list(ground.truth)
  
  I <- 0
  N <- length(unlist(gt))
  
  for (k in 1:length(cl)) {
    for(j in 1:length(gt)) {
      common <- length(intersect(cl[[k]], gt[[j]]))
      if (common == 0) next # no mutual information, so I = 0
      I <- I + common/N * log((N * common) / (length(cl[[k]]) * length(gt[[j]])))
    }
  }
  
  return(I)
}

f.entropy <- function(clue) {
  cl <- f.membership.df.to.cluster.list(clue)
  N <- length(unlist(cl))
  
  H <- 0
  for (k in 1:length(cl)) {
    size <- length(cl[[k]])
    H <- H + size/N * log(size/N)
  }
  
  return(-H)
}

f.normalized.mutual.information <- function(clue, ground.truth) {
  f.mutual.information(clue, ground.truth) / ((f.entropy(clue) + f.entropy(ground.truth)) / 2)
}

#
# USAGE EXAMPLES
#

# Load the groundtruth data.frame and convert to a membership vector.
#gt <- read.csv("instance-membership-groundtruth.csv")
#gt.M <- f.partition.to.membership(gt)

# Load the partition given by our clustering methodology and convert to a membership vector.
#cl <- read.csv("instance-membership.csv")
#names(cl) <- c("InstanceId", "MembershipId")
#cl$MembershipId <- as.factor(cl$MembershipId)
#cl.M <- f.partition.to.membership(cl)

# You can also use the clues package to calculate other metrics
#require(clues)

# Calculate all versions of the Rand Index.
#RI <- adjustedRand(gt.M, cl.M)

# Calculate the F-score for beta=1, beta=2 and beta=0.5, using the confusion matrix.
#contingency.values <- f.calculate.tp.tn.fp.fn(gt.M, cl.M)
#F1 <- f.fscore(contingency.values)
#F2 <- f.fscore(contingency.values, 2)
#F05 <- f.fscore(contingency.values, 0.5)

# Calculate the normalized mutual information.
#NMI <- f.normalized.mutual.information(cl, gt)
