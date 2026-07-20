km <- function(x, k) {
  n <- nrow(x)
  
  centers <- x[sample(1:n, k), ]
  
  clust.old <- rep(0, n)
  clust <- rep(1, n)
  itr <- 0
  
  while (sum(clust != clust.old) > 0) {
    itr <- itr + 1
    clust.old <- clust
    
    
    for (i in 1:n) {
      dis <- numeric(k)
      for (j in 1:k) {
        
        dis[j] <- sqrt((x[i, 1] - centers[j, 1])^2 + (x[i, 2] - centers[j, 2])^2)
      }
      clust[i] <- which.min(dis)
    }
    
    for (j in 1:k) {
      if (sum(clust == j) > 0) {
        centers[j, 1] <- mean(x[clust == j, 1])
        centers[j, 2] <- mean(x[clust == j, 2])
      }
    }
  }
  
  return(list(
    clustdata = x,
    clust = clust,
    centers = centers,
    size = table(clust),
    iteration = itr
  ))
}
