
# We have n observations that we want to distribute over ncluster worker nodes. 
# Distribute randomly. Returns a vector of length n with group ids. Numbered 
# from 0.
distribute_over_cluster <- function(n, ncluster, seed = 1) {
  group <- floor(seq_len(n)/(n+1)*ncluster)
  # Make sure we get the same groups every time for a give dataset; useful if
  # we want to rbind multiple sets of pairs; reset the seed at the end
  old_seed <- set_seed(1)
  on.exit(set_seed(old_seed))
  sample(group)
}

