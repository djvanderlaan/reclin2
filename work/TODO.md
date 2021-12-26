cluster
==============================================================

- [X] `predict.cluster_pairs`
- [X] `select_pairs` / `cluster_select_threshold`
- [X] `add_from_x` and `add_from_y`. 
- [X] `cluster_collect` should accept a selection column and only return
  selected pairs.
- [X] `compare_vars`
- [X] `select_n_to_m` and `select_greedy` give error and help when applied to
  cluster pairs.
- [ ] a generic predict routine: pass it a model and it will call predict on
  each node and add the results to the pairs. This can be used in combination
  with other machine learning methods.

