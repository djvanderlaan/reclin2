


### Deterministic linkage


0. Handle records from A and B with duplicate linkage keys
1. Generate all pairs that match on linkage key.
2. Force n-to-m linkage on pairs (when keys not deduplicated)
3. Generate linked data set


### Sequential deterministic linkage

1. For each combination of linkage keys

    1. Perform deterministic linkage
    2. Select records from A and B that need to be linked in next step

2. Combine all pairs
3. Force n-to-m linkage on pairs (when keys not deduplicated)
4. Generate linked data set

### Probabilistic linkage

0. Handle records from A and B with duplicate linkage keys
1. Generate pairs
2. Generate comparison vectors for each pair
3. Estimate m and u-probabilities
4. Calculate weights
5. Select weight threshold
6. Select pairs with weight above weight threshold
7. Force n-to-m linkage on pairs

Result is a set of pairs

8. Generate linked data set


### Sequential probabilistic linkage

1. For each combination of linkage keys and blocking keys 

    1. Perform probabilistic linkage
    2. Select records from A and B that need to be linked in next step

2. Combine all pairs
3. Force n-to-m linkage on pairs (when keys not deduplicated)
4. Generate linked data set


### Machine learning

0. Handle records from A and B with duplicate linkage keys
1. Generate pairs
2. Generate comparison vectors for each pair
3. Flag (selection of) pairs as match and non-match
4. Estimate ml-model
6. Classify pairs as match and non-match
6. Select matches
7. Force n-to-m linkage on pairs
8. Generate linked data set


