---
title: "Using the UWBiost561 package"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Using the UWBiost561 package}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",eval=FALSE
)
```

This document introduces to you how to use the package **UWBiost561** for graph generation and finding the maximal partial clique.

```{r}
set.seed(42)
gen_result <- generate_partial_clique(n = 10, clique_fraction = 0.5, clique_edge_density = 0.8)
adj_mat <- gen_result$adj_mat

print(adj_mat)
```

