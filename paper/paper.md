---
title: 'simpleCache: R caching for reproducible big data projects'
authors:
 - name: Nathan Sheffield
   orcid: 0000-0001-5643-4068
   affiliation: 1
 - name: VP Nagraj
   orcid: 0000-0003-0060-566X
   affiliation: 1
 - name: Vince Reuter
   orcid: 0000-0002-7967-976X
   affiliation: 1
affiliations:
 - name: University of Virginia
   index: 1
date: 28 October 2017
bibliography: paper.bib
---

# Summary

`simpleCache` is an R[@R] package providing functions for caching R objects. Its purpose is to encourage writing reusable, restartable, and reproducible analysis pipelines for projects with massive data and computational requirements. Like its name indicates, `simpleCache` is intended to be simple. Users specify a location to store caches, and then provide the function with nothing more than a cache name and instructions (R code) for how to produce the object.

In addition to the basic functionality described above, `simpleCache` has advanced options for assigning objects to specific environments, recreating caches, reloading caches and even binding caching operations to cluster computing resources via the `batchools`[@batchtools] interface. These features make the package particularly useful for large-scale data analysis and research projects. 

The authors expect `simpleCache` could be used across a variety of disciplines. Many researchers may be interested in caching objects that are computationally expensive to create during an analysis pipeline. `simpleCache` has already proven to be helpful with this kind workflow. For example, the package was used to store cached baseline data for comparison across many samples in order to determine DNA methylation heterogeneity[@RPIM]. Furthermore, `simpleCache` could be incorporated as a performance enhancement for other software. A proven example is its integeration with the LOLA R package[@LOLA] in order to more efficiently cache and retrieve genomic region databases during analysis.

# References
