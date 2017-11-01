---
title: 'simpleCache: R caching for reproducible, distributed, large-scale projects'
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

`simpleCache` is an R[@R] package that provides functions for caching R objects. Its purpose is to encourage writing reusable, restartable, and reproducible analysis for projects with large data and computational requirements. Like its name indicates, `simpleCache` is intended to be simple. Users specify a location to store caches, and then provide nothing more than a cache name and instructions (R code) for how to produce an R object. `simpleCache` either creates and saves or simply loads the result as necessary with just a single function call.

In addition to this basic functionality, `simpleCache` has advanced options for assigning objects to specific environments, recreating caches, reloading caches, and even distributing caching operations to cluster computing resources via the `batchools`[@batchtools] interface. These features make the package particularly useful for large-scale data analysis and research projects. `simpleCache` is most helpful for caching objects that are computationally expensive to create, but used in multiple scripts or by multiple users.

`simpleCache` is also useful to enhance performance in a package that relies on large databases. For example, `simpleCache` has been incorporated with the LOLA R package[@LOLA] to more efficiently cache and retrieve genomic region databases. Similarly, `simpleCache` has been used to store cached baseline statistical tables for faster lookup to determine statistical differences on tables with hundreds of millions of data points [@RPIM].

In summary, `simpleCache` provides a user-friendly interface to help the R programmer manage computationally intensive, repeated data analysis.

# References
