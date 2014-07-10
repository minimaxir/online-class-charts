online-class-charts
===================

Code needed to reproduce data analysis and charts for MIT/Harvard Online Course Data.

This is a complement to my blog article [Who Performs the Best in Online Classes?](http://minimaxir.com/2014/07/online-class-charts/).

Since MIT/Harvard prevent redistribution of the dataset, you'll have to [download the dataset](http://dx.doi.org/10.7910/DVN/26147) yourself to start the analysis.

Requires the *dplyr* R package for data analysis. For reproducing the charts:

* dplyr
* ggplot2
* extrafont (w/ [fonts installed](https://github.com/wch/extrafont))
* scales
* grid

You must be using OS X to use the same font; otherwise, change the font family in Rstart.
