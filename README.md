# `data.cube`

`data.cube` is an R package for the exploration of multidimensional datasets and for the detection of statistical outliers within. It is mainly a tool for data exploration, allowing to have a first glance at it and to formulate research hypotheses to be later tested.

The package defines a new data structure called `data.cube` that can be fed with a classical `data.frame` encoding a list of numeric observations described according to several categorical dimensions. For example, in the case of Twitter data, it can be the number of tweets (numeric observation) that have been published by a given user (first dimension) about a given topic (second dimension) at a given date (third dimension). The input `data.frame` hence takes the form of a list of quadruplets (user, topic, date, number of tweets).

Statistical outliers can then be identified among the observations by first selecting some dimensions of interest, that is by subsetting or by aggregating the input dimensions. If needed, observations can also be normalised according to the marginal values along the selected dimensions, thus comparing the observed value to an expected value obtained by the uniform redistribution of the selected marginal values. Different statistical tests can then be chosen to measure the deviation between the observed and the expected values. The package finally allows to retrieve a list of positive outliers, that is observations that are significantly higher than expected.

### Clone

```
git clone https://github.com/Lamarche-Perrin/data.cube
```

### Install R Dependencies

```
cd data.cube/
Rscript install.packages.R
```

Troubles installing `rgl` R package on Ubuntu?  
See https://stackoverflow.com/questions/29478686/troubles-installing-rgl-on-ubuntu

### Launch "Outlier Explorer" Application with `shiny`

```
cd data.cube/outlier-explorer.app/
Rscript launch.api.R
```
This application is currently hosted on the [Huma-Num](https://www.huma-num.fr/about-us) facility for digital humanities:  
https://penelope.huma-num.fr/apps/data.cube/outlier-explorer.app/

### Launch "Outlier Explorer" Web Service with `plumber`

```
cd data.cube/outlier-explorer.api/
Rscript launch.app.R
```
This Web service is hosted on the [Huma-Num](https://www.huma-num.fr/about-us) facility for digital humanities:  
https://penelope.huma-num.fr/tools/

Also see the API specifications on SwaggerHub:  
https://app.swaggerhub.com/apis-docs/Lamarche-Perrin/outlier-explorer/1.0.1


### Authors

This package has been developed by researchers of the [Complex Networks](http://www.complexnetworks.fr/) team, within the [Computer Science Laboratory of Paris 6](https://www.lip6.fr/), for the [ODYCCEUS](https://www.odycceus.eu/) project, founded by the [European Commission FETPROACT 2016-2017 program](https://ec.europa.eu/research/participants/portal/desktop/en/opportunities/h2020/calls/h2020-fetproact-2016-2017.html) under grant 732942.


### License

Copyright © 2017 Robin Lamarche-Perrin (<Robin.Lamarche-Perrin@lip6.fr>)

`data.cube` is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. It is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GN  General Public License for more details. You should have received a copy of the GNU General Public License along with this program. If not, see <http://www.gnu.org/licenses/>.
