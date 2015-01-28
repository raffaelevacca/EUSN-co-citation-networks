Social network and network science co-citations across disciplines in 1996-2013
=========================

## Intro
- This is an R project to create co-citation networks for the field of social network analysis and network science using Web of Science (WOS) data.
- The project was originally created for EUSN Visualization Award 2014. It [won the award](http://jornades.uab.cat/eusn/content/winners-eusn-2014-conference-awards-p-leifeld-r-vacca) in June 2014.
- The project uses co-citation networks to map the research field of social networks and network science across different disciplines, namely the social sciences, physics, and computer science. Each node is a *cited* author and each tie is a co-citation between two authors. The *citing* papers are all the publications in the [Thomson Reuters Web of Science](http://thomsonreuters.com/thomson-reuters-web-of-science/) that mention "social networks" or "network science" in their topic. 
- Using [WOS categories](http://images.webofknowledge.com/WOKRS56B5/help/WOS/hp_subject_category_terms_tasca.html), citing papers have been divided into two broad disciplines: the social sciences versus physics and computer science. 
- Citing papers have been downloaded for the period from 1996 to 2013, and divided into six 3-years time spans according to their publication date. This has resulted into a time series of six co-citation networks. A network animation has been created to show the evolution of the co-citation network over these six time periods.
- The project uses the R packages `igraph`, `network`, `networkDynamic` and `ndtv` for network analysis and for producing network animations.

## Data
- All the original data as downloaded from the WOS are in `./Data/` in txt format.
- The WOS search that produced the data is as follows:
```s
TOPIC: ("social network" OR "social networks" OR "network science")
Timespan: 2011-2013. Indexes: SCI-EXPANDED, SSCI, A&HCI.
```
- In the search above, 2011-2013 was set as the publication timespan. The same search was repeated for 5 more timespans: 1996-1998, 1999-2001, 2002-2004, 2005-2007, 2008-2010.

## Code
- All the R code  needed to extract the co-citation networks from the txt data files and to create the static (png) and dynamic (mp4) figures is in `./Script/`.
- All the code can be run from `00_main.R`, which `source`s the other R files included in the `./Script/` folder.
- Note that `00_main.R` is also where you set the relevant path for your working directory: you have to change that path for the project to be reproducible on your computer.

## Figures and animations
- All the figure and animation files are in `./Figures/`.
- In the figures (png), node size represents degree centrality, and edge width and color intensity represent the number of co-citations between two authors.
- Node color represents the discipline in which an author is cited: (1) Blue authors are cited by papers in the social sciences; (2) Grey authors are cited by papers in physics or computer science; (3) Red authors are cited by papers in both the disciplinary classes.
