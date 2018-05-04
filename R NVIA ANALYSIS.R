


#options(stringsAsFactors=TRUE)

indicators<-read.csv(choose.files(),header=TRUE)

#install.packages("PerformanceAnalytics")
#require(PerformanceAnalytics)

#data(indicators)
#colnames(indicators)


## containing 

#head(indicators[colnames(indicators)[grep("A", colnames(indicators))]])


str(indicators)

View(indicators)

#rm(list=ls())





## subsetting by deleting column by name

install.packages("dplyr")
require(dplyr)

rm(ind1)

ind1<-indicators
ind2<-subset(ind1, select=-c(CODE,NAME,PROV,DISTR))
ind3<-subset(ind1, select=-c(CODE, NAME,DISTR))
ind4<-subset(ind1, select=-c(CODE,DISTR))

indA<-select(ind3, starts_with("A"))
indB<-select(ind3, starts_with("B"))
indC<-select(ind3, starts_with("C"))

boxplot(indA)
boxplot(indB)
boxplot(indC)

boxplot(ind3$A01 ~ ind3$PROV)

## grouped box plots
#install.packages("MASS")
#require(MASS)





## multivariate charts

install.packages("car")
require(car)

sp(A01~A02 | PROV,
    data = ind3,
    ylab = "Performance",
    xlab = "Context",
    main = "Composite indicators, by Province")


## scatterplot matrix

require(car)
pairs(ind3[11:15])

require(RColorBrewer)

## function to put histograms on diagonal

panel.hist<-function(x, ...)
{usr <-par("usr"); on.exit(par(usr))
par(usr=c(usr[1:2],0,1.5))
h<-hist(x, plot=FALSE)
breaks <- h$breaks; nB <- length(breaks)
y<-h$counts; y<- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, ...)}

pairs(ind3[2:4],
      panel=panel.smooth,
      main = "Composite Indicator Scatterplot",
      diag.panel=panel.hist,
      pch=16,
      col=brewer.pal(3, "Pastel1")[unclass(ind3$PROV)])


## hierarchical cluster analysis

indA2<-ind4[, c(3:5)]
indA3<-subset(indA2, !is.na(A01)) # remove missing values

## transform data type ( not necessary here)

#indA3$NAME=as.character(indA3$NAME)
#indA3$PROV=as.character(indA3$PROV)

str(indA3)

# create distance matrix [ dissilimarility index]

d<-dist(indA3)

# use distance matrix as basis of clustering

c<-hclust(d)

plot(c)

rect.hclust(c,k=4,border="gray")

## k means clustering

# kmeans only works on numerical dataset


str(indA3) # check data types


km<-kmeans(indA3, 3)
km


# plot cluster graph

install.packages("cluster")
require(cluster)

clusplot(indA3,
         km$cluster,
         color=TRUE,
         lines=3,
         labels=2)


##adding labels to cluster

names(km)

cplot_label <- data.frame(indA3, km=km$cluster)

clusplot(cplot_label,
         km$cluster,
         color=TRUE,
         lines=3,
         labels=2)

?clusplot


# You can also directly return the clusters
# x <- data.frame(x, K=kmeans(x, 2)$cluster)

## transforming variables

#install.packages("datasets")
#require(datasets)





## imputation 

# browseURL("http://cran.r-project.org/web/packages/mi/index.html")
# mean imputation
# regression imputation
# multiple imputation
# multivariate imputaton by chained equation

rm(list=ls())
