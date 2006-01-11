\name{Golub}
\alias{Golub}
\docType{data}
\title{Golub data (7129 rows by 72 columns), after normalization}
\description{
These are a normalized version of the Golub leukemia data from the
\code{golubEsets} package, available from:

\url{http://www.bioconductor.org/download/experiments/}
}
\usage{data(Golub)}
\format{
  The format is:
 num [1:7129, 1:72] -1363.3 -1058.6  -541.5    74.3 -1110.1 ...
 - attr(*, "scaled:center")= num [1:72] 582 528 604 576 751 ...
 - attr(*, "scaled:scale")= num [1:72] 2474 2305 2378 2437 2438 ...
 - attr(*, "dimnames")=List of 2
  ..$ : chr [1:7129] "AFFX-BioB-5_at" "AFFX-BioB-M_at" "AFFX-BioB-3_at" "AFFX-BioC-5_at" ...
  ..$ : NULL
}
\details{
Data have been normalized and are supplied, here, as a matrix.
}
\source{See the help page for the dataset \code{golubMerge}, in the
\code{golubEsets} package, for details of the source of the original
data.
}
\references{
Molecular Classification of Cancer: Class Discovery and Class Prediction by 
Gene Expression Monitoring, Science, 531-537, 1999, T. R. Golub and 
D. K. Slonim and P. Tamayo and C. Huard and M. Gaasenbeek and J. P. Mesirov 
and H. Coller and M.L. Loh and J. R. Downing and M. A. Caligiuri and 
C. D. Bloomfield and E. S. Lander
}
\examples{
data(Golub)
## Select 20 rows from the data; show boxplots of variation across chips
boxplot(data.frame(t(Golub[sample(1:7129, 20), ]))) 
}
\keyword{datasets}
