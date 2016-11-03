\name{hddplot-package}
\alias{hddplot-package}
\alias{hddplot}
\docType{package}
\title{For high-dimensional data with known groups, derive scores for plotting}
\description{Cross-validated linear discriminant calculations
determine the optimum number of features. Test and training
scores from successive cross-validation steps determine, via a
principal components calculation, a low-dimensional global space onto
which test scores are projected, in order to plot them. Further functions
are included for didactic purposes. 
}

\details{
\tabular{ll}{
Package: \tab hddplot\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2006-01-09\cr
License: \tab GPL Version 2 or later.\cr
}
The most important functions are 

\code{cvdisc}: Determine variation in cross-validated accuracy with
number of features

\code{cvscores}: For a specific choice of number of features,
determine scores that can be used for plotting

Note also \code{scoreplot} (plot scores), \code{qqthin} (qqplots, designed
to avoid generating large files when there are many points), and
functions that are intended to illustrate issues that arise
in the plotting of expression array and other high-dimensional data
}
\author{John Maindonald

Maintainer: John Maindonald <john.maindonald@anu.edu.au>
}
\references{
Maindonald, J.H. and Burden, C.J., 2005. Selection bias in plots of
microarray or other data that have been sampled from a high-dimensional space.
In R. May and A.J. Roberts, eds., \emph{Proceedings of 12th
  Computational Techniques and Applications Conference CTAC-2004}, volume 46,
  pp. C59--C74. 

\url{http://journal.austms.org.au/V46/CTAC2004/Main/home.html} [March 15, 2005].
}
\keyword{package}

\seealso{\code{\link{cvscores}}, \code{\link{scoreplot}}
}
\examples{
## Use first 500 rows (expression values) of Golub, for demonstration.
data(Golub)
data(golubInfo)
attach(golubInfo) 
miniG.BM <- Golub[1:500, BM.PB=="BM"]  # 1st 500 rows only
cancer.BM <- cancer[BM.PB=="BM"] 
miniG.cv <- cvdisc(miniG.BM, cl=cancer.BM, nfeatures=1:10,
                    nfold=c(10,4))
miniG.scores <- cvscores(cvlist=miniG.cv, nfeatures=4, cl.other=NULL)
subsetB <- (cancer=="allB") & (tissue.mf \%in\% c("BM:f","BM:m","PB:m"))
tissue.mfB <- tissue.mf[subsetB, drop=TRUE] 
scoreplot(scorelist=miniG.scores, cl.circle=tissue.mfB, 
       circle=tissue.mfB\%in\%c("BM:f","BM:m"), 
       params=list(circle=list(col=c("cyan","gray"))), 
       prefix="BM samples -") 
detach(golubInfo) 
\dontrun{demo(biasedPlots)}
\dontrun{demo(CVscoreplot)}
}
