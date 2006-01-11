"simulateScores" <-
  function(nrows=7129, cl=rep(1:3, c(19,10,2)), x=NULL, 
           cl.other=4, x.other=NULL, nfeatures=15, dimen=2, seed=NULL){
    if(!is.null(seed))set.seed(seed)
    m <- length(cl)
    m.other <- length(cl.other)
    if(is.null(x)){
      x <- matrix(rnorm(nrows*m), nrow=nrows)
      rownames(x) <- paste(1:nrows)
    }
    else nrows <- dim(x)[1]
    if(is.null(x.other)){
      x.other <- matrix(rnorm(nrows*m.other), nrow=nrows)
      rownames(x.other) <- paste(1:nrows)
    }
    if(is.numeric(cl))cl <- paste("Gp", cl, sep="")
    if(is.numeric(cl.other))cl.other <- paste("Gp", cl.other, sep="")
    cl <- factor(cl)
    if(dimen>length(levels(cl))-1)dimen <- length(levels(cl))-1
    cl.other <- factor(cl.other)
    xx.random <- matrix(rnorm(nrows*m), nrow=nrows)
    ordfeatures <- orderFeatures(xx.random, cl=cl, values=TRUE)
    stat <- ordfeatures$stat[1:nfeatures]
    ord.use <- ordfeatures$ord[1:nfeatures]
    dfUse.ord <- data.frame(t(x[ord.use, ]))
    dfUseOther.ord <- data.frame(t(x.other[ord.use, ]))
    ordUse.lda <-  lda(dfUse.ord, grouping=cl)
    scores <- predict(ordUse.lda, dimen=dimen)$x
    scores.other <- predict(ordUse.lda, newdata=dfUseOther.ord, dimen=dimen)$x
    invisible(list(scores=scores, cl=cl, other=scores.other, 
                   cl.other=cl.other, nfeatures=nfeatures))
  }

