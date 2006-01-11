"aovFbyrow" <-
function(x=Golub, cl=golub.info$cancer){
    y <- t(x)
    qr.obj <- qr(model.matrix(~cl))
    qty.obj <- qr.qty(qr.obj,y)
    tab <- table(factor(cl))
    dfb <- length(tab)-1
    dfw <- sum(tab)-dfb-1
    ms.between <- apply(qty.obj[2:(dfb+1), , drop=FALSE]^2, 2, sum)/dfb
    ms.within <- apply(qty.obj[-(1:(dfb+1)), , drop=FALSE]^2, 2, sum)/dfw
    Fstat <- ms.between/ms.within
  }

