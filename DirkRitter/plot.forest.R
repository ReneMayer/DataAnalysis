to.dendrogram <- function(dfrep,rownum=1,height.increment=0.1){

  if(dfrep[rownum,'status'] == -1){
    rval <- list()

    attr(rval,"members") <- 1
    attr(rval,"height") <- 0.0
    attr(rval,"label") <- dfrep[rownum,'prediction']
    attr(rval,"leaf") <- TRUE

  }else{##note the change "to.dendrogram" and not "to.dendogram"
    left <- to.dendrogram(dfrep,dfrep[rownum,'left daughter'],height.increment)
    right <- to.dendrogram(dfrep,dfrep[rownum,'right daughter'],height.increment)
    rval <- list(left,right)

    attr(rval,"members") <- attr(left,"members") + attr(right,"members")
    attr(rval,"height") <- max(attr(left,"height"),attr(right,"height")) + height.increment
    attr(rval,"leaf") <- FALSE
    attr(rval,"edgetext") <- dfrep[rownum,'split var']
  }

  class(rval) <- "dendrogram"

  return(rval)
}

# randomForest(x, y=NULL,  xtest=NULL, ytest=NULL, ntree=500, ... )
tree=getTree(rfobj = s.rf, k=200, labelVar=T)

tree.list=lapply(1:20, function(x) getTree(rfobj = s.rf, k=x, labelVar=T) )

d <- to.dendrogram(tree)
dendrogram.list=lapply(tree.list, function(x) to.dendrogram(x) )


str(d)
par(mfrow=c(5,5), mar = rep(0, 4))
lapply(dendrogram.list, function(x) plot(x,center=TRUE,edgePar=list(t.cex=.5,p.col=NA,p.lty=0)))

plot(d,center=TRUE,edgePar=list(t.cex=1,p.col=NA,p.lty=0))
