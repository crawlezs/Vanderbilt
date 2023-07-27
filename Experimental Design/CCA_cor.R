CCA_cor<-function(R, var.x, var.y, m = 1, cor.only=FALSE, eps = 1e-7)
{
  ## Canonical Correlation Analysis from a correlation matrix
  ## For PSY-GS 8867 class
  ## Author: Prof. Hao Wu (Peabody College)
  
  ## R is the correlation matrix of all variables
  ## var.x contains the positions (or names) of the x variables
  ## var.y contains the positions (or names) of the y variables
  ## m is the number of retained pairs
  ## cor.only: do you only need the canonical correlations?
  ## eps: eigenvalues of the x-correlation matrix and those of the y-correlation
  ##        matrix will be considered zero if smaller than this value.
    
  R<-cov2cor(R)
  Rxx<-R[var.x,var.x]
  Ryy<-R[var.y,var.y]
  Rxy<-R[var.x,var.y]
  x.labels<-colnames(Rxx)
  y.labels<-colnames(Ryy)
  
  eig.x<-eigen(Rxx,symmetric = TRUE)
  Dx<-eig.x$values
  retain.x<- (Dx>eps)
  dx<-sqrt(Dx[retain.x])
  Ux<-eig.x$vectors[,retain.x]
  Ux.invdx<-scale(Ux,center=FALSE,scale=dx)

  eig.y<-eigen(Ryy,symmetric = TRUE)
  Dy<-eig.y$values
  retain.y<- (Dy>eps)
  dy<-sqrt(Dy[retain.y])
  Uy<-eig.y$vectors[,retain.y]
  Uy.invdy<-scale(Uy,center=FALSE,scale=dy)
  
  if (cor.only==TRUE) 
  {
    svd.result<-svd(t(Ux.invdx)%*%Rxy%*%Uy.invdy,0,0)
    return(list(loading.x=NA,
                loading.y=NA,
                weights.x = NA,
                weights.y = NA,
                ca.cor = svd.result$d))
  }
  
  svd.result<-svd(t(Ux.invdx)%*%Rxy%*%Uy.invdy)
  d<-svd.result$d[1:m]
  U<-svd.result$u[,1:m]
  V<-svd.result$v[,1:m]

  weights.x<-Ux.invdx%*%U
  weights.y<-Uy.invdy%*%V
  rownames(weights.x)<-x.labels
  rownames(weights.y)<-y.labels
  colnames(weights.x)<-colnames(weights.y)<-1:m
  weights.x
  weights.y  
  
  loading.x<-Ux%*%diag(dx)%*%U
  loading.y<-Uy%*%diag(dy)%*%V
  rownames(loading.x)<-x.labels
  rownames(loading.y)<-y.labels
  colnames(loading.x)<-colnames(loading.y)<-1:m
  loading.x
  loading.y
  
  return(list(loading.x=loading.x,
              loading.y=loading.y,
              weights.x = weights.x,
              weights.y = weights.y,
              ca.cor = d))
}