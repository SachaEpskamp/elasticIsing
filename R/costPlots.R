### Plotting function
costPlots <- function(object, filename = "elasticIsing.pdf", width= 8, height= 5)
{
  stopifnot(is(object, "elasticIsing"))
  pdf(filename,width=width,height=height)
  ###
  for (c in seq_len(dim(object$costs)[3]))
  {
    costName <- dimnames(object$costs)[[3]][c]
    
    Cost <- object$costs[,,c]
    rownames(Cost) <- colnames(Cost) <- NULL
    
    melted <- melt(Cost)
    names(melted) <- c("lambda","alpha","cost")
    # melted$lambda <- lambdavec[melted$lambda] %>% round(3) %>% as.character
    # melted$alpha <- alphavec[melted$alpha]%>% round(3) %>% as.character
    melted$lambda <- lambda[melted$lambda]
    melted$alpha <- alpha[melted$alpha]
    
    #   precision <- -Cost+max(Cost)
    #   precision <- precision / max(precision)
    #   precision <- round(precision*99) + 1
    cols <-  terrain.colors(100)
    #   preCols <- matrix(NA,nrow(precision),ncol(precision))
    #   preCols[] <- cols[c(precision)]
    
    z <- Cost[order(lambda),]
    nrz <- nrow(z)
    ncz <- ncol(z)
    zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
    
    preCols <- cols[cut(zfacet,100)]
    # preCols <- preCols[nrow(preCols):1,]
    #   library(rgl)
    # surface3d(lambda,alpha,ElasticRes$cost, color = terrain.colors(100)[precision], xlim=c(0,1))
    # axes3d( xlab = "lambda")
    # aspect3d(1,1,0.5)
    # 
    # persp3d(lambda[order(lambda)],alpha,Cost[order(lambda),], color = preCols[order(lambda),], 
    #         xlab = "Lambda", ylab = "Alpha", zlab = "Cost", aspect = c(1,1,0.5),theta=300)
    # rgl.viewpoint(30,30)
    # min <- which(Cost == min(Cost), arr.ind = TRUE)
    # lines3d(x= lambda[min[1]], y = alpha[min[2]], z = range(Cost), lwd = 3, col = "red")
    
    persp(lambda[order(lambda)],alpha,Cost[order(lambda),], col = preCols, 
          expand = 0.5 , shade = 0, border = NA,
          xlab = "Lambda", ylab = "Alpha", zlab = "Cost", theta = 130, phi = 30,
          main = costName)
    
    persp(lambda[order(lambda)],alpha,Cost[order(lambda),], col = preCols, 
          expand = 0.5 , shade = 0, border = NA,
          xlab = "Lambda", ylab = "Alpha", zlab = "Cost", theta = 25, phi = 30,
          main = costName, ticktype = "detailed")
  }
  dev.off()
}
