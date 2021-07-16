#'
#' scorePlot
#'
#' @keywords multivariate robust hplot
#' @noRd
#' @export
#' @importFrom plyr dlply llply
#'
.scorePlot <- function(spectra, so,
                       pcs = c(1, 2), ellipse = "none", tol = "none",
                       use.sym = FALSE, leg.loc = "topright", ...) {

  # Save call for possible user-provided xlim and/or ylim (base graphics)

  args <- as.list(match.call())[-1]

  # Step 0. Check the inputs

  if (length(pcs) != 2) stop("You must choose exactly two PC's to plot")

  case <- NULL # set up flags for the different classes of score results, args already checked
  if (inherits(spectra, "Spectra")) case <- "PCA"
  if (inherits(spectra, "Spectra2D")) case <- "MIA"
  if (inherits(so, "pop")) case <- "PCA" # pop returns prcomp so even though 2D treat like it's not
  if (is.null(case)) stop("Could not reconcile data object and scores object.")
  if ((case == "MIA") && (use.sym)) stop("ChemoSpec2D does not support use.sym")

  chkSpectra(spectra)

  # Step 1. Prep the data

  # For base graphics, we need to compute the ellipses *and* use them to set overall plot
  #   limits because base graphics doesn't understand how to set limits when plotting a bunch
  #   of different lines, points etc
  #
  # For ggplot2 graphics, we need to compute the ellipses but don't need to figure out any
  #   plot limits because ggplot2 understands setting the limits for a "whole" plot.
  #   Note that ggplot2 does not have a geom that does robust ellipses, so we have to calculate
  #   our own ellipse data.

  if (case == "PCA") DF <- data.frame(so$x[, pcs], group = spectra$groups)
  if (case == "MIA") DF <- data.frame(so$C[, pcs], group = spectra$groups)
  GRPS <- dlply(DF, "group", subset, select = c(1, 2))
  
  # Step 1.  Compute the ellipses if requested

  if ((ellipse == "cls") || (ellipse == "rob") || (ellipse == "both")) {
    # Compute ellipses.
    # There must be at least 3 data points per level to make a classic ellipse,
    # more to make a robust ellipse, as at least one (outlying) point may be dropped.

    gr <- sumGroups(spectra)

    for (n in 1:length(gr$group)) {
      if (gr$no.[n] == 1) message("Group ", gr$group[n], "\n\thas only 1 member (no ellipse possible)")
      if (gr$no.[n] == 2) message("Group ", gr$group[n], "\n\thas only 2 members (no ellipse possible)")
      if (gr$no.[n] == 3) message("Group ", gr$group[n], "\n\thas only 3 members (ellipse not drawn)")
    }

    idx <- which(gr$no. > 3) # Index for those groups that will get ellipses
    gr <- gr[idx, ]
    ELL <- llply(GRPS[idx], .computeEllipses) # These are the ellipses we'll need later
  }

  # Step 2.  Branch for each graphics mode.

  go <- chkGraphicsOpt()

  if (go == "base") {
    if ((ellipse == "cls") || (ellipse == "rob") || (ellipse == "both")) {

      # Get limits all possible pieces of the data
      #
      # Keep in mind the ellipses may be quite flattened and hence large.
      # At the same time, the ellipses might be quite round and
      # the scores well outside them, if there is an outlier.
      # Must check all cases!

      x.scores <- range(llply(GRPS, subset, select = 1))
      y.scores <- range(llply(GRPS, subset, select = 2))
      x.ell <- range(llply(ELL, function(x) {
        range(x[1])
      }))
      y.ell <- range(llply(ELL, function(x) {
        range(x[2])
      }))
      x.ell.r <- range(llply(ELL, function(x) {
        range(x[4])
      }))
      y.ell.r <- range(llply(ELL, function(x) {
        range(x[5])
      }))
      # extend.limits: stackoverflow.com/a/29647893/633251
      x.all <- range(x.scores, x.ell, x.ell.r)
      x.all <- x.all + diff(x.all) * 0.05 * c(-1.0, 1.15) # expand slightly for labels on right of points
      y.all <- range(y.scores, y.ell, y.ell.r)
      y.all <- y.all + diff(x.all) * 0.05 * c(-1.0, 1.15) # leave room for annotations at top of plot
    }

    if (ellipse == "none") {
      x.scores <- range(llply(GRPS, subset, select = 1))
      y.scores <- range(llply(GRPS, subset, select = 2))
      x.all <- range(x.scores) + diff(range(x.scores)) * 0.05 * c(-1.0, 1.15) # expand slightly for labels
      y.all <- range(y.scores) + diff(range(y.scores)) * 0.05 * c(-1.0, 1.15) # leave room for annotations at top of plot
    }

    # Now we have our limits, plot the scores after accounting for user provided xlim, ylim

    dPargs <- list(PCs = DF[, 1:2], spectra = spectra, case = case, use.sym = use.sym, ... = ...)

    # Allow user to give xlim, ylim but provide good defaults as well
    if (!"xlim" %in% names(args)) dPargs <- c(dPargs, list(xlim = x.all))
    if (!"ylim" %in% names(args)) dPargs <- c(dPargs, list(ylim = y.all))

    do.call(.drawPoints, dPargs)

    # Draw the ellipses if requested.

    if ((ellipse == "cls") | (ellipse == "rob") | (ellipse == "both")) .drawEllipses(ELL, gr, ellipse, use.sym, ...)

    # Do the decorations

    if (case == "PCA") {
      .addMethod(so)
      if (leg.loc != "none") .addLegend(spectra, leg.loc, use.sym, bty = "n")
      .addEllipseInfo(ellipse)
    }

    if (case == "MIA") {
      if (leg.loc != "none") .addLegend(spectra, leg.loc, use.sym = FALSE, bty = "n")
      .addEllipseInfo(ellipse)
    }

    # Label extremes if requested

    if (tol != "none") .labelExtremes(DF[, 1:2], spectra$names, tol)

  } # end of go == "base"

  if (go == "ggplot2") {
    
    if ((ellipse == "cls") || (ellipse == "rob") || (ellipse == "both")) 
    {
      
      x.scores <- range(llply(GRPS, subset, select = 1))
      y.scores <- range(llply(GRPS, subset, select = 2))
      x.ell <- range(llply(ELL, function(x) {
        range(x[1])
      }))
      y.ell <- range(llply(ELL, function(x) {
        range(x[2])
      }))
      x.ell.r <- range(llply(ELL, function(x) {
        range(x[4])
      }))
      y.ell.r <- range(llply(ELL, function(x) {
        range(x[5])
      }))
      # extend.limits: stackoverflow.com/a/29647893/633251
      x.all <- range(x.scores, x.ell, x.ell.r)
      x.all <- x.all + diff(x.all) * 0.05 * c(-1.0, 1.15) # expand slightly for labels on right of points
      y.all <- range(y.scores, y.ell, y.ell.r)
      y.all <- y.all + diff(x.all) * 0.05 * c(-1.0, 1.15) # leave room for annotations at top of plot
    }
    if (ellipse == "none") {
      x.scores <- range(llply(GRPS, subset, select = 1))
      y.scores <- range(llply(GRPS, subset, select = 2))
      x.all <- range(x.scores) + diff(range(x.scores)) * 0.05 * c(-1.0, 1.15) # expand slightly for labels
      y.all <- range(y.scores) + diff(range(y.scores)) * 0.05 * c(-1.0, 1.15) # leave room for annotations at top of plot
    }
    #print(x.all)
    #print(y)
    
    if (case == "PCA")
    {
      if(!use.sym){
      p<-ggplot(DF)+
        geom_point(aes_string(x=colnames(DF)[1],y=colnames(DF)[2]),color=spectra$colors,shape=20,size=3)
      }
      
      if (use.sym)
      {
        p<-ggplot(DF)+
          geom_point(aes(x=colnames(DF)[1],y=colnames(DF)[2]),color="black",shape=spectra$sym)
      }
    }
    
    if (case == "MIA") {
      p<-ggplot(DF)+
        geom_point(aes(x=colnames(DF)[1],y=colnames(DF)[2]),color=spectra$colors,shape=20,size=3)
    }
    
    #Changing theme to theme_bw() and removing grids
    p<-p+theme_bw()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
    
      if(ellipse == "cls")
      {
        cls.coords <- llply(ELL, function(x) {
          x[1:2]
        })
        cls.coords <- llply(cls.coords, function(x) {
          do.call(cbind, x)
        })
        df<-data.frame(x=numeric(),y=numeric(),name=character())
        for ( i in 1:length(cls.coords))
        {
          x<-c()
          y<-c()
          name<-c()
          total<-length(cls.coords[[i]])/2
          for ( j in 1:total)
          {
            x<-c(x,cls.coords[[i]][j,1])
            y<-c(y,cls.coords[[i]][j,2])
            
          }
          name<-rep(names(cls.coords)[i],total)
          temp<-data.frame(x,y,name)
          df<-rbind(df,temp)
        }
        
        
        
  #      cls.coords.df<-as.data.frame(cls.coords)
      if(!use.sym )
      {
        lines<-rep(2,length(gr$color))
         p<-p+stat_ellipse(data=df,aes(x=x,y=y,color=name),linetype=2)+
            scale_color_manual(values = gr$color)
      }
        
      if(use.sym )
      {
        color.black<-rep("black",length(gr$color))
        p<-p+stat_ellipse(data=df,aes(x=x,y=y,color=name),linetype=2)+
          scale_color_manual(values=color.black)
      }  
        
      }
      
      if (ellipse == "rob") {
        rob.coords <- llply(ELL, function(x) {
          x[4:5]
        })
        rob.coords <- llply(rob.coords, function(x) {
          do.call(cbind, x)
        })
        
        df<-data.frame(x=numeric(),y=numeric(),name=character())
        for ( i in 1:length(rob.coords))
        {
          x<-c()
          y<-c()
          name<-c()
          total<-length(rob.coords[[i]])/2
          for ( j in 1:total)
          {
            x<-c(x,rob.coords[[i]][j,1])
            y<-c(y,rob.coords[[i]][j,2])
            
          }
          name<-rep(names(rob.coords)[i],total)
          temp<-data.frame(x,y,name)
          df<-rbind(df,temp)
        }
        
        
        if (!use.sym) 
        {
          p<-p+stat_ellipse(data=df,aes(x=x,y=y,color=name))+
            scale_color_manual(values = gr$color)
        }
        
        if(use.sym )
        {
          color.black<-rep("black",length(gr$color))
          p<-p+stat_ellipse(data=df,aes(x=x,y=y,color=name))+
            scale_color_manual(values=color.black)
        }  
      }
      
      if (ellipse == "both") 
      {
        
        cls.coords <- llply(ELL, function(x) {
          x[1:2]
        })
        cls.coords <- llply(cls.coords, function(x) {
          do.call(cbind, x)
        })
        df.cls<-data.frame(x=numeric(),y=numeric(),name=character())
        for ( i in 1:length(cls.coords))
        {
          x<-c()
          y<-c()
          name<-c()
          total<-length(cls.coords[[i]])/2
          for ( j in 1:total)
          {
            x<-c(x,cls.coords[[i]][j,1])
            y<-c(y,cls.coords[[i]][j,2])
            
          }
          name<-rep(names(cls.coords)[i],total)
          temp<-data.frame(x,y,name)
          df.cls<-rbind(df.cls,temp)
        }
        
        rob.coords <- llply(ELL, function(x) {
          x[4:5]
        })
        rob.coords <- llply(rob.coords, function(x) {
          do.call(cbind, x)
        })
        
        df.rob<-data.frame(x=numeric(),y=numeric(),name=character())
        for ( i in 1:length(rob.coords))
        {
          x<-c()
          y<-c()
          name<-c()
          total<-length(rob.coords[[i]])/2
          for ( j in 1:total)
          {
            x<-c(x,rob.coords[[i]][j,1])
            y<-c(y,rob.coords[[i]][j,2])
            
          }
          name<-rep(names(rob.coords)[i],total)
          temp<-data.frame(x,y,name)
          df.rob<-rbind(df.rob,temp)
        }
        
        #print(df.rob)
        if(!use.sym )
        {
          lines<-rep(2,length(gr$color))
          p<-p+stat_ellipse(data=df.cls,aes(x=x,y=y,color=name),linetype=2)
          #  scale_color_manual(values = gr$color)+
            #scale_linetype_manual(values=lines)
          p<-p+stat_ellipse(data=df.rob,aes(x=x,y=y,color=name))+
            scale_color_manual(values = gr$color)
        }
        
        if(use.sym )
        {
          color.black<-rep("black",length(gr$color))
          p<-p+stat_ellipse(data=df.cls,aes(x=x,y=y,color=name))
            #scale_color_manual(values=color.black)
          p<-p+stat_ellipse(data=df.rob,aes(x=x,y=y,color=name),linetype=2)+
            scale_color_manual(values=color.black)
        } 
        
      }
      
      

    # label extremes
      if(tol!="none")
      {
        newList<-.getExtremeCoords(DF[,1:2],spectra$names,tol)
        xcoord <- newList$x
        ycoord <- newList$y-0.03
        l<-newList$l
        p <- p + annotate("text", x = xcoord, y = ycoord, label = l, size = 3)
      }
    
      #removing the ggplot legend 
      p<-p+ theme(legend.position = "none")
    
    
    return(p)
  } # end of go == "ggplot2"

} # End of plotScores

