#' An adapted function of the heatmapSexRatio
#' @export heatmapSexRatioAdapted

heatmapSexRatioAdapted <- function( input, highColor = "blue", lowColor = "red", verbose = FALSE, warnings = TRUE, interactive = FALSE ){

  input$SR <- as.numeric(input$SR)

  objDuplicated1 <-  input[ , c('disA' ,'disB', 'SR' ,'pairs')]
  objDuplicated2 <-  input[ , c('disB' ,'disA', 'SR' ,'pairs')]
  colnames( objDuplicated2) <- c('disA' ,'disB', 'SR' ,'pairs')
  obj <- rbind( objDuplicated1, objDuplicated2)

  m <- max(as.numeric(obj$SR))
  n <- min(as.numeric(obj$SR))

  sortedCodesA <-  unique(obj[with(obj, order(disA)), "disA"])
  sortedCodesB <-  unique(obj[with(obj, order(disB)), "disB"])

  obj$disA  <- factor(obj$disA  , levels= as.factor(sortedCodesA))
  obj$disB  <- factor(obj$disB  , levels= as.factor(sortedCodesB))

  diseases <- unique(c( as.character(obj$disA), as.character(obj$disB)))
  mydata <- as.data.frame( matrix( ncol=length(diseases), nrow=length(diseases)))
  colnames(mydata) <- diseases[ order(diseases)]
  rownames(mydata) <- diseases[ order(diseases)]

  for( i in 1:ncol(mydata)){

    dis1 <- colnames(mydata)[i]

    for( j in 1:nrow(mydata)){

      dis2 <- rownames(mydata)[j]
      selection <- obj[ obj$disA == dis1 & obj$disB == dis2, ]

      if(nrow(selection) == 0){
        selection <- obj[ obj$disB == dis1 & obj$disA == dis2, ]
      }
      mydata[i,j] <- selection[1,"SR"]
    }

  }


  mydata[lower.tri(mydata)] <- NA


  mydata <- as.data.frame(mydata)
  mydata$dis <- rownames( mydata)
  mydata <-(melt(mydata, 'dis', variable_name='variable'))
  mydata$variable <- factor(mydata$variable, levels=rev(levels(mydata$variable)))


  p <- ggplot2::ggplot(mydata , ggplot2::aes ( variable, dis ) ) +
    ggplot2::geom_tile(ggplot2::aes(fill = value), colour = "white") +
    ggplot2::scale_fill_gradient(limits = c(n,m), low = lowColor,   high = highColor, na.value = "white") +
    ggplot2::theme_grey(base_size = 11) +
    ggplot2::scale_x_discrete(expand = c(0, 0)) +
    ggplot2::geom_text(ggplot2::aes(label = value ) ) +
    ggplot2::theme( plot.margin = grid::unit ( x = c ( 5, 15, 5, 15 ), units = "mm" ),
                    axis.text.x = ggplot2::element_text ( angle = 45, size = 11, hjust = 1 ) )
  p




}

