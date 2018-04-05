#' An adapted function of the heatmapDirection
#' @export heatmapDirectionAdapted

heatmapDirectionAdapted <- function( input, fromAtoBColor = "darkgreen", fromBtoAColor = "orange", noDirectionColor = "grey", verbose = FALSE, warnings = TRUE, interactive = FALSE ){

  objDuplicated1 <-  input[ , c('disAcode' ,'disBcode', 'AtoB' , 'BtoA', 'test','correctedPvalue','result')]
  objDuplicated2 <-  input[ , c('disBcode' ,'disAcode', 'BtoA' , 'AtoB', 'test','correctedPvalue','result')]
  colnames( objDuplicated2) <-  c('disAcode' ,'disBcode', 'AtoB' , 'BtoA', 'test','correctedPvalue','result')
  obj <- rbind( objDuplicated1, objDuplicated2)


  sortedCodesA <-  unique(obj[with(obj, order(disAcode)), "disAcode"])
  sortedCodesB <-  unique(obj[with(obj, order(disBcode)), "disBcode"])

  obj$disAcode  <- factor(obj$disAcode  , levels= as.factor(sortedCodesA))
  obj$disBcode  <- factor(obj$disBcode  , levels= as.factor(sortedCodesB))

  diseases <- unique(c( as.character(obj$disAcode), as.character(obj$disBcode)))
  mydata <- as.data.frame( matrix( ncol=length(diseases), nrow=length(diseases)))
  colnames(mydata) <- diseases[ order(diseases)]
  rownames(mydata) <- diseases[ order(diseases)]

  for( i in 1:ncol(mydata)){

    dis1 <- colnames(mydata)[i]

    for( j in 1:nrow(mydata)){

      dis2 <- rownames(mydata)[j]
      selection <- obj[ obj$disAcode == dis1 & obj$disBcode == dis2, ]

      if(nrow(selection) == 0){
        selection <- obj[ obj$disBcode == dis1 & obj$disAcode == dis2, ]
      }
      mydata[i,j] <- selection[1,"result"]
    }

  }


  mydata[lower.tri(mydata)] <- NA


  mydata <- as.data.frame(mydata)
  mydata$dis <- rownames( mydata)
  mydata <-(melt(mydata, 'dis', variable_name='variable'))
  mydata$variable <- factor(mydata$variable, levels=rev(levels(mydata$variable)))
  mydata <- mydata[ ! is.na( mydata$value), ]
  mydata$value <- as.factor(mydata$value)

  h.value = c("From A to B","From B to A", "No directionality")
  h.color = c(fromAtoBColor, fromBtoAColor, noDirectionColor)
  heatmapColors = data.frame(h.value, h.color)
  hColors <- as.character(heatmapColors$h.color)
  names(hColors) <- heatmapColors$h.value


  p <- ggplot2::ggplot(mydata , ggplot2::aes ( variable, dis ) ) +
    ggplot2::geom_tile(ggplot2::aes(fill = as.factor(value)), colour = "white") +
    ggplot2::scale_fill_manual(values = hColors)+
    ggplot2::guides( fill=ggplot2::guide_legend( title="Directionality" ) ) +
    ggplot2::theme_grey(base_size = 11) +
    ggplot2::labs ( title = "Comorbidity directionality", x = "diagnosis code under study (A)", y = "disease comorbidities (B)") +
    ggplot2::scale_x_discrete(expand = c(0, 0)) +
    ggplot2::theme( plot.margin = grid::unit ( x = c ( 5, 15, 5, 15 ), units = "mm" ),
                    axis.line = ggplot2::element_line ( size = 0.7, color = "black" ), text = ggplot2::element_text ( size = 11 ) ,
                    axis.text.x = ggplot2::element_text ( angle = 45, size = 11, hjust = 1 ), panel.background = ggplot2::element_blank() )

  p
}
