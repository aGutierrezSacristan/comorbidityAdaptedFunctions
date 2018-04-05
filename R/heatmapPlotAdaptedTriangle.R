#' An adapted function of the heatmapPlot
#' @export heatmapPlotAdaptedTriangle

heatmapPlotAdaptedTriangle <- function( input , selectValue = "score", cutOff = 1, npairs = 1, interactive = FALSE, lowColor = "#0000FF", highColor = "yellow", verbose = FALSE ) {

  message("Checking the input object")
  checkClass <- class(input)[1]

  if(checkClass != "cAnalysis" & checkClass != "molecularcAnalysis"){
    message("Check the input object. Remember that this
            object must be obtained after applying the query
            function to your input file. The input object class must
            be:\"cAnalysis\" or \"molecularcAnalysis\"")
    stop()
  }

  if(class(input)[1]== "cAnalysis"){
    objInitial <- input@result
    objDuplicated1 <-  objInitial[ , c('disAcode' ,'disBcode', 'disA' ,'disB', 'AB', 'AnotB', 'BnotA' ,'notAnotB', 'fisher', 'oddsRatio', 'relativeRisk', 'phi' ,'expect' ,'score', 'correctedPvalue')]
    objDuplicated2 <-  objInitial[ , c('disBcode', 'disAcode', 'disB', 'disA', 'AB' ,'BnotA', 'AnotB' ,'notAnotB' ,'fisher', 'oddsRatio' ,'relativeRisk', 'phi' ,'expect', 'score' ,'correctedPvalue')]
    colnames( objDuplicated2) <- c('disAcode' ,'disBcode', 'disA' ,'disB', 'AB', 'AnotB', 'BnotA' ,'notAnotB', 'fisher', 'oddsRatio', 'relativeRisk', 'phi' ,'expect' ,'score', 'correctedPvalue')
    obj <- rbind( objDuplicated1, objDuplicated2)
    obj <- obj[as.numeric(obj$AB) >= npairs, ]
    column <- which(colnames(obj )==selectValue)
    obj$value <- as.numeric(obj[,column])

    if( selectValue == "correctedPvalue"  | selectValue == "fisher"){
      obj  <- obj [as.numeric(obj$value) <= cutOff,]
    }else{
      obj  <- obj [as.numeric(obj$value) >= cutOff,]
    }


    obj$disAdesc <- obj$disAcode
    obj$disBdesc <- obj$disBcode


    m <- max(as.numeric(obj [,column]))
    n <- min(as.numeric(obj [,column]))


    sortedCodesA <-  unique(obj[with(obj, order(disAcode)), "disAdesc"])
    sortedCodesB <-  unique(obj[with(obj, order(disBcode)), "disBdesc"])

    obj$disAdesc  <- factor(obj $disAdesc  , levels= as.factor(sortedCodesA))
    obj$disBdesc  <- factor(obj $disBdesc  , levels= as.factor(sortedCodesB))

    diseases <- unique(c( as.character(obj$disAdesc), as.character(obj$disBdesc)))
    mydata <- as.data.frame( matrix( ncol=length(diseases), nrow=length(diseases)))
    colnames(mydata) <- diseases[ order(diseases)]
    rownames(mydata) <- diseases[ order(diseases)]

    for( i in 1:ncol(mydata)){

      dis1 <- colnames(mydata)[i]

      for( j in 1:nrow(mydata)){

        dis2 <- rownames(mydata)[j]
        selection <- obj[ obj$disAdesc == dis1 & obj$disBdesc == dis2, ]

        if(nrow(selection) == 0){
          selection <- obj[ obj$disBdesc == dis1 & obj$disAdesc == dis2, ]
        }
        mydata[i,j] <- selection[1,"value"]
      }

    }


    mydata[lower.tri(mydata)] <- NA


    mydata <- as.data.frame(mydata)
    mydata$dis <- rownames( mydata)
    mydata <-(melt(mydata, 'dis', variable_name='variable'))
    mydata$variable <- factor(mydata$variable, levels=rev(levels(mydata$variable)))




    if( interactive == FALSE ){
      p <- ggplot2::ggplot(mydata , ggplot2::aes ( variable, dis ) ) +
        ggplot2::geom_tile(ggplot2::aes(fill = value), colour = "white") +
        ggplot2::scale_fill_gradient(limits = c(n,m), low = lowColor,   high = highColor, na.value = "white") +
        ggplot2::theme_grey(base_size = 13) +
        ggplot2::labs ( x = "ICD9 codes", y = "ICD9 codes") +
        ggplot2::scale_x_discrete(expand = c(0, 0)) +
        ggplot2::geom_text(ggplot2::aes(label = value ) ) +
        ggplot2::theme( plot.margin = grid::unit ( x = c ( 5, 15, 5, 15 ), units = "mm" ),
                        axis.text.x = ggplot2::element_text ( angle = 45, size = 11, hjust = 1 ), panel.background = ggplot2::element_blank() )
      p
    }
  }

}
