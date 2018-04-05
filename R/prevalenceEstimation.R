#' An new function to visualize and analyze the prevalence of disorders
#' @export prevalenceEstimation


prevalenceEstimation <- function( input, sex, cutOff = 5, visualization = FALSE, fisherTest = TRUE, fisherCutOff = 0.001 ){

  if( length( sex ) == 1){
    input <- input[ input$SEX_CD == sex, ]
    prevalenceData <- as.data.frame( table( input$Phenotype))
    prevalenceData$prevalence <- (as.numeric( prevalenceData$Freq)/length(unique(input$PATIENT_NUM)))*100
    selection <- prevalenceData[ prevalenceData$prevalence >= cutOff, ]

    if( visualization == TRUE){

      ord <- selection[order( -selection [ "prevalence" ] ), "Var1" ]
      selection$Var1 <- factor( selection$Var1, levels= as.factor( ord ) )

      p <- ggplot2::ggplot ( selection, ggplot2::aes ( x = Var1, y = prevalence ), order = ord )  +
        ggplot2::geom_bar ( stat = "identity", fill = "darkcyan" ) +
        ggplot2::labs ( title = "Phenotype prevalence" , x = "phentoype", y = "prevalence (%)")

      p <- p + ggplot2::theme_classic( ) +
        ggplot2::theme( plot.margin = ggplot2::unit ( x = c ( 5, 15, 5, 15 ), units = "mm" ),
                        axis.line = ggplot2::element_line ( size = 1, color = "black" ), text = ggplot2::element_text ( size = 12 ) ,
                        axis.text.x = ggplot2::element_text ( angle = 45, size = 12, hjust = 1 ))

      return( p )
    }

    return( selection )

  }else{
    inputA <- input[ input$SEX_CD == sex[1], ]
    inputB <- input[ input$SEX_CD == sex[2], ]

    inputAprevalence <- as.data.frame( table( inputA$Phenotype))
    inputAprevalence$prevalence <- (as.numeric( inputAprevalence$Freq)/length(unique(inputA$PATIENT_NUM)))*100
    selectionA <- inputAprevalence[ inputAprevalence$prevalence >= cutOff, ]
    selectionA$variable <- sex[1]

    inputBprevalence <- as.data.frame( table( inputB$Phenotype))
    inputBprevalence$prevalence <- (as.numeric( inputBprevalence$Freq)/length(unique(inputB$PATIENT_NUM)))*100
    selectionB <- inputBprevalence[ inputBprevalence$prevalence >= cutOff, ]
    selectionB$variable <- sex[2]

    selectionA <- selectionA[ selectionA$Var1 %in% selectionB$Var1, ]
    selectionB <- selectionB[ selectionB$Var1 %in% selectionA$Var1, ]

    finalDataSet <- rbind( selectionA, selectionB)
    finalDataSet$variable <- as.factor( finalDataSet$variable )

    if( fisherTest == TRUE){

      finalDataSet$fisher <- NA
      for( i in 1:nrow( finalDataSet ) ){
        selection <- finalDataSet[ finalDataSet$Var1 %in% finalDataSet$Var1[ i ], ]
        subsetA <- selection[ selection$variable == sex[1], "Freq"]
        subsetB <- selection[ selection$variable == sex[2], "Freq"]
        statistic <- matrix(c( subsetA, length(unique(inputA$PATIENT_NUM))-subsetA, subsetB, length(unique(inputB$PATIENT_NUM))-subsetB), nrow=2   )
        finalDataSet$fisher[i] <- fisher.test(statistic)$p.value
      }

      finalDataSet <- finalDataSet[ finalDataSet$fisher <= fisherCutOff, ]
    }

    if( visualization == TRUE){
      p <- ggplot2::ggplot(finalDataSet, ggplot2::aes(Var1, prevalence)) +
        ggplot2::geom_bar(ggplot2::aes(fill = variable),
                          position = "dodge",
                          stat="identity",
                          colour = "black")

      p <- p + ggplot2::scale_fill_manual(values=c("darkblue", "orange"))
      p <- p + ggplot2::theme_classic( ) + ggplot2::theme( plot.margin = ggplot2::unit ( x = c ( 5, 15, 5, 15 ), units = "mm" ),
                                                           axis.line = ggplot2::element_line ( size = 0.7, color = "black" ), text = ggplot2::element_text ( size = 14 ) ,
                                                           axis.text.x = ggplot2::element_text ( angle = 45, size = 10, hjust = 1 ))
      return ( p )
    }

    return ( finalDataSet)
  }
}
