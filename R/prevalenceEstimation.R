#' An new function to visualize and analyze the prevalence of disorders
#' @export prevalenceEstimation


prevalenceEstimation <- function( input, sex, cutOff = 5, visualization = FALSE, fisherTest = TRUE, fisherCutOff = 0.001 ){

  input <- input@qresult

  if( length( sex ) == 1){
    input <- input[ input$patient_sex == sex, c("patient_id", "diagnosis_code")]
    input <- input[! duplicated( input ) , ]
    prevalenceData <- as.data.frame( table( input$diagnosis_code))
    prevalenceData$prevalence <- (as.numeric( prevalenceData$Freq)/length(unique(input$patient_id)))*100
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

    selection <- selection[ order(-selection$prevalence),]

    return( selection )

  }else{
    inputA <- input[ input$patient_sex == sex[1],  c("patient_id", "diagnosis_code")]
    inputB <- input[ input$patient_sex == sex[2],  c("patient_id", "diagnosis_code")]

    inputA <- inputA[! duplicated( inputA ) , ]
    inputB <- inputB[! duplicated( inputB ) , ]

    inputAprevalence <- as.data.frame( table( inputA$diagnosis_code))
    inputAprevalence$prevalence <- (as.numeric( inputAprevalence$Freq)/length(unique(inputA$patient_id)))*100
    selectionA <- inputAprevalence[ inputAprevalence$prevalence >= cutOff, ]
    selectionA$variable <- sex[1]

    inputBprevalence <- as.data.frame( table( inputB$diagnosis_code))
    inputBprevalence$prevalence <- (as.numeric( inputBprevalence$Freq)/length(unique(inputB$patient_id)))*100
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
        statistic <- matrix(c( subsetA, length(unique(inputA$patient_id))-subsetA, subsetB, length(unique(inputB$patient_id))-subsetB), nrow=2   )
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
