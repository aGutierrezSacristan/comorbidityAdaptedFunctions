#' An new function to visualize and analyze the age of the first diagnosis given a list of disorders
#' @export ageFirstDiagnosis


ageFirstDiagnosis <- function( input, querypatients, diseaseList, sex, cutOff = 5, visualization = FALSE, fisherTest = TRUE, fisherCutOff = 0.001 ){

  querypatients <- querypatients@qresult
  input <- input[ input$PATIENT_NUM %in% querypatients$patient_id, ]
  input <- input[, c("PATIENT_NUM","SEX_CD", "code", "BIRTH_DATE", "START_DATE")]
  input <- input[ input$code %in% diseaseList, ]
  input <- input[ ! duplicated( input ), ]

  input$AGE <- as.numeric((as.Date(input$START_DATE) - as.Date(input$BIRTH_DATE)))%/%365
  sortTable <- input[order(input$PATIENT_NUM,input$START_DATE,decreasing=FALSE),]

  sortTable <- sortTable[! duplicated( sortTable$PATIENT_NUM), ]

  if( length( sex ) == 1){
    subset <- sortTable[ sortTable$SEX_CD == sex, ]
    prevalenceData <- as.data.frame( table( subset$AGE ) )
    prevalenceData$prevalence <- round((as.numeric( prevalenceData$Freq)/length(unique(subset$PATIENT_NUM)))*100, 2)
    selection <- prevalenceData[ prevalenceData$prevalence >= cutOff, ]


    if( visualization == TRUE){

      ord <- as.factor( 0 : 18 )
      selection$Var1 <- factor( selection$Var1, levels= as.factor( ord ) )

      p <- ggplot2::ggplot ( selection, ggplot2::aes ( x = Var1, y = prevalence ), order = ord )  +
        ggplot2::geom_bar ( stat = "identity", fill = "darkcyan" ) +
        ggplot2::labs ( title = "Age when patients first diagnosed with ASD" , x = "Age", y = "prevalence (%)")

      p <- p + ggplot2::theme_classic( ) +
        ggplot2::theme( plot.margin = ggplot2::unit ( x = c ( 5, 15, 5, 15 ), units = "mm" ),
                        axis.line = ggplot2::element_line ( size = 1, color = "black" ), text = ggplot2::element_text ( size = 12 ) ,
                        axis.text.x = ggplot2::element_text ( angle = 45, size = 12, hjust = 1 ))

      return( p )
    }

    return( selection )

  }else{
    inputA <- sortTable[ sortTable$SEX_CD == sex[1], ]
    inputB <- sortTable[ sortTable$SEX_CD == sex[2], ]

    inputAprevalence <- as.data.frame( table( inputA$AGE))
    inputAprevalence$prevalence <- round( (as.numeric( inputAprevalence$Freq)/length(unique(inputA$PATIENT_NUM)))*100, 2)
    selectionA <- inputAprevalence[ inputAprevalence$prevalence >= cutOff, ]
    selectionA$variable <- sex[1]

    inputBprevalence <- as.data.frame( table( inputB$AGE))
    inputBprevalence$prevalence <- round((as.numeric( inputBprevalence$Freq)/length(unique(inputB$PATIENT_NUM)))*100,2)
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

      ord <- as.factor( 0 : 18 )
      finalDataSet$Var1 <- factor( finalDataSet$Var1, levels= as.factor( ord ) )

      p <- ggplot2::ggplot(finalDataSet, ggplot2::aes(Var1, prevalence), order = ord ) +
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
