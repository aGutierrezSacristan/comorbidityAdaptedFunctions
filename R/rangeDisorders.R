#' An adapted function of the rangeDisorders
#' @export rangeDisorders
#'
rangeDisorders <- function( input, sex  ){

  input <- bchQueryData@qresult
  input <- input[ input$patient_sex == sex, c("patient_id","diagnosis_code") ]
  inputNoDuplication <- input[ ! duplicated( input ), ]
  inputPrevalence<- as.data.frame( table( inputNoDuplication$patient_id))


  inputPrevalence$Range <- NA

  for( i in 1:nrow( inputPrevalence ) ){

    if( inputPrevalence$Freq[ i ] < 5 ){

      inputPrevalence$Range[ i ] <- as.character( inputPrevalence$Freq[ i ] )
    }else if( inputPrevalence$Freq[ i ]  >= 5 & inputPrevalence$Freq[ i ] < 10){

      inputPrevalence$Range[ i ] <- "[5, 10)"
    }else if( inputPrevalence$Freq[ i ]  >= 10 & inputPrevalence$Freq[ i ] < 20){

      inputPrevalence$Range[ i ] <- "[10, 20)"
    }else if( inputPrevalence$Freq[ i ]  >= 20 & inputPrevalence$Freq[ i ] < 50){

      inputPrevalence$Range[ i ] <- "[20, 50)"
    }else{
      inputPrevalence$Range[ i ] <- ">= 50"
    }


  }

  output <- as.data.frame( table( inputPrevalence$Range ) )
  output$Percentage <- round( output$Freq / length(unique( input$patient_id))*100, 2)

  desiredOrder <- c("1", "2", "3", "4", "[5, 10)", "[10, 20)", "[20, 50)", ">= 50")
  output <- output[ order( match( output$Var1, desiredOrder)), ]
  colnames( output ) <- c("Number of disorders", "Number of patients", "% of patients")

  return( output )
}
