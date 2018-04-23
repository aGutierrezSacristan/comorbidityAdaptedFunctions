#' A new function to analyze if a phenotype is diagnosed before or after ASD
#' @export beforeAfterASD

beforeAfterASD <- function( input, autismCodes, phenotypeList ){


  asdDAta <- input[ input$code %in% autismIcd9codes, c("PATIENT_NUM", "BIRTH_DATE", "SEX_CD", "Phenotype", "START_DATE") ]
  sortasdData <- asdDAta[order(asdDAta$PATIENT_NUM,asdDAta$START_DATE,decreasing=FALSE),]
  sortasdData <- sortasdData[! duplicated( sortasdData$PATIENT_NUM ), ]

  outputTable <- data.frame(matrix(vector(), length(unique(phenotypeList)), 5))
  colnames( outputTable ) <- c("phenotype", "female_fromPheno2ASD", "female_fromASD2Pheno", "male_fromPheno2ASD", "male_fromASD2Pheno")

  for( cont in 1:length(unique( phenotypeList ) ) ){


    phenoCodeData <- input[ input$Phenotype == phenotypeList[ cont ], c("PATIENT_NUM", "BIRTH_DATE", "SEX_CD", "Phenotype", "START_DATE") ]

    sortphenoData <- phenoCodeData[order(phenoCodeData$PATIENT_NUM,phenoCodeData$START_DATE,decreasing=FALSE),]
    sortphenoData <- sortphenoData[! duplicated( sortphenoData$PATIENT_NUM ), ]

    commonPatients <- unique(sortphenoData[ sortphenoData$PATIENT_NUM %in% sortasdData$PATIENT_NUM, "PATIENT_NUM"])


    outputDirection <- data.frame(matrix(vector(), length(unique(commonPatients)), 3))
    colnames( outputDirection ) <- c("patient", "gender", "direction")


    for( i in 1:length( commonPatients ) ){

      selectionPhen <- sortphenoData[ sortphenoData$PATIENT_NUM == commonPatients[ i ], ]
      selectionASD  <- sortasdData[ sortasdData$PATIENT_NUM == commonPatients[ i ], ]

      if(  as.numeric((as.Date(selectionPhen$START_DATE) - as.Date(selectionASD$START_DATE) ) ) > 0  ){
        outputDirection[i, ] <- c( selectionPhen$PATIENT_NUM, selectionPhen$SEX_CD,  "ASD -> ADHD")
      }else{
        outputDirection[i, ] <- c( selectionPhen$PATIENT_NUM, selectionPhen$SEX_CD,  "ADHD -> ASD")
      }
    }

    outputDirectionFemale <- outputDirection[ outputDirection$gender == "F", ]
    summary(as.factor( outputDirectionFemale$direction ) )

    outputDirectionMale   <- outputDirection[ outputDirection$gender == "M", ]
    summary(as.factor( outputDirectionMale$direction ) )


    outputTable[cont, ] <- c( phenotypeList[ cont ],
                           summary(as.factor( outputDirectionFemale$direction ) )[1],
                           summary(as.factor( outputDirectionFemale$direction ) )[2],
                           summary(as.factor( outputDirectionMale$direction ) )[1],
                           summary(as.factor( outputDirectionMale$direction ) )[2]
                           )

  }

  outputTable$totalFemale <- as.numeric( outputTable$female_fromPheno2ASD ) + as.numeric( outputTable$female_fromASD2Pheno )
  outputTable$totalMale <-  as.numeric( outputTable$male_fromPheno2ASD ) + as.numeric( outputTable$male_fromASD2Pheno )
  outputTable$female_fromPheno2ASD_percentage <- round( as.numeric( outputTable$female_fromPheno2ASD ) / as.numeric( outputTable$totalFemale)*100, 2)
  outputTable$female_fromASD2pheno_percentage <- round( as.numeric( outputTable$female_fromASD2Pheno ) / as.numeric( outputTable$totalFemale)*100, 2)
  outputTable$male_fromPheno2ASD_percentage   <- round( as.numeric( outputTable$male_fromPheno2ASD ) / as.numeric( outputTable$totalMale)*100, 2)
  outputTable$male_fromASD2pheno_percentage   <- round( as.numeric( outputTable$male_fromASD2Pheno ) / as.numeric( outputTable$totalMale)*100, 2)


  return( outputTable )


}
