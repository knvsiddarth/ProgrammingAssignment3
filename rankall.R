# the program consist of four parts to explain
# first is just reading the .csv and assimilating it required in dataframe
# second part checks if the value is given to show the rank.
# thrid part shows the best hopitals allover 
# fourth part shows the worst hospitals.

rankall <- function(outcome, value = "best"){
    ## Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    data_store   <- as.data.frame(cbind(outcome_data[, 2],  # hospital
                                outcome_data[, 7],  
                                outcome_data[, 11], 
                                outcome_data[, 17],  
                                outcome_data[, 23]), 
                          stringsAsFactors = FALSE)
    colnames(data_store) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")

    data_store[, eval(outcome)] <- as.numeric(data_store[, eval(outcome)])
    
    ## Check that state and outcome are valid
    ## if the rank is number to rank it 
    
    if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop('invalid outcome')
    } else if (is.numeric(value)) {

        # split the dataframe by state to rank hospitals
        in_region <- with(data_store, split(data_store, state)) 

        # to display them as list using for loop
        odr_list <- list()
        for (j in seq_along(in_region)){

            in_region[[j]] <- in_region[[j]][order(in_region[[j]][, eval(outcome)], 
                                                 in_region[[j]][, "hospital"]), ]

            odr_list[[j]]  <- c(in_region[[j]][value, "hospital"], in_region[[j]][, "state"][1])
        }

        #to put the results in the variable
        final_result <- do.call(rbind, odr_list)

        output <- as.data.frame(final_result, row.names = final_result[, 2], stringsAsFactors = FALSE)

        names(output) <- c("hospital", "state")

    } else if (!is.numeric(value)) {

        # it is the evaluating when you rank the outcome by the least 30 day Mortailty rate 
        if (value == "best") {

            in_region <- with(data_store, split(data_store, state))

            odr_list <- list()

            for (j in seq_along(in_region)){
                in_region[[j]] <- in_region[[j]][order(in_region[[j]][, eval(outcome)], 
                                                     in_region[[j]][, "hospital"]), ]
                ord_list[[j]]  <- c(in_region[[j]][1, c("hospital", "state")])
            }

            final_result <- do.call(rbind, ordered)

            output <- as.outcome_data.frame(final_result, stringsAsFactors = FALSE)

            rownames(output) <- output[, 2]
        } else if (value == "worst") {

         # it is the evaluating when you rank the outcome by the highest 30 day Mortailty rate 
            in_region <- with(data_store, split(data_store, state))

            odr_list <- list()

            for (j in seq_along(in_region)){

                in_region[[j]] <- in_region[[j]][order(in_region[[j]][, eval(outcome)], 
                                                     in_region[[j]][, "hospital"], 
                                                     decreasing = TRUE), ]

               ord_list[[j]]  <- c(in_region[[j]][1, c("hospital", "state")])
            }

            final_result <- do.call(rbind, ordered)

            output <- as.data.frame(final_result, stringsAsFactors = FALSE)

            rownames(output) <- output[, 2]
        } else {
            stop('invalid value')
        }
    }
return(output)
}