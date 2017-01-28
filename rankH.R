rankhospital <- function(state, outcome, rank = "best"){
   # Read outcome of care measures.csv file
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    # presenting required columns in dataframe
    data_store <- as.data.frame(cbind(outcome_data[, 2],   # Hospital
                                outcome_data[, 7],   # State
                                outcome_data[, 11],  # Heart attack
                                outcome_data[, 17],  # Heart failure
                                outcome_data[, 23]), # Pneumonia
                       stringsAsFactors = FALSE)
    colnames(data_store) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")    
    ## Check that state and outcome are valid
    if (!state %in% data_store[, "state"]) {
        stop('invalid state')
    } else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop('invalid outcome')
    } else if (is.numeric(rank)) {
        region_name <- which(data_store[, "state"] == state)
        given_metric <- data_store[region_name, ]                     # extracting dataframe for the called state
        given_metric[, eval(outcome)] <- as.numeric(given_metric[, eval(outcome)])
        given_metric <- given_metric[order(given_metric[, eval(outcome)], given_metric[, "hospital"]), ]
        output <- given_metric[, "hospital"][rank]
    } else if (!is.numeric(rank)){
        if (rank == "best") {
             output <- best(state, outcome)
        } else if (rank == "worst") {
                region_name <- which(data_store[, "state"] == state)
                given_metric <- data_store[region_name, ]    
                given_metric[, eval(outcome)] <- as.numeric(given_metric[, eval(outcome)])
                given_metric <- given_metric[order(given_metric[, eval(outcome)], given_metric[, "hospital"], decreasing = TRUE), ]
                output <- given_metric[, "hospital"][1]
        } else {
            stop('invalid rank')
        }
    }
return(output)
}