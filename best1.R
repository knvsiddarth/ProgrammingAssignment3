# The given document contains out come 4706 observations and 46 variables 
# The required variables hospital, state, and 30 day mortality rate of types of diseases. 

best <- function(state, outcome) {
    # Read outcome of care measures.csv file
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    # presenting required columns in dataframe
    data_store  <- as.data.frame(cbind(outcome_data[, 2],   # Hospital
                                outcome_data[, 7],   # State
                                outcome_data[, 11],  # Heart attack
                                outcome_data[, 17],  # Heart failure
                                outcome_data[, 23]), # Pneumonia
                       stringsAsFactors = FALSE)
    colnames(data_store) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
                                        
    # To check that state and outcome given are valid using %in% operator

    if(!state %in% data_store[, "state"]){
        stop('invalid state')
    } else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop('invalid outcome')
    } else {
        region_name <- which(data_store[, "state"] == state)
        given_metric <- data_store[region_name, ]    # extracting data for the called state
        eval_data <- as.numeric(given_metric[, eval(outcome)])
        min_val <- min(eval_data, na.rm = TRUE)
        result  <- given_metric[, "hospital"][which(eval_data == min_val)]
        output  <- result[order(result)]
    }
return(output)
}