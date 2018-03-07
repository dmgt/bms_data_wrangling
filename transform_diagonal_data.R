#' Transform diagonal data to remove NA's
#' 
#' Authors: Dana Miller, Dana Seidel
#' 
#' Transforms a file in this format:
#'                    Time Sensor1 Sensor2 Sensor3
#'    1 2018/03/01 9:01 am     1.1      NA      NA
#'    2 2018/03/01 9:01 am      NA     1.4      NA
#'    3 2018/03/01 9:01 am      NA      NA     0.0
#'    4 2018/03/01 9:02 am     1.3      NA      NA
#'    5 2018/03/01 9:02 am      NA     1.7      NA
#'    6 2018/03/01 9:02 am      NA      NA     1.5
#'
#' Into this format:
#'                    Time Sensor1 Sensor2 Sensor3
#'                    <chr>   <dbl>   <dbl>   <dbl>
#'    1 2018/03/01 9:01 am     1.1     1.4     0.0
#`    2 2018/03/01 9:02 am     1.3     1.7     1.5

#` After passing the following tests: 
#` 1 and 2 - Test if  duplicate values in any row or column
#` If duplicate found, error message includes location of duplicate
#'
#' # Sample data for demonstrating errors
#' # Test with duplicate row
#' a <- c("2018/03/01 9:01 am",
#'       "2018/03/01 9:01 am" ,
#'       "2018/03/01 9:01 am",
#'       "2018/03/01 9:01 am",
#'       "2018/03/01 9:02 am",
#'       "2018/03/01 9:02 am" ,
#'       "2018/03/01 9:02 am")
#' b <- c( 1.1, 4.4, NA, NA, 1.3, NA, NA)
#' c <- c(NA, NA, 1.4, NA, NA, 1.7, NA )
#' d <- c(NA, NA, NA, 0, NA, NA, 1.5)
#' df1 <- bind_cols( Time = a, Sensor1 = b, Sensor2 = c, Sensor3 = d)

#' #Test with duplicate column 

#' e <- c("2018/03/01 9:01 am",
#'       "2018/03/01 9:01 am",
#'       "2018/03/01 9:01 am",
#'       "2018/03/01 9:02 am",
#'       "2018/03/01 9:02 am" ,
#'       "2018/03/01 9:02 am")
#' f <- c(1.1, NA, NA, 1.3, NA, NA)
#' g <- c(1.0, 1.4, NA, NA, 1.7, NA)
#' h <- c(NA, NA, 0, NA, NA, 1.5)

#' df2 <- bind_cols( Time = e, Sensor1 = f, Sensor2 = g, Sensor3 = h)

#' transform_diagonal_data(df1)

#' transform_diagonal_data(df2)
#' 
#' More tests can be added

transform_diagonal_data <- function(x){
    #Test if any row contains more than one numeric value
    count_numeric <- function(x) sum(!is.na(x) && is.numeric(x))
    
    test1 <- x %>%
        rowwise() %>%
        summarise_all(count_numeric) %>%
        rowSums(.) == 1
    
    if(any(test1 == FALSE)){    ##any is because test1 has a value for each row
        return(cat(paste("Error 1: Row", which(test1 == FALSE), # if you remove `return` here, the warning will not kill the process
                         "contains multiple readings\n"))) # using cat means that if multiple rows fail, you will get a unique error for each.
    }
    
    #Test if any time/sensor has more than one numeric value
    test2 <- x %>%
        gather(key = sensor, value = data, - Time) %>%    # tidy format - puts all readings in one column with two parallel column of sensor names and times
        group_by(Time, sensor) %>%     #sets up grouping for next pipe
        na.omit %>%    # count numeric values for each sensor for each timepoint
        tally    # removes rows with NA's, keeping numeric data for each sensor for each timepoint
    if(any(test2$n > 1)){
        return(cat(paste("Error 2:", filter(test2, n > 1) %>% pull(sensor),  #this warning may need to be editted a bit for real data
                         "contains multiple readings for time step",
                         filter(test2, n > 1) %>% pull(Time), "\n")))
    }
    
    # If tests pass, transform and return data!
    df <- x %>%
        group_by(Time) %>%
        summarise_all(funs(sum), na.rm = TRUE)
    return(df)
}




