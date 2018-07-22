## Simultaneously require that several input$ variables exist before moving on
massValidateNeed <- function(...) {
    arguments <- list(...)
    for (a in arguments) {
        validate(need(a, ""))
    }
}


## Check that the input doesn't have too many digits
areShort <- function(...) {
    arguments <- list(...)
    for (a in arguments) {
        if (nchar(a) > 7) {
            stop("Input cannot be more than 7 digits long.")
        }
    }
}


## Determine whether several inputs are integers
areIntegers <- function(...) {
    arguments <- list(...)
    for (a in arguments) {
        if (grepl("\\D", a) || grepl("\\D", a)) {
            stop("A decimal number was given in place of an integer.")
        }
    }
}


## Determine whether a number is between 0 and 1 inclusive
areBetween0And1 <- function(...) {
    arguments <- list(...)
    for (a in arguments) {
        if (a < 0 || a > 1) {
            stop("A value that should be bounded by 0 and 1 is not.")
        }
    }
}
