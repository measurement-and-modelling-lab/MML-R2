function (number) {

    number <- round(number, 5)
    number[number==0] <- "< 0.00001"
    number[number==1] <- "> 0.99999"

    return(number)

}
