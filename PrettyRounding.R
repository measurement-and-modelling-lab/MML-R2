function (number, digits) {

    rounded <- round(number, 5)

    if (rounded == 0) {
        format(number, scientific=TRUE)
    } else if (rounded == 1) {
        substr(number, 1, 7)
    } else {
        rounded
    }

}
