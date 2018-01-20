function (table, header) {

    cat('<table border="1" bordercolor="808080"')
    for (i in 1:nrow(table)) {
        cat('<tr>')
        for (j in 1:length(table[i,])) {
          cat('<td align="center">', table[i,j], '</td>', sep="")
        }
        cat('</tr>')
    }
	 cat('</table>')
}