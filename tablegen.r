function (table, header) {
  
  #Create HTML Table
  
	cat('<table border="1" width=200px>')
    for (i in 1:nrow(table)) {
        if (i == 1 & header == TRUE) {
			cat('<tr>')
			for (i in 1:length(table[1,])) {
				cat('<td align="center">', table[1,i], '</td>', sep="")
			}
			cat('</tr>')

		} else {
			cat('<tr>')
			for (j in 1:length(table[i,])) {
                            if (j == 1) {
                                    if (header == TRUE) {
										cat('<td align="right">', table[i,j], '&nbsp;&nbsp;&nbsp;</td>', sep="")
                                    } else {
										cat('<td>&nbsp;&nbsp;&nbsp;', table[i,j], '</td>', sep="")
                                    }
                            } else {
                                if (header == TRUE) {
									cat('<td align="right">', table[i,j], '&nbsp;&nbsp;&nbsp;</td>', sep="")
                                } else {
									cat('<td align="right">', table[i,j], '&nbsp;&nbsp;&nbsp;</td>', sep="")
                                }
                            }
			}
			cat('</tr>')
		}
	}
	cat('</table>')
}