function(calculation,v1,v2,v3,v4){
    if (calculation %in% c("ci1","ci2")) {


		string <- strsplit(v4, "")[[1]]
		for (char in string) {
			if (!(char=='.' || char%in%c(0:9))) {
				return(NULL)
			}
		}
        for (b in list(v1,v2,v3,v4)) {
            result <- tryCatch({
                is.na(as.numeric(b))
                }, warning = function(w) {
                    'problem'
                }, error = function(e) {
                    'problem'
                }, finally = {
                })
         
            if ('problem' %in% result) {
            return(NULL)
            }
        }

        v1 <- as.numeric(v1)
        if (grepl("\\D", v1)) {
            return(NULL)
        }
        
        v2 <- as.numeric(v2)
        if (grepl("\\D", v2)) {
            return(NULL)
        }

        v3 <- as.numeric(v3)
        if (v3 < 0 || v3 > 1) {
            return(NULL)
        }

		v4 <- as.numeric(v4)
		if (v4 < 0 || v4 > 1) {
			return(NULL)
		}
    }
    
    if (calculation == "pa") {
        for (b in list(v1,v2,v3,v4)) {
            result <- tryCatch({
                is.na(as.numeric(b))
                }, warning = function(w) {
                    'problem'
                }, error = function(e) {
                    'problem'
                }, finally = {
                })
         
            if ('problem' %in% result) {
            return(NULL)
            }
        }
        
        v1 <- as.numeric(v1)
        if (grepl("\\D", v1)) {
            return(NULL)
        }
        
        v2 <- as.numeric(v2)
        if (grepl("\\D", v2)) {
            return(NULL)
        }

        v3 <- as.numeric(v3)
        if (v3 < 0 || v3 > 1) {
            return(NULL)
        }
        
        v4 <- as.numeric(v4)
        if (v4 < 0 || v4 > 1) {
            return(NULL)
        }
    }
    
    if (calculation == "ssc") {
        for (b in list(v1,v2,v3,v4)) {
            result <- tryCatch({
                is.na(as.numeric(b))
                }, warning = function(w) {
                    'problem'
                }, error = function(e) {
                    'problem'
                }, finally = {
                })
         
            if ('problem' %in% result) {
            return(NULL)
            }
        }
        
        v1 <- as.numeric(v1)
        if (grepl("\\D", v1)) {
            return(NULL)
        }
        
        v2 <- as.numeric(v2)
        if (v2 < 0 || v2 > 1) {
            return(NULL)
        }

        v3 <- as.numeric(v3)
        if (v3 < 0 || v3 > 1) {
            return(NULL)
        }
        
        v4 <- as.numeric(v4)
        if (v4 < 0 || v4 > 1) {
            return(NULL)
        }
    }

    return(TRUE)
}
