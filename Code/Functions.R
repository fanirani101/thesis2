## FUNCTIONS

input_manage <- function(fp,j){
  # Function to remove NA rows from the data frame "set", i.e. if a student is enrolled but does not take
  ## either the first or resit exams.
  # 
  # Returns the jth column of the data frame after the NA rows are removed
  
  if(missing(j)){
    j<-2
  }
  
  set <- as.data.frame(read_excel(fp, col_names = TRUE))
  c2 <- SplitPath(fp)$filename
  
  k = 1
  out_set <- as.data.frame(set[,c(1,j)])
  d <- data.frame()
  for (i in 1:dim(set)[1]){
    if (is.na(set[,j])[i] == TRUE){
      d[k,1] <- i
      k <- k+1
      }
  }
  if (sum(dim(d)) == 0){
    out_set <- set
  } else{
    out_set <- as.data.frame(out_set[-as.matrix(d),])
  }
  names(out_set) <- names(set)[c(1,j)]
  out_set <- out_set[order(out_set[,2],decreasing = TRUE),]
  names(out_set) <- c("Username", c2)
  return(out_set)
} 

update_set <- function(early_set, later_set, name, by_name){
  # Function to merge the first exam and resit exam results, then ensure that the student 
  ## is awarded the highest of the two exam grades using the merged data set, and then finally 
  ## the resit grades (of those who didn't take the first exam) is added. This creates a final
  ## exam grade set for the course.
  # 
  # The input requirements are two column data frames, namely the first exam and the resit, 
  ## respectively, the requested output name of the column, and the name of the column by 
  ## which to merge by.
  #early_set <- StatR_1819_first
  #later_set <- StatR_1819_resit
  #name <- "Calculus 2 1819"
  #by_name <- "Username"
  if (dim(early_set)[1] == 0 ){final <- later_set}
  else if (dim(later_set)[1] == 0){final <- early_set}
  else{
    m <- merge(early_set, later_set, by = c(by_name), all=TRUE)
    grade <- as.matrix(pmax(m[,2], m[,3], na.rm=TRUE))
    usr <- as.matrix(m[1])
    final <- as.data.frame(cbind(usr, grade))
  }
  names(final)[2] <- name
  final <- final[order(final[,2],decreasing = TRUE),]
  row.names(final) <- seq(dim(final)[1])
  return(final)
}

grade_getr <- function(L1, L2, s){
  # This function takes the list of years (L1), a list of courses (L2) and a string (s),
  ## and outputs a list of data frames with the name <course>_<year>_<string>, e.g. analysis_1617_final
  
  if (!require(stringr)) install.packages("stringr")
  library(stringr)
  
  all_names <- (Map( function(y) Map(function(x) str_c(y, x, sep="_"), L1), L2))
  all_names <- unlist(Map(function(x) str_c(x, "_", s), all_names))
  
  L <- list()
  i <- 1
  for (x in all_names){
    try(
      {L[[i]] <- get(x)
      i <- i+1},
      silent=TRUE
    )
  }
  return(L)
}

max_grade_getr <- function(L1, df){
  # This function calculates the maximum grade across all years for the
  ## courses given in a list (L1), in a data frame (df).
  
  if (!require(stringr)) install.packages("stringr")
  library(stringr)
    
  L2 <- list()
  i = 1
  for (y in L1){
    df2 <- data.frame(matrix(NA, ncol=2, nrow=dim(df)[1]))
    names(df2) <- c("Username", str_to_title(y))
    df2[,1] <- df[,1]
    for (k in 1:dim(df2)[1]){
      if (all(is.na(df[k, grep(y, names(df), ignore.case= TRUE)])) == TRUE){
        df2[k,2] <- max(as.numeric(df[k, grep(y, names(df), ignore.case= TRUE)]), na.rm=FALSE)
      }
      else {
        df2[k,2] <- max(as.numeric(df[k, grep(y, names(df), ignore.case= TRUE)]), na.rm=TRUE)
      }
    }
    df2 <- df2[complete.cases(df2),]
    L2[[i]] <- df2
    i <- i+1
  }
  
  df_out <- Reduce(function(x, y) merge(x, y, by="Username", all=TRUE), L2)
  
  return(df_out)
}

cohort_split <- function(df, ss){
  # This function takes a data frame (df) of student grades, where the year in 
  ## which a grade is received is visible in the column names of the data frame, 
  ## and splits the data frame into cohort groups based on which year the 
  ## student started studying. 
  #
  # The user defined string (ss) dictates the first year courses all students 
  ## must complete, and helps distinguish students' starting year.
  
  if (!require(stringr)) install.packages("stringr")
  library(stringr)
  
  s <- unique(unlist(str_split(names(df), " ")))
  s <- unique(unlist(strsplit(trimws(gsub("([A-Za-z]*)([0-9]*)", "\\1 \\2", s)), " ")))
  
  years <- sort(unlist(regmatches(s, gregexpr("\\d{4}", s))))
  start_year <- min(years)
  
  L = list()
  for (y in years){
    dff <- df
    i <- match(y, years)
    # This if-statement removes those students who were examined in previous 
    ## years
    if (y != min(years)){
      k <- match(y, years)
      dff <- dff[apply(
          is.na(
          dff[, (
            # This matches the first-year course names...
            grepl(paste(ss, collapse="|"), ignore.case=TRUE, names(dff)) &
            ## intersected with the previous years.
            grepl(paste(years[seq(1,k-1)], collapse="|"), ignore.case=TRUE, names(dff)) 
            ) , drop=FALSE ]
          ), 1, function(x) all(x)
        ), , drop=FALSE ]
    }
    # After omitting students who started earlier, we isolate those students who
    ## took starting courses (ss) in the year (y)
    dff <- dff[!apply(dff[, 
                          (grepl(paste(ss, collapse="|"), ignore.case=TRUE, names(dff)) &
                          grepl(y, names(dff))) , drop=FALSE
                          ], 1, function(x) all(is.na(x))), ]
    # This removes any completely NA columns
    dff <- dff[, !apply(dff, 2, function(x) all(is.na(x))) , drop=FALSE]
    # This names the list element with the respective cohort year and stores 
    ## the data frame.
    L[[paste("Cohort", y, collapse="_")]] <- dff
  }
  return(L)
}

max_grade_getr2 <- function(L1, df){
  # This function calculates the maximum grade across all years for the
  ## courses given in a list (L1), in a data frame (df).
  
  if (!require(stringr)) install.packages("stringr")
  library(stringr)
  
  s1 <- trimws(str_to_title(unique(str_extract(unlist(L1), "[A-Za-z]*[ ]?[0-9]?"))))
  names_s <- str_extract(gsub("\\d{4}", "", gsub("[.]", " ", unlist(names(df)))), "[A-Za-z]*[ ]?[0-9]?")
  s2 <- trimws(str_to_title(unique(names_s)))
  L_int <- intersect(s1, s2)
  
  L2 <- list()
  i = 1
  for (y in L_int){
    df2 <- data.frame(matrix(NA, ncol=2, nrow=dim(df)[1]))
    names(df2) <- c("ID", str_to_title(y))
    row.names(df2) <- row.names(df)
    df2[,1] <- row.names(df2)
    for (k in 1:dim(df2)[1]){
      if (all(is.na(df[k, grep(y, names_s, ignore.case= TRUE)])) == TRUE){
        df2[k,2] <- max(as.numeric(df[k, grep(y, names_s, ignore.case= TRUE)]), na.rm=FALSE)
      }
      else {
        df2[k,2] <- max(as.numeric(df[k, grep(y, names_s, ignore.case= TRUE)]), na.rm=TRUE)
      }
    }
    df2 <- na.omit(df2)
    L2[[i]] <- df2
    i <- i+1
  }
  
  df_out <- Reduce(function(x, y) merge(x, y, by="ID", all=TRUE), L2)[, -1]
  
  return(df_out)
}

pass_fail_split <- function(L, pg = 5.5){
  # This function takes a list (L) of data frames, where each data frame is 
  ## separated into two new data frames, i.e. a "pass" and a "fail" data frame. 
  ## Those who pass have a grade greater than or equal to the pass grade (pg; 
  ## default pg = 5.5).
  
  # L <- L_2
  
  x <- length(L)
  
  for (k in seq(1,x)){
    df <- L[[k]]
    df <- cbind(data.frame(Pass = rep(0, dim(df)[1])), df)
    
    for (i in seq(1,dim(df)[1])){
      if (all(df[i, -1] >= pg, na.rm=TRUE)){
        df$Pass[i] <- 1
      }
    }
    L[[k]] <- df
  }
  
  return(L)
}

cor_mat_fun <- function(data){
  # This function adds significance stars to the correlation p-value matrix 
  ## produced by the functions in the ppcor package.
  
  x <- data
  
  new_p <- matrix(sapply(x$p.value, parameters::format_p, stars = TRUE, digits = 3), dim(x$p.value)[1], dim(x$p.value)[2])
  attributes(new_p) <- attributes(x$p.value)
  new_p <- gsub("[p = ]", "", new_p)
  new_p[upper.tri(new_p, diag=TRUE)] <- ""
  
  x$p.value <- new_p
  
  return(x)
}

mice_select <- function(df){
  # This function manipulates the predictor function used in the MICE function to complete the data frame. 
  
  if (!require(mice)) install.packages("mice")
  library(mice)
  if (!require(ppcor)) install.packages("ppcor")
  library(ppcor)
  
  # df <- cohort_1718_fail
  
  while (dim(cc(df))[1] <= dim(cc(df))[2]){
    x <- md.pattern(df)
    n <- dim(x)[1]
    m <- dim(x)[2]
    j <- grep(dimnames(x)[[2]][m-1], names(df), ignore.case = TRUE)
    df <- df[, -j]
  } 
  
  while (dim(cc(df))[1] <= 100){
    init_cor <- cor_mat_fun(spcor(cc(df), method="kendall"))$statistic
    
    L <- list()
    
    for (j in seq(1, dim(cc(df))[2])){
      L[[j]] <- cor_mat_fun(spcor(cc(df[,-j]), method="kendall"))$statistic
      names(L)[j] <- names(df)[j]
      L[[j]] <- L[[j]] - init_cor[-j,][,-j]
      for (i in seq(1, dim(L[[j]])[1])){
        for (k in seq(1, dim(L[[j]])[2])){
          L[[j]][i,k] <- L[[j]][i,k] ^ 2
        }
      }
      L[[j]] <- sum(L[[j]]) / (sum(dim(L[[j]])[1] + dim(L[[j]])[2]) - 1)
    }
    
    L <- unlist(L)
    
    j <- Position(function(x) x== min(L), L)
    
    df <- df[,-j]
    
    
  }
  
  return(df)
}
