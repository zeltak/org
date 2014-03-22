# function to assign closest
# merge data by day to the closest of K nearest neighbors (with a distance constraint)
# could be better (not require merge for left join afterwards)
# see: http://stackoverflow.com/a/12450918
nearestbyday <- function(matrix1, matrix2, dt1, dt2, dt1varname, dt2varname, 
                         closestname = "closestmet", varstoget = "avewsp", 
                         knearest = 5, maxdistance = NA){
  require(FNN)
  knearest <- min(knearest, nrow(matrix2))
  knnname <- paste0(closestname, "knn")
  nobsname <- paste0(closestname, "nobs")
  # calculate nearest neighbors using package FNN
  knn_store <- get.knnx(matrix2, matrix1, k = knearest)
  # restrict by distance
  if(!is.na(maxdistance)){
    knn_store[["nn.dist"]][knn_store[["nn.dist"]] > maxdistance] <- NA
    knn_store[["nn.index"]] <- knn_store[["nn.index"]] * (knn_store[["nn.dist"]] * 0 + 1)
  }
  # store the indices for nearest neighbors in a long DT
  knn_out <- data.table(matrix(knn_store[["nn.index"]])) 
  knn_out[, dt1varname := rep(rownames(matrix1), knearest), with = F]
  knn_out[, closestname := as.character(row.names(matrix2[knn_out[, V1],])), with = F]
  knn_out[, V1 := NULL]
  knn_out[, knnname := rep(1:knearest, each = nrow(matrix1)), with = F]
  # drop points not within maxdistance
  knn_out <- knn_out[!is.na(get(closestname))]
  # use setkeyv to pass a column by name
  setkeyv(knn_out, closestname)
  setnames(dt2, dt2varname, closestname)
  # if not character - coerce
  if(class(dt2[,closestname,with = F][[1]]) != "character"){
    dt2[, closestname := as.character(closestname), with = F]
  }
  setkeyv(dt2, closestname)
  # lengthen dt2 with every possible site each day might match
  # after dropping missing observations
  dt2long <- dt2[!is.na(get(varstoget))][knn_out, allow.cartesian = T]
  # store the number of valid observations
  dt2long[, nobsname := .N, by=c(dt1varname,"day"), with = F]
  setkeyv(dt2long, cols = c(dt1varname, "day", knnname))
  # join to itself (set1 and day are keys) and take first record for fast selection
  closestvar <- dt2long[unique(dt2long[,c(dt1varname, "day"), with = F]), mult = "first"]
  gc()# clear memory
  # put the name back in dt2
  setnames(dt2, closestname, dt2varname)
  # inspect our result
  print(tables(silent = T)[NAME == "closestvar"])
  # return it silently
  invisible(closestvar)
}


makepointsmatrix <- function(datatable, xvar, yvar, idvar) {
  dtnames <- names(datatable)
  unique.dt <- unique(datatable[,c(xvar,yvar, idvar), with = F])
  out.m <- as.matrix(unique.dt[,c(xvar,yvar), with = F])
  dimnames(out.m)[[1]] <- unique.dt[,idvar, with = F][[1]]
  invisible(out.m)
}