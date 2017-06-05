#' @title add_hash_column
#' @importFrom digest digest 
#' @importFrom tidyr unite_
#' @importFrom parallel mcmapply
#' @importFrom assertthat is.string
#' @importFrom parallel detectCores
#' @import data.table
#' @description adds hash column (more info: \link[digest]{digest}). Hash is calculated on columne specified in `colnames_for_hash` argument (all columns by default). Function may be useful for creating Primary Key column (e.g. in MySQL )
#'
#' @param DT_frame data.table. 
#' @param hash_colname character; name of hash column
#' @param colnames_for_hash character vector; column names to create hash
#' @param excluded_colnames character vector; column names NOT to create hash
#' @param unite logical; if TRUE, then firstly uses 'tidyr::unite_' on 'colnames_for_hash' and only then creates hash. It matter when data.table is big frame, because it is much faster
#' @param cores integer; number of cores to create hash (works only when unite = TRUE); default: 1
#' @param sort_colnames_for_hash logical; if TRUE, sorts columns alphabetically before hash creation
#'
#' @return data.table
#' @export
#' @examples 
#' data <- data.table(a = c("X", "y", "Z"), b = c("a", "a", "Aa"), c = c("q", "w", "e"))
#' dt_hash <- add_hash_column(data, colnames_for_hash = c("a", "b"),
#'                            excluded_colnames = c(), unite = TRUE, cores = 1L,
#'                            sort_colnames_for_hash = TRUE)
add_hash_column <- function(DT_frame, hash_colname = "hash", colnames_for_hash = colnames(DT_frame), excluded_colnames = c(), unite = TRUE, cores = 1L, sort_colnames_for_hash = TRUE ) {
  
  if (!is.data.table(DT_frame))
    stop("'DT_frame' is not a data.table")
  
  if (!is.string(hash_colname))
    stop("'hash_colname' is not a string")
  
  if (!is.logical(unite))
    stop("'unite' is not a logical value")
  
  if (!is.logical(sort_colnames_for_hash))
    stop("'sort_colnames_for_hash' is not a logical value")
  
  if (!is.integer(cores))
    stop("'cores' is not an integer")
  
  if (cores > detectCores())
    stop(sprintf("The number of cores is unavailable (max. %s)", detectCores() ))
  
  if (is.vector(colnames_for_hash) == FALSE | is.list(colnames_for_hash) == T)
    stop("'colnames_for_hash' is not a vector.")
  
  if ((length(excluded_colnames) > 0 && is.vector(excluded_colnames) == FALSE) | is.list(excluded_colnames) == T )
    stop("'excluded_colnames' is not a vector.")
  
  if (not_empty(colnames_for_hash) == FALSE )
    stop("Vector 'colnames_for_hash' is empty.")
  
  weird_colnames <- c( setdiff(colnames_for_hash, colnames(DT_frame)),
                       setdiff(excluded_colnames, colnames(DT_frame)) )
  if (length(weird_colnames))
    stop(sprintf("Column names: (%s) are missing in 'DT_frame'", paste0(weird_colnames, collapse = ", ")) )
  
  # pozbywam się (potencjalne) kolumn nie do hasha
  colnames_for_hash <- setdiff(colnames_for_hash, excluded_colnames)
  
  # sortuje wektor kolumn do hasha
  if (sort_colnames_for_hash == TRUE)
    colnames_for_hash <- sort(colnames_for_hash)
  
  if (unite == FALSE) {
    DT_frame[, eval(copy(hash_colname)) := digest(.SD[, colnames_for_hash, with = FALSE]), by = 1:nrow(DT_frame)]
  } else {
    DT_frame <- copy( unite_(DT_frame, col = hash_colname, from = colnames_for_hash, sep = "___", remove = FALSE) )
    
    DT_frame[, eval(copy(hash_colname)) := mcmapply(digest, get(hash_colname), mc.cores = getOption("mc.cores", cores)) ]
  }
  
  DT_frame
}
