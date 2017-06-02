#' @title add_hash_column
#' @importFrom digest digest 
#' @importFrom tidyr unite_
#' @importFrom parallel mcmapply
#' @import data.table
#' @description Funkcja dodająca kolumnę hashem (więcej informacji o sposobie tworzenia hasha: \link[digest]{digest}). Hash wyliczany jest na podstawie wartości w polach (kolumnach) wyspecyfikowanych w argumencie `colnames_for_hash` (domyślnie są to wszystkie kolumny w `DT_frame`). Funkcja może mieć zastosowanie przy tworzeniu Primary Key w tabeli, która ma trafić na bazę MySQL
#'
#' @param DT_frame data.table. Ramka, do której dodana zostanie kolumna `hash`.
#' @param hash_colname nazwa kolumny z tworzonym hashem
#' @param colnames_for_hash character vector. Wektor z nazwami kolumn,
#'     które będą brane dodatkowo przy wyliczaniu hasha.
#' @param excluded_colnames character vector. Nazwy kolumn, które NIE zostaną wzięte
#'        do wyliczania kolumny `hash`.
#' @param unite logical; jeśli TRUE, to najpierw tworzy nowa kolumnę przy pomocy 'tidyr::unite_' i dopiero na niej tworzy hash; przy większych ramkach danych ma to znaczenie, bo jeśli TRUE, to działa dużo szybciej
#' @param cores integer; ile corów ma być używanych przy tworzeniu hash'a (działa tylko kiedy unite = TRUE); domyślnie 1
#' @param sort_colnames_for_hash logical; jeśli TRUE, to sortuje wektor z kolumnami
#'
#' @importFrom assertthat is.string
#' @importFrom parallel detectCores
#'
#' @return data.table
#' @export
#' @examples 
#' data <- data.table(a = c("X", "y", "Z"), b = c("a", "a", "Aa"), c = c("q", "w", "e"))
#' dt_hash <- add_hash_column(data, colnames_for_hash = c("a", "b"),
#'                            excluded_colnames = c(), unite = TRUE, cores = 1L,
#'                            sort_colnames_for_hash = TRUE)
add_hash_column <- function(DT_frame, hash_colname = "hash", colnames_for_hash = colnames(DT_frame), excluded_colnames = c(), unite = TRUE, cores = 1L, sort_colnames_for_hash = TRUE ) {
  
  if (!is.string(hash_colname))
    stop("'hash_colname' musi byc klasy stringiem")
  
  if (!is.logical(unite))
    stop("'unite' musi byc wartoscia logiczna")
  
  if (!is.logical(sort_colnames_for_hash))
    stop("'sort_colnames_for_hash' musi byc wartoscia logiczna")
  
  if (!is.integer(cores))
    stop("'cores' musi byc klasy integer")
  
  if (cores > detectCores())
    stop(sprintf("Mozesz uzyc maksymalnie %s rdzeni.", detectCores() ))
  
  if (is.vector(colnames_for_hash) == FALSE)
    stop("'colnames_for_hash' is not a vector.")
  
  if (length(excluded_colnames) > 0 && is.vector(excluded_colnames) == FALSE)
    stop("'excluded_colnames' is not a vector.")
  
  if (not_empty(colnames_for_hash) == FALSE )
    stop("Vector 'colnames_for_hash' is empty.")
  
  if (!is.data.table(DT_frame))
    stop("Error (in function 'add_hash_column'): 'DT_frame' must be a data.table")
  
  weird_colnames <- c( setdiff(colnames_for_hash, colnames(DT_frame)),
                       setdiff(excluded_colnames, colnames(DT_frame)) )
  if (length(weird_colnames))
    stop("Error (in function 'add_hash_column'): column names: (%s) are missing from 'DT_frame'")
  
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
