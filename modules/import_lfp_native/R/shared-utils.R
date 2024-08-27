clean_compose_setup <- function(compose_setup, electrodes = "") {
  electrodes <- dipsaus::parse_svec(electrodes)
  if(!is.list(compose_setup) || !length(compose_setup)) { return(list()) }

  new_channels <- NULL
  re <- lapply(compose_setup, function(item) {
    item$number <- as.integer(item$number)
    if(length(item$number) != 1 || is.na(item$number)) { return() }
    if(item$number <= 0) { stop("Invalid channel number: ", item$number) }
    if(item$number %in% c(electrodes, new_channels)) {
      stop("Cannot compose channel ", item$number, ". Reason: conflict with existing channels.")
    }
    from <- dipsaus::parse_svec(item$from)
    if(!length(from)) {
      stop("Cannot compose channel ", item$number, ". Reason: empty list of channels to compose from.")
    }
    if(!all(from %in% electrodes)) {
      from <- from[!from %in% electrodes]
      stop("Cannot compose channel ", item$number, ". Reason: Invalid `from` channel(s): ", dipsaus::deparse_svec(from))
    }
    weights <- item$weights
    if(is.character(weights)) {
      weights <- trimws(unlist(strsplit(weights, ",")))
      weights <- weights[!weights %in% '']
    }
    if(!length(weights)) {
      weights <- 1 / length(from)
    }
    weights <- as.numeric(weights)
    if(length(weights) == 1) {
      weights <- rep(weights, length(from))
    }
    if(length(from) != length(weights)) {
      stop("Cannot compose channel ", item$number, ". Reason: inequal size of `from` channels and weights.")
    }
    if(anyNA(weights) || any(weights == 0)) {
      stop("Cannot compose channel ", item$number, ". Reason: weights must be non-zero numbers.")
    }
    normalize <- isTRUE(item$normalize) || identical(item$normalize, "yes")
    list(
      number = item$number,
      from = from,
      weights = weights,
      normalize = normalize
    )
  })
  re <- dipsaus::drop_nulls(re)
  if(!length(re)) { return(list()) }
  re
}
