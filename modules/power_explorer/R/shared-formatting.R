
pretty_round <- function(x, allow_negative_round=FALSE) {
  round(x, get_pretty_digits(x, allow_negative_round = allow_negative_round))
}

get_pretty_digits <- function(x, allow_negative_round=FALSE) {
  max_x <- max(abs(x))

  dig = 0

  if(allow_negative_round && max_x > 100 ) {
    dig = -1
  } else if(max_x < 1) {
    dig = abs(floor(log10(max_x)))
  }

  dig
}
