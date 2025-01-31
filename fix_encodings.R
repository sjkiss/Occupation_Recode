FixEncoding <- function() {
  # create the unicode ranges from https://www.i18nqa.com/debug/utf8-debug.html
  range <- c(sprintf("%x", seq(strtoi("0xa0"), strtoi("0xff"))))
  unicode <- vapply(range, FUN.VALUE = character(1), function(x) { parse(text = paste0("'\\u00", x, "'"))[[1]] })
  # add the ones that are missing (red ones in https://www.i18nqa.com/debug/utf8-debug.html)
  unicode <- c(c("\u0168", "\u0152", "\u017d", "\u0153", "\u017e", "\u0178", "\u2019", "\u20ac", "\u201a", "\u0192", "\u201e", "\u2026", "\u2020", "\u2021", "\u02c6", "\u2030", "\u0160", "\u2030"), unicode)
  once <- vapply(unicode, FUN.VALUE = character(1), function(x) { 
    Encoding(x) <- "Windows-1252"
    iconv(x, to = "UTF-8")
  })
  fix_once <- unicode
  names(fix_once) <- once
  twice <- vapply(once, FUN.VALUE = character(1), function(x) { 
    Encoding(x) <- "Windows-1252"
    iconv(x, to = "UTF-8")
  })
  fix_twice <- unicode
  names(fix_twice) <- twice
  triple <- vapply(twice, FUN.VALUE = character(1), function(x) { 
    Encoding(x) <- "Windows-1252"
    iconv(x, to = "UTF-8")
  })
  fix_triple <- unicode
  names(fix_triple) <- triple
  fixes <- c(fix_triple, fix_twice, fix_once)
  return(fixes)
}

fixes <- FixEncoding()