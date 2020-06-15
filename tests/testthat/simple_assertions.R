# some functions used only in tests
# (these are not documented, hopefully meaning is clear from name and code)


simple_pass <- function(x) {
  # always passes
  x
}

simple_is_character <- function(x) {
  if (!is(x, "character"))
    stop("not character")
  x
}

# using TRUE / character output
simple_character_check <- function(x) {
  if (!is(x, "character"))
    return("not character")
  TRUE
}

simple_is_len1 <- function(x) {
  if (length(x)!=1)
    stop("not length 1")
  x
}

# one additional argument
simple_above <- function(x, threshold=0) {
  if (!all(x>threshold)) {
    stop("not strictly above thrshold")
  }
  x
}

# two additional arguments
simple_range <- function(x, lower=0, upper=100) {
  if (!all(x>lower & x<upper)) {
    stop("not strictly in range")
  }
  x
}

simple_percentage <- function(x) {
  simple_range(x, 0, 100)
}

# correlations
simple_spearman <- function(x, col1, col2, threshold=0) {
  cor <- cor.test(x[[col1]], x[[col2]], method="spearman")
  if (cor$estimate<threshold)
    stop("columns not correlated")
  x
}

