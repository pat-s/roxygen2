object_defaults <- function(x) UseMethod("object_defaults")

#' @export
object_defaults.default <- function(x) list()

#' @export
object_defaults.data <- function(x) {
  str_out <- rd(object_format(x$value))

  list(
    roxy_tag("docType", "data"),
    roxy_tag("format", str_out),
    roxy_tag("keywords", "datasets")
  )
}

#' @export
object_defaults.package <- function(x) {
  desc <- x$value$desc

  description <- as.character(desc$Description)
  logo_path <- file.path(x$value$path, "man", "figures", "logo.png")
  if (file.exists(logo_path)) {
    fig <- "\\if{html}{\\figure{logo.png}{options: align='right' alt='logo' width='120'}}"
    description <- paste0(fig, "\n\n", description)
  }

  list(
    roxy_tag("docType", "package"),
    roxy_tag("name", package_suffix(desc$Package)),
    # "NULL" prevents addition of default aliases, see also #202
    roxy_tag("aliases", paste("NULL", desc$Package, package_suffix(desc$Package))),
    roxy_tag("title", paste0(desc$Package, ": ", desc$Title)),
    roxy_tag("description", description),
    roxy_tag("seealso", package_seealso(desc)),
    roxy_tag("author", package_authors(desc))
  )
}

#' @export
object_defaults.s4class <- function(x) {
  list(
    roxy_tag("docType", "class")
  )
}

#' @export
object_defaults.rcclass <- function(x) {
  list(
    roxy_tag("docType", "class")
  )
}

#' @export
object_defaults.s4method <- function(x) {
  list(
    roxy_tag("docType", "class")
  )
}
