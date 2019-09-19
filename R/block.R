roxy_block <- function(tags,
                       file,
                       line,
                       call,
                       object = NULL) {
  stopifnot(is.list(tags))
  stopifnot(is.character(file), length(file) == 1)
  stopifnot(is.integer(line), length(line) == 1)

  structure(
    list(
      tags = tags,
      file = file,
      line = line,
      call = call,
      object = object
    ),
    class = "roxy_block"
  )
}

is_roxy_block <- function(x) inherits(x, "roxy_block")

#' @export
print.roxy_block <- function(x, ...) {
  call <- deparse(x$call, nlines = 2)
  if (length(call) == 2) {
    call <- paste0(call[[1]], " ...")
  }

  cat_line("<roxy_block> @ ", block_location(x))
  cat_line("  Tags: ", paste0(names(x$tags), collapse = ", "))
  cat_line("  Call: ", call)
  cat_line("  Obj ? ", !is.null(x$object))
}

block_create <- function(tokens, call, srcref,
                         registry = list(),
                         global_options = list()) {

  tags <- parse_tags(tokens,
    registry = registry,
    global_options = global_options
  )
  if (length(tags) == 0) return()

  roxy_block(tags,
    file = attr(srcref, "srcfile")$filename,
    line = as.vector(srcref)[[1]],
    call = call
  )
}

block_set_env <- function(block, env,
                          registry = list(),
                          global_options = list()
                          ) {

  block <- block_evaluate(block, env, registry = registry, global_options = global_options)
  block <- block_find_object(block, env)
  block
}

block_evaluate <- function(block, env,
                           registry = list(),
                           global_options = list()
                           ) {

  tags <- block_get_tags(block, "eval")
  if (length(tags) == 0) {
    return(block)
  }

  # Evaluate
  results <- lapply(tags, roxy_tag_eval, env = env)
  results <- lapply(results, function(x) {
    if (is.null(x)) {
      character()
    } else {
      paste0("#' ", x)
    }
  })

  # Tokenise and parse
  tokens <- lapply(results, tokenise_block,
    file = block$file,
    offset = block$line
  )
  tags <- lapply(tokens, parse_tags,
    registry = registry,
    global_options = global_options
  )

  # Interpolate results back into original locations
  block_replace_tags(block, "eval", tags)
}

block_find_object <- function(block, env) {
  stopifnot(is_roxy_block(block))

  object <- object_from_call(
    call = block$call,
    env = env,
    block = block,
    file = block$file
  )
  block$object <- object

  # Add in defaults generated from the object
  defaults <- object_defaults(object)
  defaults <- c(defaults, list(roxy_tag("backref", block$file)))

  default_tags <- map_chr(defaults, "tag")
  defaults <- defaults[!default_tags %in% block_tags(block)]

  block$tags <- c(block$tags, defaults)
  block
}

block_location <- function(block) {
  if (is.null(block)) {
    NULL
  } else {
    paste0(basename(block$file), ":", block$line)
  }
}

block_warning <- function(block, ...) {
  warning(
    block_location(block), ": ", ...,
    call. = FALSE,
    immediate. = TRUE
  )
  NULL
}

# block accessors ---------------------------------------------------------

block_tags <- function(block) {
  map_chr(block$tags, "tag")
}

block_has_tags <- function(block, tag) {
  any(block_tags(block) %in% tag)
}

block_get_tags <- function(block, tags) {
  block$tags[block_tags(block) %in% tags]
}
block_get_tag <- function(block, tag) {
  matches <- which(block_tags(block) %in% tag)
  n <- length(matches)
  if (n == 0) {
    NULL
  } else if (n == 1) {
    block$tags[[matches]]
  } else {
    roxy_tag_warning(block$tags[[matches[[2]]]], "May only use one @", tag, " per block")
    block$tags[[matches[[1]]]]
  }
}
block_get_tag_value <- function(block, tag) {
  block_get_tag(block, tag)$val
}

block_replace_tags <- function(block, tags, values) {
  indx <- which(block_tags(block) %in% tags)
  stopifnot(length(indx) == length(values))

  tags <- lapply(block$tags, list)
  tags[indx] <- values

  block$tags <- compact(unlist(tags, recursive = FALSE))
  block
}

# parsing -----------------------------------------------------------------

parse_tags <- function(tokens, registry = list(), global_options = list()) {
  markdown_activate(tokens, global_options = global_options)

  tokens <- parse_description(tokens)
  compact(lapply(tokens, parse_tag, registry = registry))
}

parse_tag <- function(x, registry) {
  stopifnot(is.roxy_tag(x))

  if (identical(x$tag, "eval")) {
    tag_code(x)
  } else if (identical(x$tag, "include")) {
    tag_value(x)
  } else if (identical(x$tag, "order")) {
    tag_value(x)
  } else if (x$tag %in% ls(registry)) {
    registry[[x$tag]](x)
  } else {
    roxy_tag_warning(x, "unknown tag")
  }
}

parse_description <- function(tags) {
  if (length(tags) == 0) {
    return(tags)
  }

  tag_names <- vapply(tags, `[[`, "tag", FUN.VALUE = character(1))
  if (tag_names[1] != "") {
    return(tags)
  }

  intro <- tags[[1]]
  intro$val <- str_trim(intro$val)
  if (intro$val == "") {
    return(tags[-1])
  }

  tags <- tags[-1]
  tag_names <- tag_names[-1]

  paragraphs <- str_split(intro$val, fixed('\n\n'))[[1]]

  # 1st paragraph = title (unless has @title)
  if ("title" %in% tag_names) {
    title <- NULL
  } else if (length(paragraphs) > 0) {
    title <- roxy_tag("title", paragraphs[1], intro$file, intro$line)
    paragraphs <- paragraphs[-1]
  } else {
    title <- roxy_tag("title", "", intro$file, intro$line)
  }

  # 2nd paragraph = description (unless has @description)
  if ("description" %in% tag_names || length(paragraphs) == 0) {
    description <- NULL
  } else if (length(paragraphs) > 0) {
    description <- roxy_tag("description", paragraphs[1], intro$file, intro$line)
    paragraphs <- paragraphs[-1]
  }

  # Every thing else = details, combined with @details
  if (length(paragraphs) > 0) {
    details_para <- paste(paragraphs, collapse = "\n\n")

    # Find explicit @details tags
    didx <- which(tag_names == "details")
    if (length(didx) > 0) {
      explicit_details <- map_chr(tags[didx], "val")
      tags <- tags[-didx]
      details_para <- paste(c(details_para, explicit_details), collapse = "\n\n")
    }

    details <- roxy_tag("details", details_para, intro$file, intro$line)
  } else {
    details <- NULL
  }

  c(compact(list(title, description, details)), tags)
}

