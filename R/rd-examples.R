topic_add_examples <- function(topic, block, base_path) {
  tags <- block_get_tags(block, c("examples", "example"))

  for (tag in tags) {
    if (tag$tag == "examples") {
      example <- tag$val
    } else {
      example <- read_example_from_path(str_trim(tag$val), base_path, block = block)
    }
    topic$add_simple_field("examples", example)
  }
}

read_example_from_path <- function(path, base_path, block = NULL) {
  nl <- str_count(path, "\n")
  if (any(nl) > 0) {
    block_warning(block, "@example spans multiple lines. Do you want @examples?")
    return()
  }

  path <- file.path(base_path, path)
  if (!file.exists(path)) {
    block_warning(block, "@example ", path, " doesn't exist")
    return()
  }

  code <- read_lines(path)
  escape_examples(code)
}

# Works like escape, but unescapes special rd example commands.
# Also unescapes quotes because they must already be in strings and hence
# don't need an additional layer of quoting.
escape_examples <- function(x) {
  x <- escape(x)
  x <- gsub("\\\\dont", "\\dont", x, fixed = TRUE)
  x <- gsub("\\\\'", "\\'", x, fixed = TRUE)
  x <- gsub('\\\\"', '\\"', x, fixed = TRUE)
  x
}
