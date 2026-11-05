locate_package_dir <- function(start = getwd()) {
  path <- normalizePath(start, winslash = "/", mustWork = TRUE)

  repeat {
    if (file.exists(file.path(path, "DESCRIPTION"))) {
      return(path)
    }

    parent <- dirname(path)
    if (identical(parent, path)) {
      stop(
        "No package root found from '",
        start,
        "'. ",
        "Run this script from the package directory or one of its subdirectories.",
        call. = FALSE
      )
    }

    path <- parent
  }
}

check_markdown_dependencies <- function() {
  required_pkgs <- c("knitr", "remotes", "rmarkdown")
  missing_pkgs <- required_pkgs[
    !vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing_pkgs) > 0) {
    stop(
      "Missing packages: ",
      paste(missing_pkgs, collapse = ", "),
      ". ",
      "Install them before running this script.",
      call. = FALSE
    )
  }

  invisible(required_pkgs)
}

yaml_string <- function(x) {
  paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
}

trim_empty_lines <- function(lines) {
  while (length(lines) > 0 && !nzchar(lines[[1]])) {
    lines <- lines[-1]
  }

  while (length(lines) > 0 && !nzchar(lines[[length(lines)]])) {
    lines <- lines[-length(lines)]
  }

  lines
}

install_package_for_docs <- function(pkg_dir) {
  lib_dir <- file.path(tempdir(), "docs-build-library")

  if (dir.exists(lib_dir)) {
    unlink(lib_dir, recursive = TRUE, force = TRUE)
  }

  dir.create(lib_dir, recursive = TRUE, showWarnings = FALSE)
  old_libpaths <- .libPaths()
  .libPaths(c(lib_dir, old_libpaths))

  remotes::install_local(
    pkg_dir,
    upgrade = "never",
    quiet = TRUE,
    dependencies = FALSE,
    build_vignettes = FALSE,
    force = TRUE
  )

  structure(
    list(
      lib_dir = normalizePath(lib_dir, winslash = "/", mustWork = TRUE),
      old_libpaths = old_libpaths
    ),
    class = "pdf_docs_install"
  )
}

restore_library_paths <- function(install_info) {
  .libPaths(install_info$old_libpaths)
}

help_topic_to_markdown <- function(topic, pkg_name, lib_dir) {
  help_path <- do.call(
    utils::help,
    list(topic = topic, package = pkg_name, lib.loc = lib_dir)
  )

  if (!length(help_path) || !nzchar(help_path[[1]])) {
    stop("No help topic found for '", topic, "'.", call. = FALSE)
  }

  help_file <- utils:::.getHelpFile(help_path)
  help_text <- trim_empty_lines(capture.output(tools::Rd2txt(help_file, fragment = TRUE)))

  c(
    sprintf("## %s", topic),
    "",
    "```text",
    help_text,
    "```",
    ""
  )
}

extract_vignette_title <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")

  if (length(lines) < 3 || lines[[1]] != "---") {
    return(tools::file_path_sans_ext(basename(path)))
  }

  front_matter_end <- which(lines[-1] == "---")
  if (!length(front_matter_end)) {
    return(tools::file_path_sans_ext(basename(path)))
  }

  front_matter <- lines[seq_len(front_matter_end[[1]] + 1)]
  title_line <- grep("^title\\s*:", front_matter, value = TRUE)

  if (!length(title_line)) {
    return(tools::file_path_sans_ext(basename(path)))
  }

  title <- sub("^title\\s*:\\s*", "", title_line[[1]])
  title <- sub("^['\"]", "", title)
  title <- sub("['\"]$", "", title)
  title
}

create_combined_markdown <- function(
  pkg_dir,
  out_dir,
  pkg_name,
  pkg_version,
  pkg_title,
  pkg_description,
  lib_dir,
  vignette_files
) {
  reference_topics <- sort(sub(
    "\\.Rd$",
    "",
    basename(list.files(file.path(pkg_dir, "man"), pattern = "\\.Rd$"))
  ))

  combined_path <- file.path(
    out_dir,
    sprintf("%s_%s_full_documentation.Rmd", pkg_name, pkg_version)
  )

  bibliography <- file.path(pkg_dir, "vignettes", "airGRiwrm.bib")
  lines <- c(
    "---",
    paste("title:", yaml_string(sprintf("%s %s full documentation", pkg_name, pkg_version))),
    paste("subtitle:", yaml_string(pkg_title)),
    paste("date:", yaml_string(as.character(Sys.Date()))),
    "output:",
    "  rmarkdown::md_document:",
    "    variant: markdown"
  )

  if (file.exists(bibliography)) {
    lines <- c(
      lines,
      paste(
        "bibliography:",
        yaml_string(normalizePath(bibliography, winslash = "/", mustWork = TRUE))
      )
    )
  }

  lines <- c(
    lines,
    "---",
    "",
    "```{r setup, include=FALSE}",
    sprintf(
      ".libPaths(c(%s, .libPaths()))",
      encodeString(lib_dir, quote = '"')
    ),
    "options(knitr.duplicate.label = 'allow')",
    "knitr::opts_chunk$set(collapse = TRUE, comment = \"#>\")",
    "render_child_vignette <- function(path) {",
    "  child_output <- tryCatch(",
    "    knitr::knit_child(path, quiet = TRUE),",
    "    error = function(err) {",
    "      c(",
    "        sprintf(\"> **Vignette rendering error** (`%s`): %s\", basename(path), conditionMessage(err)),",
    "        \"\"",
    "      )",
    "    }",
    "  )",
    "",
    "  cat(child_output, sep = \"\\n\")",
    "}",
    "```",
    "",
    "# Package reference",
    "",
    paste0("**Package:** ", pkg_name),
    "",
    paste0("**Version:** ", pkg_version),
    "",
    pkg_description,
    ""
  )

  for (topic in reference_topics) {
    lines <- c(lines, help_topic_to_markdown(topic, pkg_name, lib_dir))
  }

  lines <- c(lines, "# Vignettes", "")

  for (i in seq_along(vignette_files)) {
    vignette_file <- normalizePath(vignette_files[[i]], winslash = "/", mustWork = TRUE)
    vignette_title <- extract_vignette_title(vignette_file)

    lines <- c(
      lines,
      sprintf("## %s", vignette_title),
      "",
      sprintf("```{r vignette_%d, results='asis', echo=FALSE}", i),
      sprintf(
        "render_child_vignette(%s)",
        encodeString(vignette_file, quote = '"')
      ),
      "```",
      ""
    )
  }

  writeLines(lines, combined_path, useBytes = TRUE)
  combined_path
}

build_pkg_markdown_bundle <- function(
  pkg_dir = locate_package_dir(),
  out_dir = file.path(pkg_dir, "docs", "markdown")
) {
  check_markdown_dependencies()

  pkg_dir <- normalizePath(pkg_dir, winslash = "/", mustWork = TRUE)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  desc <- read.dcf(file.path(pkg_dir, "DESCRIPTION"))
  pkg_name <- desc[1, "Package"]
  pkg_version <- desc[1, "Version"]

  old_wd <- getwd()
  setwd(pkg_dir)
  on.exit(setwd(old_wd), add = TRUE)

  install_info <- install_package_for_docs(pkg_dir)
  on.exit(restore_library_paths(install_info), add = TRUE)

  vignette_files <- list.files(
    file.path(pkg_dir, "vignettes"),
    pattern = "\\.Rmd$",
    recursive = TRUE,
    full.names = TRUE
  )

  combined_markdown <- create_combined_markdown(
    pkg_dir = pkg_dir,
    out_dir = out_dir,
    pkg_name = pkg_name,
    pkg_version = pkg_version,
    pkg_title = desc[1, "Title"],
    pkg_description = desc[1, "Description"],
    lib_dir = install_info$lib_dir,
    vignette_files = vignette_files
  )

  bundle_path <- file.path(
    out_dir,
    sprintf("%s_%s_full_documentation.md", pkg_name, pkg_version)
  )

  rmarkdown::render(
    input = combined_markdown,
    output_format = rmarkdown::md_document(variant = "markdown"),
    output_file = bundle_path,
    envir = new.env(parent = globalenv()),
    clean = TRUE,
    quiet = TRUE
  )

  invisible(list(
    document = combined_markdown,
    vignettes = unname(vignette_files),
    bundle = bundle_path
  ))
}

build_pkg_markdown_bundle()
