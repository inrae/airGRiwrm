# Increment dev version in DESCRIPTION file
# This script reads the VERSION from DESCRIPTION, checks if it matches the dev pattern
# (x.y.z.9nnn), and increments the dev number by 1 if so.

increment_dev_version <- function() {
  desc_path <- "DESCRIPTION"

  # Read DESCRIPTION file
  desc_lines <- readLines(desc_path, warn = FALSE)

  # Find the Version line
  version_line_idx <- grep("^Version:", desc_lines)

  if (length(version_line_idx) == 0) {
    stop("No 'Version:' field found in DESCRIPTION file.")
  }

  # Extract current version
  old_version <- trimws(sub("^Version:\\s*", "", desc_lines[version_line_idx]))

  cat(sprintf("[dev-version] Current version: %s\n", old_version))

  # Check if version matches dev pattern x.y.z.9nnn
  dev_pattern <- "^([0-9]+\\.[0-9]+\\.[0-9]+)\\.9([0-9]{3})$"
  match_result <- regmatches(old_version, regexpr(dev_pattern, old_version))

  if (match_result == "") {
    warning(
      "[dev-version] Version '",
      old_version,
      "' does not match dev pattern (x.y.z.9nnn). Leaving unchanged."
    )
    return(invisible(NULL))
  }

  # Parse base version and dev number
  base_version <- sub("\\.9[0-9]{3}$", "", old_version)
  dev_number <- as.integer(sub("^.*\\.9", "", old_version))

  # Increment dev number
  new_dev_number <- dev_number + 1

  # Format new version (maintaining 3-digit zero-padded format)
  new_version <- sprintf("%s.9%03d", base_version, new_dev_number)

  cat(sprintf("[dev-version] New version:     %s\n", new_version))

  # Update the Version line in DESCRIPTION
  desc_lines[version_line_idx] <- sprintf("Version: %s", new_version)

  # Write back to DESCRIPTION
  writeLines(desc_lines, desc_path)

  cat(
    "[dev-version] Successfully incremented dev version from",
    old_version,
    "to",
    new_version,
    "\n"
  )

  invisible(new_version)
}

# Run the function
increment_dev_version()
