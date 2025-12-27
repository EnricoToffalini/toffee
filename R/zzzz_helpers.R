.install_renderthis = function() {
  if (!requireNamespace("renderthis", quietly = TRUE)) {
    if (!requireNamespace("remotes", quietly = TRUE)) {
      stop(
        "Package 'renderthis' is required but not installed.\n",
        "Install it manually with:\n",
        "  install.packages('remotes')\n",
        "  remotes::install_github('jhelvy/renderthis')"
      )
    }
    message("Installing 'renderthis' from GitHub...")
    remotes::install_github("jhelvy/renderthis")
  }
}