# Contributing to GlycoDash

This document provides guidelines for contributing to the project.

## How can I contribute?

### Reporting bugs

Before creating bug reports, please check existing issues to avoid duplicates. When creating a bug report, include as many details as possible:

- **Use a clear and descriptive title.**
- **Describe the exact steps to reproduce the problem.**
- **Provide specific examples** (sample files, screenshots, etc.).
- **Describe the behavior you observed** and what you expected to see.
- **Include details about your environment**:
  - OS version (Windows, macOS, Linux)
  - GlycoDash version
  - R version and, if relevant, package versions from `renv.lock`
  - Input data type (LaCyTools, SweetSuite, or Skyline)

### Suggesting enhancements

Enhancement suggestions are welcome! Please provide:

- **A clear and descriptive title.**
- **A detailed description of the proposed functionality.**
- **Explain why this enhancement would be useful.**
- **List any alternative solutions or features you've considered.**

### Pull requests

1. **Fork the repository** and create your branch from `alfa`.
2. **Follow the existing code style** and conventions.
3. **Add tests** if you're adding new functionality.
4. **Update documentation** to reflect your changes.
5. **Ensure all tests pass** before submitting.
6. **Write clear commit messages** describing what and why.

#### Pull request process

1. Update `README.md` with details of changes if needed.
2. Update `NEWS.md` following the existing format.
3. The PR will be merged once reviewed and approved.

> **Note:** All pull requests should target the `alfa` branch. Do not open pull requests directly against `master`.

## Development setup

1. **Install R 4.5.0, RStudio, and RTools 4.5**
   - [R 4.5.0](https://cran.r-project.org/bin/windows/base/old/) — can be installed alongside other R versions
   - [RStudio](https://posit.co/download/rstudio-desktop/)
   - [RTools 4.5](https://cran.r-project.org/bin/windows/Rtools/rtools45/rtools.html) — required to build some packages from source; use the default installer settings

2. **Configure RStudio to use R 4.5.0**

   Go to `Tools → Global Options → General → R version → Change…`, select R 4.5.0, apply the changes, and restart RStudio.

3. **Clone the repository**
   ```bash
   git clone https://github.com/Center-for-Proteomics-and-Metabolomics/GlycoDash.git
   cd GlycoDash
   git checkout alfa
   ```

4. **Open the project**

   Double-click `glycodash.Rproj`. RStudio will open the project and install the `renv` package automatically if it is not already present.

5. **Restore the package library**
   ```r
   renv::restore()
   ```
   This downloads and installs all required packages exactly as specified in `renv.lock`. ⏳ *This may take several minutes.*

6. **Run the application**

   Open `dev/run_dev.R` and press `Ctrl+Shift+Enter` to launch the app. If prompted to install **roxygen2**, confirm the installation.

## Coding conventions

- Follow the [golem](https://thinkr-open.github.io/golem/) framework conventions and the file naming scheme described in [`ARCHITECTURE.md`](ARCHITECTURE.md).
- Follow the [tidyverse style guide](https://style.tidyverse.org/) for R code.
- Use meaningful variable and function names.
- Add [roxygen2](https://roxygen2.r-lib.org/) documentation to all non-trivial functions in `fct_` files. Run `golem::document_and_reload()` to verify that documentation builds correctly.
- Keep functions focused and concise.
- Add comments for complex logic.

## Dependency management

If you install or update any packages during development, please record the changes in `renv.lock` before submitting your pull request:

```r
renv::snapshot()
```

Include the updated `renv.lock` in your pull request.

## Testing

- Add tests for new features in the `tests/` directory.
- Ensure existing tests pass before submitting a pull request.
- Aim for good test coverage of new code.

## Questions?

Feel free to open an issue with your question or reach out to the maintainers.

## License

By contributing, you agree that your contributions will be licensed under the MIT License.
