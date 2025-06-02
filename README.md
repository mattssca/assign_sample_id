# AssignSampleIDs

[![R-CMD-check](https://github.com/mattssca/assign_sample_id/actions/workflows/main.yml/badge.svg)](https://github.com/mattssca/assign_sample_id/actions/workflows/main.yml)

**AssignSampleIDs** is an R package that provides tools for generating, assigning, and managing sample IDs in laboratory workflows. It includes functions for creating unique identifiers, handling sample metadata, and ensuring consistency in data management.

## Installation

You can install the development version of the package from GitHub using the `devtools` package:

```r

# Install devtools if not already installed

install.packages("devtools")


# Install AssignSampleIDs from GitHub

devtools::install_github("mattssca/assign_sample_id")
```

Alternatively, if you have the package source files, you can install it locally:

```
# Replace "path/to/AssignSampleIDs" with the path to the package directory
install.packages("path/to/AssignSampleIDs", repos = NULL, type = "source")
```

## Usage

Load the package and start using its functions:

```
# Load package
library(AssignSampleIDs)

# Example: load test data
test_data = AssignSampleIDs::test_data

# Run funciton on test data
result <- assign_sample_id(
  this_data = test_data,
  start_id = 100,
  lab_id_col = "lab_id",
  personal_id_col = "personal_id",
  date_col = "date_of_sample",
  verbose = TRUE
)
```

## Features

* Generate unique sample IDs.
* Assign and manage metadata for laboratory workflows.
* Ensure consistency in data management.

## Access the Vignette

After installing the package, you can view the vignette using:

```
browseVignettes("AssignSampleIDs")
```

## Shiny App

For an interactive user experience, give [the shiny app](https://mattsada.shinyapps.io/AssignSampleIDs/) a go!

## Docker Container

The shiny app is also available as a Docker container. Follow the next steps to pull the image and run the Docker image.

```
#pull the image
docker pull mattssca/assignsampleids-app:latest

#run the container
docker run -d -p 3838:3838 mattssca/assignsampleids-app:latest
```

To access the app, open your web browser and go to: [http://localhost:3838](http://localhost:3838)

## Contributing

Contributions are welcome! If you encounter any issues or have suggestions for improvements, please open an issue or submit a pull request.

## License

This package is licensed under the MIT License. See the `LICENSE` file for details.
