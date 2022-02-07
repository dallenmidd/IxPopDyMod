# IxPopDyMod 0.2.0 
* Added new `config`s to the package.
  - `ogden2005`, which replicates an existing deer tick population model by Ogden et al. (2005).
  - `winter_tick`, based on literature on the winter tick and used as an example of the package's flexibility for studying different tick species.
  - `host_example_config`, `infect_example_config`, and `temp_example_config`, which are used as examples of package use. Added examples with these `config`s to the readme.
* Generalized `config()` to take a single `predictors` argument, instead of separate `weather` and `host_comm` arguments. This simplifies the `config()` structure, and allows using other types of data as a predictor for transitions. 
  - `write_config()` arguments are updated to reflect this change.
  - Some internal functions are updated to support this change.
* Added `snow_cover_fun()` transition function which is used in the `winter_tick` model configuration.
* Fixed `write_config()` to write steps and initial population as integers rather than decimal numbers.

# IxPopDyMod 0.1.0: First CRAN Submission
* First launch of `IxPopDyMod` package
