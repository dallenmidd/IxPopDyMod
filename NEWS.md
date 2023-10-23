# IxPopDyMod (development version)

# IxPopDyMod 0.3.0

* Rewrote `config()`, breaking the components of a `config` object out into a hierarchy of (new) S3 objects: `predictors`, `life_cycle`, `parameters`, `predictor_spec`, `transition_function`, and `transition`. This has numerous advantages, including the ability for users to build up individual components of a `config` with validation from the new classes, rather than constructing the entire `config` object in one step. This update means users need to modify any existing `config` objects to fit the new schema. The most significant changes are:
  - Life cycles are represented with the `life_cycle` class, which contains a list of `transition` objects. Previously, this data was represented in the `transitions` and `parameters` attributes of a `config`.
  - Modified the mechanism for passing functions to transitions to compute transition probability or duration. These functions are now wrapped in a class, `transition_function`. Previously, `config`s contained strings with the name of functions to `get()` from the environment; now we pass the `transition_function`s themselves. This removes the model's dependence on the global R environment, improving reproducibility.
  - Using `predictor_spec` and `parameters`, passing parameter and predictor values to `transition_function`s is now done more explicitly.
* Updated the example `config`s to conform to the new schema.
* Removed several pieces of non-core functionality that we think are better handled by users. This refocuses the package on its core purpose: configuring and running tick population models.
  - Removed `read_config()` and `write_config()`, which provided the ability to serialize and deserialize `config`s. These functions were not sufficient to comprehensively serialize all `config`s for reproducible model results. Now, the package only handles in-memory objects.
  - Removed `run_all_configs()` and the related `parallel` package dependency. This function was just a thin wrapper over a call to `lapply` or a parallel apply operation.
  - Removed `graph_lifecycle()` for visualizing the flow between life stages, and the dependency on the package `igraph`. Instead, `life_cycle` includes a print method with useful information on the flow between life stages.
  - Removed `vary_param()` and `vary_many_params()`.
  - Removed the graphing functions `graph_population_each_group()` and `graph_population_overall_trend()`. These functions were intended as an easy way to visualize model results, but they did not handle all cases, since model output can be highly variable. We now recommend users write their own code, e.g. with `ggplot2`, to visualize results.
* Removed three columns from the data frame output from `run()`: `age_group`, `process` and `infected`. These columns were populated based on the assumption that life stage names were structured as a three-character string like `<process><infected><age_group>`, and used downstream in the graphing functions. Now, users are completely free to name life stages as they wish.
* Added `annual_growth_rate()`, for determining the annual factor by which population changes. Results from the `growth_rate()` function can be sensitive to the specific time period being modeled -- this aims to be a more universally applicable alternative.
* Refactored `growth_rate()` to use base R.

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
