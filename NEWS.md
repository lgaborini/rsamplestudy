# rsamplestudy 0.3.1

* `make_idx_splits`: documented case where `same_source` is `TRUE`, a `source_quest` is specified. (a message is thrown)
* `make_idx_splits` and `make_dataset_splits` try to honor `same_source` when specified over sources. In documented cases, this is not possible, and will fail gracefully.
* `make_idx_splits` and `make_dataset_splits` gain parameter `strict`: if `TRUE`, any inconsistency will fail.
* new underlying error framework, assertions should get better checked during tests.

# rsamplestudy 0.3

* Adding restrictions on allowed sources: parameters `source_ref_allowed`, `source_quest_allowed`.
* Exported function `sample_safe`: like `sample`, but safer for 1-length vectors.

# rsamplestudy 0.2.1

* Fixed tests for `background = 'unobserved'`.

# rsamplestudy 0.2.0

* Added a `NEWS.md` file to track changes to the package.
* Added `background = 'unobserved'` option to `make_dataset_splits` and `make_idx_splits`.
