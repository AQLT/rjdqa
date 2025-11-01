# rjdqa 0.1.6

- new parameter `add_obs_to_forecast` to `simple_dashboard()` and `simple_dashboard2()`.

- new parameter `td_effect` to `simple_dashboard()` and `simple_dashboard2()` to indicate if the residual trading days effect test should be printed.
By default (`td_effect = NULL`) the test is only printed for monthly series.

- The title of table of outliers of `simple_dashboard2()` now also depend on the number of outliers detected by the model.

- improvement placement of table of outliers in `simple_dashboard2()`.

- improvement of `sc_dashboard()` to handle user-defined calendar regressors.

# rjdqa 0.1.5

- correction of the use of `tail()` on `ts` object to avoid error.

# rjdqa 0.1.4

- Fix of minimum versions of dependencies.

- Correction of the orders of the outliers in `simple_dashboard2()`.

# rjdqa 0.1.3

- deprecated function `sa_dashboard()` removed.

- legend in `simple_dashboard()`.

- new parameter in `simple_dashboard()` to remove the "Others" contribution in the variance decomposition table and to rescale the contributions to sum to 100.

- new function `simple_dashboard2()`: same as `simple_dashboard()` but with smaller text to add informations on last outliers.


# rjdqa 0.1.2

- documentation updated.

- `simple_dashboard()` function.

- `sa_dashboard()` deprecated, use `sc_dashboard()` instead.

# rjdqa 0.1.1

## Bug fixed

-  correction for `X13` models with log decomposition.
