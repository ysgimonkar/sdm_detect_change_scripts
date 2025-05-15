# How well can statistical models detect ecological change in simulated Southern Ocean assemblages? 

The code and data provided here will allow anyone to reproduce the analyses and results described in the manuscript.

The order of analysis scripts should be as follows (for scenario 1 but is the same for all other scenarios):
1. Simulate the scenario: scenario1_simulations.R
2. Fit the models: boral.R, brt.R, glm.R, hmsc.R, mistnet.R, sam.R. Beore running the mistnet.R script, please run the mistnet_cv.R script to save the cross-validation metrics for faster processing of the later script.
3. Evaluating model diagnostics: model_diagnostics.R
4. Evaluating partial effects: scenario1_partial_effects.R
5. Evaluating spatio-temporal trends:
   a. preparing_extrapolation_data.R
   b. scenario1_preprocess_model_extrapolation.R
   c. preprocess_model_extrapolation_BORAL.R
   d. preprocess_model_extrapolation_HMSC.R
   e. scenario1_model_extrapolations.R
