data("feature_selection_data", package = "XIFF")

# Fit the model 
featureFit <- selectBestFeaturesBoruta(feature_selection_data)

# Plot the result
plot(featureFit$fit, las = 1, horizontal = TRUE)

# Show resulting table
featureFit$stats
