# **SARI changelog**

# octubre 2022 [2022.10]

### Fixed:
-Bug when reading hour string "000000" from PBO files
-Bug when reduce sampling is active but no averaging window is provided
-Bug when changing from LS to KF and the series were previously resampled

### Changed:
-Checking if there are noise analysis results when trying to plot the noise spectrum
-Additional checks for several input values
-Checking if the series are more or less evenly sampled before updating the KF process noise
-Improving the warning message when reading the station.info file
-Removing console warning when reading the custom offset file
-Removing KF fit, wavelet and filter results when changing time units
-Removing values of residual threshold when restoring all removed points
-More detailed warning when fitting long-period sinusoids
-Changing behavior when error bars are read, activated and deactivated
-Notifications and warnings are only shown once for each message
-Preventing changing to time unit weeks in a PBO series

### Added:
-Showing new warning on screen when the size of the browser window changes
-Showing new message on screen when uploading a RINEX file
-New plot to show the time-variable trend estimate from a KF fit
-Warning on screen when changing the time units and the series were previously resampled
-New kalman filtering approach for estimating the instantaneous linear rate
-Adding time-variable measurement and process noises to the KF fit
-New option to disable scrolling before taking a full page screenshot
-Adding error bars to the KF fit and posibility to remove normalized outliers
-Adding units to the sinusoidal phase estimates
-Loading a SARI file with a previous KF fit

# septiembre 2022 [2022.09]

### Fixed:
- Estimation of the sinusoid phase error when the sine component is zero
- Restoring removed points in primary series when a longer secondary series was uploaded
- Updating the periodogram limits when the series sampling is reduced
- Indexing automatically removed points
- Unexpected line commented when setting start.time in the wavelet
- Checking noise analysis results before plotting the noise power spectrum

### Changed:
- Checking for non-zero a priori state covariances in the KF fit
- Checking for numeric values provided by the user in the a priori state of offset parameters
- Improved guess of the a priori KF state obtained from a previous LS fit
- Preventing the client to change the plots width when the user interacts with them
- Showing transparent error bars when running local on Windows machines
- Disable auto toggle by default until the user provides a residual threshold
- Showing compact ordinate axis on the series plots
- Removing KF fit results when restoring a previously removed point for which there is no solution
- Some variables translated into English

### Added:
- Enabling the possibility to remove outliers from the KF residuals

# agosto 2022 [2022.08]

### Fixed:
- Checking the number of common points between the primary and secondary series before computing the Pearson's correlation coefficient
- Allowing for several words in the last column (variable number of columns) of the PBO format
- Removing fixed number of expected header lines in NGL and PBO series
	
### Changed:
- Adding different levels of console verbosity

### Added:
- Checking number of columns in input PBO and NGL series

# julio 2022 [2022.07]

### Fixed

### Changed
- Organization, translation and cleaning of the code.
- Faster estimation of the instantaneous rate from the Kalman filter fit.
- Improved implementation of the MIDAS algorithm.

### Added
- Adding information of the Kalman filter parameters in the downloaded results file.
- When zooming in, the histogram & statistics are limited to the series within the zoom-in window.

# abril 2022 [2022.04]

### Fixed
- Reading and extracting information from the customized offset file.
	
### Changed
- Improved performance by reducing the number of times some reactive functions are called unnecessarily.
- Adding more checks for PBO and NGL input formats.

### Added
- Option to show or remove the trend from a tectonic plate motion model.
- Showing Pearson's correlation coefficient between primary and secondary series.
- Option to show the secondary series on the same y-axis of the primary series.

# marzo 2022 [2022.03]

### Fixed
- Allowing to remove points automatically based on residuals from the smoothing when the LS fit is activated and no model component is selected.
- Subtracting the mean value from the series selected to compute the wavelet
- Removed points with the option "all components" activated do not participate to the moving average when reducing the sampling.
- The epsilon parameter in Vondrak's smoother has been improved so that the band-pass cutoff periods depend less on the number of points in the series.

### Changed
- Optimizing the number of times LS fit is run when changing the model components.
	
### Added
- The amplitudes and phases of the fitted sinusoids are estimated from the sine and cosine values and are shown at the bottom of the model fit results.
- The formal uncertainty of the fitted model series and the residual model series are now included in the downloaded results file.
- The residuals from the smoothed series are included in the downloaded results file.

# febrero 2022 [2022.02]

### Fixed
- correcting point indexes when automatically restoring the removed points
- downloading the results file when the removed points are included as comments
- avoiding fatal error when user asks for a column that does not exists in a 1D file
- removing leading white spaces from user inputs
- checking if the oversampling, the shortest and the longest periods given by the user allows computing at least 2 different periods to have something to plot in the periodogram

### Changed
- when increasing the oversampling of the periodogram after zooming in, the new high-resolution periodogram will be computed for the periods of the selected window only

# enero 2022 [2022.01]

### Fixed
- check the series values before computing the histogram
- reading the polynomial degree from an uploaded SARI file
- kalman filter state estimates were advanced one time step in the smoothing run (they were assigned to n+1 epoch instead of n)
- improved centering of the secondary series in the series plot box
- removing points from the smoothed residuals of the fit
- normalizing the error bars before computing the Vondrak smoother from the residual series
- the list of antenna/receiver changes is updated when a different sitelog is uploaded
- FTP access to the station.info file from SOPAC

### Changed
- no need to set the number of dimensions nor the NEU/ENU column format anymore
- selecting the input format for the secondary series is not available if primary series is 1D (secondary must be 1D too)
- reconstructing the wavelet decomposition is deprecated as its success depends on the input series and the decomposition itself
- the variance-covariance of the residuals of the fit is no longer computed
- offset verification is deactivated when the user removes the epochs of the estimated offsets
- deleting the MLE results on screen when opening the help tab
- the option of smoothing the fit residuals with Vondrak is not reset when changing the coordinate tab
- coordinate tab name changed for 1D input series

### Added
- PBO (pos v1.1.0) and NGL (tenv3) series are now available as input formats for both primary and secondary series (in addition to NEU/ENU series)
- automatically skipping records from the input series that contain uncommented text in NEU/ENU or 1D formats
- the estimated values from the MLE noise analysis are automatically used in the offset validation values if they are significant
- new option to include the removed points as commented lines in the downloaded results file
- if the Kalman filter includes a time-variable rate, the instantaneous rate is now provided in the downloaded results file in addition to the time-variable average rate
- reading discontinuities from an IGS-like "soln" file

# julio 2021 [2021.07]

### Fixed
- the estimated velocity uncertainty is deleted when resetting the series
- reading more than 9 antenna or receiver changes from the sitelog (changes numbered 10, 20, etc. were previously skipped, but not 11, 12, etc.)
- computing the wavelet transform for series having different sampling and time units
- the location of the maximum "amplitude" on the heatmap of the wavelet transform

### Changed
- minor changes to adapt the code to the R version 4.1.0 with all the package/dependencies updated.
- the LS fit is updated (and not removed) when changing the input column in mode 1D
- the shortest period of the periodogram is not limited to the pseudo-Nyquist period. The pseudo-Nyquist period is still being proposed by default
- the limits of the oversampling parameter of the periodogram have been enlarged up to 100

### Added
- the series column numbers for data and errorbars are included in the downloaded results file and also to the periodogram and wavelet files

# mayo 2021 [2021.05]

### Fixed
- finding discontinuities automatically with daily or weekly time units
- plotting the series when the LS fit fails
- plotting the noise model PSD over the periodogram
- reading a station.info file with empty fields or spaces
- reading a custom offset file for both the primary and secondary series
- removing offset estimates from the results file if they are not included in the model

### Changed
- option "all components" is active by default
- improved a priori values for the intercept and trend

### Added
- option to reconstruct and download a band-pass filtered version of the series from the wavelet transform
- option to click on the wavelet plot to get values
- verification of repeated offset epochs from the Nevada steps file
- new warning when trying to find discontinuities without residual series

# marzo 2021 [2021.03]

### Fixed
- plotting error-bars for both primary and secondary series as independent closed polygons
- keeping error-bars values in memory even if the error-bar option if off

### Changed
- the normalization of the power spectrum reflects now the decomposition of the series variance
- the integral of the power spectrum of the fitted noise model is also scaled to match the series variance

### Added
- option to force the y-axis scale of the secondary series to match the y-axis scale of the primary series

# febrero 2021 [2021.02]

### Fixed
- verifying input column numbers for primary and secondary series

### Changed
- "All components" option allows now to keep the same deleted points when changing the column number in mode 1D

# enero 2021 [2021.01]

### Added
- this changelog

### Fixed
- the dates from the sitelog and station.info files are transformed following the selected time unit.

# noviembre 2020 [2020.11]

### Added
- info on the component number is added to the downloaded results file
- new parameter to adjust the temporal resolution of the wavelet

### Changed
- improved presentation of the fitted model's equation
- improved estimation of the duration of the wavelet computation
- improved wavelet heatmap

### Fixed
- resetting the wavelet when the fitted model is modified

# octubre 2020 [2020.10]

### Added
- showing a rough estimate of the duration of the wavelet computation

### Changed
- improved estimation of the PSD of the fitted noise model from FN and WN
- removing outliers when estimating the y-axis range of the series
- avoiding fitting again the model when opening the help page

### Fixed
- checking if the input max/min period range in the periodogram is smaller than the periodogram resolution
- updating the wavelet bounds when the series are resampled

# agosto 2020 [2020.08]

### Added
- histogram of the interannual linear rates for MIDAS
- averaging the values of the primary and secondary series

### Changed
- updating the MIDAS estimate when outliers are removed
- independent process noise values for different sinusoidal components

### Fixed
- resampling of 1D series
- removing NA values from the secondary series

# julio 2020 [2020.07]

### Changed
- removing outliers from their normalized residuals not available for the Vondrak filter

### Fixed
- checking the input ofac value is valid

# junio 2020 [2020.06]

### Fixed
- estimating the Vondrak filter from the KF residuals

# mayo 2020 [2020.05]

### Added
- new option for plotting the series with points, lines or points & lines
- the PSD of the fitted noise model is plotted on top of the periodogram
- information on the waveform period added to the downloaded results file
  
### Changed
- improved estimation of the Nyquist period with irregular sampling
- improved plotting of the error bars
- keeping the LS fit when uploading a secondary series
  
### Fixed
- transforming the time units of the estimated sinusoidal periods when uploading a SARI file with a LS sinusoidal fit
- updating the minimum period of the periodogram when resampling the series
- removing the periodogram period bounds when resetting the page
- allowing changing the input series format when the series upload fails
- checking for inf/-inf values in the input series

# abril 2020 [2020.04]

### Added
- new welcoming message to guide the user towards the help page

### Changed
- improved a priori KF state for the linear trend

### Fixed
- time units of the periodogram in the downloaded periodogram file

# marzo 2020 [2020.03]

### Added
- selecting the columns of the secondary series when option 1D is selected
- option to plot the amplitude or power spectrum
- computation of the series scatter from the periodogram
- option to upload a secondary series without the same format of the primary series
- option to modify the station(s) ID
- detailed information of the data from the uploaded series
- option to compute the Vondrak filter from the fit residuals

### Changed
- R package spectral updated from 1.0.1 to 1.3
- periodogram is computed in the background independently of changes in the plot

### Fixed
- checking the validity of the input low/high cut-off periods of the Vondrak filter

# febrero 2020 [2020.02]

### Added
- station(s) ID(s) can be modified by the user
- warning when the series contain NA or NaN values

### Changed
- resampling the series only available after plotting the series

### Fixed
- error bar column removed from the downloaded results file if the user did not use error bars

# enero 2020 [2020.01]

### Added
- option to select the period bounds and the resolution of the wavelet

### Changed
- improved plot of the wavelet heatmap with colors and contours

# diciembre 2019 [2019.12]

### Changed
- reactivity when the user changes the column separator or the column format
- resetting removed outliers not available when fitting a KF

### Fixed
- verifying validity of the input sinusoidal periods

# octubre 2019 [2019.10]

### Added
- option to upload a LS model from a SARI file
- additional popups with short description

### Changed
- primary and secondary series have independent y-axis on the plot
- the automatically detected offsets are deleted when a new model fit is done

# agosto 2019 [2019.08]

### Added
- link to download the periodogram data
- showing comma-separated list of changes from sitelog, station.info and custom files
- offset dates are included in the downloaded results file
- option to automatically detect offsets in the series
- option to hide the secondary series
- offset verification from a KF fit

### Changed
- the offset verification is not available if there are not offsets in the model
- considering columns with multiple spaces in the custom offset file
- removing duplicate dates in the custom offset file
- uploading or plotting a new series is not available if there is one already plotted

### Fixed
- reacting to changes of the selected number of dimensions
- error bars of the estimated noise parameters
- column numbers in the secondary series in 1D format

# junio 2019 [2019.06]

### Changed
- names of coordinate tabs
- names on help, save and print icons
- save button is not available when showing the help page

### Fixed
- updating the error bar values when the user switch them off and on

# mayo 2019 [2019.05]

### Added
- detection of mobile connection
- option to plot the histogram for different type of series

### Changed
- the period of the secondary series is truncated to the one from the primary series
- improved reactivity when changing coordinate tabs

### Fixed
- verification of values in the secondary series
- verification of shortest, longest and ofac values in the periodogram
- labels of the y-axis on the periodogram
- updating the range of the y-axis of the secondary series when changing coordinate tabs

# abril 2019 [2019.04]

### Added
- option to upload a LS model from a previously saved SARI file
- showing a warning when uploading a SARI model from an older version
- option to choose the column number from the file when selecting 1D series
- high-pass and low-pass periods of the Vondrak filter indicated in the downloaded results file
- waveform period indicated in the downloaded results file
- estimated noise parameters indicated in the downloaded results file
- the station ID is automatically extracted from the file name

### Changed
- improved welcoming message

### Fixed
- updating the MLE value when fitting a new noise model
- improving the estimation of the slope of the periodogram
- verifying the length of the series before running MIDAS
- deleting the x- and y-axis ranges when resetting the series
- updating the MIDAS estimate when changing the coordinate tab

# marzo 2019 [2019.03]

### Added
- option to automatically estimate the measurement noise of the KF

### Fixed
- adding hyperlinks when printing the help page into a PDF file
- correcting the link to the sitelog example
- updating the estimated velocity uncertainty when the noise analysis is deactivated
- resampling series in 1D mode

# febrero 2019 [2019.02]

### Added
- updating the estimated velocity uncertainty from the noise analysis
- option for estimating the periodic waveform

### Changed
- removing outliers is not available when fitting a KF
- updating the fitting when the series is resampled

### Fixed
- printing the columns for the model, residuals, filter and waveform in the downloaded results file

# enero 2019 [2019.01]

### Added
- option to verify the significance of the estimated offsets
- option to estimate the wavelets of the series
- option to fit a power-law noise model
- printing the MIDAS estimate(s) in the downloaded results file
- button to print and download the help page into a PDF file
- option to show or correct the secondary series
- option to add process noise to the sine or sine+cosine components of the sinusoidal variation

### Changed
- adding max/min values for the noise parameters
- toggling points is only available if there is a brushed area on the plot
- resetting outliers is only available if there are outliers removed
- the left panel and the main panel have separate vertical scrollbars
- outliers can be removed without fitting a model first

### Fixed
- updating the series when the sampling is deactivated
- correcting the estimation of the error bars for the resampled series
- checking if there are enough wavelet scales to be estimated
- printing the results of the noise analysis
- updating the Vondrak filter plot when activating/deactivating the model fit
- validating the results from the LS and KF fits
- keeping the MIDAS estimate with changes of the fitted model
- validating the hessian matrix in the noise analysis
- validating the input period in the series resampling

# diciembre 2018 [2018.12]

### Added
- option to reduce the sampling of the series

### Fixed
- printing the model equation correctly

# noviembre 2018 [2018.11]

### Fixed
- validating the input reference epoch
- removing the fit results when all the model components are deactivated

# octubre 2018 [2018.10]

### Changed
- the residual error bars are estimated from the original error bars (limited memory usage)
- the run MLE button is only available when a noise model is selected

### Fixed
- periodogram from the filter series or the filter residual series
- noise analysis from the filter residual series

# septiembre 2018 [2018.09]

### Added
- detecting if it is a LAN or WAN connection
- periodogram and histogram with the Vondrak filter residuals
- option to remove outliers from the Vondrak filter residuals
- option to high-pass and band-pass the series with the Vondrak filter
- option to fit a polynomial model

### Changed
- plotting residuals with their estimated error bars
- allowing an exponential/logarithmic epoch outside the series period
- improving the a priori values of the exponential/logarithmic models
- stacking notifications instead of superimposing
- debouncing the inputs from the user

### Fixed
- verifying the number of columns of the input file
- reading files in mode 1D
- updating the longest period of the periodogram when outliers are removed

# agosto 2018 [2018.08]

### Added
- input values for the initial state and errors in KF
- simplified MIDAS estimate as a priori value for the trend

### Changed
- validate the series available to compute the periodogram
- save button is only available if something was done with the series
- rough estimate of the measurement noise in the KF from the series scatter
- Formatting the model equation and including it in the downloaded results file

### Fixed
- adding estimate of the intercept (mean value) when the trend is not fitted in LS and KF
- verifying that the offset epoch is within the series period
- verifying there are enough data to estimate the exponential/logarithmic models
- verifying the Vondrak filter and KF finished without errors
- deleting the model fit if the LS/KF fit failed

# julio 2018 [2018.07]

### Fixed
- Doing nothing if the user selected a model component without epochs
- Estimating the a priori decay time depending on the selected time unit
- Translating the sinusoidal period into the selected time unit

# junio 2018 [2018.06]

- app deployed in ShinyApps
