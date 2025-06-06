# **SARI changelog**


# junio 2025 [2025.06]

### Fixed
- correcting the from/to epochs of the remove period option
- improving the separation between linear and non-linear LS models
- correcting the numeric formating on windows

### Changed
- improving the fullSeries option of the secondary series
- improving the merging of the primary and secondary series
- correcting the assessment of the time axis shift between the primary and secondary series when there are no common epochs
- improving the series format when reducing the sampling
- improving the formatting of the time axis when the series epoch is given in calendar date
- adding the excluded points on the downloaded file from the 3D tab
- replacing N/A by NA in the dowloaded file
- using a pre-downloaded list of stations from the DORIS server

### Added
- adding a new option to merge the primary and secondary series into a single series
- adding the new EOSTLS station database
- adding flag for some headless browser connections


# mayo 2025 [2025.05]

### Fixed
- fixing the series sampling after undoing a resampling
- correcting the averaged sampling of the series after a reset
- improving the plate motion correction when uploading a secondary series
- improving the gia correction of the secondary series
- improving the scale factor of the secondary series
- improving checks on the downloaded series file
- correcting the 3D plots when adding a 1D secondary series
- avoiding updating the ID of the primary series when reading the secondary series file
- removing duplicated epochs when reading a soln.snx file
- removing the pointer to the soln.snx file after a reset
- fixing the product names of the formater server

### Changed
- adapting the code to the new R 4.4.3 version
- improving the shift of the time axis of the secondary series when merging with the primary series
- disabling the series units option when the series format is known
- removing the fitted model from the 3D plot when it is removed from a component
- updating the overview plot when changing the series units
- removing the plot of the secondary series when a new secondary series is uploaded
- avoiding removing the fitted model when loading a new secondary series
- improving the format of the resampled series & the midas/entropy estimates
- removing empty spaces in the model equation printed in the results file
- splitting the LS equation model in several lines printed in the results summary on screen
- changing the debounce of the input averaging period to 1 sec
- correcting the station list from the EARTHSCOPE server

### Added
- adding the series from the DORIS server (L. Soudarin, G. Moreaux)


# abril 2025 [2025.04]

### Fixed
- changing the epochs extracted from the sitelog, station.info, soln and custum files when the series time units are not years
- correcting the merging of the primary and secondary series in 1D format
- centering the series of the overview plot on the estimated 1D models when removing a plate/gia model
- centering the series of the 3D plot on the estimated 1D models when removing a plate/gia model
- removing the plate/gia model also from the toggled points
- increasing the number of header lines read from a PBO file

### Changed
- improving the uploading of secondary series
- updating the break epochs with changes of the time units
- updating the gia correction when changing the time units
- removing duplicated sinudoidal periods
- disabling all the metadata prints on screen for the 3D tabs
- correcting the IGS20 product name when downloading the sitelog files
- updating the NLG server address
- updating the names of the UGA series files

### Added
- adding a new option to fit segmented models with LS between break epochs (J. Nicolas)
- adding a new option to remove a portion of the series between two epochs
- adding the IGS20 and ENS solutions from the Formater server
- allowing the download of all the coordinate components at once when correcting with a secondary series
- allowing the download of all the coordinate components at one when removing a plate or gia model
- adding information on screen about an empty file on a remote server


# marzo 2025 [2025.03]

### Fixed
- improving the parsing of sitelog files (F. Feriol)
- checking the validity of the significance of the estimated parameters
- correction when some points are deleted on the 3D tab before selecting a coordinate
- resetting the scale factor of the secondary series after loading a series from the EOSTLS server
- correcting the dates of the antenna changes extracted from the spotgins discontinuity file
- checking if data gaps can be flagged

### Changed
- avoiding removing the median value from the vertical series when correcting for the horizontal plate motion
- avoiding computing the minimum entropy from series with a constant value
- improving the format of the results of the midas and minimum entropy estimates on screen
- removing the authentication for the formater server
- adding the column names of the correlation matrix at the bottom only with many parameters
- checking the LS summary output before changing the colors

### Added
- plotting the receiver changes from the soln and custom files with dashed lines on the residuals plot


# febrero 2025 [2025.02]

### Fixed
- avoiding rounding the series sampling value when changing the time units (F. Feriol)
- avoiding the primary series sampling being modified by the secondary series sampling and by the average option

### Changed
- changing the symbol and size of the secondary series (M. Métois)
- moving the reset button away from the toggle button (F. Feriol)
- keeping only the common epochs between several uploaded secondary series (F. Feriol)
- forcing Firefox to not autocomplete the input variables between sessions (F. Feriol)
- deactivating the secondary axis options when loading a new secondary series
- improving the change of the averaging period when changing the time units
- improving the format of the summary of the LS fit results on screen
- adding one decimal more when transforming day units into year units
- improving the verification of the downloaded file from the remote server
- moving the label with the component name to the left on the 3D plots when the time units are not years

### Added
- allowing the upload of several secondary series of different format simultaneously
- separation of the antenna and receiver from the soln and custom files


# enero 2025 [2025.01]

### Fixed
- deactivating the draconitic option when loading a SARI model with sinusoids (F. Feriol)
- reducing the sensibility of the click & collect option when zooming in on the series with a touchpad
- disabling the error bars option when the error bar column is not read in 1D format
- correcting the computation of the a priori intercept value when changing from a KF to a LS fit
- updating the reference epoch of the sinusoids automatically if the input value is not valid

### Changed
- changing the station names of the sonel logfiles from 4 to 9 characters (M. Gravelle)
- hidding the load SARI model option when showing the 3D plot tab (F. Feriol)
- fixing the a priori values of the sinusoidal fit to zero (F. Feriol)
- reducing the sensibility of the LS fit to the a priori values
- improving the format of the estimated parameters in the downloaded file
- improving the format of the KF results on screen
- improving the format of the estimated sinusoidal components on screen
- improving the format of the series statistics on screen

### Added
- new ERA5LAND product from the EOSTLS server
- new IGS20 produt from the NGL server
- new notification on the screen when compiling the LS model function
- new notification on the screen when firing the click & collect option


# diciembre 2024 [2024.12]

### Fixed
- correcting the formatting of the series values (D. Rodriguez Collantes)
- enabling the remote series selector after showing the help page
- correcting the reading of a neu file that contains alphabetic characters
- correcting the loading of more than one secondary series file
- correcting bug when it is not possible to compute the formal error of the fit residuals
- correcting the difference between primary and secondary series when some points were already deleted
- avoiding removing all the points of the series manually
- enabling the removal of points on the 3D plots before selecting any coordinate component

### Changed
- enabling the series download after correcting a GIA model
- improving the information of the corrected models in the downloaded file
- improving the fit of a LS model uploaded from a SARI file
- improving the zoom-in on the 3D plots
- updating the overview plot when using a plate or GIA model
- reducing the number of error messages when the reading of the input series fails
- adding a one-second buffer from plot clicks to identify double-clicks & drags on touchpads
- avoiding firing the wavelet and location maps when a new session starts
- correcting the definition of the data ranges in the 3D plots
- improving the processing of the plate and gia models for 1D series
- updating the api of the sonel server (M. Guichard & M. Gravelle)

### Added
- new option to automatically fit draconitic harmonics by constellation (M. Gravelle)
- new click & collect option to include the clicked points on the plots to the list of offset epochs (M. Gravelle)


# noviembre 2024 [2024.11]

### Fixed
- avoiding missing information on the original sampling of the series

### Changed
- making changes due to the new api of the EPOS server (A. Walpersdorf)

### Added


# octubre 2024 [2024.10]

### Fixed
- checking the amount of valid points after merging with the secondary series

### Changed
- improving the format of the transformed IGS and SIRGAS series

### Added
- loading series from local files in the IGS PLH format


# septiembre 2024 [2024.09]

### Fixed
- adding checks for the station coordinates in the series file header
- correcting the extraction of the station coordinates from a local PSMSL file
- correcting the missing status when averaging two 1D series
- correcting the use of the fullSeries, sameScale and same_axis options for 1D series
- correcting the zoom range when loading a secondary series but not using it

### Changed
- improving the change of time units for ordinal time series in days or weeks
- updating the plots following changes of the fullSeries option
- improving the text of the help pop-up of the location map
- improving the notification on screen about wrong time epochs

### Added
- new ad hoc C++ functions to optimize the noise analysis on Windows
- showing the closest point to click on the periodogram (F. Feriol)


# agosto 2024 [2024.08]

### Fixed
- correcting bug when merging two secondary series from local files
- correcting the primary series with a secondary series, both in 1D format
- loading a 1D series after a 3D series has been loaded and reset
- updating the scientific format flag when resetting and loading a new series

### Changed
- replacing the series names by the product names in case of downloading several EOSTLS secondary series
- automatically scrolling the blocks of the left panel to the center of the screen when opening them
- setting the number of decimals in the series after averaging or merging them
- clicking the tooltips opens the help file in a new browser tab in html format (F. Feriol)
- adding a secondary time axis with different units to the top of the periodogram
- deactivating the wavelet option when one of its parameters changes
- updating the uncertainties on the overview plot when fitting a noise model
- improving the estimate of the a priori value of the logarithmic offset
- allowing downloading the file of the remote secondary series even after correcting the primary series
- 'a priori' labels by 'initial' labels in the KF fit

### Added
- two new tabs (3D and residuals) in the visualization panel to plot all the coordinate components simultaneously (M. Métois, F. Feriol)
- plotting the fitted model on the overview plot together with the estimated rate and the dispersion of the model residuals (M. Métois)
- option to show & hide the location map with a spinner and height fitted to the screen size
- setting the format of monthly PSMSL RLR series from local files automatically


# julio 2024 [2024.07]

### Fixed
- fixing bug when correcting the primary series with a new uploaded secondary series
- improving the estimation of the spectral index of the power-law noise model
- checking the periodogram data before writing it to a file
- removing duplicate values from the series when computing the minimum entropy
- fixing the uncertainty of the noise series from the residuals of a Kalman fit

### Changed
- improving the assessment of the number of decimal places to use
- avoiding showing a zero uncertainty for the estimted spectral index of a power-law model
- updating the expected processing time of the MLE fit
- updating the values of the Student's t-test after a MLE fit
- converting CNES Julian days into Modified Julian days when changing the time units
- making the help popups to open the help file at the right spot

### Added


# junio 2024 [2024.06]

### Fixed
- correcting the estimation of the power-law crossover period
- fixing bug with a new uploaded secondary series when the previous secondary has been merged with the primary series
- fixing bug with the series truncation when the provided epochs have already been used in a previous truncation

### Changed
- removing the model and residuals spectra when the series fit is deleted
- reducing the z-index value of the zoomed-in label so it hides under the navbar
- disabling text selection on the left panel
- updating the options of the secondary series when only the primary series is shown
- improving the format of extremely small values
- extracting the a priori value of the power-law spectral index from the power spectrum

### Added
- separating the residual series into the contribution of the different noise model components with the Wiener filter [P Rebischung]
- uploading local series in GeoCVS format from EarthScope and in PLH format from SIRGAS
- converting the CNES Julian days into Modified Julian days
- adding a random and anonymous session id to the log file


# mayo 2024 [2024.05]

### Fixed
- taking into account the removed points when checking the validity of the resampling period
- correcting the plate model name in the downloaded file
- checking the kalman filter results before adding them in the downloaded file
- correcting the estimation of the offset between the primary and secondary series
- correcting the series averaging when different points have been removed for each series component

### Changed
- allowing resampling the secondary series down to periods slightly larger than the sampling rate
- allow downloading the series after removing only the plate motion
- changing the name and location of the downloaded remote files to avoid collisions between users
- updating the color of the run KF button when changing the model components after a KF fit

### Added


# abril 2024 [2024.04]

### Fixed
- bug when the download of a remote SPOTGINS file fails and a new local NEU file is loaded
- correcting the measurement noise value extracted from a SARI file with a KF fit
- correcting the column names in the downloaded file when fitting a KF
- reading the PSMSL tide gauge station names having single quotes
- checking the series values before computing the series summary
- setting the time units for the PSMSL series

### Changed
- improving the check for the regular sampling of the primary and secondary series
- avoiding updating the location map unnecessarily

### Added
- new VLM corrections from several GIA models
- new tooltips for each drop-down block of the left panel

# marzo 2024 [2024.03]

### Fixed
- improving the estimate of the linear trend uncertainty from a PL noise fit
- improving the a priori linear trend value when the sampling is extremely small
- checking the numeric values of the epoch's column in a 1D file
- fixing the zoom on series with zero values
- downloading the list of available JPL stations if the list is not available on disk
- correcting the name of the secondary series in the downloaded file
- avoiding processing the URL for mobile connections
- correcting the print of the sinusoidal phase units
- differentiating between position and tropo series from SPOTGINS
- removing the second MIDAS trend estimate when there are no offsets left in the LS model

### Changed
- forcing the location of the map center to be closer to the mark location when zooming in and out
- patching the scrolling behaviour when loading a file (shiny bug)
- checking if the time units of the series are known or set before plotting
- removing the flag of known time units when resetting the analysis
- keeping the error bars active when loading a secondary series from EOSTLS
- correcting EOSTSL by EOSTLS
- correcting the series units from the PSMSL
- checking the coordinates extracted from a SPOTGINS position series
- reducing the screen width for mobile devices

### Added
- option to estimate the linear trend based on the Shannon entropy
- showing the number of removed points in the series information block

# febrero 2024 [2024.02]

### Fixed
- fixing the noise crossover and the uncertainty ratio for the RW and PL models when the time units are not years
- fixing the power law variance scaling (typo from 2023.12 version)
- showing the crossover period between RW and FN when WN is not estimated
- taking into account the N@E option when merging the secondary series
- taking into account the N@E option when computing the plate motion model of the secondary series
- checking the values of the error bars extracted from the input series file
- taking into account the truncation of the series in the overview plot
- taking into account the points already removed from the series when applyting the truncation
- fixing the time units in the results file when the series are resampled and the time units of the series change
- fixing the units of the sinusoidal phase estimates in the results file
- fixing the undefined errorbars of the periodic waveform when there is only one sample per epoch
- running the offset verification only when hitting the button
- avoiding computing and then hiding the periodogram with a change of the coordinate component

### Changed
- adding the units of the estimated power-law noise amplitudes to the results file
- updating the predicted duration of the MLE analysis with the Nelder & Mead method
- removing the offset verification results when deactivating the option
- removing the filter residuals when deactivating the Vondrak filter option
- removing the noise analysis results when changing the fitted model
- removing the periodogram plot when the corresponding model or smoother series are not valid
- improving the format of the numeric values in the results file
- new format of the series units in the results file
- adding the coordinate component in the name of the downloaded periodogram file

### Added
- new links on the left panel to open the uploaded ancillary files on a new browser tab
- more information on screen when reading a NEU/ENU series fails

# enero 2024 [2024.01]

### Fixed
- reading a KF model from very old SARI files
- showing the process noise crossover period only when necessary
- correcting the variance scale of the noise power spectrum when different spectra are computed
- changing the secondary series shown when uploading new secondary series files
- improving the warning on screen when the page is reloaded and a remote SARI instance is being initiated
- updating the time axis of the series when the time units change or points are removed permanently
- checking the series units before transforming lat/lon into XYZ coordinates
- correcting the default number of epochs of the wavelet
- removing the max/min periods of the periodogram when changing the column of the epochs in a 1D series

### Changed
- updating the ITRF plate model from ITRF2014 to ITRF2020
- setting the priority of the model residuals over the smoother residuals
- correcting the number of decimals in some columns of the downloaded file
- adding the coordinate component to the name of the downloaded file when using the local directory option
- hiding the names of the coordinate component tabs till a file is loaded
- improving the fullSeries option & the range of the time axis
- extracting the station coordinates for the secondary remote series
- updating the location map when the secondary series changes
- changing the name of the popup window with the series name
- updating the overview plot in the popup window when the series plot changes
- closing the popup window with the reset button
- adding error bars to the periodic waveform
- improving the swap button with the sitelog names and the scale factor of the secondary series
- disabling the sigmas option when loading a remote PSMSL series

### Added
- popup message when the page is reloaded by the user on an active remote session
- new option to truncate the time axis of the series (toggle or delete)
- the downhill simplex method to improve the estimation of the spectral index of the PL noise model analysis

# diciembre 2023 [2023.12]

### Fixed
- Checking for input non-numeric values of the KF measurement error
- Correcting the scale factor of the station coordinates in mm for series from the EPOS server
- Improving the a priori max/min periods of the periodogram
- Correcting the units and label of the histogram
- Updating the location of the predicted plate motion as the user zooms in on the series
- Forcing the extracted station ID into a string to avoid numeric station IDs
- Checking the information extracted from the sitelog and station.info files before changing the time units

### Changed
- Optimizing the processing of several secondary series from the EOSTLS remote server
- Automatically setting the format of the secondary series based on the file name
- New formatting of the downloaded periodogram file
- Computing the plate model independently of the reading of the series file
- New database system that keeps the original and resampled series for both primary and secondary
- Updating the series units according to the name of the uploaded file
- Allowing changes of the selected plate motion model and plate
- Changing the trend, sinusoidal and polynomial reference epoch values when the series time units change
- Removing the plate model also from the secondary series
- Checking the validity of the information extracted from the sitelog
- Updating the available loading models from the EOSTLS server
- Removing the plate motion model when making the difference between the primary and secondary series
- Skipping optional packages that were not installed (only for local sessions)

### Added
- New package dependencies: geojsonio, ragg
- Links on the left panel for saving the files of the primary and secondary remote series
- Support for epochs provided in ISO 8601 calendar date and time format
- Interactive plate boundaries with their names on the location map (if the optional geojsonio package was installed)
- Showing the station ID on the location map
- New overview button to open a popup window with a plot of the current view of the three coordinate components together
- New swap button to interchange the roles of the primary and secondary series
- Warning on screen when the units of the series are unknown
- Warning on screen when using both ECCO and TUGO loading models in the secondary series
- Access to the series from the PSMSL server

# noviembre 2023 [2023.11]

### Fixed
- Loading a PBO series from a URL query
- Checking if the EPOS JSON file was correctly downloaded
- Holding the zoom on the primary series when toggling points
- Loading a local series using the sari.sh script
- Avoiding overflow in the text of the notifications on the screen

### Changed
- Converting any series format into a ENU format
- Deactivating the confirmation on screen when resetting the analysis
- Transforming the epochs extracted from the station.info, sitelog, soln.snx and custom file following the selected time units of the series
- Removing the crossover period shown on the screen if the noise model is not available
- Sorting the extracted epochs from the custom offset file

### Added
- Combining several loading series from the EOSTSL server as a single secondary series
- Automatic download of sitelogs from the RENAG, EUREF, EPOS, SONEL and SIRGAS servers
- Automatic upload of the soln.snx file from the IGS server
- Automatic upload of the steps.txt file from the NGL server
- Automatic upload of the formater_offsets.dat file from the FORMATER server

# octubre 2023 [2023.10]

### Fixed
- Checking the number of stations found in remote server
- Identifying the North/East columns of a SPOTGINS series when loaded from a file
- Correcting the server name EOSTSL by EOSTLS when checking the series units
- Checking if the series units are defined when computing the plate motion
- Correct units on the plot of the instantaneous rate
- Fixing the checks of geographic coordinates for the station
- Bug when clicking back on the web browser
- Opening the plate model file in a new browser tab
- Checking for numerical values before setting the numeric format for the downloaded result file

### Changed
- Improving the extraction of the station name from the series file
- New location marker on map to avoid problems with Docker
- Setting R package mvcwt as an option
- Adding the coordinate component name to the name of the downloaded results file
- Starting and running a new SARI container if necessary with the script sari.sh
- Improved formatting of numericals on screen and improved LS a priori values for the KF fit
- Improved handling of the optimized measurement noise in the KF fit
- New url for the Formater server

### Added
- Accesss to the GNSS series from the EarthScope/UNAVCO server
- Asking for confirmation to refresh the webpage on remote sessions (better use the reset button)

# septiembre 2023 [2023.09]

### Fixed
- Updated dependencies for the SARI Docker container.
- Zooming in on the primary, secondary and residual series, with and without the same scale option
- Loading a SARI file with the new debouncers set in the previous version
- Reactivity of the histogram after changes in the series or residuals
- Checking wrong input values of the scale factor option for the secondary series
- Removing the station1/station2 values and keeping the product1/product2 values when the download of the remote series fails
- Computing the plate motion using information from the primary series only
- Internal links of the online help file from desktop sessions
- Allowing printing the help file into a PDF file after the UI has been temporarily hidden by the splash screen when loading the page

### Changed
- Using a more colorblind-friendly palette for the plots
- Wider lines for the series, residual and periodogram plots
- Rearranging the layout of a few options for a better user experience
- Automatically changing the series format option by looking at the name of the uploaded file
- Increasing the shinyapps instance startup timeout to 30 s
- Improved handling of the input offset epochs, skipping those irrelevant without stopping the analysis
- Time reference of the exponential function set at the beginning for better fitting co-seismic displacements
- Showing the PSD of the noise model only when the PSD of the residuals is plotted
- Showing the series information block only when the series have been read
- Improving the centering of the secondary series over the primary series with the same scale option
- Allowing removing a plate motion model from 1D series
- Allowing downloading the results when only removing a plate motion model

### Added
- New package dependency: leaflet
- Showing location map when the station coordinates are known
- EPOS and SIRGAS servers
- Showing the units of the series and of the estimated parameters when they are known
- New option to plot the total length of the secondary series or only the period in common with the primary series
- Adding the plate model and plate name in the downloaded file

# agosto 2023 [2023.08]

### Fixed
- Correcting bug with all debouncers that made the app to react too quickly to the user typing
- Correcting the transformation of the time units in GPS weeks for the SPOTGINS series
- Correcting bug when the median value is interpolated (series statistics & histogram)
- Improved check of local session in docker
- Fixed permission problem in docker when writing into a local file
- Resetting the value of the product variable if the download of the series fails
- Checking the selected window before zooming in
- Checking for offset epochs outside the data limits, duplicated offsets or no observations between offsets
- Checking the number of points before firing the MLE time estimate
- Fixed bug when the primary and secondary series have exactly the same sampled points

### Changed
- The app will wait 1 s after the user stops typing values before reacting
- Enabling the reset button to remove the uploaded series even if they are not plotted yet
- Selecting the remote product automatically if there is only one available
- Plotting the secondary series automatically right after being downloaded from the remote server
- Improving the checks of the downloaded series from the remote server
- Disabling the option neu/enu when downloading a NEU/ENU series with known format
- Improving the centering of the secondary series over the primary series
- Allowing averaging the series to a regular sampling close to the original sampling
- Limiting the number of decimals of the estimated parameters in the downloaded file

### Added
- Changing the tab names into coordinate components when this is known from the series format
- Automatically downloading the list of available stations for the selected remote server & product
- New package dependencies (RCurl, XML, jsonlite)
- Using authentication for the Formater server
- Option to type in expressions to compute the averaging period (for instance =7/365.25)
- Plotting the SPOTGINS series in different time units
- Plotting of the FN+RW noise power spectrum including information about the crossover period(s)
- Starting a local docker session with sari.sh
- Downloading the station coordinates for the JPL series

# julio 2023 [2023.07]

### Fixed
- Time units of the harmonics of the sinusoidal periods
- Fixing unknown previous points in the series when removing new

### Changed
- Fully rewritten noise analysis section
- Noise analysis with MLE changed from the constrained L-BFGS-B method to an unconstrained quasi-Newton method
- Improved handling of data gaps for the noise analysis and Kalman filter
- Checking extracted series before removing the plate motion
- Removing access to SIRGAS server: URL not found
- New dependency: package mnormt for pd.solve function
- sari.sh: logging session by default and saving log only if option -v

### Added
- New option to download and plot a GNSS series from a remote server
- New option to use different plate motion models
- Access to the series from the SONEL server
- New noise model option: FN+RW, also for the offset verification
- New option to skip the estimation of the formal uncertainty of the noise parameters
- The expected MLE processing time is shown on screen

# junio 2023 [2023.06]

### Fixed
- Bug with empty spaces before a comment at the begining of the line
- Bug when removing residuals manually and the removed3D option is not active
- Bug with extremely large integers in the input series
- Bug when loading a NEU file with more than 4 and less than 7 columns (not standard NEU, but OK)

### Changed
- Faster verification of the significance of offsets
- Loading a local series on a SARI session without using options -w and -p in sari.sh

### Added
- New ui function for the different coordinate tabs
- Bad epochs in the series are indicated on the screen when loading a file

# mayo 2023 [2023.05]

### Fixed
- Problem running xdg-open in WSL2
- Histogram reacting to changes of the column numbers
- Saving removed points before averaging the series
- Problem with png-cairo in R 4.2.3
- Forcing the C locale to avoid warnings on Linux

### Changed
- Hiding interface when webpage is loading
- Much faster offset verification
- Showing original file header only for 1D series
- Improved messages for the noise analysis results
- Checking output when averaging the series
- Improving centering of the secondary series with the same scale option

### Added
- Sinusoidal amplitude & phase estimates in the results file
- Access to series from the Formater server
- Access to series from the IGS server
- Access to series from the SIRGAS server

# abril 2023 [2023.04]

### Fixed

- Correcting path of some EOSTLS products
- Bug related to daylight saving time changes when transforming calendar dates into decimal year
- Bug when reading the min & max measurement noise values in KF
- Bug with error bars not defined for the removed points
- Bug when typing the higher harmonics in the sinusoidal fit
- Bug when resetting the analysis

### Changed

- Shifting the secondary series to match the sampling of the primary series
- Updating plots when changing the scale factor of the secondary series
- Improved computation of the error bars for averaged series
- Checking output when merging primary and secondary series
- Checking downloaded remote series
- Showing header of current used series when available
- Deleting reference epochs in the fitted model when changing the time units
- Improved formatting of the information on the series range and sampling
- Station IDs in upper case
- Updating EUREF server series to C2235

### Added

- Shell script to launch SARI from a Unix-like terminal
- Progress bar on some long computations
- Option to upload several secondary series at the same time
- Secondary time axis with decimal years at the top of the plots
- Checking sampling of primary vs secondary series
- Option to average the secondary series
- Transforming PBO dates into GPS weeks
- Message on screen with the number of KF iterations
- Automatically scrolling the page when the periodogram or the wavelet finish
- Series from the JPL server
- Removing UNAVCO from the available servers (problem with IdM)

# marzo 2023 [2023.03]

### Fixed

- Bug with the initial wavelet epochs when the series has less than 500 points
- Bug in the measurement noise optimization with the time variable error bars implemented in the UKF
- Bug when plotting the error bars and loading primary and secondary series at the same time

### Changed

- Simplified managment of status of points in the series (used, removed, deleted)
- Updating plots with new width after browser window changes

### Added

- Option to parse URL parameters to load primary and secondary GNSS series
- Option to apply a scale factor to the secondary series
- Option to swap N/E components of the secondary series
- Automatic extraction of the station coordinates for the plate model from NGL and PBO files
- Devmode and fullstacktrace options for debugging

# febrero 2023 [2023.02]

### Fixed

- Bug when plotting the primary and secondary series at the same time with different number of points
- Bug when the sinusoidal period is the same as the series sampling

### Changed

- Improved location of the FN power spectrum on top of the periodogram with different time units
- The link to download the periodogram data appears only when necessary
- Simplifying the code for plotting the histogram and checking for stationarity

### Added


# enero 2023 [2023.01]

### Fixed

- Fixing click on SARI logo before loading a series
- Removing points when showing/correcting a secondary series
- Averaging the series with points removed permanently

### Changed

- Disabling a few more options when showing the help page

### Added

- Sinusoidal periods units to the estimated amplitude and phase results
- Compressed form to fit sinusoidal harmonics


# diciembre 2022 [2022.12]

### Fixed:
- Checking selected coordinate component before plotting
- Bug on the computation of the sinusoidal amplitude and phase errors
- Checking series values before computing stats and stationarity
- Bug with the instantenous rate series not being shown when needed

### Changed:
- Improved CSS style of the fit results section
- Updated install instructions to run SARI locally from R/RStudio

### Added:
- New option to delete points from the series permanently
- Removing points from the series plot after fitting a KF
- Keeping in memory valid points from the primary and secondary series independently

# noviembre 2022 [2022.11]

### Fixed:
- Showing plot of instantaneous rate only when necessary

### Changed:
- Automatically removing obsolete notifications on screen


# octubre 2022 [2022.10]

### Fixed:
- Bug when reading hour string "000000" from PBO files
- Bug when reduce sampling is active but no averaging window is provided
- Bug when changing from LS to KF and the series were previously resampled

### Changed:
- Checking if there are noise analysis results when trying to plot the noise spectrum
- Additional checks for several input values
- Checking if the series are more or less evenly sampled before updating the KF process noise
- Improving the warning message when reading the station.info file
- Removing console warning when reading the custom offset file
- Removing KF fit, wavelet and filter results when changing time units
- Removing values of residual threshold when restoring all removed points
- More detailed warning when fitting long-period sinusoids
- Changing behavior when error bars are read, activated and deactivated
- Notifications and warnings are only shown once for each message
- Preventing changing to time unit weeks in a PBO series

### Added:
- Showing new warning on screen when the size of the browser window changes
- Showing new message on screen when uploading a RINEX file
- New plot to show the time-variable trend estimate from a KF fit
- Warning on screen when changing the time units and the series were previously resampled
- New kalman filtering approach for estimating the instantaneous linear rate
- Adding time-variable measurement and process noises to the KF fit
- New option to disable scrolling before taking a full page screenshot
- Adding error bars to the KF fit and posibility to remove normalized outliers
- Adding units to the sinusoidal phase estimates
- Loading a SARI file with a previous KF fit

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
