#!/usr/bin/env bash

# Alvaro Santamaria 07/03/2023

#########################################################################################################################

usage () { echo "
`basename $0` opens a SARI session on the web browser from the command line.

This script only runs on desktop environnements of Unix-like systems.
The SARI session can be either local (running on your machine) or remote (running on the Shinyapps server).
To open a local session, all the R packages required to run SARI must be installed first (see the INSTALL file).
Time series can be uploaded using local or remote files from specific servers and products indicaded below.

Syntax: $(basename $0) -l|r [-w server1+server2 -p product1+product2 -s series1+series2 -v]

	-l 			: starts a local SARI session (no series uploaded)
	-r 			: starts a remote SARI session on Shinyapps.io (no series uploaded)
	-w server1+server2	: uploads primary (+secondary) file from server (see available servers below)
	-p product1+product2 	: uploads primary (+secondary) file from product (see available products below)
	-s series1+series2	: path to a local file or remote station ID with 4 (e.g., PIMI), 9 (e.g., PIMI00FRA)
				  or more 14 (e.g., PIMI_10025M001) depending on the server used
	-v 			: saves a log of the current SARI session in $saridir
	-h			: shows this help

	Five different sessions can be started:

	Empty local session 			$(basename $0) -l
	Empty remote session 			$(basename $0) -r
	Local session with local series		$(basename $0) -l -w local -p product -s path/to/my/series
	Local session with remote series	$(basename $0) -l -w server -p product -s ID
	Remote session with remote series	$(basename $0) -r -w server -p product -s ID

	server1+server2, product1+product2 and station1+station2 are used to load two series (primary and secondary) 
	at the same time from the same or different servers/products.

	The remote series will be downloaded to the local server (your machine in $saridir)
	or the remote server (Shinyapps). It is not possible to upload a local series to the Shinyapps server via
	this script. The in-app SARI interface must be used for that.

	In a local session, the SARI app runs in the background of your machine till this script is interrupted by 
	pressing Ctrl+C. The script also exits if the SARI app stops.

        +--------+--------------------------------------+---------+----------------------------------------------------+
        | Server | Product                              | Station | Reference                                          |
        +--------+--------------------------------------+---------+----------------------------------------------------+
        | LOCAL  | NEU, PBO, NGL, 1D                    | path    |                                                    |
        | RENAG  | UGA                                  | 4 char  | http://renag.resif.fr/en/                          |
        | NGL    | FINAL, RAPID                         | 4 char  | http://geodesy.unr.edu/                            |
        | EUREF  | PBO                                  | 9 char  | https://epncb.eu/_organisation/about.php           |
        | UNAVCO | CWU, NMT, PBO                        | 4 char  | https://www.unavco.org/data/gps-gnss/gps-gnss.html |
        | EOSTLS | ATMIB, ATMMO, ECCO, ECCO2, ERA5IB,   | 14 char | http://loading.u-strasbg.fr/                       |
        |        | ERA5TUGO, ERA5HYD, ERAHYD, ERAIN,    |         |                                                    |
        |        | GRACE, GLDAS, GLDAS2, GLORYS, MERRA, |         |                                                    |
        |        | MERRA2ATM, MERRA2HYD                 |         |                                                    |
        +--------+--------------------------------------+--------------------------------------------------------------+

" 1>&2; exit 1; }

usage_short () { echo "
Syntax: $(basename $0) -l|r [-w server1+server2 -p product1+product2 -s series1+series2 -v]

	-l 			: starts a local SARI session (no series uploaded)
	-r 			: starts a remote SARI session on Shinyapps.io (no series uploaded)
	-w server1+server2	: uploads primary (+secondary) file from server (see available servers below)
	-p product1+product2 	: uploads primary (+secondary) file from product (see available products below)
	-s series1+series2	: path to a local file or remote station ID with 4 (e.g., PIMI), 9 (e.g., PIMI00FRA) 
				  or 14 characters (e.g., PIMI_10025M001) depending on the server used
	-v 			: saves a log of the current SARI session in $saridir
	-h			: shows the full help

	Empty local session 			$(basename $0) -l
	Empty remote session 			$(basename $0) -r
	Local session with local series 	$(basename $0) -l -w local -p product -s path/to/my/series
	Local session with remote series	$(basename $0) -l -w server -p product -s ID
	Remote session with remote series	$(basename $0) -r -w server -p product -s ID

" 1>&2; exit 1; }

#########################################################################################################################

# Setting a trap to do a clean exit
cleaning () {
	rm -f $saridir/app_$now.R
	if [[ ! -z $pid ]]; then
		netstat -anp 2> /dev/null | grep :$port | grep LISTEN | grep -E "$pid/R\s+$" | sed 's$/R$$' | awk 'system("kill "$NF"")'
	fi
}
trap cleaning EXIT

# Checking dependencies
netstat -h > /dev/null 2>&1
if [[ $? != 0 ]]; then
	echo WARNING: netstat is not available. Local sessions may not work as expected
fi
Rscript --help > /dev/null 2>&1
if [[ $? != 0 ]]; then
	echo WARNING: Rscript is not available. Impossible to open local sessions.
fi
uname --help > /dev/null 2>&1
if [[ $? != 0 ]]; then
	echo FATAL: uname is not available.
	exit 1
fi
xdg-open --help > /dev/null 2>&1
if [[ $? != 0 ]]; then
	echo FATAL: xdg-utils is not available.
	exit 1
fi

# Setting directory paths and checking the SARI app file
currentdir="$(pwd)"
saridir="$(dirname $(dirname $(realpath $0)))"
if [[ ! -f $saridir/app.R ]]; then
	echo Unable to find the SARI app.R script in $saridir
	exit 1
fi

# Getting input command-line options
while getopts :lrw:p:s:vh option; do
	case $option in
		w  )	server=$OPTARG;;
		p  )	product=$OPTARG;;
		s  )	station=$OPTARG;;
		l  )	local=true;;
		r  )	remote=true;;
		v  )	logging=true;;
		h  )    usage;;
	        \? )	echo "Unknown option: -$OPTARG" >&2; usage_short;;
	        :  )	echo "Missing option argument for -$OPTARG" >&2; usage_short;;
	        *  )	echo "Unimplemented option: -$OPTARG" >&2; usage_short;;
	esac
done

# Setting output log file
now=$(date '+%Y%m%d_%H%M%S')
if [[ -z $logging ]]; then
	out="/dev/null"
else
	out="$saridir/SARI_$now.log"
	echo "Logging the SARI session in $out"
fi

# Setting the listening port for local sessions
if [[ ! -z $local ]]; then
	declare -i port=6999
	running=0
	while [[ $running == 0 ]]; do
		port+=1
		if [[ $port > 7030 ]]; then
			echo The number of ports tried exceeds 30
			exit 1
		fi
		netstat -an | grep :$port | grep LISTEN > /dev/null 2>&1
		running=$?
	done
fi

# Removing calls to png-cairo (problem in WSL) and deactivating devmode (problem with local session reload)
uname -a | grep microsoft > /dev/null 2>&1
wsl=$?
if [[ ! -f $saridir/app_$now.R ]]; then
	if [[ $wsl == 0 ]]; then
		sed 's/, type = "cairo-png"//' $saridir/app.R | sed 's/devmode(TRUE)/devmode(FALSE)/' > $saridir/app_$now.R
	else
		sed 's/devmode(TRUE)/devmode(FALSE)/' $saridir/app.R > $saridir/app_$now.R
	fi
fi

# Splitting parameters of the primary and secondary series
if [[ ! -z $server && ! -z $product && ! -z $station ]]; then
	IFS=+ read -r server1 server2 <<< $server
	server1=$(echo $server1 | tr '[:upper:]' '[:lower:]')
	server2=$(echo $server2 | tr '[:upper:]' '[:lower:]')
	IFS=+ read -r product1 product2 <<< $product
	product1=$(echo $product1 | tr '[:upper:]' '[:lower:]')
	product2=$(echo $product2 | tr '[:upper:]' '[:lower:]')
	IFS=+ read -r station1 station2 <<< $station
fi

# Setting list of available URL parameters
servers=" local renag ngl euref unavco eostls "
products=" enu neu pbo ngl 1d uga final rapid cwu nmt atmib atmmo ecco ecco2 era5ib era5tugo era5hyd erahyd erain grace gldas gldas2 glorys merra merra2atm merra2hyd "

# Checking valid arguments
contains() {
	if [[ ! " $1 " =~ " $2 " ]]; then
		echo FATAL: $2 in not a valid input argument
		exit 1
	fi
}
contains "$servers" $server1
contains "$servers" $server2
contains "$products" $product1
contains "$products" $product2

# Catching SARI installation error
checkR() {
	if ! ps -p $pid > /dev/null; then
		echo Problem running Rscript to start SARI. Check in the log that all the SARI dependencies are correctly installed.
		exit 1
	fi
}

# Setting the type of session from the input command-line options
waiting() {
	if ! ps -p $pid > /dev/null; then
		echo Problem running Rscript to start SARI
	else	
		echo "Press Ctrl+C to stop the SARI session"
		wait $pid
	fi
}

# Remote session
if [[ -z $local && ! -z $remote ]]; then

	# Empty remote session
	if [[ -z $server1 && -z $product1 && -z $station1 ]]; then
		xdg-open "https://alvarosg.shinyapps.io/sari"

	# Remote session with remote file
	elif [[ ! -z $server1 && ! -z $product1 && ! -z $station1 ]]; then
	
		if [[ $server1 == local ]]; then
			echo It is not possible to use a local file on a remote session
			usage_short
			exit 1
		fi

		# Primary and secondary series
		if [[ ! -z $server2 && ! -z $product2 && ! -z $station2 ]]; then

			if [[ $server2 == local ]]; then
				echo It is not possible to use a local file on a remote session
				usage_short
				exit 1
			fi
			
			xdg-open "https://alvarosg.shinyapps.io/saribeta/?server=$server1&product=$product1&station=$station1&server2=$server2&product2=$product2&station2=$station2"

		# Primary series only
		else
			xdg-open "https://alvarosg.shinyapps.io/saribeta/?server=$server1&product=$product1&station=$station1"
		fi
	
	else
		echo Missing some or too many input options
		usage_short
	fi

# Local session
elif [[ ! -z $local && -z $remote ]]; then
	if [[ -f $saridir/app_$now.R ]]; then

		Rscript --vanilla --silent -e "library(shiny)" -e "runApp('$saridir/app_$now.R', port = $port, launch.browser = F, display.mode = 'normal')" &> $out & 
		pid=$!
		if [[ $wsl != 0 ]]; then
			echo Loading R packages...
			sleep 4
		else
			sleep 2
		fi
		checkR

		# Local session with local or remote file
		if [[ ! -z $server1 && ! -z $product1 && ! -z $station1 ]]; then

			# local file
			if [[ $server1 == local ]]; then
				station1="$currentdir/$station1"
				if [[ ! -f $station1 ]]; then
					echo File not found: $station1
					exit 1
				fi
			fi
			if [[ $server2 == local ]]; then
				station2="$currentdir/$station2"
				if [[ ! -f $station2 ]]; then
					echo File not found: $station2
					station2=""
					server2=""
					product2=""
				fi
			fi

			# Primary and secondary series
			if [[ ! -z $server2 && ! -z $product2 && ! -z $station2 ]]; then
				checkR
				xdg-open "http://127.0.0.1:$port/?server=$server1&product=$product1&station=$station1&server2=$server2&product2=$product2&station2=$station2"
			# Primary series only
			else
				checkR
				xdg-open "http://127.0.0.1:$port/?server=$server1&product=$product1&station=$station1"
			fi
			waiting

		# Empty local session
		elif [[ -z $server1 && -z $product1 && -z $station1 ]]; then
			xdg-open "http://127.0.0.1:$port"
			waiting

		else
			echo Missing some or too many input options
			usage_short
		fi
	else
		echo Problem with the SARI app script
		exit 1
	fi

# no more types of sessions
else 
	echo Missing some or too many input options
	usage_short
fi

# That's all folks!
