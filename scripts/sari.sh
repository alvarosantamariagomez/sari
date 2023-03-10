#!/usr/bin/env bash

# Alvaro Santamaria 07/03/2023

################################################################################################################

usage () { echo "
`basename $0` opens a SARI session on your web browser from the command line.

This script only runs on desktop environnements of Unix-like systems.
The SARI session can be either local (running on your machine) or remote (running on the Shinyapps server).
To open a local session, all the R packages required to run SARI must be installed first (see the INSTALL file).
Time series can be uploaded using local or remote files from specific servers and products indicaded below.

Syntax: $(basename $0) -l|r [-w server -p product -s file/station -v]
	-l 		: starts a local SARI session (no series uploaded)
	-r 		: starts a remote SARI session on Shinyapps.io (no series uploaded)
	-w server 	: uploads a file from server (see available servers below)
	-p product 	: uploads a file from product (see available products below)
	-s file/station	: path to a local file or remote station ID with 4 (PIMI), 9 (PIMI00FRA) or 
	                  more characters (PIMI_10025M001) depending on the server used
	-v 		: keeps log of the current SARI session in $saridir

	Five different sessions can be started:

	Empty local session 			$(basename $0) -l
	Empty remote session 			$(basename $0) -r
	Local session with local file 		$(basename $0) -l -w local -p product -s path/to/my/series
	Local session with remote file		$(basename $0) -l -w server -p product -s ID
	Remote session with remote file		$(basename $0) -r -w server -p product -s ID

	Note: in a local session, the SARI app runs in the background of your machine till this script is 
	interrupted by pressing ctrl+c. The script also exits if the SARI app stops.

        +--------+--------------------------------------+----------------------------------------------------+
        | Server | Product                              | Reference                                          |
        +--------+--------------------------------------+----------------------------------------------------+
        | LOCAL  | NEU, PBO, NGL, 1D                    |                                                    |
        | RENAG  | UGA                                  | http://renag.resif.fr/en/                          |
        | NGL    | FINAL, RAPID                         | http://geodesy.unr.edu/                            |
        | EUREF  | PBO                                  | https://epncb.eu/_organisation/about.php           |
        | UNAVCO | CWU, NMT, PBO                        | https://www.unavco.org/data/gps-gnss/gps-gnss.html |
        | EOSTLS | ATMIB, ATMMO, ECCO, ECCO2, ERA5IB,   | http://loading.u-strasbg.fr/                       |
        |        | ERA5TUGO, ERA5HYD, ERAHYD, ERAIN,    |                                                    |
        |        | GRACE, GLDAS, GLDAS2, GLORYS, MERRA, |                                                    |
        |        | MERRA2ATM, MERRA2HYD                 |                                                    |
        +--------+--------------------------------------+----------------------------------------------------+

" 1>&2; exit 1; }

################################################################################################################

# Setting a trap to do a clean exit
cleaning () {
	rm -f $saridir/app_$now.R
	netstat -anp 2> /dev/null | grep :$port | grep LISTEN | grep -E "/R\s+$" | sed 's$/R$$' | awk 'system("kill "$NF"")'
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
while getopts :lrw:p:s:v option; do
	case $option in
		w  )	server=$OPTARG;;
		p  )	product=$OPTARG;;
		s  )	station=$OPTARG;;
		l  )	local=true;;
		r  )	remote=true;;
		v  )	logging=true;;
	        \? )	echo "Unknown option: -$OPTARG" >&2; usage;;
	        :  )	echo "Missing option argument for -$OPTARG" >&2; usage;;
	        *  )	echo "Unimplemented option: -$OPTARG" >&2; usage;;
	esac
done

# Setting output log file
now=$(date '+%Y%m%d_%H%M%S')
if [[ -z $logging ]]; then
	out="/dev/null"
else
	out="$saridir/SARI_$now.log"
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

# Setting the type of session from the input command-line options

# Remote session
if [[ -z $local && ! -z $remote ]]; then

	# Empty remote session
	if [[ -z $server && -z $product && -z $station ]]; then
		xdg-open "https://alvarosg.shinyapps.io/sari"

	# Remote session with remote file
	elif [[ ! -z $server && ! -z $product && ! -z $station ]]; then
		xdg-open "https://alvarosg.shinyapps.io/saribeta/?server=$server&product=$product&station=$station"
	
	else
		echo Missing some or too many input options
		usage
	fi

# Local session
elif [[ ! -z $local && -z $remote ]]; then
	if [[ -f $saridir/app_$now.R ]]; then

		Rscript --vanilla --silent -e "library(shiny)" -e "runApp('$saridir/app_$now.R', port = $port, launch.browser = F, display.mode = 'normal')" &>> $out &
		pid=$!
		if ! ps -p $pid > /dev/null; then
			echo Problem running Rscript
			exit 1
		fi
		if [[ $wsl != 0 ]]; then
			echo Loading R packages...
			sleep 4
		fi

		# Local session with local or remote file
		if [[ ! -z $server && ! -z $product && ! -z $station ]]; then

			# local file
			if [[ $(echo $server | tr '[:upper:]' '[:lower:]') == local ]]; then
				station="$currentdir/$station"
				if [[ ! -f $file ]]; then
					echo File not found: $file
					exit 1
				fi
			fi

			# local or remote file
			xdg-open "http://127.0.0.1:$port/?server=$server&product=$product&station=$station"
			wait $pid

		# Empty local session
		elif [[ -z $server && -z $product && -z $station ]]; then
			xdg-open "http://127.0.0.1:$port"
			wait $pid

		else
			echo Missing some or too many input options
			usage
		fi
	else
		echo Problem with the SARI app script
		exit 1
	fi

# no more types of sessions
else 
	echo Missing some or too many input options
	usage
fi

# That's all folks!
