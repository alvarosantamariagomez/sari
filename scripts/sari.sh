#!/usr/bin/env bash

### Copyright (C) 2023 Alvaro Santamaria-Gomez, 7 March 2023
### alvaro.santamaria at get.omp.eu
###
### This program is free software: you can redistribute it and/or modify
### it under the terms of the GNU General Public License as published by
### the Free Software Foundation, either version 3 of the License, or
### any later version.
###
### This program is distributed in the hope that it will be useful,
### but WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### GNU General Public License for more details.
###
### You should have received a copy of the GNU General Public License
### along with this program.  If not, see <https://www.gnu.org/licenses/>.

#########################################################################################################################

usage () { echo "
`basename $0` opens a SARI session on the web browser from the command line.

This script only runs on desktop environnements of Unix-like systems including Windows/WSL.
The SARI session can be either local (running on your machine) or remote (running on the Shinyapps server).
There are two options to open a local session:
1) having the Docker Deamon/Desktop up and running with the SARI image downloaded beforehand (see the README file).
2) having all the R packages required to run SARI installed beforehand (see the INSTALL file).
Time series can be uploaded from the command line using local or remote files from specific servers and products
indicaded below.

Syntax: $(basename $0) -l|d|r [-w server1+server2 -p product1+product2 -s series1+series2 -v]

	-l 			: starts a local SARI session using the SARI source code
	-d			: starts a local SARI session using a Docker image
	-r 			: starts a remote SARI session on Shinyapps.io (no series uploaded)
	-w server1+server2	: uploads primary (+secondary) file from server (see available servers below)
	-p product1+product2 	: uploads primary (+secondary) file from product (see available products below)
	-s series1+series2	: path to a local file or remote station ID with 4 (e.g., PIMI), 9 (e.g., PIMI00FRA)
				  or more 14 (e.g., PIMI_10025M001) depending on the server used
	-v 			: keeps the log of the current local SARI session in $saridir
	-h			: shows this help

	Six different sessions can be started:

	Empty local session from source code	$(basename $0) -l
	Empty local session from Docker image	$(basename $0) -d
	Empty remote session 			$(basename $0) -r
	Local session with local series		$(basename $0) -l -w local -p product -s path/to/my/series
	Local session with remote series	$(basename $0) -l -w server -p product -s ID
	Remote session with remote series	$(basename $0) -r -w server -p product -s ID

	server1+server2, product1+product2 and station1+station2 are used to load one or two series (primary+secondary)
	at the same time from the same or different servers/products.

	The remote series will be downloaded to the local server (your machine) or the remote server (Shinyapps).
	It is not possible to upload a local series to the Shinyapps server via this script. The in-app SARI interface 
	must be used for that.

	In a local session, the SARI app runs in the background of your machine till this script is interrupted by 
	pressing Ctrl+C. The script also exits if the SARI app stops.

        +------------+--------------------------------------+---------+------------------------------------------------+
        | Server     | Product                              | Station | Reference                                      |
        +------------+--------------------------------------+---------+------------------------------------------------+
        | LOCAL      | ENU, NEU, PBO, NGL, 1D*              | path    |                                                |
        | RENAG      | UGA                                  | 4 char  | http://renag.resif.fr/en/                      |
        | FORMATER   | SPOTGINS_POS, UGA                    | 9 char  | https://en.poleterresolide.fr/                 |
        | EPOS       | INGV, SGO-EPND, UGA-CNRS, ROB-EUREF  | 9 char  | https://www.epos-eu.org/                       |
	| SONEL      | ULR7A                                | 4 char  | https://www.sonel.org/                         |
	| IGS        | IGS20                                | 4 char  | https://igs.org/products/                      |
        | EUREF      | IGb14                                | 9 char  | https://epncb.eu/_organisation/about.php       |
        | NGL        | FINAL, RAPID                         | 4 char  | http://geodesy.unr.edu/                        |
        | JPL        | REPRO2018A                           | 4 char  | https://sideshow.jpl.nasa.gov/post/series.html |
        | EARTHSCOPE | CWU, PBO, NMT                        | 4 char  | https://www.earthscope.org                     |
        | SIRGAS     | IGb14                                | 4 char  | https://www.sirgas.org/en/                     |
        | EOSTLS     | ATMIB, ATMMO, ECCO, ECCO2, ERA5IB,   | 14 char | http://loading.u-strasbg.fr/                   |
        |            | ERA5TUGO, ERA5HYD, ERAHYD, ERAIN,    |         |                                                |
        |            | GRACE, GLDAS, GLDAS2, GLORYS, MERRA, |         |                                                |
        |            | MERRA2ATM, MERRA2HYD                 |         |                                                |
        | PSMSL      | RLR                                  | number  | https://psmsl.org/                             |
        +------------+--------------------------------------+---------+------------------------------------------------+
	* 1D products must have the epochs in the first column, the data in the second and the errorbars in the third

" 1>&2; exit 1; }

usage_short () { echo "
Syntax: $(basename $0) -l|d|r [-w server1+server2 -p product1+product2 -s series1+series2 -v]

	-l 			: starts a local SARI session using the SARI source code
	-d			: starts a local SARI session using a Docker image
	-r 			: starts a remote SARI session on Shinyapps.io (no series uploaded)
	-w server1+server2	: uploads primary (+secondary) file from server (see available servers with option -h)
	-p product1+product2 	: uploads primary (+secondary) file from product (see available products with option -h)
	-s series1+series2	: path to a local file or remote station ID with 4 (e.g., PIMI), 9 (e.g., PIMI00FRA)
				  or more 14 (e.g., PIMI_10025M001) depending on the server used
	-v 			: keeps the log of the current SARI session in $saridir
	-h			: shows detailed help

	Empty local session from source code	$(basename $0) -l
	Empty local session from Docker image	$(basename $0) -d
	Empty remote session 			$(basename $0) -r
	Local session with local series		$(basename $0) -l -w local -p product -s path/to/my/series
	Local session with remote series	$(basename $0) -l -w server -p product -s ID
	Remote session with remote series	$(basename $0) -r -w server -p product -s ID

" 1>&2; exit 1; }

#########################################################################################################################

# Setting list of available URL parameters
servers=" local renag formater epos sonel igs euref ngl jpl earthscope sirgas eostls psmsl "
products=" enu neu pbo ngl uga spotgins_pos ingv sgo-epnd uga-cnrs rob-euref ulr7a igs20 igb14 final rapid cwu nmt repro2018a atmib atmmo ecco ecco2 era5ib era5tugo era5hyd erahyd erain grace gldas gldas2 glorys merra merra2atm merra2hyd rlr "
products_local=" enu neu pbo ngl 1d "

# Setting a trap to do a clean exit
cleaning () {
	if [[ ! -z $local ]]; then
		rm -f $saridir/app_$now.R
		if [[ ! -z $pid ]]; then
			netstat -anp 2> /dev/null | grep :$port | grep LISTEN | grep -E "$pid/R\s+$" | sed 's$/R$$' | awk 'system("kill "$NF"")'
		fi
	elif [[ ! -z $docker ]]; then
		if [[ ! -z $pid ]]; then
			docker stop sari 1> /dev/null
		fi
	fi
	if [[ ! $logging ]]; then
		rm -f $out
	fi
}
trap cleaning EXIT

# Getting input command-line options
while getopts :ldrw:p:s:vh option; do
	case $option in
		w  )	server=$OPTARG;;
		p  )	product=$OPTARG;;
		s  )	station=$OPTARG;;
		l  )	local=true;;
		d  )	docker=true;;
		r  )	remote=true;;
		v  )	logging=true;;
		h  )    usage;;
	        \? )	echo "Unknown option: -$OPTARG" >&2; usage_short;;
	        :  )	echo "Missing option argument for -$OPTARG" >&2; usage_short;;
	        *  )	echo "Unimplemented option: -$OPTARG" >&2; usage_short;;
	esac
done

# Checking dependencies
uname --help > /dev/null 2>&1
if [[ $? != 0 ]]; then
	echo FATAL: uname is not available.
	exit 1
fi

uname -a | grep microsoft > /dev/null 2>&1
wsl=$?

if [[ $wsl == 0 ]]; then # we are on WSL
	wslview -v > /dev/null 2>&1
	if [[ $? != 0 ]]; then
		echo FATAL: wslview is not available.
		exit 1
	fi
	browser=wslview
else
	xdg-open --help > /dev/null 2>&1
	if [[ $? != 0 ]]; then
		echo FATAL: xdg-utils is not available.
		exit 1
	fi
	browser=xdg-open
fi

if [[ ! -z $local ]]; then
	netstat -h > /dev/null 2>&1
	if [[ $? != 0 ]]; then
		echo FATAL: netstat is not available.
		exit 1
	fi
	Rscript --help > /dev/null 2>&1
	if [[ $? != 0 ]]; then
		echo WARNING: Rscript is not available.
		exit 1
	fi
fi

if [[ ! -z $docker ]]; then
	docker > /dev/null 2>&1
	if [[ $? != 0 ]]; then
		echo FATAL: docker is not available.
		exit 1
	fi
	docker ps > /dev/null 2>&1
	if [[ $? != 0 ]]; then
		echo FATAL: docker deamon/desktop is not running.
		exit 1
	fi
	if [[ "$(docker images -q alvarosg/sari:latest 2> /dev/null)" == "" ]]; then
		echo FATAL: SARI docker image not found.
		exit 1
	fi
fi

# Setting directory paths and checking the SARI app file for local sessions
currentdir="$(pwd)"
saridir="$(dirname $(dirname $(realpath $0)))"
if [[ ! -z $local ]]; then
	if [[ ! -f $saridir/app.R ]]; then
		echo Unable to find the SARI app.R script in $saridir
		exit 1
	fi
fi

# Setting output log file for local sessions
if [[ ! -z $local || ! -z $docker ]]; then
	now=$(date '+%Y%m%d_%H%M%S')
	out="$saridir/SARI_$now.log"
	echo "Logging the SARI session in $out"
fi

# Removing devmode (problem with local session reload)
if [[ ! -z $local ]]; then
	if [[ ! -f $saridir/app_$now.R ]]; then
		sed 's/devmode(TRUE)/devmode(FALSE)/' $saridir/app.R > $saridir/app_$now.R
	fi
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

# Splitting parameters of the primary and secondary series
if [[ ! -z $server ]]; then
	IFS=+ read -r server1 server2 <<< $server
	server1=$(echo $server1 | tr '[:upper:]' '[:lower:]')
	server2=$(echo $server2 | tr '[:upper:]' '[:lower:]')
fi
if [[ ! -z $product ]]; then
	IFS=+ read -r product1 product2 <<< $product
	product1=$(echo $product1 | tr '[:upper:]' '[:lower:]')
	product2=$(echo $product2 | tr '[:upper:]' '[:lower:]')
fi
if [[ ! -z $station ]]; then
	IFS=+ read -r station1 station2 <<< $station
fi

# Checking valid arguments
contains() {
	if [[ ! " $1 " =~ " $2 " ]]; then
		echo FATAL: $2 is not a valid input argument
		exit 1
	fi
}
contains "$servers" $server1
contains "$servers" $server2
if [[ $server1 == local ]]; then
	contains "$products_local" $product1
else
	contains "$products" $product1
fi
if [[ $server2 == local ]]; then
	contains "$products_local" $product2
else
	contains "$products" $product2
fi

# Catching SARI installation error
checkR() {
	if ! ps -p $pid > /dev/null; then
		echo Problem running Rscript to start SARI. Check in the log that all the SARI dependencies are correctly installed.
		exit 1
	fi
}

# Blocking the foreground of the terminal
waiting() {
	if [[ ! -z $local ]]; then
		if ! ps -p $pid > /dev/null; then
			echo Problem running Rscript to start SARI
		else	
echo -e '\n
_______________/\/\/\/\/\_____/\/\_______/\/\/\/\/\____/\/\/\/\_________________
____________/\/\___________/\/\/\/\_____/\/\____/\/\____/\/\____________________
_____________/\/\/\/\____/\/\____/\/\__/\/\/\/\/\______/\/\_____________________
__________________/\/\__/\/\/\/\/\/\__/\/\__/\/\______/\/\______________________
_________/\/\/\/\/\____/\/\____/\/\__/\/\____/\/\__/\/\/\/\____unknown pleasures\n'
			echo SARI session available at http://127.0.0.1:$port
			echo Press Ctrl+C to stop the SARI session
			wait $pid
		fi
	elif [[ ! -z $docker ]]; then
		if ! ps -p $pid > /dev/null; then
			echo Problem running the docker container to start SARI
		else	
echo -e '\n
_______________/\/\/\/\/\_____/\/\_______/\/\/\/\/\____/\/\/\/\_________________
____________/\/\___________/\/\/\/\_____/\/\____/\/\____/\/\____________________
_____________/\/\/\/\____/\/\____/\/\__/\/\/\/\/\______/\/\_____________________
__________________/\/\__/\/\/\/\/\/\__/\/\__/\/\______/\/\______________________
_________/\/\/\/\/\____/\/\____/\/\__/\/\____/\/\__/\/\/\/\____unknown pleasures\n'
			echo SARI session available at http://localhost:3838
			echo Press Ctrl+C to stop the SARI container
			wait $pid
		fi
	fi
}

# Setting the type of session from the input command-line options
# Remote session
if [[ -z $local && -z $docker && ! -z $remote ]]; then

	# Empty remote session
	if [[ -z $server1 && -z $product1 && -z $station1 ]]; then
		echo Opening new SARI session on the browser
		$browser "https://alvarosg.shinyapps.io/sari"

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
			
			echo Opening new SARI session on the browser
			$browser "https://alvarosg.shinyapps.io/sari/?server=$server1&product=$product1&station=$station1&server2=$server2&product2=$product2&station2=$station2"

		# Primary series only
		else
			echo Opening new SARI session on the browser
			$browser "https://alvarosg.shinyapps.io/sari/?server=$server1&product=$product1&station=$station1"
		fi
	
	else
		echo Missing some or too many input options
		usage_short
	fi

# Local session
elif [[ ! -z $local && -z $docker && -z $remote ]]; then
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
		if [[ -z $server1 && ! -z $station1 ]]; then
			server1=local
		fi
		if [[ -z $product1 && ! -z $station1 ]]; then
			product1=enu
		fi
		if [[ -z $server2 && ! -z $station2 ]]; then
			server2=local
		fi
		if [[ -z $product2 && ! -z $station2 ]]; then
			product2=enu
		fi
		if [[ ! -z $server1 && ! -z $product1 && ! -z $station1 ]]; then

			# local file
			if [[ $server1 == local ]]; then
				if [[ ${station1:0:1} != "/" ]]; then
					station1="$currentdir/$station1"
				fi
				if [[ ! -f $station1 ]]; then
					echo File not found: $station1
					exit 1
				fi
			fi
			if [[ $server2 == local ]]; then
				if [[ ${station2:0:1} != "/" ]]; then
					station2="$currentdir/$station2"
				fi
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
				echo Opening new SARI session on the browser
				$browser "http://127.0.0.1:$port/?server=$server1&product=$product1&station=$station1&server2=$server2&product2=$product2&station2=$station2"
			# Primary series only
			else
				checkR
				echo Opening new SARI session on the browser
				$browser "http://127.0.0.1:$port/?server=$server1&product=$product1&station=$station1"
			fi
			waiting

		# Empty local session
		elif [[ -z $server1 && -z $product1 && -z $station1 ]]; then
			echo Opening new SARI session on the browser
			$browser "http://127.0.0.1:$port"
			waiting

		else
			echo Missing some or too many input options
			usage_short
		fi
	else
		echo Problem with the SARI app script
		exit 1
	fi

# Docker session
elif [[ -z $local && ! -z $docker && -z $remote ]]; then

	echo Starting the SARI container
	docker ps -a | grep sari:latest > /dev/null 2>&1
	container=$?
	if [[ $container -gt 0 ]]; then
		docker run -a stdout -a stderr -i --name sari -p 3838:3838 -v /home/SARI alvarosg/sari:latest > $out 2>&1 &
		pid=$!
	else
		docker start -ai sari > $out 2>&1 &
		pid=$!
	fi

	# Local session with local or remote file
	if [[ ! -z $server1 && ! -z $product1 && ! -z $station1 ]]; then

		# local file
		if [[ $server1 == local ]]; then
			if [[ ${station1:0:1} != "/" ]]; then
				station1="$currentdir/$station1"
			fi
			if [[ ! -f $station1 ]]; then
				echo File not found: $station1
				exit 1
			fi
		fi
		if [[ $server2 == local ]]; then
			if [[ ${station2:0:1} != "/" ]]; then
				station2="$currentdir/$station2"
			fi
			if [[ ! -f $station2 ]]; then
				echo File not found: $station2
				station2=""
				server2=""
				product2=""
			fi
		fi

		# Primary and secondary series
		if [[ ! -z $server2 && ! -z $product2 && ! -z $station2 ]]; then
			echo Opening new SARI session on the browser
			$browser "http://127.0.0.1:3838/?server=$server1&product=$product1&station=$station1&server2=$server2&product2=$product2&station2=$station2"
		# Primary series only
		else
			echo Opening new SARI session on the browser
			$browser "http://127.0.0.1:3838/?server=$server1&product=$product1&station=$station1"
		fi
		waiting

	# Empty local session
	elif [[ -z $server1 && -z $product1 && -z $station1 ]]; then
		echo Opening new SARI session on the browser
		$browser "http://127.0.0.1:3838"
		waiting
	else
		echo Missing some or too many input options
		usage_short
	fi

# no more types of sessions
else 
	echo Missing some or too many input options
	usage_short
fi

# That's all folks!
