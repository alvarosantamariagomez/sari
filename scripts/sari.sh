#!/bin/bash

# Alvaro Santamaria 07/03/2023

###############################################################################################################

# Going to SARI main directory
saridir="$(dirname $(dirname $(realpath $0)))"
cd $saridir

usage () { echo "
`basename $0` opens a SARI session on your web browser.

Time series can be uploaded using local or remote files from specific servers and products indicaded below.
The script only runs on desktop environnements that include xdg-utils, as in most Linux distros.
To open a local session, all the R packages required to run SARI must be installed first (see the INSTALL file).

Usage: $(basename $0) -l -r -w server -p product -s file/station
	-l 		: opens a local SARI session (no series uploaded)
	-r 		: opens a remote SARI session on Shinyapps.io (no series uploaded)
	-w server 	: see available servers below
	-p product 	: see available products below
	-s file/station	: path to a local file or remote station ID with 4 (PIMI), 9 (PIMI00FRA) or 
	                  more characters (PIMI_10025M001) depending on the server used
	-v 		: keeps SARI session log in $saridir

	Five different sessions can be started:

	Empty local session 			$(basename $0) -l
	Empty remote session 			$(basename $0) -r
	Local session with local file 		$(basename $0) -w local -p product -s path/to/my/series
	Local session with remote file		$(basename $0) -l -w server -p product -s ID
	Remote session with remote file		$(basename $0) -r -w server -p product -s ID

	Note: a local session means the SARI app will start in the background at localhost 127.0.0.1 and will
	listen for connections on port 7777. The SARI app will continue listening in the background till this 
	script is interrupted by pressing ctrl+c. The script also exits if the SARI app stops.

        +-----------------------------------------------+-----------------------------------------------------+
        | Server | Product                              | Reference                                           |
        |--------+--------------------------------------+-----------------------------------------------------|
        | LOCAL  | NEU, PBO, NGL, 1D                    |                                                     |
        | RENAG  | UGA                                  | http://renag.resif.fr/en/                           |
        | NGL    | FINAL, RAPID                         | http://geodesy.unr.edu/                             |
        | EUREF  | PBO                                  | https://epncb.eu/_organisation/about.php            |
        | UNAVCO | CWU, NMT, PBO                        | https://www.unavco.org/data/gps-gnss/gps-gnss.html  |
        | EOSTLS | ATMIB, ATMMO, ECCO, ECCO2, ERA5IB,   | http://loading.u-strasbg.fr/                        |
        |        | ERA5TUGO, ERA5HYD, ERAHYD, ERAIN,    |                                                     |
        |        | GRACE, GLDAS, GLDAS2, GLORYS, MERRA, |                                                     |
        |        | MERRA2ATM, MERRA2HYD                 |                                                     |
        +-----------------------------------------------+-----------------------------------------------------+

" 1>&2; exit 1; }

###############################################################################################################

# Checking dependencies
xdg-open --help > /dev/null 2>&1
if [[ $? != 0 ]]; then
	echo WARNING: xdg-utils is not available. Local and remote sessions may not work as expected
fi
Rscript --help > /dev/null 2>&1
if [[ $? != 0 ]]; then
	echo WARNING: Rscript is not available. Local sessions may not work as expected
fi
netstat -h > /dev/null 2>&1
if [[ $? != 0 ]]; then
	echo WARNING: netstat is not available. Local sessions may not work as expected
fi

# Getting input options
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

# Set output
if [[ -z $logging ]]; then
	out="/dev/null"
else
	now=$(date '+%Y%m%d_%H%M%S')
	out="SARI_$now.log"
fi

# Check if SARI is running and set trap to close SARI on exit
running=$(netstat -an | grep :7777 | grep LISTEN)
cleaning () {
	netstat -anp 2> /dev/null | grep :7777 | grep LISTEN | grep "/R$" | sed 's$/R$$' | awk 'system("kill  "$NF"")'
	rm -f app_linux.R
}
trap cleaning EXIT

# Problem with png-cairo in WSL
uname -a | grep microsoft > /dev/null 2>&1
if [[ $? == 0 ]]; then
	sed 's/, type = "cairo-png"//' app.R > app_linux.R
else
	cp app.R app_linux.R
fi

# Set session from input options and get it done

# Empty local session
if [[ ! -z $local && -z $remote && -z $server && -z $product && -z $station ]]; then
	Rscript -e "library(shiny)" -e "runApp('app_linux.R', port = 7777, launch.browser = T, display.mode = 'normal')" &> $out
# Empty remote session
elif [[ ! -z $remote && -z $local && -z $server && -z $product && -z $station ]]; then
	xdg-open "https://alvarosg.shinyapps.io/sari"
# Local session with local file
elif [[ -z $local && -z $remote && ! -z $server && ! -z $product && ! -z $station ]]; then
	if [[ $(echo $server | tr '[:upper:]' '[:lower:]') == local ]]; then
		if [[ -f $station ]]; then
			if [[ $running != 0 ]]; then
				Rscript --vanilla --silent -e "library(shiny)" -e "runApp('app_linux.R', port = 7777, launch.browser = F, display.mode = 'normal')" &> $out &
				pid=$!
			fi
			xdg-open "http://127.0.0.1:7777/?server=$server&product=$product&station=$station"
			wait $pid
		else
			echo File not found: $station
			exit 1
		fi
	else
		echo "Missing local (-l) or remote (-r) option"
		usage
	fi
# Local session with remote file
elif [[ ! -z $local && -z $remote && ! -z $server && ! -z $product && ! -z $station ]]; then
	if [[ $(echo $server | tr '[:upper:]' '[:lower:]') != local ]]; then
		if [[ $running != 0 ]]; then
			Rscript --vanilla --silent -e "library(shiny)" -e "runApp('app_linux.R', port = 7777, launch.browser = F, display.mode = 'normal')" &> $out &
			pid=$!
		fi
		xdg-open "http://127.0.0.1:7777/?server=$server&product=$product&station=$station"
		wait $pid
	else
		echo Wrong server: $server
		usage
	fi
# Remote session with remote file
elif [[ -z $local && ! -z $remote && ! -z $server && ! -z $product && ! -z $station ]]; then
	xdg-open "https://alvarosg.shinyapps.io/saribeta/?server=$server&product=$product&station=$station"
# no more types of sessions
else 
	echo Missing some or too many input options
	usage
fi

# That's all folks!
