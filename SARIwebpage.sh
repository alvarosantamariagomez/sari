#!/bin/bash

set -o errexit

#showing script usage
if [[ ! $1 ]]; then
  echo "
 This BASH script modifies the HTML and CSS files of a downloaded SARI web page.

 The result is a static web page with limited JavaScript functionality that can
 be seen on a web browser almost like it was during the live session.
 There is no connection to the server and no interaction with the SARI app.

 Usage: "$(basename $0)" downloaded-HTML-file
  "
  exit 1
fi

#checking input file
if [[ ! -f "$1" ]]; then
  echo "File $1 not found"
  exit 1
fi
head -1 "$1" | grep html > /dev/null
if [[ $? != 0 ]]; then
  echo "File $1 is not a HTML file"
  exit 1
fi
dir=$(ls -d "${1%.*}_"*)
if [[ ! -d "$dir" ]]; then
  echo "Directory $dir not found"
  exit 1
fi

#modifying HTML file
sed -i 's/data-display-if=\"output.*\" data-ns-prefix/data-display-if=\"\" data-ns-prefix/' "$1"

#modifying CSS file
sed -i 's/opacity:\.5/opacity:\.0/' "$dir"/shiny*.css
