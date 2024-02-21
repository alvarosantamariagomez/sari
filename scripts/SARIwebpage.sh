#!/bin/bash

# showing script usage
if [[ ! $1 ]]; then
  echo "
 This BASH script modifies the HTML and CSS files of a web page downloaded from the SARI app.

 The result is a static HTML web page with limited JavaScript functionality that can be seen
 on a web browser almost like it was during the live session (*).
 There is no connection to the server and no interaction with the SARI app.

 (*) Maybe not all the web browsers fully support this.

 Usage: "$(basename $0)" downloaded-HTML-file
  "
  exit 1
else
  file="$1"
fi

# checking the input HTML file
if [[ ! -f "$file" ]]; then
  echo "File ${file} not found"
  exit 1
fi
head -1 "$1" | grep html > /dev/null
if [[ $? != 0 ]]; then
  echo "File ${file} is not an HTML file"
  exit 1
fi

# checking the web files directory
dir=$(ls -d "${1%.*}_"* 2> /dev/null)
if [[ "${dir}" == "" ]]; then
  echo "No directory found with the web files for ${file}"
  exit 1
fi
set -- $dir
if [[ -d "$2" ]]; then
  echo "There is more than one directory with the web files for ${file}"
  echo "${dir}"
  exit 1
fi
dir="$1"

# checking the CSS file
css=$(ls "${dir}"/shiny.*css 2> /dev/null)
if [[ "${css}" == "" ]]; then
  echo "No CSS file found in the web files directory ${dir}/"
  exit 1
fi
if [[ $(echo "${css}" | wc -l) -gt 1 ]]; then
  echo "There is more than one CSS file in the web files directory ${dir}/"
  echo "${css}"
  exit 1
fi

# modifying the HTML file
file_out="${file}.$RANDOM"
sed 's/data-display-if=\"output.*\" data-ns-prefix/data-display-if=\"\" data-ns-prefix/' "${file}" > "${file_out}"
if [[ $? != 0 ]]; then
  echo "Something went wrong when trying to modify the HTML ${file}. Nothing was done."
  rm -f "${file_out}"
  exit 1
fi

# modifying the CSS file
css_out="${css}.$RANDOM"
sed 's/opacity:\.5/opacity:\.0/' "${css}" > "${css_out}"
if [[ $? != 0 ]]; then
  echo "Something went wrong when trying to modify the CSS file ${css}. Nothing was done."
  rm -f "${css_out}"
  rm -f "${file_out}"
  exit 1
else
  mv "${css_out}" "${css}"
  mv "${file_out}" "${file}"
fi

echo "All good!"
