#!/bin/bash

# I don't want the command line build to increment the build number automatically
# Backup the LPI file. After the build, restore those files to reset the build number.

function build_default () {

	local app="${1}"
	local cpu=$(uname -m)
	local os=$(uname -s)
	[[ "${os}" == "Darwin" ]] && os=macOS

	[[ -f "${app}".app ]] && rm "${app}".app
	[[ -f "${app}" ]] && rm "${app}"
	cp ${app}.ver ${app}.lpi
	[[ -e ${app} ]] && rm ${app}
	lazbuild -B -r ${app}.lpr

	if [[ -e ${app}.app ]] ; then
		# Patch Mac App plist with Icon Field
		grep '<key>CFBundleIconFile</key>' ${app}.app/Contents/Info.plist >/dev/null
		if [[ $? -ne 0 ]] ; then
			grep -B 10000 '^</dict>' ${app}.app/Contents/Info.plist | grep -v '^</dict>' >plist.tmp
			echo '  <key>CFBundleIconFile</key>'>>plist.tmp
			echo '  <string>AppIcon.icns</string>'>>plist.tmp
			#echo '  <key>CFBundleTypeIconFile</key>'>>plist.tmp
			#echo '  <string>FilesIcon.icns</string>'>>plist.tmp
			echo '</dict>'>>plist.tmp
			echo '</plist>'>>plist.tmp
			cp -fav plist.tmp ${app}.app/Contents/Info.plist
			rm plist.tmp
		fi
		# Add Icon to Mac App
		if [[ ! -e ${app}.app/Contents/Resources/AppIcon.icns ]] ; then
			cp -fav ${app}.icns ${app}.app/Contents/Resources/AppIcon.icns
		fi
		# Cause Finder to update Mac App Icon
		touch ${app}.app

		# Create Binaries version
		[[ -e ../binaries/${cpu}/${os}/${app}.app ]] && rm -rf ../binaries/${cpu}/${os}/${app}.app
		mkdir -p ../binaries/${cpu}/${os}/${app}.app
		cp -fav ${app}.app/* ../binaries/${cpu}/${os}/${app}.app/
		# Replace Link with Program Binary
		rm ../binaries/${cpu}/${os}/${app}.app/Contents/MacOS/${app}
		cp -fav ${app} ../binaries/${cpu}/${os}/${app}.app/Contents/MacOS/${app}

	elif [[ -e ${app} ]] ; then
		mkdir -p ../binaries/${cpu}/${os}
		cp ${app} ../binaries/${cpu}/${os}/
	else
		exit 1
	fi

}

function build_app () {

	local app="${1}"

	cp ${app}.lpi ${app}.ver
	build_default ${app}
	cp ${app}.ver ${app}.lpi
	rm ${app}.ver

}

function lines () {
	m=$(wc -l *.pas *.lpr | tail -n 1 | cut -d 't' -f 1)
	# s=$(wc -l example/*.conf example/*.ini *.nls *.oss *.url | tail -n 1 | cut -d 't' -f 1)
	# t=$(wc -l example/template/*.css example/template/*.html | tail -n 1 | cut -d 't' -f 1)
	# c=$(( ${m} + ${s} + ${t} ))

	echo "Program sources: "${m}
	# echo "Settings files:  "${s}
	# echo "Template files:  "${t}
	# echo "Total lines:      "${c}
	echo
}

build_app CpME cpme

lines

