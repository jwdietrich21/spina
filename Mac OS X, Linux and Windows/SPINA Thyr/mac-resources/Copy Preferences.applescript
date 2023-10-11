-- This Script copies preferences and reference values files
-- that are named following the old naming convention to
-- new files labeled according to a more specific naming convention.

on FileExists(theFile)
	tell the application "System Events"
		if exists file theFile then
			return true
		else
			return false
		end if
	end tell
end FileExists

set AppleScript's text item delimiters to ":"
set messageReply to ""

set SPINA_GLOBAL_ID to "net.sf.spina"
set SPINA_THYR_GLOBAL_ID to "net.sf.spina.thyr"

set oldPreferencesFile to (path to preferences folder as (text)) & SPINA_GLOBAL_ID & ".xml" as string
set oldRRFile to (path to preferences folder as (text)) & SPINA_GLOBAL_ID & ".ref-ranges.xml" as string

set NewPreferencesFile to (path to preferences folder as (text)) & SPINA_THYR_GLOBAL_ID & ".xml" as string
set NewRRFile to (path to preferences folder as (text)) & SPINA_THYR_GLOBAL_ID & ".ref-ranges.xml" as string

if FileExists(NewPreferencesFile) then
	set messageReply to "Preferences file not copied as a file with new bundle ID existed. " & {return}
else
	if FileExists(oldPreferencesFile) then
		tell the application "Finder"
			set newFile to duplicate file oldPreferencesFile without replacing
			set the name of (newFile as alias) to last text item of NewPreferencesFile
		end tell
		set messageReply to "Preferences file copied. " & {return}
	else
		set messageReply to "Preferences file missing. " & {return}
	end if
end if

if FileExists(NewRRFile) then
	set messageReply to messageReply & "Reference ranges file not copied as a file with new bundle ID existed."
else
	if FileExists(oldRRFile) then
		tell the application "Finder"
			set newFile to duplicate file oldRRFile without replacing
			set the name of (newFile as alias) to last text item of NewRRFile
		end tell
		set messageReply to messageReply & "Reference ranges file copied."
	else
		set messageReply to messageReply & "Reference ranges missing."
	end if
end if

display dialog messageReply buttons {"OK"} default button 1
--display dialog oldPreferencesFile
--display dialog NewPreferencesFile