# tag: Tom Lord Tue Dec  4 15:09:57 2001 (Makefiles/dotx.sed)
#
# dotx.sed -
#
################################################################
# Copyright (C) 2001 Tom Lord
# 
# See the file "COPYING" for further information about
# the copyright and warranty status of this work.
# 


# This sed script is used during the construction
# process to extract information from pre-processed
# source files.
#
# See, for example, the macro SCM_PROC.
#
# The effect of this script is to delete all lines
# from the input except for those between a line that
# contains "%%%" and a line that contains the matching
# "@@@" 
#
# The "%%%" are replaced by newlines and the "@@@" are
# deleted from the output.
#

/%%%/! {
	 d
	 b
       }
:lookforend
s/%%%/\
/
/@@@/! {
	 N
	 b lookforend
       }
s/@@@//
/%%%/ b lookforend
