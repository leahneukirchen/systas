
# This rebuilds all the indexes in an existing
# library to repair side effects of an old bug.
# 
# This script is harmless (if not interrupted) on
# any library, and a good idea if you are using
# arch-1.0pre15 or earlier (after you upgrade to
# pre16 or later).
# 
# Interruptions: if you interrupt this script half-way through,
# you _can_ mess up your revision library.  But that's 
# no big deal -- just run it again and it will recover.
# 

set -e

cd "`larch my-revision-library`"

for archive in `larch library-archives` ; do
  for category in `larch library-categories $archive` ; do
    for branch in `larch library-branches $archive/$category` ; do
      for version in `larch library-versions $archive/$branch` ; do
        cd "$archive/$category/$branch/$version" 
        for rev in `echo *patch-* *base-0` ; do
	  if test ! -e "$rev" ; then
	    # might be an unexpanded glob
	    continue
	  fi
          cd $rev
          printf "Fixing %s\n" "$rev"
          larch inventory --source --both --all --tags > ,,index
          sort -k 1 ,,index > ,,index-by-name
          cd ..
        done
	cd ../../../..
      done
    done
  done
done
