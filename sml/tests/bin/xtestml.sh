#! /bin/ksh
#set -x

#
# testml.sh [-T <testDir>] [-sml <sml>] [-tmp <tmpName>] [-depositOnly] 
# 	    [-cpu <limit>]
#
CMD=${0##*/}\>

# defaults
CPULIMIT=400
SML=/usr/local/bin/sml
KSH=/bin/ksh
DIFF=/bin/diff
ECHO=print
TESTDIR=bugs
BADDIR=$TESTDIR/bad
TSML=$TESTDIR/tsml
TMPFILE=$TESTDIR/tmp
TESTMODE="TESTONLY"
OPENBUGSLIST=$TESTDIR/openbugs
LOGFILE=$TESTDIR/LOG.$SUFFIX
DIFF=0

ARCH=sparc
OPSYS=solaris

#
# use the arch-n-opsys script to determine the ARCH/OS if possible
#
if [ -f ./arch-n-opsys.sh ]; then
  ARCH_N_OPSYS=`./arch-n-opsys.sh`
  if [ "$?" = "0" ]; then
    eval $ARCH_N_OPSYS
  fi
fi
SUFFIX="$ARCH-$OPSYS"

#
# Command line processing
#

function printUsage {
 $ECHO -u2 "xtestml.sh testdir"
 $ECHO -u2 "    [-f <testfile>]"        
 $ECHO -u2 "    [-tmp <tmpfileName>   default=$TSML and $TMPFILE]"
 $ECHO -u2 "    [-sml <executable>    default=$SML]"
 $ECHO -u2 "    [-cpu <cpulimit>      default=$CPULIMIT]"
 $ECHO -u2 "    [-openbugs <filename> default=$OPENBUGSLIST]"
 $ECHO -u2 "    [-bad <dir>           default=$BADDIR]"
 $ECHO -u2 "    [-log <log-file>]     default=$LOGFILE"
 $ECHO -u2 "    [-diff]               default=<no diffs>"
 $ECHO -u2 "    [-depositOnly]"
 $ECHO -u2 "    [-help]"
}

#
# testdir must be the first (manditory) parameter.
#
if [[ $# -eq 0 ]] then
    printUsage
    exit 1
else
    case $1 in
      -*) 
	printUsage
	exit 1
	;;
      *)      
	TESTDIR=$1
	shift
	;;
    esac
fi

BADDIR=$TESTDIR/bad.$SUFFIX
LOGFILE=$TESTDIR/LOG.$SUFFIX
TSML=$TESTDIR/tsml
TMPFILE=$TESTDIR/tmp.$SUFFIX
TESTMODE="TESTONLY"
OPENBUGSLIST=$TESTDIR/openbugs
RESULTS=$TESTDIR/RESULTS.$SUFFIX

while [[ $# -ne 0 ]] do
  	arg=$1; shift
	case $arg in
	  -f)
		if [[ $# -eq 0 ]] then
			$ECHO -u2 $CMD must name test file with -f option
			exit 1
		fi
		TESTFILE=$1; shift
		;;
	  -sml)
		if [[ $# -eq 0 ]] then
			$ECHO -u2 $CMD must name executable with -sml option
			exit 1
		fi
		SML=$1; shift
		;;
	  -tmp)
		if [[ $# -eq 0 ]] then
			$ECHO -u2 $CMD must name temporary to use with -tmp option
			exit 1
		fi
		TSML=$1; TMPFILE=$1.tmp; shift
		;;
	  -depositOnly)
		TESTMODE="DEPOSITONLY"
		;;
	  -openbugs)
		if [[ $# -eq 0 ]] then
			$ECHO -u2 $CMD must specify a file name with -openbugs option
			exit 1
		fi
		OPENBUGSLIST=$1; shift
		;;
	  -cpu)
		if [[ $# -eq 0 ]] then
			$ECHO -u2 $CMD must cpu limit with -cpu option
			exit 1
		fi
		CPULIMIT=$1; shift
		;;
	  -bad)
		if [[ $# -eq 0 ]] then
		  $ECHO -u2 $CMD must supply directory with -bad option
		  exit 1
		fi
		BADDIR=$1; shift
		;;
          -log)
		if [[ $# -eq 0 ]] then
		    $ECHO -u2 "Error: must provide log file with -log option"
		    exit 1
		fi
		LOGFILE=$1; shift
		;;
          -diff)    
		DIFF=1
		;;
	  -help)
		 printUsage 
		 exit 0
	        ;;
	  *)
	        $ECHO -u2 ${CMD} bad option $arg
	        printUsage
		exit 1
	esac
done		


#
# Do the requisite directories and files exist
#			
if [[ ! (-d $TESTDIR/tests) ]] then
  $ECHO ${CMD} Error: Testing directory does not contain tests/
  exit 1
elif [[ ! (-d $TESTDIR/outputs) ]] then
  $ECHO ${CMD} Error: Testing directory does not contain outputs/
  exit 1
elif [[ ! (-a $OPENBUGSLIST) ]] then 
  $ECHO ${CMD} Error: openbugs files does not exist - $OPENBUGSLIST
  exit 1
elif [[ (-a $BADDIR) ]] then
  $ECHO ${CMD} Error: $BADDIR directory/file already exist--please delete
  exit 1  
fi

#
# Should check that the mkdir succeeds.
#
mkdir $BADDIR


#
# Build version of compiler with stuff turned off
#
if [[ ! (-x $SML) ]] then
	$ECHO ${CMD} ML compiler $SML does not exist or is not executable
	exit 1
fi


$ECHO -u2 ${CMD} Building special version of SML ...
$SML @SMLquiet @SMLdebug=/dev/null << xxx 1>/dev/null
  	Compiler.Control.primaryPrompt:="";
  	Compiler.Control.secondaryPrompt:="";
  	(SMLofNJ.exportML "$TSML"; ());
xxx

#
# This check is disabled --- we need some way to access the appropriate
#   arch-n-opsys information to associate the right suffix with the
#   exported heap image.
#
#if [[ ! (-a $TSML.*) ]] then
#	$ECHO ${CMD} Error: could not export ML image
#	exit 1
#fi


function testMLFile 
{
	$KSH <<-YYY 1>$TMPFILE 2>&1
		ulimit -t $CPULIMIT
		(echo "(*#line 0 \"$srcFile\"*)"; cat $file) | \
		    $SML @SMLquiet @SMLdebug=/dev/null @SMLload=$TSML 
	YYY
}	


function compareOutput
{
    outFile1=${outPath}/${srcFile%%.sml}.out
    $ECHO -u2 "compareOutput: TMPFILE = $TMPFILE"
    $ECHO -u2 "compareOutput: outFile1 = $outFile1"
    if [[ ! ( -r $TMPFILE) ]] then
          $ECHO -u2 "$TEMPFILE does not exist
    fi
    if [[ ! ( -r $outFile1) ]] then
          $ECHO -u2 "$outFile1 does not exist
    fi
	$DIFF -b $TMPFILE ${outPath}/${srcFile%%.sml}.out >/dev/null
	if [[ $? -eq 0 ]] then
		fgrep "$file" $OPENBUGSLIST >/dev/null
		if [[ $? -eq 0 ]] then
			$ECHO " openbug (unchanged)"
		else
			$ECHO " pass"
		fi
	else
		fgrep "$file" $OPENBUGSLIST >/dev/null
		if [[ $? -eq 0 ]] then
			$ECHO " **** possible bug fix"
		else
			$ECHO " **** fail"
		fi
		mv $TMPFILE $BADDIR/${srcFile%%.sml}.out
	fi
}


#
# GO FOR IT!!
# 
if [[ "$TESTFILE" != "" ]] then
  TESTS="$TESTFILE"
else
  TESTS="$TESTDIR/tests/*"
fi

for file in $TESTS; do
	srcFile=${file##*/tests/}
	srcPath=${file%%/tests/*}
	outPath=$srcPath/outputs
	outFile=$outPath/${srcFile%%.sml}.out
  $ECHO -u2 "srcFile = $srcFile"
  $ECHO -u2 "outFile = $outFile"
	# does srcFile have valid extension
	if [[ ${srcFile%%.sml} = $srcFile ]] then continue; fi 
	if [[ ! (-a $file) ]] then continue; fi

	case $TESTMODE in
	  "TESTONLY")
		$ECHO -n "${CMD}  Testing ${file##*/tests/} 	   ... "
		$ECHO -u2 "${CMD}  >>Testing ${file##*/tests/}"
		testMLFile
		$ECHO -u2 "${CMD}  Test finished ${file##*/tests/} 	   ... "
		if [[ ! ( -r $outFile) ]] ; then
		        mv $TMPFILE $BADDIR/${srcFile%%.sml}.out
			$ECHO " output file does not exist!!"
		else 
		    $ECHO -u2 "about to compare"
			compareOutput
		    $ECHO -u2 "finished compare"
		fi
		;;
	"DEPOSITONLY")
		if [[ ! (-a $outFile) ]] then
			$ECHO -n "${CMD}  Testing ${file##*/tests/} 	   ... "
			testMLFile
			cp $TMPFILE $outFile
			if [[ ! (-a $outFile) ]] then
				$ECHO ${CMD} Error: could not create $outFile
				exit 2
			fi
			$ECHO " deposited ${srcFile%%.sml}.out"
		fi
		;;
	esac
done >> $LOGFILE

#*******************************************************************
#! /bin/ksh

# process.sh -- processes a log created by testml from
# the commands:
#
#      script LOG
#      testml [testml options] 2>/dev/null
#
# For all tests that fail or possible bug fixes, the source code and 
# the conflicting outputs are concatenated.
# For openbugs and new test cases, the source file and generated output
# is concatenated.

# 
# Usage process.sh  [-log <log-file>] [-test <test-dir>] [-bad <baddir>]
#

#
# Command line processing.
#

# initial default values for usage message


#
# test if the output directory and LOG file exists
#
if [[ ! ( -d $BADDIR) ]] then 
	echo "process> output directory ($BADDIR) does not exist."
	exit 1
elif [[ ! ( -a $LOGFILE) ]] then
	echo 'process> log-file ($LOGFILE) does not exist.'
	exit 1
elif [[ -a $RESULTS ]] then
    $ECHO $CMD $RESULTS exists -- please remove.
    exit 1
fi
#
# generate the various sets of files.

FAIL=`cat $LOGFILE | grep fail | awk '{print $3}'`
OPENBUGS=`cat $LOGFILE | grep unchanged | awk '{print $3}'`
FIXED=`cat $LOGFILE | grep fix | awk '{print $3}'`
NEW=`cat $LOGFILE | grep exist | awk '{print $3}'`

#
# Process files that failed
#
function changedFiles
{
    for f in $1; do
	  echo :::::::::::::::::::::::::::::$f::::::::::::::::::::::::::
	  cat $TESTDIR/tests/$f
	  if [[ $DIFF -eq 0 ]] then
	     echo ---------------------------bad---------------------------
	     cat $BADDIR/${f%.sml}.out
  	     echo ---------------------------outputs-----------------------
  	     cat $TESTDIR/outputs/${f%.sml}.out
 	     echo " "
	  else
	    echo ---------------------------diff--------------------------
	    echo diff $BADDIR/${f%.sml}.out  $TESTDIR/outputs/${f%.sml}.out
	    diff $BADDIR/${f%.sml}.out  $TESTDIR/outputs/${f%.sml}.out
	  fi
    done
}

function newFiles
{
    for f in $NEW; do
	  echo :::::::::::::::::::::::::::$f::::::::::::::::::::::::::::
	  cat $TESTDIR/tests/$f	  
	  echo ---------------------------new----------------------------
	  cat $BADDIR/${f%.sml}.out
	  echo " "
    done
}

function unchangedFilesOLD
{
    for f in $OPENBUGS; do
	  echo :::::::::::::::::::::::::::$f::::::::::::::::::::::::::::
	  cat $TESTDIR/tests/$f
	  echo -------------------------unchanged-----------------------
	  cat $TESTDIR/outputs/${f%.sml}.out
	  echo " "
    done
}

function unchangedFiles
{
    for f in $OPENBUGS; do
          echo $f
    done
}

function banner 
{
  echo "**************************************************"
  print $1
  echo "**************************************************"
}


#
# go for it!
#
banner "\t\tTESTS  THAT   FAILED" >> $RESULTS
changedFiles "$FAIL" >> $RESULTS

banner "\t\tPOTENTIAL BUG FIXES"  >> $RESULTS
changedFiles "$FIXED" >> $RESULTS

banner "\t\tNEW TEST CASES" >> $RESULTS
newFiles >> $RESULTS

banner "\t\tOPENBUGS" >> $RESULTS
unchangedFiles >> $RESULTS


#*******************************************************************
#
# cleanup
#
rm -f $TSML.$SUFFIX $TMPFILE $LOGFILE

	
exit 0
