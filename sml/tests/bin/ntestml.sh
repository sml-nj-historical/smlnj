#! /bin/ksh
#set -x

#
# testml.sh [-T <testDir>] [-sml <sml>] [-tmp <tmpName>] [-depositOnly] 
# 	    [-cpu <limit>]
#
CMD=${0##*/}\>

CPULIMIT=400
SML=${SML:-"/usr/local/bin/sml"}
KSH=/bin/ksh
#DIFF=/bin/diff
#DIFF=/opt/gnu/bin/diff
DIFF=diff
ECHO=print
TESTDIR=bugs
BADDIR=$TESTDIR/bad
TSML=$TESTDIR/tsml
TMPFILE=$TESTDIR/tmp
TESTMODE="TESTONLY"
OPENBUGSLIST=$TESTDIR/openbugs

# default ARCH and OPSYS
ARCH=${ARCH:-sparc}
OPSYS=${OPSYS:-solaris}

#
# use the arch-n-opsys script to determine the ARCH/OS if possible
#
if [[ (-f ./bin/arch-n-opsys.sh) && (-x ./bin/arch-n-opsys.sh) ]]
then
  ARCH_N_OPSYS=`./bin/arch-n-opsys.sh`
  if [[ "$?" = "0" ]]
  then
    eval $ARCH_N_OPSYS
  fi
fi
SUFFIX="$ARCH-$OPSYS"

#
# Command line processing
#

function printUsage {
 $ECHO -u2 "testml.sh testdir"
 $ECHO -u2 "    [-f <testfile>]"        
 $ECHO -u2 "    [-tmp <tmpfileName>   default=$TSML and $TMPFILE]"
 $ECHO -u2 "    [-sml <executable>    default=$SML]"
 $ECHO -u2 "    [-cpu <cpulimit>      default=$CPULIMIT]"
 $ECHO -u2 "    [-openbugs <filename> default=$OPENBUGSLIST]"
 $ECHO -u2 "    [-bad <dir>           default=$BADDIR]"
 $ECHO -u2 "    [-depositOnly]"
 $ECHO -u2 "    [-help]"
}

#
# testdir must be the first (manditory) parameter.
#
if [[ $# -eq 0 ]]
then
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
TSML=$TESTDIR/tsml
TMPFILE=$TESTDIR/tmp.$SUFFIX
TESTMODE="TESTONLY"
OPENBUGSLIST=$TESTDIR/openbugs

while [[ $# -ne 0 ]]
do
  	arg=$1; shift
	case $arg in
	  -f)
		if [[ $# -eq 0 ]]
	        then
			$ECHO -u2 $CMD must name test file with -f option
			exit 1
		fi
		TESTFILE=$1; shift
		;;
	  -sml)
		if [[ $# -eq 0 ]]
		then
			$ECHO -u2 $CMD must name executable with -sml option
			exit 1
		fi
		SML=$1; shift
		;;
	  -tmp)
		if [[ $# -eq 0 ]]
		then
			$ECHO -u2 $CMD must name temporary to use with -tmp option
			exit 1
		fi
		TSML=$1; TMPFILE=$1.tmp; shift
		;;
	  -depositOnly)
		TESTMODE="DEPOSITONLY"
		;;
	  -openbugs)
		if [[ $# -eq 0 ]]
		then
			$ECHO -u2 $CMD must specify a file name with -openbugs option
			exit 1
		fi
		OPENBUGSLIST=$1; shift
		;;
	  -cpu)
		if [[ $# -eq 0 ]]
		then
			$ECHO -u2 $CMD must cpu limit with -cpu option
			exit 1
		fi
		CPULIMIT=$1; shift
		;;
	  -bad)
		if [[ $# -eq 0 ]]
		then
		  $ECHO -u2 $CMD must supply directory with -bad option
		  exit 1
		fi
		BADDIR=$1; shift
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
if [[ ! (-d $TESTDIR/tests) ]]
then
  $ECHO -u2 ${CMD} Error: Testing directory does not contain tests/
  exit 1
elif [[ ! (-d $TESTDIR/outputs) ]]
then
  $ECHO -u2 ${CMD} Error: Testing directory does not contain outputs/
  exit 1
elif [[ ! (-a $OPENBUGSLIST) ]]
then 
  $ECHO -u2 ${CMD} Error: openbugs files does not exist - $OPENBUGSLIST
  exit 1
elif [[ (-a $BADDIR) ]]
then
  $ECHO -u2 ${CMD} Error: $BADDIR directory/file already exist--please delete
  exit 1  
fi

#
# Should check that the mkdir succeeds.
#
mkdir $BADDIR


#
# Build version of compiler with stuff turned off
#
if [[ ! (-x $SML) ]]
then
	$ECHO ${CMD} ML compiler $SML does not exist or is not executable
	exit 1
fi


$ECHO -u2 ${CMD} Building special version of SML for $OPSYS ...
$SML @SMLquiet @SMLdebug=/dev/null << xxx 1>/dev/null
(*      CM.autoload "host-compiler.cm";  (Compiler already autolaoded) *)
  	Compiler.Control.primaryPrompt:="";
  	Compiler.Control.secondaryPrompt:="";
        Compiler.Control.Print.printLength := 1000;
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
$ECHO -u2 -n "."
case $OPSYS in
 hpux)
        # ulimit is broken on hppa (any other ways to limit cpu time?)
	$KSH <<-YYY 1>$TMPFILE 2>&1
		(echo "(*#line 0 \"$srcFile\"*)"; cat $file) | \
		    $SML @SMLquiet @SMLdebug=/dev/null @SMLload=$TSML 
YYY
        ;;
 linux | aix | irix6 | solaris | dunix)
	$KSH <<-YYY 1>$TMPFILE 2>&1
		ulimit -t $CPULIMIT
		(echo "(*#line 0 \\"$srcFile\\"*)"; cat $file) | \
		    $SML @SMLquiet @SMLdebug=/dev/null @SMLload=$TSML 
YYY
        ;;
 *)
	$KSH -x <<-YYY 1>$TMPFILE 2>&1
		ulimit -t $CPULIMIT
		(echo "(*#line 0 \"$srcFile\"*)"; cat $file) | \
		    $SML @SMLquiet @SMLdebug=/dev/null @SMLload=$TSML 
YYY
        ;;
esac
}	


function compareOutput
{
	$DIFF -b $TMPFILE ${outPath}/${srcFile%%.sml}.out >/dev/null
	if [[ $? -eq 0 ]]
	then
		fgrep "$file" $OPENBUGSLIST >/dev/null
		if [[ $? -eq 0 ]]
		then
			$ECHO " openbug (unchanged)"
		else
			$ECHO " pass"
		fi
	else
		fgrep "$file" $OPENBUGSLIST >/dev/null
		if [[ $? -eq 0 ]]
		then
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
if [[ "$TESTFILE" != "" ]]
then
  TESTS="$TESTFILE"
else
  TESTS="$TESTDIR/tests/*"
fi

for file in $TESTS
do
	srcFile=${file##*/tests/}
	srcPath=${file%%/tests/*}
	outPath=$srcPath/outputs
	outFile=$outPath/${srcFile%%.sml}.out

	# does srcFile have valid extension
	if [[ ${srcFile%%.sml} = $srcFile ]]
	then continue;
	fi 
	if [[ ! (-a $file) ]]
	then continue;
	fi

	case $TESTMODE in
	  "TESTONLY")
		$ECHO -n "${CMD}  Testing ${file##*/tests/} 	   ... "
		testMLFile
		if [[ ! ( -r $outFile) ]]
		then
		        mv $TMPFILE $BADDIR/${srcFile%%.sml}.out
			$ECHO " output file does not exist!!"
		else
			compareOutput
		fi
		;;
	"DEPOSITONLY")
		if [[ ! (-a $outFile) ]]
		then
			$ECHO -n "${CMD}  Testing ${file##*/tests/} 	   ... "
			testMLFile
			cp $TMPFILE $outFile
			if [[ ! (-a $outFile) ]]
			then
				$ECHO ${CMD} Error: could not create $outFile
				exit 2
			fi
			$ECHO " deposited ${srcFile%%.sml}.out"
		fi
		;;
	esac
done

#
# cleanup
#
rm -f $TSML.$SUFFIX $TMPFILE /tmp/testblast

	
exit 0

