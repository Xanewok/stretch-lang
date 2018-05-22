if [ -d /home/students ]; then
	BUILD="make"
	INTERP="./interpreter"
else
	BUILD="stack build"
	INTERP="stack exec stretchi"
fi

eval $BUILD

tmp_out=$(mktemp)
tmp_err=$(mktemp)

# typeck tests are tested by test/Spec.hs
for f in {good,bad}/{,parse/}*.str; do
	# Skip non-files
	if [ ! -f ${f} ]; then
		continue
	fi

	# Generate expected test data if ran with `./test prepare`
	if [ "$1" = "prepare" ]; then
		eval $INTERP $f 1>${f%str}stdout 2>${f%str}stderr
		continue
	fi

	# Perform checks only when there is expected test data
	if [ ! -f ${f%str}stdout ] && [ ! -f ${f%str}stderr ]; then
		echo -e "\e[1mSkipping \e[0m$f..."
		continue
	else
		echo -e "\e[1mTesting \e[0m$f..."
	fi

	eval $INTERP "$f" 1>$tmp_out 2>$tmp_err

	# Compare stdout with expected
	if [ -f ${f%str}stdout ]; then
		diff $tmp_out "${f%str}stdout"
		es=$?
		if [ $es -ne 0 ]; then
			echo -e "stdout [\e[31m FAIL\e[0m ]"
		else
			echo -e "stdout [\e[32m PASS\e[0m ]"
		fi
	fi

	# Compare stderr with expected
	if [ -f ${f%str}stderr ]; then
		diff $tmp_err "${f%str}stderr"
		es=$?
		if [ $es -ne 0 ]; then
			echo -e "stderr [\e[31m FAIL\e[0m ]"
		else
			echo -e "stderr [\e[32m PASS\e[0m ]"
		fi
	fi
done

rm ${tmp_out}
rm ${tmp_err}
