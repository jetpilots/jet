#!/bin/sh
rm -f *.gcno *.gcda
make -B jetc-cov # because it generates .gcno
# ./jetc-cov.bin simptest0.jet
# mkdir .covtest 2>/dev/null
# cd .covtest
# for f in ../../programs/*.c
# do ln -sf $f .
# done
# for f in ../../modules/*.h
# do ln -sf $f .
# done
#  ln -sf ../../main.gcno .
#  ln -sf ../../main.gcda .

NTOT=0
NERR=0
for f in `find tests -name '*.jet'`
do
    RET=0
    ./jetc-cov "$f" d > /dev/null  2>&1
    RET=$((RET+$?))
    ./jetc-cov "$f" > /dev/null  2>&1
    RET=$((RET+$?))
    ./jetc-cov "$f" l > /dev/null  2>&1
    RET=$((RET+$?))
    ./jetc-cov "$f" t > /dev/null  2>&1
    RET=$((RET+$?))
    [ $RET == 0 ] && ST="\e[33mOK\e[0m" || ST="\e[31mERR\e[0m:$RET"
    [ $RET == 0 ] || NERR=$((NERR+1))
    printf "[$ST] $f\n" 1>&2
    NTOT=$((NTOT+1))
done
echo "*** $NERR of $NTOT failed"
cd programs
ln -sf ../main.gcno .
ln -sf ../main.gcda .
cd - > /dev/null 2>&1

# rm ../main.c
echo >  coverage.txt
echo "Unit                                        Unexec      Lines   Branches    Taken1+" >> coverage.txt
echo "----                                        ------      -----   --------    -------" >> coverage.txt

gcov -f -b -a programs/main.c | awk '

$1=="File" || $1=="Function" {
    printf "\n%s %-36s ",tolower(substr($1,1,4)), substr($2,2,length($2)-2)
}

END {
    printf "\n-- Executable lines = %.0f, not executed = %.0f --> %.1f%% coverage", totalLines, totalUnlines, (1-totalUnlines/totalLines)*100.0
}

$1=="Lines" || $1=="Branches" {
    var = substr($2,10)
    if ($1=="Lines") {
        lines = substr(var,1,length(var)-1)/100.0 * $4
        unlines = $4 - lines
        totalLines += $4*1
        totalUnlines+=unlines
        if (unlines > 0) {printf "%8.0f ", unlines}
            else { printf "%8s ", "" }
    }
    if (var*1.0 < 100.0) { printf "%9.0f%% ", var*1.0 }
    else { printf "%10s ", "" }
}

$1=="Taken" {
    taken = substr($4,6)*1.0
    if (taken < 100.0) printf "%9.0f%% ", taken
}

' | sort >> coverage.txt
#[ $? -ne 0 ] || less -S coverage.txt
# cd -
