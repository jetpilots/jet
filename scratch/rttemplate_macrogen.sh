#!/bin/sh
for i in `seq 2 15`
do
printf "#define CAT$i("
for j in `seq $((i-1))`
do
printf "i$j, "
done
printf "...) JN(${i}D_##i1, "
for j in `seq 2 $((i-1))`
do
printf "JN(i$j, "
done
printf "__VA_ARGS__"
for j in `seq $((i-1))`
do printf ")"
done
echo
done

for i in `seq 3 15`
do
printf "#define CTSUM$i("
for j in `seq $((i-1))`
do
printf "i$j, "
done
printf "...) ("
for j in `seq $((i-1))`
do
printf " i$j + "
done
# for j in `seq 2 $((i-1))`
# do
# printf "CTSUM$j(i$j, "
# done
printf "__VA_ARGS__)"
# for j in `seq $((i-2))`
# do printf ")"
# done
echo
done



for i in `seq 3 15`
do
printf "#define CTMUL$i("
for j in `seq $((i-1))`
do
printf "i$j, "
done
printf "...) ("
for j in `seq $((i-1))`
do
printf " i$j * "
done
# for j in `seq 2 $((i-1))`
# do
# printf "CTSUM$j(i$j, "
# done
printf "__VA_ARGS__)"
# for j in `seq $((i-2))`
# do printf ")"
# done
echo
done


for i in `seq 3 15`
do
printf "#define CTCOM$i("
for j in `seq $((i-1))`
do
printf "i$j, "
done
printf "...) {"
for j in `seq $((i-1))`
do
printf " i$j,  "
done
# for j in `seq 2 $((i-1))`
# do
# printf "CTSUM$j(i$j, "
# done
printf "__VA_ARGS__}"
# for j in `seq $((i-2))`
# do printf ")"
# done
echo
done
