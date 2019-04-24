
top=`pwd`

rm_out() {
    rm -f *.json *.pretty *.err *.c
}

rm_gold() {
    rm -f *.json.gold *.c.gold
}

cd $top/test/out
rm_out
cd $top/test/out/mlayout
rm_out
cd $top/test/gold/
rm_gold
cd $top/test/gold/mlayout
rm_gold
