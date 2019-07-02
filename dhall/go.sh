# set -x

PATH="$PATH":~/.local/bin

if [ -z $1 ] ; then
    src=XTreeTest.dhall
else
    cd $(dirname "$1")
    src=$(basename "$1")
fi

# echo "\"$src\""

dhall format --inplace "$src"
# dhall lint --inplace $src
dhall --explain < "$src"
