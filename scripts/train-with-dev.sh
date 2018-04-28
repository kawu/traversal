DEP="/home/waszczuk/departage/local-data/tools/departage"
TRAIN="$DEP train -c dhall/config.dhall -t ../train.cupt -d ../dev.cupt"

for mweTyp in "$@"; do
  $TRAIN --mwe $mweTyp -m model/$mweTyp.model &> logs/$mweTyp.log &
done

wait
