DEP="/home/waszczuk/departage/local-data/tools/departage"
TAG="$DEP tag -c dhall/config.dhall"

mkdir fifo

mkfifo fifo/fifoInit 
$DEP clear < /dev/stdin > fifo/fifoInit &
lastFifo=fifoInit

for mweTyp in "$@"; do
  # echo $mweTyp
  mkfifo fifo/fifo$mweTyp
  $TAG -m model/$mweTyp.model --mwe $mweTyp < fifo/$lastFifo > fifo/fifo$mweTyp &
  lastFifo=fifo$mweTyp
done

cat fifo/$lastFifo

# $DEP clear | \
# $TAG -m model/IRV.model --mwe IRV | \
# $TAG -m model/LVC.full.model --mwe LVC.full | \
# $TAG -m model/LVC.cause.model --mwe LVC.cause | \
# $TAG -m model/MVC.model --mwe MVC | \
# $TAG -m model/VID.model --mwe VID
