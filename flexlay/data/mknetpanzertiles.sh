#! /bin/sh

echo "Generating netpanzertiles.xml"
echo "<resources>" > netpanzertiles.xml
echo "  <section name=\"tiles\">" >> netpanzertiles.xml
for i in images/netpanzertiles/*.png; 
do
#  BASENAME=`basename $i`
  BASENAME=${i##images/netpanzertiles/tile}
  echo "<sprite name=\"${BASENAME%%.png}\"><image file=\"$i\"/></sprite>";
done >> netpanzertiles.xml
echo "  </section>" >> netpanzertiles.xml
echo "</resources>" >> netpanzertiles.xml

echo "Generating netpanzertiles.scm"
echo "(windstille-tiles" > netpanzertiles.scm
for i in images/netpanzertiles/*.png; 
do
#  BASENAME=`basename $i`
  BASENAME=${i##images/netpanzertiles/tile}
  BASENAME=${BASENAME%%.png}
  echo "(tile (id ${BASENAME})"
  echo "      (image \"tiles/${BASENAME}\")"
  echo "      (colmap 255 255 255 255 255 255 255 255))"
done >> netpanzertiles.scm
echo ")" >> netpanzertiles.scm

# EOF #
