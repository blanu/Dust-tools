for x in {0..10}
do
  echo "$x,127" >short.mask 
  dist/build/replay-client/replay-client captures/openvpn-tcp.ps short.mask
  read -p "Continue from $x"
done
