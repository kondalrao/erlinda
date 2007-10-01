if [ $# -gt 0 ];
then
  erlc -o lib/fslib/ebin -I lib/fslib/include lib/fslib/src/*.erl 2>/dev/null|grep -v Warning
  erlc -o lib/gas/ebin -I lib/fslib/include lib/gas/src/*.erl 2>/dev/null|grep -v Warning
fi;
erlc -o ebin -I include/ -Ilib/fslib/include src/space/*.erl |grep -v Warning
cd release/local
cat ../../src/space/tuple_space.app.src |sed "s/%VSN%/1.0/g" > tuple_space.app
erl -boot start_sasl -sname node1 -config erlinda_rel.config -pz ../../ebin
