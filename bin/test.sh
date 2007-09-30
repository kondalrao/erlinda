if [ $# -gt 0 ];
then
  erlc -I lib/fslib/include lib/fslib/src/*.erl -o lib/fslib/ebin 2>/dev/null|grep -v Warning
  erlc -I lib/fslib/include lib/gas/src/*.erl -o lib/gas/ebin 2>/dev/null|grep -v Warning
fi;
erlc -I include/ -Ilib/fslib/include src/space/*.erl -o ebin|grep -v Warning
cd release/local
cat ../../src/space/tuple_space.app.src |sed "s/%VSN%/1.0/g" > tuple_space.app
erl -boot start_sasl -sname node1 -config erlinda_rel.config -pz ../../ebin
