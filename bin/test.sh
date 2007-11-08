clear
rm -rf release/local/tupledb/
if [ $# -gt 0 ];
then
  erlc -o lib/fslib/ebin -I lib/fslib/include lib/fslib/src/*.erl 2>/dev/null|grep -v Warning
  erlc -o lib/gas/ebin -I lib/fslib/include lib/gas/src/*.erl 2>/dev/null|grep -v Warning
fi;
erlc -o ebin -I include/ -Ilib/fslib/include src/util/*.erl src/space/*.erl test/space/*.erl|grep -v Warning
cd release/local
cat ../../src/space/tuple_space.app.src |sed "s/%VSN%/1.0/g" > tuple_space.app

rm -rf tupledb
mkdir -p tupledb

if [ $# -gt 0 ];
then
  echo erl -boot start_sasl -mnesia dir tupledb -sname node1 -config erlinda_rel.config -pz ../../ebin 
  erl -boot start_sasl -mnesia dir tupledb -sname node1 -config erlinda_rel.config -pz ../../ebin 
else
  echo erl -boot start_sasl -mnesia dir tupledb -sname node1 -config erlinda_rel.config -pz ../../ebin -noshell -s multi_tuple_space_test test -s init stop
  erl -boot start_sasl -mnesia dir tupledb -sname node1 -config erlinda_rel.config -pz ../../ebin -noshell -s multi_tuple_space_test test -s init stop
  #erl -boot start_sasl -mnesia dir tupledb -sname node1 -config erlinda_rel.config -pz ../../ebin -noshell -s tuple_space_test test -s init stop
  #erl -boot start_sasl -mnesia dir tupledb -sname node1 -config erlinda_rel.config -pz ../../ebin -noshell -s mnesia_ts_test test -s init stop
  #######erl -boot start_sasl -sname node1 -config erlinda_rel.config -pz ../../ebin -noshell -s tuple_util_test test -s init stop
fi;
