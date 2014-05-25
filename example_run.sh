test0 +RTS -h; test1 +RTS -h; hp2ps -c test0.hp ; hp2ps -c test1.hp

test2 trace.json +RTS -h -p -sstderr; hp2ps -c test2.hp
