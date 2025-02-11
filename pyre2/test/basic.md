# Simple CLI tests

## No errors on the empty file

```scrut
$ $PYRE2 check ../empty -a
* 0 errors, * (glob)
[0]
```

## We can typecheck two files with the same name

```scrut
$ echo "x: str = 12" > $TMPDIR/same_name.py && \
> echo "x: str = True" > $TMPDIR/same_name.pyi && \
> $PYRE2 check $TMPDIR/same_name.py $TMPDIR/same_name.pyi
ERROR */same_name.py*:1:10-* (glob)
ERROR */same_name.py*:1:10-* (glob)
 INFO 2 errors, * (glob)
[1]
```
