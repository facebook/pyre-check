# Simple CLI tests

## No errors on the empty file

```scrut
$ $PYRE2 check ../empty -a
* 0 errors, * (glob)
```

## We can typecheck two files with the same name

```scrut
$ echo "x: str = 12 # E: str" > $TMPDIR/same_name.py && \
> echo "x: str = True # E: str" > $TMPDIR/same_name.pyi && \
> $PYRE2 check $TMPDIR/same_name.py $TMPDIR/same_name.pyi | sort
 INFO 2 errors, * (glob)
ERROR */same_name.py:1:10-12:* (glob)
ERROR */same_name.pyi:1:10-14:* (glob)
```
