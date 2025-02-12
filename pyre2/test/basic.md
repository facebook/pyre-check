# Simple CLI tests

## No errors on the empty file

```scrut
$ echo "" > $TMPDIR/empty.py && $PYRE2 check $TMPDIR/empty.py -a
* 0 errors, * (glob)
[0]
```

## Error on a non-existent file

```scrut {output_stream: stderr}
$ $PYRE2 check $TMPDIR/does_not_exist
No files matched pattern `*/does_not_exist` (glob)
[1]
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

## We don't report from nested files

```scrut
$ echo "x: str = 12" > $TMPDIR/hidden1.py && \
> echo "import hidden1; y: int = hidden1.x" > $TMPDIR/hidden2.py && \
> $PYRE2 check $TMPDIR/hidden2.py --include=$TMPDIR
ERROR */hidden2.py:1:26-35: EXPECTED str <: int (glob)
 INFO 1 errors, * (glob)
[1]
```

## We do report from nested with --check-all

```scrut
$ echo "x: str = 12" > $TMPDIR/shown1.py && \
> echo "import shown1; y: int = shown1.x" > $TMPDIR/shown2.py && \
> $PYRE2 check $TMPDIR/shown2.py --include=$TMPDIR --check-all
ERROR */shown*.py:1:* (glob)
ERROR */shown*.py:1:* (glob)
 INFO 2 errors, * (glob)
[1]
```

## We can do our own globbing

```scrut
$ echo "x: str = 12" > $TMPDIR/glob1.py && \
> echo "x: str = 12" > $TMPDIR/glob2.py && \
> $PYRE2 check "$TMPDIR/glob*.py"
ERROR */glob*.py:1:* (glob)
ERROR */glob*.py:1:* (glob)
 INFO 2 errors, * (glob)
[1]
```
