# Simple CLI tests

## No errors on the empty file

```scrut
$ echo "" > $TMPDIR/empty.py && $PYREFLY check --python-version 3.13.0 $TMPDIR/empty.py --search-path $TMPDIR -a  2>&1 | grep -v "overrides"
* INFO * errors* (glob)
[0]
```

## Error on a non-existent file

```scrut {output_stream: stderr}
$ $PYREFLY check $TMPDIR/does_not_exist --python-version 3.13.0 --search-path $TMPDIR/does_not_exist
No files matched pattern `*/does_not_exist` (glob)
[1]
```

## We can typecheck two files with the same name

```scrut
$ echo "x: str = 12" > $TMPDIR/same_name.py && \
> echo "x: str = True" > $TMPDIR/same_name.pyi && \
> $PYREFLY check --python-version 3.13.0 $TMPDIR/same_name.py $TMPDIR/same_name.pyi --search-path $TMPDIR
ERROR */same_name.py*:1:10-* (glob)
ERROR */same_name.py*:1:10-* (glob)
 INFO 2 errors* (glob)
[1]
```

## We don't report from nested files

```scrut
$ echo "x: str = 12" > $TMPDIR/hidden1.py && \
> echo "import hidden1; y: int = hidden1.x" > $TMPDIR/hidden2.py && \
> $PYREFLY check --python-version 3.13.0 $TMPDIR/hidden2.py --search-path=$TMPDIR
ERROR */hidden2.py:1:26-35: `str` is not assignable to `int` [bad-assignment] (glob)
 INFO 1 errors* (glob)
[1]
```

## We do report from nested with --check-all

```scrut
$ echo "x: str = 12" > $TMPDIR/shown1.py && \
> echo "import shown1; y: int = shown1.x" > $TMPDIR/shown2.py && \
> $PYREFLY check --python-version 3.13.0 $TMPDIR/shown2.py --search-path=$TMPDIR --check-all 2>&1 | grep -v "overrides"
ERROR */shown*.py:1:* (glob)
ERROR */shown*.py:1:* (glob)
* INFO * errors* (glob)
[0]
```

## We can do our own globbing

```scrut
$ echo "x: str = 12" > $TMPDIR/glob1.py && \
> echo "x: str = 12" > $TMPDIR/glob2.py && \
> $PYREFLY check --python-version 3.13.0 "$TMPDIR/glob*.py" --search-path $TMPDIR
ERROR */glob*.py:1:* (glob)
ERROR */glob*.py:1:* (glob)
 INFO 2 errors* (glob)
[1]
```

## We return an error when all files are filtered by project_excludes

```scrut {output_stream: stderr}
$ echo "x: str = 12" > $TMPDIR/excluded.py && \
> $PYREFLY check --python-version 3.13.0 $TMPDIR/excluded.py --project-excludes="$TMPDIR/*"
All found `project_includes` files were filtered by `project_excludes` patterns.
`project_includes`:* (glob)
`project_excludes`:* (glob)
[1]
```

## Error on a non-existent search-path/site-package-path

```scrut
$ echo "" > $TMPDIR/empty.py && $PYREFLY check --python-version 3.13.0 $TMPDIR/empty.py \
> --search-path $TMPDIR/abcd --site-package-path $TMPDIR/abcd
*WARN Nonexistent `site_package_path` found:* (glob)
*WARN Nonexistent `search_path` found:* (glob)
* INFO * errors* (glob)
[0]
```
