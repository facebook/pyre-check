#!/usr/bin/env python3

import fnmatch
import os
import re


class ShardedFileComponents(object):
    """
    Splits a sharded file name or pattern into its components:
    - directory
    - stem
    - extension
    - shard_index
    - shard_total
    For shard patterns @n, the shard index is set to -1.
    For shard patterns @*, the shard total is also set to -1.
    """

    def __init__(self, filepattern):
        self.directory, root = os.path.split(filepattern)
        m = re.match(r"([^@]+)@([^.@]+)(\.[^.@]*)?$", root)
        if not m:
            raise ValueError("Not a sharded file: {}".format(filepattern))

        self.extension = m.group(3) if m.lastindex >= 3 else ""
        self.stem = m.group(1) if m.lastindex >= 1 else ""

        shards = m.group(2) if m.lastindex >= 2 else ""
        if shards == "*":
            self.shard_index = -1
            self.shard_total = -1
        else:
            try:
                self.shard_total = int(shards)
                self.shard_index = -1
            except ValueError:
                # Look for ?????-of-????? pattern.
                m = re.match(r"(\d{5})-of-(\d{5})$", shards)
                if not m:
                    raise ValueError("Invalid shard specification: {}".format(shards))
                self.shard_index = int(m.group(1))
                self.shard_total = int(m.group(2))

        if self.directory == "":
            self.directory = "."

        # Sanity check shard total.
        if self.shard_total == 0:
            raise ValueError("Invalid shard total 0")

    def get_shard_filename(self, index):
        if index < 0 or index >= self.shard_total:
            raise ValueError("Invalid shard index")
        return os.path.join(
            self.directory,
            "{}@{:05d}-of-{:05d}{}".format(
                self.stem, index, self.shard_total, self.extension
            ),
        )

    def is_at_n_pattern(self):
        return self.shard_total > 0 and self.shard_index == -1

    def is_at_star_pattern(self):
        return self.shard_total == -1 and self.shard_index == -1


class ShardedFile(object):
    """A sharded file object represents a set of files sharded according to the
    following pattern:

         path/stem@?????-of-?????.ext

    The ????? represent 0-padded decimal numbers. For example:
    1) path/foo@00000-of-00001.ext is the single shard of a 1-sharded file.
    1) path/foo@00000-of-00002.ext and path/foo@00001-of-00002.ext are the
       two shards of a 2-sharded file.

    There are 2 ways to name a sharded file set:
    1) path/foo@*.ext -- match a sharded file with arbitrary # of shards.
    2) path/foo@n.ext -- match a sharded file with n shards.

    A sharded file object is constructed by giving it a file pattern of the
    above form. The pattern is checked and the appropriate shards found. Errors
    are indicated as properties on the sharded file.

    The list of shards is available in shard_file_names.
    """

    def __init__(self, pattern):
        """
        Determine shards from pattern and record errors.
        """
        comps = ShardedFileComponents(pattern)
        if comps.is_at_star_pattern():
            comps.shard_total = self._find_unambiguous_shard_total(comps, pattern)
        elif not comps.is_at_n_pattern():
            raise ValueError("Pattern should use @n or @* for shard specification.")

        # now we have an @n spec.
        self._set_shard_file_names(comps)
        self._shard_file_names.sort()

    def get_filenames(self):
        return self._shard_file_names

    def _set_shard_file_names(self, pcomps):
        self._shard_file_names = []
        for i in range(pcomps.shard_total):
            filename = pcomps.get_shard_filename(i)
            if not os.path.isfile(filename):
                raise ValueError("Shard {} does not exist.".format(filename))
            self._shard_file_names.append(filename)

    def _find_unambiguous_shard_total(self, pcomps, pattern):
        dir = pcomps.directory
        if not os.path.isdir(dir):
            raise ValueError("Not a directory {}".format(dir))

        pattern = pcomps.stem + "@?????-of-?????" + pcomps.extension
        seen_shard_count = -1
        for file in os.listdir(dir):
            if fnmatch.fnmatch(file, pattern):
                try:
                    comps = ShardedFileComponents(file)
                except ValueError:
                    continue
                if seen_shard_count == -1:
                    seen_shard_count = comps.shard_total
                elif seen_shard_count != comps.shard_total:
                    [a, b] = sorted([seen_shard_count, comps.shard_total])
                    raise ValueError(
                        "@* matches ambiguous shard sets: @{} and @{}".format(a, b)
                    )
        if seen_shard_count == -1:
            raise ValueError("Pattern matches no sharded file set: {}".format(pattern))
        return seen_shard_count
