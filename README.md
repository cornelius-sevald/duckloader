duckloader
==========

duckloader can turn a list of workship IDs (or links)
of Duck Game maps into a playlist that can be loaded in Duck Game.

duckloader uses steamcmd to download the workshop contents,
so it will need to be installed in your path.

INSTALL
-------

Install using cabal by typing `cabal install exe:duckloader`.

Make sure that the install location (default ~/.cabal/duckloader)
is in your PATH.

USAGE
-----

duckloader will read a list of workshop IDs from a file (or stdin),
with each ID on a separate line. Everything after a '#'
character is ignored for each line.

duckloader will then output the duck game playlist to a
specified file (or stdout).

Type `duckloader --help` for more detail.

LICENSE
-------

duckloader is licensed under the terms of the BSD-3-Clause license.
See the [LICENSE] file for further detail.
