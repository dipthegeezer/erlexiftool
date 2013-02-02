erlexiftool
===========

Server that calls out to perl exiftool for metadata information.

## Description

The library interfaces with the perl exiftool library using port commands.
Commands in a binary format are sent to the perl script and the data returned
will also be in a binary format. Note you must have perl installed on your system
for this library to work.

## Installation

Add the library to your rebar config

```
{
  deps,
  [
   {erlexiftool, "0.1.*", {git, "git://github.com/dipthegeezer/erlexiftool.git", "HEAD"}},
   ....
  ]
}.

```

## API

```

application:start(erlexiftool),

%% Ask for metadata on file
Proplist = erlexiftool:parse("/path/to/file.something"),

%% Data is returned as list of tuples.
{directory, BinDir} = proplists:lookup(directory, Proplist),
Dir = binary_to_list(BinDir),

%% Do some further stuff

application:stop(erlexiftool),

```

## Future Work

I would like if possible to rewrite the perl library in erlang. If someone
out there is courageous enough then go for it.

## License

(The MIT License)

Copyright (c) 2013 dipthegeezer

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.