#!/bin/sh
cat test/$1.lgl | jbuilder exec bin/ex.bc a
