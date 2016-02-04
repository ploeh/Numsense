#!/bin/bash
if test "$OS" = "Windows_NT"
then
  # use .Net

  packages/FAKE.4.11.3/tools/FAKE.exe $@ --fsiargs Build.fsx
else
  # use mono

  mono packages/FAKE.4.11.3/tools/FAKE.exe $@ --fsiargs -d:MONO Build.fsx
fi
