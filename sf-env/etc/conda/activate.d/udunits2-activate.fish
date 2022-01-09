#!/usr/bin/env fish

if set -q UDUNITS2_XML_PATH
  set -gx _CONDA_SET_UDUNITS2_XML_PATH "$UDUNITS2_XML_PATH"
end

# On Linux the share data is in $CONDA_PREFIX/share, but
# Windows keeps it in $CONDA_PREFIX/Library/share
if test -d "$CONDA_PREFIX/share/udunits"
  set -gx UDUNITS2_XML_PATH "$CONDA_PREFIX/share/udunits/udunits2.xml"
else if test -d "$CONDA_PREFIX/Library/share/udunits"
  set -gx UDUNITS2_XML_PATH "$CONDA_PREFIX/Library/share/udunits/udunits2.xml"
end
