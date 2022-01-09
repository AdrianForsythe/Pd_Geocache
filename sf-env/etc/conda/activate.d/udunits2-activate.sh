#!/usr/bin/env bash

# Store existing env vars and set to this conda env
# so other installs don't pollute the environment.

if [[ -n "$UDUNITS2_XML_PATH" ]]; then
    export _CONDA_SET_UDUNITS2_XML_PATH=$UDUNITS2_XML_PATH
fi

# On Linux the share data is in $CONDA_PREFIX/share, but
# Windows keeps it in $CONDA_PREFIX/Library/share
if [ -d $CONDA_PREFIX/share/udunits ]; then
    export UDUNITS2_XML_PATH=$CONDA_PREFIX/share/udunits/udunits2.xml
elif [ -d $CONDA_PREFIX/Library/share/udunits ]; then
    export UDUNITS2_XML_PATH=$CONDA_PREFIX/Library/share/udunits/udunits2.xml
fi
