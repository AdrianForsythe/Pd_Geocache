#!/usr/bin/env fish

# Restore previous env vars if they were set.
set -e UDUNITS2_XML_PATH

if set -q _CONDA_SET_UDUNITS2_XML_PATH
    set -gx  UDUNITS2_XML_PATH "$_CONDA_SET_UDUNITS2_XML_PATH"
    set -e _CONDA_SET_UDUNITS2_XML_PATH
end
