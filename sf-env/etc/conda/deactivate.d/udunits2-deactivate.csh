#!/usr/bin/env csh

# Restore previous env vars if they were set.
unsetenv UDUNITS2_XML_PATH

if ( $?_CONDA_SET_UDUNITS2_XML_PATH ) then
    setenv UDUNITS2_XML_PATH "$_CONDA_SET_UDUNITS2_XML_PATH"
    unsetenv _CONDA_SET_UDUNITS2_XML_PATH
endif
