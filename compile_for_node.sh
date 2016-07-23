#!/bin/bash

set -e

compile () {
    js_of_ocaml \
	--pretty \
	--custom-header='#!/usr/bin/env node' \
	mailer.byte \
	-o ocaml_mailing_list
    chmod +x ocaml_mailing_list
}

compile
