set -a # export all vars
# set -x # debug

root=$(realpath $(dirname $BASH_SOURCE))/..

DISABLE_NATIVE="echo"

PKGS="-package str"
SYNTAX=""

# 8~"pattern-matching is not exhaustive"; 
# 11~"this match case is unused";
# 26~"unused variable s2"
WARN="-w @f@p@u@s@40-8-11-26"

  ocamlc="$DISABLE_BYTE ocamlfind ocamlc   $WARN $PKGS $SYNTAX"
ocamlopt="$DISABLE_NTVE ocamlfind ocamlopt $WARN $PKGS $SYNTAX"
ocamldep="ocamlfind ocamldep $PKGS"

mk_cma="$DISABLE_BYTE ocamlfind ocamlc"
mk_cmxa="$DISABLE_NTVE ocamlfind ocamlopt"


# function assert_in_build() {
#     if [ "${PWD##*/}" = "_build" ]; then return 0; else echo "error: not in _build"; return -1; fi
# }
