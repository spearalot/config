(require 'erlang)
(require 'tempo)

(tempo-define-template "erlang-export"
		       '("-export([" r "])." > n)
		       "exp"
		       "Insert a -export([]). statement"
		       'erlang-tempo-tags)

(tempo-define-template "erlang-export"
		       '("-export([" r "])." > n)
		       "exp"
		       "Insert a -export([]). statement"
		       'erlang-tempo-tags)

(tempo-define-template "erlang-include-lib"
		       '("-include_lib(\"" r "\")." > n)
		       "lib"
		       "Insert a -include_lib(). statement"
		       'erlang-tempo-tags)

(tempo-define-template "erlang-export-all"
		       '("-compile(export_all)." > n)
		       "cmp"
		       "Insert a -compile(export_all). statement"
		       'erlang-tempo-tags)

(tempo-define-template "erlang-include-eunit-lib"
		       '("-include_lib(\"eunit/include/eunit.hrl\")." > n)
		       "eulib"
		       "Insert a -include_lib(). statement"
		       'erlang-tempo-tags)

(tempo-define-template "erlang-include-eqc-lib"
		       '("-undef(LET)." > n
                         "-include_lib(\"eqc/include/eqc.hrl\")." > n)
		       "qclib"
		       "Insert a -include_lib(). statement"
		       'erlang-tempo-tags)

(tempo-define-template "erlang-unit-hdr"
		       '("-ifdef(TEST)." > n
                         "-ifdef(EQC)." > n
                         "-include_lib(\"eunit/include/eunit.hrl\")." > n
                         "-undef(LET)." > n
                         "-include_lib(\"eqc/include/eqc.hrl\")." > n n
                         o
                         "-endif. %EQC" > n
                         "-endif. %TEST" > n)
		       "thdr"
		       "Insert default unit test headers statement"
		       'erlang-tempo-tags)
