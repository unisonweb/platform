" Vim syntax file, adapted from Haskell file by John Williams <jrw@pobox.com>
"
" Language:		unison
" Maintainer:		Unison Computing
" Last Change:		Aug 23, 2018
" Original Author:      Paul Chiusano and Rúnar Bjarnason
"
" Options-assign a value to these variables to turn the option on:
"
" u_highlight_delimiters - Highlight delimiter characters--users
"			    with a light-colored background will
"			    probably want to turn this on.
" u_highlight_boolean - Treat True and False as keywords.
" u_highlight_types - Treat names of primitive types as keywords.
" u_highlight_more_types - Treat names of other common types as keywords.
" u_highlight_debug - Highlight names of debugging functions.
" u_allow_hash_operator - Don't highlight seemingly incorrect C
"			   preprocessor directives but assume them to be
"			   operators
" 2023 Jan  6: Update for current syntax (dt)
" 2018 Aug 23: Adapt Haskell highlighting to Unison, cleanup.
" 2004 Feb 19: Added C preprocessor directive handling, corrected eol comments
"	       cleaned away literate unison support (should be entirely in
"	       lunison.vim)
" 2004 Feb 20: Cleaned up C preprocessor directive handling, fixed single \
"	       in eol comment character class
" 2004 Feb 23: Made the leading comments somewhat clearer where it comes
"	       to attribution of work.

" Remove any old syntax stuff hanging around
if version < 600
  syn clear
elseif exists("b:current_syntax")
  finish
endif

syntax include @markdown $VIMRUNTIME/syntax/markdown.vim

syn cluster markdownLikeDocs contains=markdownBold,markdownItalic,markdownLinkText,markdownListMarker,markdownOrderedListMarker,markdownH1,markdownH2,markdownH3,markdownH4,markdownH5,markdownH6

syn match uOperator "[-!#$%&\*\+/<=>\?@\\^|~]"
syn match uDelimiter "[\[\](){},.]"

" Strings and constants
syn match   uSpecialChar	contained "\\\([0-9]\+\|o[0-7]\+\|x[0-9a-fA-F]\+\|[\"\\'&\\abfnrtv]\|^[A-Z^_\[\\\]]\)"
syn match   uSpecialChar	contained "\\\(NUL\|SOH\|STX\|ETX\|EOT\|ENQ\|ACK\|BEL\|BS\|HT\|LF\|VT\|FF\|CR\|SO\|SI\|DLE\|DC1\|DC2\|DC3\|DC4\|NAK\|SYN\|ETB\|CAN\|EM\|SUB\|ESC\|FS\|GS\|RS\|US\|SP\|DEL\)"
syn match   uSpecialCharError	contained "\\&\|'''\+"
syn region  uString		start=+"+  skip=+\\\\\|\\"+  end=+"+  contains=uSpecialChar
syn match   uCharacter		"[^a-zA-Z0-9_']'\([^\\]\|\\[^']\+\|\\'\)'"lc=1 contains=uSpecialChar,uSpecialCharError
syn match   uCharacter		"^'\([^\\]\|\\[^']\+\|\\'\)'" contains=uSpecialChar,uSpecialCharError
syn match   uNumber		"\<[0-9]\+\>\|\<0[xX][0-9a-fA-F]\+\>\|\<0[oO][0-7]\+\>\|\<0[bB][01]\+\>"
syn match   uFloat		"\<[0-9]\+\.[0-9]\+\([eE][-+]\=[0-9]\+\)\=\>"

" Keyword definitions. These must be patterns instead of keywords
" because otherwise they would match as keywords at the start of a
" "literate" comment (see lu.vim).
syn match uModule		"\<namespace\>"
syn match uImport		"\<use\>"
syn match uTypedef		"\<\(unique\|structural\|∀\|forall\)\>"
syn match uStatement		"\<\(ability\|do\|type\|where\|match\|cases\|;\|let\|with\|handle\)\>"
syn match uConditional		"\<\(if\|else\|then\)\>"

syn match uBoolean "\<\(true\|false\)\>"

syn match uType "\<\C[A-Z][0-9A-Za-z_'!]*\>"
syn match uName "\<\C[a-z_][0-9A-Za-z_'!]*\>"

" Comments
syn match   uLineComment      "---*\([^-!#$%&\*\+./<=>\?@\\^|~].*\)\?$"
syn region  uBlockComment     start="{-"  end="-}" contains=uBlockComment
syn region  uPragma	      start="{-#" end="#-}"
syn region  uBelowFold	      start="^---" skip="." end="." contains=uBelowFold

" Docs
syn region  uDocBlock         matchgroup=unisonDoc start="{{" end="}}" contains=uDocTypecheck,uDocQuasiquote,uDocDirective,uDocCode,uDocCodeInline,uDocCodeRaw,uDocMono,@markdownLikeDocs
syn region  uDocQuasiquote    contained matchgroup=unisonDocQuote start="{{" end= "}}" contains=TOP
syn region  uDocCode          contained matchgroup=unisonDocCode start="^\s*```\s*$" end="^\s*```\s*$" contains=TOP
syn region  uDocTypecheck     contained matchgroup=unisonDocCode start="^\s*@typecheck\s*```\s*$" end="^\s*```\s*$" contains=TOP
syn region  uDocCodeRaw       contained matchgroup=unisonDocCode start="^\s*```\s*raw\s*$" end="^\s*```\s*$" contains=NoSyntax
syn region  uDocCodeInline    contained matchgroup=unisonDocCode start="`\@<!``" end="`\@<!``" contains=TOP
syn match   uDocMono          "''[^']*''"
syn region  uDocDirective     contained matchgroup=unisonDocDirective start="\(@\([a-zA-Z0-9_']*\)\)\?{{\@!" end="}" contains=TOP

syn match uDebug "\<\(todo\|bug\|Debug.trace\)\>"

" things like
"    > my_func 1 3
"    test> Function.tap.tests.t1 = check let
"      use Nat == +
"      ( 99, 100 ) === (withInitialValue 0 do
"          :      :      :
syn match uWatch "^[A-Za-z]*>"

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_u_syntax_inits")
  if version < 508
    let did_u_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

   HiLink       uWatch                           Debug
   HiLink       uDocMono                         Delimiter
   HiLink       unisonDocDirective               Import
   HiLink       unisonDocQuote                   Delimiter
   HiLink       unisonDocCode                    Delimiter
   HiLink       unisonDoc                        String
   HiLink       uBelowFold                       Comment
   HiLink       uBlockComment                    Comment
   HiLink       uBoolean                         Boolean
   HiLink       uCharacter                       Character
   HiLink       uComment                         Comment
   HiLink       uConditional                     Conditional
   HiLink       uConditional                     Conditional
   HiLink       uDebug                           Debug
   HiLink       uDelimiter                       Delimiter
   HiLink       uDocBlock                        String
   HiLink       uDocDirective                    Import
   HiLink       uDocIncluded                     Import
   HiLink       uFloat                           Float
   HiLink       uImport                          Include
   HiLink       uLineComment                     Comment
   HiLink       uLink                            Type
   HiLink       uName                            Identifier
   HiLink       uNumber                          Number
   HiLink       uOperator                        Operator
   HiLink       uPragma                          SpecialComment
   HiLink       uSpecialChar                     SpecialChar
   HiLink       uSpecialCharError                Error
   HiLink       uStatement                       Statement
   HiLink       uString                          String
   HiLink       uType                            Type
   HiLink       uTypedef                         Typedef

   delcommand   HiLink
endif


let b:current_syntax = "unison"

" Options for vi: ts=8 sw=2 sts=2 nowrap noexpandtab ft=vim
