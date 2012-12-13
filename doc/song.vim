" Vim syntax file
" Language:	Song
" Maintainer:	Simon Heath <snh1@cec.wustl.edu>
" Last Change:	2004 November 4

" Basic things only...
" Based on the modula 3 syntax file
" It's rather ad-hoc, really.

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" Song is case-sensitive
syntax case match

" Song keywords
syn keyword songKey and or not xor << >> inc dec == = /== /=
syn keyword songKey <= >= < > <- <-+ <-- <-* <-/ + - * / % ;
syn keyword songKey if then elif else for do from to by while try with
syn keyword songKey finally goto version def sizeof asm continue break
syn keyword songKey ret new alloc free static

" Song declerations
syn keyword songDecl end ;; module import use import use global
syn keyword songDecl const protocol class type struct : :: ( )

" Song types
syn keyword songType int8 uint8 byte int16 uint16 short ushort
syn keyword songType int32 uint32 int uint int64 uint64 long ulong
syn keyword songType float32 float float64 double char str bool NONE func

" Comments
syn region songComment start="{-" end="-}"
syn region songComment start="//" end="\n"

" Strings and characters
syn region songString start=+"+ end=+"+
syn region songString start=+'+ end=+'+

syn keyword songTodo contained TODO FIXME XXX

" songCommentGroup allows adding matches for special things in comments
syn cluster songCommentGroup contains=songTodo

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_modula3_syntax_inits")
  if version < 508
    let did_modula3_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  " The default methods for highlighting.  Can be overridden later
  HiLink songKey	Statement
  HiLink songDecl	Operator
  HiLink songType	Type
  HiLink songComment	Comment
  HiLink songString	String
  HiLink songTodo	Todo

  delcommand HiLink
endif

let b:current_syntax = "song"
