#!/usr/bin/env python
# songpp
# The Song pre-processor.  This replaces indents with _INDENT and _DEDENT
# tokens, turns from's into for's, and until's into while's.
#
# I find it rather amusing that I'm writing the indent-parser in Python.
# I'll have to re-write it in Ocaml someday...  But now I know what I mean
# by prototyping.  ^_^
#
# Simon Heath
# 19/02/2004

import sys

global indentlevel
global indentstack
indentlevel = 0
indentstack = [0]

TABSIZE = 8

def usage():
   print "Usage: songpp filename.sng"
   sys.exit( 1 )

def indexoffirstchar( n ):
   idx = 0
   for x in n:
      if x == ' ':
         idx += 1
      elif x == '\t':
         print "ERROR: Tabs are illegal!!!"
	 sys.exit( 1 )
      else:
         return idx
   return idx

def mergestrings( strlst ):
   fin = ""
   for x in strlst:
      fin += x
   return fin

# Returns 1 if the indent level has increased, -1 if it's decreased,
# and 0 if it's stayed the same.
def indentLevelChange( n ):
   k = indexoffirstchar( n )
   if k == indentlevel:
      return 0
   elif k > indentlevel:
      return 1
   else:
      return -1

# This one works by building a list of the indent values, then running
# through the list.  Much easier than parsing directly.
def parseIndents( strlst ):
   indentslst = []
   lineno = 0
   indentno = 0
   indentstack = [0]
   for x in strlst:
      # Skip blank lines and comments.
      # XXX: This does NOT handle block comments yet.
      ic = indexoffirstchar( x )
      # Ignore blank lines, starting comments, or lines with only spaces.
      if x == '\n' or x[ic] == '$' or ic + 1 == len( x ):
         indentslst.append( indentslst[-1] )
	 lineno += 1
      else:
         indentslst.append( ic )
         lineno += 1

   lineno = 0
   for x in indentslst:
      if x > indentstack[-1]:
         indentstack.append( x )
         indentno += 1
         strlst[lineno] = " _INDENT " + strlst[lineno]
      elif x < indentstack[-1]:
         while x < indentstack[-1]:
            indentstack.pop()
            indentno -= 1
            strlst[lineno] = " _DEDENT " + strlst[lineno]
         if x != indentstack[-1]:
            print "Indent mismatch on line %d\n" % (lineno + 1)
            sys.exit( 1 )
      else:
         pass
      lineno +=1

   if indentno != 0:
      #print "Something went wiggy: indentno = %d\n" % (lineno + 1)
      #print indentstack
      #print indentslst
      #sys.exit( 1 )
      while len( indentstack ) > 1:
         indentstack.pop()
         strlst.append( " _DEDENT " )
         indentno -= 1

   # Final sanity check
   if indentno != 0:
      print "Something went wiggy: indentno = %d\n" % (lineno + 1)
      print indentstack
      print indentslst
      sys.exit( 1 )
      
   return mergestrings( strlst )



# def parseIndents( strlst ):
#    outlst = []
#    lineno = 0
#    global indentlevel
#    global indentstack
#    for x in strlst:
#       print indentlevel
#       print indentstack
      
#       if len( indentstack ) == 0:
#          indentstack = [0]
#       lineno += 1
#       k = indentLevelChange( x )
            
#       if k > 0:
#          outlst.append( " _INDENT " )
# 	 outlst.append( x )
# 	 indentstack.append( indentlevel )
#          #print "Indented from " + str( indentlevel ) + " to",
# 	 indentlevel = indexoffirstchar( x )
#          #print indentlevel
#       elif k < 0:
#          outlst.append( " _DEDENT " )
#          indentstack.pop()
#          indentlevel = indexoffirstchar( x )
#          while indentstack[-1] != indentlevel:
#             outlst.append( " _DEDENT " )
#             if len( indentstack ) > 1:
#                indentstack.pop()
#             else:
#                print "Error on line %d: Mismatched indents." % (lineno)
#                sys.exit( 1 )

#          outlst.append( x )

         
#          #print "Dedented from " + str( indentlevel ) + " to",
# #	 indentlevel = indexoffirstchar( x )
#          #print indentlevel
# #	 if not indentstack.__contains__( indentlevel ):
# #	    print "Error on line %d: Mismatched indents." % (lineno)
# #            sys.exit( 1 )
# #	 indentstack.pop()
#       else:
#          #print "Stayed at indent level %d" % (indentlevel)
#          outlst.append( x )
   
#    while( len( indentstack ) > 0 ):
#       indentstack.pop ()
#       outlst.append( " _DEDENT " )

#    return mergestrings( outlst )


# def parseInd( str ):
#    current = 0
#    col = 0
#    blankline = 0
#    while 1:
#       c = str[current]
#       current += 1
#       if( c == ' ' ):
#          col += 1
#       else if( c == '\t' ):
#          col += TABSIZE
#       else:
#          break
#    # Skim past comments and newlines
#    if( c == '$' ):
#       while( c != '\n' ):
#          current += 1
#          c = str[current]
#    if( c == '{' and str[current + 1] == '-' ):
#       while( 1 ):
#          if( c == '-' and str[current + 1] == '}' ):
#             break
#          else:
#             current += 1
#             c = str[current]
#    if( c == '\n' ):
#       blankline = 1
#    
#    if( col > indentlevel ):
#       indentstack.push( indentlevel )
#       indentlevel = col
      


def main():
   if len( sys.argv ) < 2:
      usage()
   f = open( sys.argv[1], 'r' )
   data = f.readlines()
   f = open( (sys.argv[1] + '.pp'), 'w' )
   f.write( parseIndents( data ) )
   f.close()


if __name__ == "__main__":
  main()
