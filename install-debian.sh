#!/bin/sh

set -e

ELDIR=/usr/share/emacs/site-lisp/emacspeak/lisp
ELCDIR=/usr/share/emacs21/site-lisp/emacspeak
SRCFILES=$(echo speechd*.el)
LOADFILES='"speechd.el"'
PATCHED=0

echo 'Installing ...'
rm -f *.elc
rm -f $ELDIR/speechd*.el
rm -f $ELCDIR/speechd*.elc
if ! diff -q dtk-interp.el $ELDIR/dtk-interp.el; then
  PATCHED=1
fi
for F in *.el; do
  cp $F $ELDIR/
  chown root.root $ELDIR/$F
  chmod a+r $ELDIR/$F
done

if [ $(fgrep -c 'tts_set_punctuations' $ELDIR/dtk-speak.el) -gt 0 ] ||
   [ $(fgrep -c "(dtk-quote" $ELDIR/dtk-tcl.el) -gt 0 ] ||
   [ $(fgrep -c "skip-syntax-forward inherit-chunk-separator-syntax" $ELDIR/dtk-tcl.el) -gt 0 ]; then
  PATCHED=1
  cat emacspeak.patch | (cd $ELDIR; patch)
fi

emacs -q -no-site-file -batch -eval "(let ((load-files '($LOADFILES))) (while load-files (load (expand-file-name (car load-files))) (setq load-files (cdr load-files))))" -f batch-byte-compile $SRCFILES
if [ $PATCHED = 1 ] && [ "$1" != --quick ] || [ "$1" = --recompile ]; then
  /usr/lib/emacsen-common/emacs-package-install emacspeak
fi
for F in *.elc; do
  mv $F $ELCDIR/
  chown root.root $ELCDIR/$F
  chmod a+r $ELCDIR/$F
done
echo '... done.'
