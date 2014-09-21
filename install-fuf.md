# Installing FUF and SURGE

This page explains how to install the FUF and SURGE text generation system (syntactic realization),
with special attention to a Windows platform.

To make things portable and easy, all the instructions are given so that the whole installation of Lisp, Emacs, Slime, Quicklisp
and FUF/SURGE are all put in a single folder.  We will call this folder c:\home\ for illustration.

We assume you have git installed and the commandline command "git clone" works.
If you need to install it - check [git install](http://git-scm.com/book/en/Getting-Started-Installing-Git).

The tree of main folders eventually present will be:
```
C:\home\
                .emacs
                .sbclrc
		sbcl.cmd
		emacs.cmd
		quicklisp.lisp
        \emacs
        \sbcl
        \slime
        \quicklisp
	          \local-projects\fuf
```


## Pre-requisites

### SBCL Common Lisp Compiler

Download the [SBCL Common Lisp compiler](http://www.sbcl.org/platform-table.html).
  
* The download is about 10 MB - choose your platform (Windows, Linux, MacOS).
  * Under Windows, just run the setup file (msi) -- direct it to the HOME
    folder, e.g.: `C:\Home\sbcl-1.2.1\`
  * Create a batch file to execute SBCL simply -- in a file sbcl.cmd
    under c:\Home:
```
@set HOME=c:\home
@"%HOME%\sbcl-1.2.1\sbcl.exe" --core "%HOME%\sbcl-1.2.1\sbcl.core" %*
```

### Emacs

* If you are on Windows, download the
  [Emacs](http://ftp.gnu.org/gnu/emacs/windows/) text editor
  (which is the best environment to program in Lisp).
  Version 24.3 as of September 2014 is about 47MB.
  Direct download is available
  [here](http://ftp.gnu.org/gnu/emacs/windows/emacs-24.3-bin-i386.zip).
  * Unzip this folder under c:\home\emacs-24.3
* If you are on MacOS or Linux, Emacs is most probably already installed.
* Create a batch file c:\home\emacs.cmd with the following content:
```
@set HOME=c:\home
@%HOME%\emacs-24.3\bin\runemacs %*
```
  This causes the ~/ variable under Emacs to be bound to c:\home.

### Install EliEmacs Mode

Eliemacs from Eli Barzilay is a very good configuration package for Emacs.

* In a command shell, do:
```
  cd c:\home
  git clone https://github.com/elibarzilay/eliemacs.git
  copy eliemacs\eliemacs.elc
```

* Edit the file ~/.emacs with the following line:
```
;; Adjust this based on the size of your screen
(setq window-configurations
      (lambda (frame)
        `(fn: "Consolas-20" w: 86 h: 22 x: 0 y: -28)))
(setq visible-bell 1)
;; No welcome screen
(setq fake-initial-key 27)
(load "~/eliemacs.elc")
```

### Slime Emacs Mode

* Install the SLIME Emacs mode to interact between Emacs and Common Lisp:
  * If you use git, do:
```
    git clone https://github.com/slime/slime.git
```
  * Else download [Slime](http://common-lisp.net/project/slime/)

  * Uncompress the contents of the slime.tgz file into "C:\home\slime".

  * Edit "~/.emacs" and add these lines:
```
(add-to-list 'load-path "~/slime/")  ;; your SLIME directory
(setq inferior-lisp-program "sbcl")  ;; your Lisp system
(require 'slime)
(slime-setup '(slime-fancy))
```


### Quicklisp

Quicklisp is a package manager for Common Lisp.
FUF/SURGE is packaged as a Quicklisp compatible package.

* Download [quicklisp.lisp](http://beta.quicklisp.org/quicklisp.lisp)
  into c:\home
* Install quicklisp by typing the following 4 commands:
```
cd c:\home
sbcl --load quicklisp.lisp

-- Inside the CommonLisp shell type then:
(quicklisp-quickstart:install)
(ql:add-to-init-file)
(ql:quickload "quicklisp-slime-helper")
```

* Add to your .emacs file the following line:
```
(load (expand-file-name "~/quicklisp/slime-helper.el"))
```

## Installing FUF and SURGE

* Clone the FUF git into c:\home\quicklisp\local-projects:
```
cd c:\home\quicklisp\local-projects
git clone https://github.com/melhadad/fuf.git
```

## Loading and Testing FUF

* Start emacs
* Start lisp inside emacs: invoke the command "M-x slime"
  (that is, type "escape x slime enter")
* Load FUF: `(ql:quickload :fuf54)`
* Load FUF tests: `(ql:quickload :fuf54-test)`
* Go into the FUF package: `(in-package :fug5)`

## Loading and Testing SURGE

* Load SURGE: `(ql:quickload :surge22)`
* Load FUF tests: `(ql:quickload :surge22-test)`
* Go into the FUF package: `(in-package :fug5)`

## Start Up with SURGE pre-loaded

* Edit your ~/.sbclrc file with this content:
```
;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :surge22)
(in-package :fug5)
```

## Running Specific FUF or SURGE Tests

In general, when you use SURGE, you want to prepare input specifications
for sentences. The following process can be used to prepare and test your
input specifications:

* Create input structures (best is to start from an example similar
  to one found in "surge/inputs")
* The macro "def-test" is used to associate a SURGE input specification
  (an FD) with the expected sentence. For example:
```
(def-test t1
  "This car is expensive."
  ((cat clause)
   (proc ((type ascriptive)
	  (mode attributive)))        ;; default
   (partic ((carrier ((lex "car")
                      (cat common)
                      (distance near)))
	    (attribute ((lex "expensive")
	                (cat ap)))))))
```

Keep your test pairs in a development file and add the line
(in-package "FUG5") at the beginning of your file.

* To test the interpretation of your input specification, use the
  `fug5:test` function:
```
(fug5:test :item 't1)
```

* If everything works fine, you will see the following input:
```
CL-USER> (fug5:test :item 't1)

====================
FUG5::T1 --> "This car is expensive."

[Used 88 backtracking points - 19 wrong branches - 8 undos]

[Used 89 backtracking points - 19 wrong branches - 8 undos]
OK
====================

1 test run - 1 correct.
```

* If the name of the test you invoke does not exist
  (no correspond def-test was entered), you will see:
```
(fug5:test :item 'd1)

====================
D1 --> NIL

[Used 1 backtracking points - 0 wrong branches - 0 undos]

[Used 1 backtracking points - 0 wrong branches - 0 undos]
Expected NIL
Instead  "<fail>"
====================

1 test run - 0 correct.
The following tests are incorrect: (D1)
```

* If the input specification is not good, you will see a message similar
  to this:
```
FUG5::T146 --> "The Denver Nuggets surprisingly beat the Celtics."

[Used 90 backtracking points - 21 wrong branches - 10 undos]

[Used 110 backtracking points - 37 wrong branches - 14 undos]
Expected "The Denver Nuggets surprisingly beat the Celtics."
Instead  "Denver Nuggets surprisingly beat Celtics."
====================

```

* Sometimes, the grammar will get exhausted trying many alternatives without
  terminating the search process. The test macro limits the search space to up
  to 2000 backtracking points.  When the input you specified is too complex
  or causes the grammar to search over this limit, you will see the
  following message:

```
====================
FUG5::A4 --> "John takes a book from Mary."
;;; Unification stopped after 2001 backtracking points.
Expected "John takes a book from Mary."
Instead  "Mary <unknown cat FUG5::VERB-GROUP: take><unknown cat FUG5::NP: book><unknown cat FUG5::PP: nil><unknown cat FUG5::PP: nil>."
====================
```

* At other times, the input you enter is not compatible with the
  expectations of SURGE. In this case, you will see:
```
====================
FUG5::T97 --> "The person to whom John will give a blue book."
Expected "The person to whom John will give a blue book."
Instead  "<fail>"
====================
```

## Debugging your SURGE Input Specifications

In any case when the input specification fails to produce the sentence you expect,
you want to find out the reason for the failure. To get information on the
unification process, use the function:

```
(fug5:trace-on)
```
And to remove the trace info, use:
```
(fug5:trace-off)
```
This produces lots of information.  You can control the verbosity of the trace
by using:

```
(fug5:trace-level 30)  ;; minimum level of trace detail
...
(fug5:trace-level 0)   ;; maximum level of trace detail
```


Last modified 19 Sep 2014 - Michael Elhadad
