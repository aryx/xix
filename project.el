(setq p "/home/pad/github/plan9-ml")

(setq 
 pad-ocaml-project-path p
 pad-ocaml-project-subdirs 
 (split-string 
  "commons
   formats/objects formats/executables
   assembler linker mk shell compiler
   "
  ))


(setq
 pad-ocaml-project-prog     "linker/5l"
 pad-ocaml-project-args 
 (join-string 
  (list 
   ""
   (case 3
     (1 (concat p "/linker/test.5"))
     (2 (concat p "/linker/helloworld.5"))
     (3 (concat (concat p "/linker/hello.5") " " (concat p "/linker/world.5")))
     )
   )))


(setq
 pad-ocaml-project-prog     "mk/mk"
 pad-ocaml-project-args 
 (join-string 
  (list 
   ""
   (case 2
     (1 (concat "-debugger -f " (concat p "/mk/mkfile")))
     (2 (concat "-debugger -f " (concat p "/mk/tests/mk-empty-var")))
     (3 "-debugger -f /home/pad/plan9/windows/rio/mkfile")
     )
   )))

(setq
 pad-ocaml-project-prog     "shell/rc"
 pad-ocaml-project-args 
 (join-string 
  (list 
   ""
   (case 3
     (1 (concat "-debugger -test_parser " (concat p "/shell/tests/hello.rc")))
     (2 (concat "-debugger -dump_opcodes"))
     (3 (concat "-m " (concat p "/shell/rcmain-unix") " -i -r -dump_ast"))
     )
   )))

(setq
 pad-ocaml-project-prog     "compiler/5c"
 pad-ocaml-project-args 
 (join-string 
  (list 
   ""
   (case 2
     (1 (concat "-debugger -test_parser " (concat p "/compiler/tests/hello.rc")))
     (2 (concat "-debugger " (concat p "/compiler/tests/helloworld.c")))
     )
   )))


