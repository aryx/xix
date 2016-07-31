(setq p "/home/pad/github/plan9-ml")

(setq 
 pad-ocaml-project-path p
 pad-ocaml-project-subdirs 
 (split-string 
  "commons
   formats/objects formats/executables
   assembler linker mk
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
   (case 1
     (1 (concat "-debugger -f " (concat p "/mk/mkfile")))
     )
   )))

