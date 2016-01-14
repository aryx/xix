(setq p "/home/pad/github/plan9-ml")

(setq 
 pad-ocaml-project-path p
 pad-ocaml-project-subdirs 
 (split-string 
  "commons
   formats/objects formats/executables
   assembler linker
   "
  ))


(setq
 pad-ocaml-project-prog     "linker/5l"
 pad-ocaml-project-args 
 (join-string 
  (list 
   ""
   (case 1
     (1 (concat p "/linker/test.5"))
     )
   )))

