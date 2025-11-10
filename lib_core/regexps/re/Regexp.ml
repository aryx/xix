
(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Regexp AST and build helpers.
 *
 * history
 * - was defined in re.ml with many other types, and kept abstract in re.mli
 *   but better to separate concerns given re.ml was really huge. In any
 *   case it can still remain abstract in re.mli
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type t =
  | Set of Cset.t
  | Sequence of t list
  | Alternative of t list
  (* ints ?? *)
  | Repeat of t * int * int option

  | Beg_of_line | End_of_line
  | Beg_of_word | End_of_word | Not_bound
  | Beg_of_str | End_of_str
  (* ?? *)
  | Last_end_of_line 
  (* ?? *)
  | Start | Stop

  (* ?? *)
  | Sem of Automata.sem * t
  | Sem_greedy of Automata.rep_kind * t

  (* ?? *)
  | Group of t | No_group of t | Nest of t

  | Case of t | No_case of t

  | Intersection of t list
  | Complement of t list
  | Difference of t * t

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let rec is_charset (r : t) : bool =
  match r with
    Set _ ->
      true
  | Alternative l | Intersection l | Complement l ->
      List.for_all is_charset l
  | Difference (r, r') ->
      is_charset r && is_charset r'
  | Sem (_, r) | Sem_greedy (_, r)
  | No_group r | Case r | No_case r ->
      is_charset r
  | Sequence _ | Repeat _ | Beg_of_line | End_of_line
  | Beg_of_word | End_of_word | Beg_of_str | End_of_str
  | Not_bound | Last_end_of_line | Start | Stop | Group _ | Nest _ ->
      false

(*****************************************************************************)
(* Character sets *)
(*****************************************************************************)

let cany = [0, 255]

let cseq c c' = Cset.seq (Char.code c) (Char.code c')
let csingle c = Cset.single (Char.code c)

let cupper =
  Cset.union (cseq 'A' 'Z') (Cset.union (cseq '\192' '\214') (cseq '\216' '\222'))
let clower = Cset.offset 32 cupper

(*****************************************************************************)
(* Builders *)
(*****************************************************************************)

(* old: those small builder wrappers allowed to keep the regexp type abstract *)

let str s =
  let l = ref [] in
  for i = String.length s - 1 downto 0 do
    l := Set (csingle s.[i]) :: !l
  done;
  Sequence !l
let char c = Set (csingle c)

let alt l =
  match l with
  | [r] -> r
  | _   -> Alternative l
let seq l =
  match l with
  | [r] -> r
  | _   -> Sequence l
let repn r i j =
  if i < 0 then invalid_arg "Re.repn";
  begin match j with Some j when j < i -> invalid_arg "Re.repn" | _ -> () end;
  Repeat (r, i, j)
let rep r = repn r 0 None
let rep1 r = repn r 1 None
let opt r = repn r 0 (Some 1)
let bol = Beg_of_line
let eol = End_of_line
let bow = Beg_of_word
let eow = End_of_word
let word r = seq [bow; r; eow]
let not_boundary = Not_bound
let bos = Beg_of_str
let eos = End_of_str
let leol = Last_end_of_line
let start = Start
let stop = Stop
let longest r = Sem (`Longest, r)
let shortest r = Sem (`Shortest, r)
let first r = Sem (`First, r)
let greedy r = Sem_greedy (`Greedy, r)
let non_greedy r = Sem_greedy (`Non_greedy, r)
let group r = Group r
let no_group r = No_group r
let nest r = Nest r

let set str =
  let s = ref [] in
  for i = 0 to String.length str - 1 do
    s := Cset.union (csingle str.[i]) !s
  done;
  Set !s

let rg c c' = Set (cseq c c')

let inter l =
  let r = Intersection l in
  if is_charset r then r else
  invalid_arg "Re.inter"

let compl l =
  let r = Complement l in
  if is_charset r then r else
  invalid_arg "Re.compl"

let diff r r' =
  let r'' = Difference (r, r') in
  if is_charset r'' then r'' else
  invalid_arg "Re.diff"

let case r = Case r
let no_case r = No_case r

(*****************************************************************************)
(* Shortcuts *)
(*****************************************************************************)

let empty = alt []
let epsilon = seq []

let any = Set cany
let notnl = Set (Cset.diff cany (csingle '\n'))

let lower = alt [rg 'a' 'z'; char '\181'; rg '\223' '\246'; rg '\248' '\255']
let upper = alt [rg 'A' 'Z'; rg '\192' '\214'; rg '\216' '\222']

let alpha = alt [lower; upper; char '\170'; char '\186']
let digit = rg '0' '9'
let alnum = alt [alpha; digit]

let ascii = rg '\000' '\127'
let blank = set "\t "
let cntrl = alt [rg '\000' '\031'; rg '\127' '\159']
let graph = alt [rg '\033' '\126'; rg '\160' '\255']
let print = alt [rg '\032' '\126'; rg '\160' '\255']
let punct =
  alt [rg '\033' '\047'; rg '\058' '\064'; rg '\091' '\096';
       rg '\123' '\126'; rg '\160' '\169'; rg '\171' '\180';
       rg '\182' '\185'; rg '\187' '\191'; char '\215'; char '\247']
let space = alt [char ' '; rg '\009' '\013']
let xdigit = alt [digit; rg 'a' 'f'; rg 'A' 'Z']
