open! Core

module Position = struct
  module T = struct
    type t =
      { row : int
      ; col : int
      }
    [@@deriving compare, sexp]
  end

  include Comparable.Make (T)
  include T

  let equal t1 t2 = Int.equal (compare t1 t2) 0
  let right t = { row = t.row; col = t.col + 1 }
  let down t = { row = t.row + 1; col = t.col }
  let up t = { row = t.row - 1; col = t.col }
  let left t = { row = t.row; col = t.col - 1 }
  let offsets = [ right; down; up; left ]
  let to_string t = Int.to_string t.row ^ " " ^ Int.to_string t.col
end

module Cell = struct
  module T = struct
    type t =
      { value : String.t
      ; pos : Position.t
      }
    [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)

  let equal t1 t2 = Position.equal t1.pos t2.pos
  let pos t = t.pos
  let is_end t = String.equal t.value "E"
  let is_wall t = String.equal t.value "#"
  let is_start t = String.equal t.value "S"
end

let get_contents input_file =
  In_channel.read_lines (File_path.to_string input_file)
;;

let parse_file contents =
  contents
  |> List.mapi ~f:(fun r s ->
       String.to_list s
       |> List.mapi ~f:(fun c value ->
            { Cell.value = String.of_char value
            ; pos = { Position.row = r; col = c }
            }))
  |> List.concat
;;

let get_board board_list =
  board_list
  |> List.map ~f:(fun cell -> Cell.pos cell, cell)
  |> Position.Map.of_alist_exn
;;

let rec dfs ~(map : Cell.t Position.Map.t) ~parent ~cell ~path : Cell.t list =
  let new_path = path @ [ cell ] in
  let children =
    List.map Position.offsets ~f:(fun offset ->
      let new_pos = offset (Cell.pos cell) in
      match Map.find map new_pos with
      | Some child ->
        if not (Cell.equal parent child) then child else assert false
      | None -> assert false)
  in
  if Cell.is_end cell
  then new_path
  else if not (Cell.is_wall cell)
  then
    List.fold children ~init:new_path ~f:(fun acc child_cell ->
      dfs ~map ~parent:cell ~cell:child_cell ~path:acc)
  else []
;;

let solve input_file : unit =
  let contents = get_contents input_file in
  let cell_list = parse_file contents in
  let cell = List.find_exn cell_list ~f:(fun cell -> Cell.is_start cell) in
  let map = get_board cell_list in
  let solution = dfs ~map ~parent:cell ~cell ~path:[] in
  print_s [%message "" (solution : Cell.t list)];
  List.iter solution ~f:(fun cell ->
    print_endline (Cell.pos cell |> Position.to_string))
;;

let solve_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file containing a maze and find a solution"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:"FILE a file containing a maze"
      in
      fun () -> solve input_file]
;;

let command =
  Command.group ~summary:"maze commands" [ "solve", solve_command ]
;;
