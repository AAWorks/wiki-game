open! Core
open! Wikipedia_namespace
open! File_fetcher
module Title = String

module Article = struct
  module T = struct
    type t =
      { url : String.t
      ; title : Title.t
      }
    [@@deriving compare, sexp, hash]
  end

  include T
  include Comparable.Make (T)

  let of_url s =
    let url_list = String.split s ~on:'/' in
    { url = s; title = List.last_exn url_list }
  ;;

  let equal t1 t2 = String.equal t1.url t2.url
  let url t = t.url
  let title t = t.title
end

module Connection = struct
  module T = struct
    type t = Article.t * Article.t [@@deriving compare, sexp]
  end

  (* This funky syntax is necessary to implement sets of [Connection.t]s.
     This is needed to defined our [Network.t] type later. Using this
     [Comparable.Make] functor also gives us immutable maps, which might come
     in handy later. *)
  include Comparable.Make (T)

  let equal (art11, art12) (art21, art22) =
    Article.equal art11 art21 && Article.equal art12 art22
  ;;
end

(* [get_linked_articles] should return a list of wikipedia article lengths
   contained in the input.

   Note that [get_linked_articles] should ONLY return things that look like
   wikipedia articles. In particular, we should discard links that are: -
   Wikipedia pages under special namespaces that are not articles (see
   https://en.wikipedia.org/wiki/Wikipedia:Namespaces) - other Wikipedia
   internal URLs that are not articles - resources that are external to
   Wikipedia - page headers

   One nice think about Wikipedia is that stringent content moderation
   results in uniformity in article format. We can expect that all Wikipedia
   article links parsed from a Wikipedia page will have the form
   "/wiki/<TITLE>". *)
let get_linked_articles contents : string list =
  let open Soup in
  parse contents
  $$ "a[href]"
  |> to_list
  |> List.map ~f:(fun a -> R.attribute "href" a)
  |> List.filter ~f:(fun link ->
       Option.is_none (namespace link)
       && String.is_prefix link ~prefix:"/wiki/")
  |> List.sort ~compare:(fun a b -> String.compare a b)
  |> List.remove_consecutive_duplicates ~equal:(fun a b -> String.equal a b)
;;

let print_links_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Print all of the valid wiki page links on a page"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_linked_articles contents) ~f:print_endline]
;;

let get_linked_articles_as_records contents : Article.t list =
  let open Soup in
  parse contents
  $$ "a[href]"
  |> to_list
  |> List.map ~f:(fun a -> R.attribute "href" a)
  |> List.filter ~f:(fun link ->
       Option.is_none (namespace link)
       && String.is_prefix link ~prefix:"/wiki/")
  |> List.sort ~compare:(fun a b -> String.compare a b)
  |> List.remove_consecutive_duplicates ~equal:(fun a b -> String.equal a b)
  |> List.map ~f:(fun url -> Article.of_url url)
;;

let rec bfs ~depth ~q ~explored ~(howfetch : How_to_fetch.t)
  : (Article.t * Article.t) list
  =
  match q with
  | [] -> explored
  | head :: tail ->
    if depth = 0
    then explored
    else (
      let new_explored =
        List.fold
          (fetch_exn howfetch ~resource:(Article.url head)
           |> get_linked_articles_as_records)
          ~init:[]
          ~f:(fun acc article ->
            if not
                 (List.mem explored (head, article) ~equal:Connection.equal
                  || List.mem
                       explored
                       (article, head)
                       ~equal:Connection.equal)
            then acc @ [ head, article ]
            else acc)
      in
      let new_q = tail @ List.map new_explored ~f:(fun (_, art) -> art) in
      bfs
        ~depth:(depth - 1 + List.length new_explored)
        ~q:new_q
        ~explored:(explored @ new_explored)
        ~howfetch)
;;

let get_network ~max_depth ~(origin : Article.t) ~how_to_fetch =
  let q = [ origin ] in
  let explored = [ origin, origin ] in
  match bfs ~depth:max_depth ~q ~explored ~howfetch:how_to_fetch with
  | _head :: tail -> Connection.Set.of_list tail
  | [] -> Connection.Set.empty
;;

module G = Graph.Imperative.Graph.Concrete (Title)

module Dot = Graph.Graphviz.Dot (struct
  include G

  (* These functions can be changed to tweak the appearance of the generated
     graph. Check out the ocamlgraph graphviz API
     (https://github.com/backtracking/ocamlgraph/blob/master/src/graphviz.mli)
     for examples of what values can be set here. *)
  let edge_attributes _ = [ `Dir `None ]
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
  let vertex_name v = v
  let default_vertex_attributes _ = []
  let graph_attributes _ = []
end)

(* [visualize] should explore all linked articles up to a distance of
   [max_depth] away from the given [origin] article, and output the result as
   a DOT file. It should use the [how_to_fetch] argument along with
   [File_fetcher] to fetch the articles so that the implementation can be
   tested locally on the small dataset in the ../resources/wiki directory. *)

let visualize
  ?(max_depth = 3)
  ~(origin : Article.t)
  ~output_file
  ~how_to_fetch
  ()
  : unit
  =
  let network = get_network ~max_depth ~origin ~how_to_fetch in
  let graph = G.create () in
  Set.iter network ~f:(fun (article1, article2) ->
    (* [G.add_edge] auomatically adds the endpoints as vertices in the graph
       if they don't already exist. *)
    let title1, title2 = Article.title article1, Article.title article2 in
    let group =
      ( title1
        |> String.substr_replace_all ~pattern:"(" ~with_:""
        |> String.substr_replace_all ~pattern:")" ~with_:""
      , title2
        |> String.substr_replace_all ~pattern:"(" ~with_:""
        |> String.substr_replace_all ~pattern:")" ~with_:"" )
    in
    G.add_edge_e graph group);
  Dot.output_graph
    (Out_channel.create (File_path.to_string output_file))
    graph
;;

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        let origin = Article.of_url origin in
        visualize ~max_depth ~origin ~output_file ~how_to_fetch ();
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

(* [find_path] should attempt to find a path between the origin article and
   the destination article via linked articles.

   [find_path] should use the [how_to_fetch] argument along with
   [File_fetcher] to fetch the articles so that the implementation can be
   tested locally on the small dataset in the ../resources/wiki directory.

   [max_depth] is useful to limit the time the program spends exploring the
   graph. *)
let find_path ?(max_depth = 3) ~origin ~destination ~how_to_fetch () =
  ignore (max_depth : int);
  ignore (origin : string);
  ignore (destination : string);
  ignore (how_to_fetch : File_fetcher.How_to_fetch.t);
  failwith "TODO"
;;

let find_path_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "Play wiki game by finding a link between the origin and destination \
       pages"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and destination =
        flag "destination" (required string) ~doc:" the destination page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      in
      fun () ->
        match find_path ~max_depth ~origin ~destination ~how_to_fetch () with
        | None -> print_endline "No path found!"
        | Some trace -> List.iter trace ~f:print_endline]
;;

let command =
  Command.group
    ~summary:"wikipedia game commands"
    [ "print-links", print_links_command
    ; "visualize", visualize_command
    ; "find-path", find_path_command
    ]
;;
