(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

open Js_core

let error s =
  GlobalVariables.alert ("Error: "^s);
  failwith s

let document = Window.document GlobalVariables.window

module Helper = struct
  let removeAll element =
    while
      match Node.last_child element with
      | Some child -> Node.remove_child element child; true
      | None -> false
    do () done

  let element_of_id id =
    match Document.get_element_by_id document id with
    | Some element -> element
    | None -> error (Printf.sprintf "Element of id '%s' not found" id)

  let input_of_id id =
    match Html.retype (element_of_id id) with
    | `Input input -> input
    | _ ->
      error (Printf.sprintf "Element of id '%s' should be an input element." id)

  let hide element =
    Element.set_attribute element "style" "display: none"

  let show element =
    Element.remove_attribute element "style"

  let tabs_logic l = if l = [] then () else
      let tabs, contents = List.split l in
      let tabs = Array.of_list tabs in
      let contents = Array.of_list contents in
      let size = Array.length contents in
      let activate k =
        Element.set_class_name tabs.(k) "active";
        let div, act = contents.(k) in
        Lazy.force act;
        show div;
        for i = 0 to size - 1 do
          if i <> k then begin
            Element.set_class_name tabs.(i) "";
            hide @@ fst contents.(i);
          end
        done;
      in
      activate 0;
      for k = 0 to size - 1 do
        Element.set_onclick tabs.(k) (fun () -> activate k);
      done

  (** [sortable_table cols row inside] create a table

    @param cols is a list of detailing the columns of table
      An element is a triple [(header, cmp, proj)] where:
      + [header] is an HTML element
      + [cmp] is a compare function see {compare}
      + [proj] is a projection computing for each row (see below) the
        corresponding element of the column.

    @param row is a list of element

    @param inside is an HTMl element where the table should be inserted.
      All previous content is removed. *)
  let rec sortable_table cols rows inside =
    let open Document in
    (* we clean the inside *)
    removeAll inside;
    (* We create a table:
     * <table>
     *    <thead>
     *      <tr></tr>
     *    </thead>
     *    <tbody></tbody>
     * </table>  *)
    let table = create_html_table document in
    Node.append_child inside table;
    let thead = create_html_thead document in
    Node.append_child table thead;
    let tbody = create_html_tbody document in
    Node.append_child table tbody;
    let first_row = create_html_tr document in
    Node.append_child thead first_row;

    (* Creating the header *)
    List.iter (fun (header, cmp, _) ->
        let th = create_html_th document in
        (* Computing the closure for onclick evenement.
         * We recompute the table after sorting the rows.
         * If we click again on the header <th>, the order is reversed,
         * hence the new [fun x y -> cmp y x] *)
        Element.set_onclick th (fun () ->
            let rows = List.sort cmp rows in
            let cols =
              List.map (fun (header, cmp, proj) ->
                  (header, (fun x y -> cmp y x), proj)) cols
            in
            sortable_table cols rows inside
          );
        Node.append_child th header;
        Node.append_child first_row th
      ) cols;
    List.iter (fun row ->
        let tr = create_html_tr document in
        Node.append_child tbody tr;
        List.iter (fun (_, _, proj) ->
            let td = create_html_td document in
            let cell = proj row in
            Node.append_child tr td;
            Node.append_child td cell
          ) cols
      ) rows

  (** [format_number s] returns a copy of string [s] with a space inserted
      every three elements.
      So [format_number "12345678"] returns ["12 345 678"] *)
  let format_number s =
    let n = String.length s in
    let b = Buffer.create (n + n / 3) in
    let m = (n-1) mod 3 in
    for k = 0 to n-1 do
      Buffer.add_char b s.[k];
      if k mod 3 = m && k < n-1 then
        Buffer.add_char b ' '
    done;
    Buffer.contents b

  (** [create ~text ~class_name ~style name] returns an HTML element

    {[
      <name style="~style" class="~class_name">
        ~text
      </name>
    ]}
  *)
  let create ?text ?class_name ?style name =
    let element = Document.create_element document name in
    (match text with
     | Some text -> Node.set_text_content element text
     | _ -> ());
    (match style with
     | Some style -> Element.set_attribute element "style" style
     | _ -> ());
    (match class_name with
     | Some class_name -> Element.set_class_name element class_name
     | _ -> ());
    element

  (** [record_table l] returns a table element
      {[
        <table class="vertical">
          <tr><th>n_0</th><td>v_0</td></tr>
          ...
          <tr><th>n_i</th><td>v_i</td></tr>
        </table>
      ]}

      Where [l] is a list of tuple of name/values [n_i, v_i] *)
  let record_table l =
    let table = create ~class_name:"vertical" "table" in
    List.iter (fun (name, value) ->
        let tr = create "tr" in
        let th = create ~text:name "th" in
        let td = create ~text:value "td" in
        Node.append_child table tr;
        Node.append_child tr th;
        Node.append_child tr td)
      l;
    table

  let get_fixed_table () =
    let main = element_of_id "main" in
    match Document.get_element_by_id document "div_table" with
    | Some div -> Node.append_child main div; div
    | None ->
        let div = create "div" ~class_name:"fixed" in
        let _ = Element.set_attribute div "id" "div_table" in
        let _ = Node.append_child main div in
        div

end

module Graph = struct

  type id = int [@@js]

  type kind = Landmark.Graph.kind =
    | Normal [@js "normal"]
    | Root [@js "root"]
    | Counter  [@js "counter"]
    | Sampler [@js "sampler"]
  [@@js] [@@js.enum]

  let rec floatarray_of_js (objs: Ojs.t): floatarray =
    let n = Ojs.int_of_js (Ojs.get_prop_ascii objs "length") in
    Float.Array.init n (fun i -> Ojs.float_of_js (Ojs.array_get objs i))
  and floatarray_to_js (arr: floatarray): Ojs.t =
    let n = Float.Array.length arr in
    let a = Ojs.array_make n in
    for i = 0 to n - 1 do
      Ojs.array_set a i (Ojs.float_to_js (Float.Array.get arr i))
    done;
    a

  type node = Landmark.Graph.node = {
    id: int;
    kind : kind;
    landmark_id : string;
    name: string;
    location: string;
    calls: int;
    time: float;
    children: id list;
    sys_time: float;
    allocated_bytes: float;
    distrib: floatarray;
  } [@@js] [@@js.verbatim_names]

  type graph = Landmark.Graph.graph = {nodes: node array; label: string; root: id} [@@js]

  let graph_of_string s =
    try graph_of_js (JSON.parse s) with Ojs_exn.Error _ -> error "Invalid input format."

  let string_of_graph s = JSON.stringify (graph_to_js s)

  let has_sys_time {nodes; _} =
    Array.exists (fun {sys_time; _} -> sys_time <> 0.0) nodes

  let has_allocated_bytes {nodes; _} =
    Array.exists (fun {allocated_bytes; _} -> allocated_bytes <> 0.0) nodes

  module IntMap = Map.Make(Int)
  module IntSet = Set.Make(Int)

  let set_fpr fmt set =
    IntSet.iter (Format.fprintf fmt "%i ") set

  module Tuple
      (M1: Set.OrderedType)
      (M2: Set.OrderedType)
      : Set.OrderedType with type t = M1.t * M2.t =
  struct
    type t = M1.t * M2.t
    let compare (x1, x2) (y1, y2) =
      let res1 = M1.compare x1 y1 in
      if res1 = 0 then M2.compare x2 y2 else res1
  end

  module SString: (Set.OrderedType with type t = string * string) =
    Tuple(String)(String)

  module StringSet = Set.Make(String)
  module SStringSet = Set.Make(SString)
  module StringMap = Map.Make(String)
  module SStringMap = Map.Make(SString)

  (** [get_loc_metric graph get id] returns
      [get(id) - Σ_{child in children(id)} get(child)]*)
  let get_local_metric (graph: graph) (get: node -> float) (id: id): float =
    let node = graph.nodes.(id) in
    let time = get node in
    List.fold_left
      (fun acc id' -> acc -. get graph.nodes.(id'))
      time
      node.children

  let name_2_ids (graph: graph) name =
    Array.fold_left
      (fun acc node -> if node.name = name then IntSet.add node.id acc else acc)
      IntSet.empty
      graph.nodes

  (** [group_by_name graph ids] group index accoring to their [name] field in
      [graph].*)
  let group_by_name graph (ids: IntSet.t) =
    IntSet.fold
      (fun id acc -> let name = graph.nodes.(id).name in
        match StringMap.find_opt name acc with
        | None -> StringMap.add name (IntSet.singleton id) acc
        | Some s -> StringMap.add name (IntSet.add id s) acc)
      ids
      StringMap.empty

  let merge_nodes (graph: graph) ids id children: node =
    let elem = IntSet.choose ids in
    let name = graph.nodes.(elem).name in
    let location = graph.nodes.(elem).location in
    let kind = graph.nodes.(elem).kind in
    let landmark_id = graph.nodes.(elem).landmark_id in
    let distrib = graph.nodes.(elem).distrib in
    let calls = IntSet.fold
      (fun id -> (+) graph.nodes.(id).calls)
      ids 0 in
    let time = IntSet.fold
      (fun id -> (+.) graph.nodes.(id).time)
      ids 0. in
    let sys_time = IntSet.fold
      (fun id -> (+.) graph.nodes.(id).sys_time)
      ids 0. in
    let allocated_bytes = IntSet.fold
      (fun id -> (+.) graph.nodes.(id).allocated_bytes)
      ids 0. in
    {children; id; kind; landmark_id; name; location; calls; time; sys_time; allocated_bytes; distrib}

  (** [merge_graph graph name] returned a merged verion of [graph] where all
      nodes with name [name] form the root of the new graph. *)
  let merge_graph (graph: graph): string -> graph =
    fun name ->
      if name = "ROOT" then graph else
      let get_uid: unit -> int =
        let i = ref 0 in
        fun () -> let r = !i in incr i; r in
      let rec treat_node ids acc =
        let id = get_uid () in
        let children = IntSet.fold
          (fun id acc ->
            let ch = graph.nodes.(id).children in
            List.fold_left (fun acc id -> IntSet.add id acc) acc ch)
          ids IntSet.empty in
        let children = group_by_name graph children in
        let (acc, children) = StringMap.fold
            (fun _ ids (acc, children) ->
              let id, acc = treat_node ids acc in
              acc, id::children)
            children (acc, []) in
        let node = merge_nodes graph ids id children in
        let acc = IntMap.add id node acc in
        id, acc in
      let ids = name_2_ids graph name in
      if IntSet.is_empty ids then error ("Unknonw function '"^name^"'");
      let root, map = treat_node ids IntMap.empty in
      let nodes = Array.init (IntMap.cardinal map) (fun i -> IntMap.find i map) in
      {nodes; root; label=""}

  (** [get_all_name graph] returns a set containing all [name] field values of
      nodes in [graph]. *)
  let get_all_name graph: StringSet.t =
    Array.fold_left
      (fun acc n -> StringSet.add n.name acc)
      StringSet.empty
      graph.nodes

  (** [get_loc_aggregated graph aggreg_graph get] returns a *)
  let get_loc_aggregated (graph0: graph) (graph: graph) (get: node -> float) : float Array.t =
    let map = Array.fold_left
      (fun acc node ->
        let key = node.name, node.location in
        let t_acc = SStringMap.find_opt key acc |> Option.value ~default:0.0 in
        let t = get_local_metric graph0 get node.id in
        SStringMap.add key (t +. t_acc) acc)
      SStringMap.empty
      graph0.nodes in
    Array.init
      (Array.length graph.nodes)
      (fun id -> let node = graph.nodes.(id) in
        SStringMap.find (node.name, node.location) map)

  (** [agregated_table graph element] print the table corresponding to the
      aggregated value in [graph] in [element]. *)
  let aggregated_table graph0 =
    let graph = Landmark.Graph.aggregate_landmarks graph0 in
    let loc_cycles = get_loc_aggregated graph0 graph (fun {time; _} -> time) in
    let all_nodes =
      List.sort
        (fun {time = time1; _} {time = time2; _} -> compare time2 time1)
        (Landmark.Graph.nodes graph)
    in
    let text x = Document.create_text_node document x in
    let profile_with_sys_time =
      if has_sys_time graph then
        let loc_sys_time = get_loc_aggregated graph0 graph (fun {sys_time; _} -> sys_time) in
        [
          (text "Tot Time", (fun x y -> compare x.sys_time y.sys_time),
            fun {sys_time; _} -> text (Printf.sprintf "%.0f" sys_time |> Helper.format_number));
          (text "Fun Time", (fun x y -> compare loc_sys_time.(x.id) loc_sys_time.(y.id)),
            fun {id; _} -> text (Printf.sprintf "%.0f" loc_sys_time.(id) |> Helper.format_number));
        ]
      else []
    in
    let profile_with_allocated_bytes =
      if has_allocated_bytes graph then
        let loc_allocated_bytes = get_loc_aggregated graph0 graph (fun {allocated_bytes; _} -> allocated_bytes) in
        [
          (text "Tot Alloc Bytes", (fun x y -> compare x.allocated_bytes y.allocated_bytes),
            fun {allocated_bytes; _} -> text (Printf.sprintf "%.0f" allocated_bytes |> Helper.format_number));
          (text "Fun Alloc Bytes", (fun x y -> compare loc_allocated_bytes.(x.id) loc_allocated_bytes.(y.id)),
            fun {id; _} -> text (Printf.sprintf "%.0f" loc_allocated_bytes.(id) |> Helper.format_number));
        ]
      else []
    in
    let cols = [
      (text "Name", (fun x y -> compare x.name y.name),
        fun {name; _} -> text name);
      (text "Location", (fun x y -> compare x.location y.location),
        fun {location; _} -> text location);
      (text "Calls", (fun x y -> compare x.calls y.calls),
        fun {calls; _} -> text (string_of_int calls |> Helper.format_number));
      (text "Tot Cycles", (fun x y -> compare x.time y.time),
        fun {time; _} -> text (Printf.sprintf "%.0f" time |> Helper.format_number));
      (text "Fun Cycles", (fun x y -> compare loc_cycles.(x.id) loc_cycles.(y.id)),
        fun {id; _} -> text (Printf.sprintf "%.0f" loc_cycles.(id) |> Helper.format_number));
    ] @ profile_with_sys_time @ profile_with_allocated_bytes
    in
    Helper.sortable_table cols all_nodes

  (** [get_calls grap srcs dsts] returns the number of *)
  let get_calls graph srcs dsts =
    let treat_src src acc =
      let children = graph.nodes.(src).children in
        List.fold_left
          (fun acc child -> if IntSet.mem child dsts
            then acc + graph.nodes.(child).calls
            else acc)
          acc
          children in
    IntSet.fold treat_src srcs 0

  (** [inverse_graph graph name] returned an inversed graph where all *)
  let inverse_graph graph : string -> graph =
    let parents: IntSet.t IntMap.t Lazy.t =
      let treat_node acc node =
        List.fold_left
          (fun (acc: IntSet.t IntMap.t) (child: int) ->
            match IntMap.find_opt child acc with
            | None -> IntMap.add child (IntSet.singleton node.id) acc
            | Some parents -> IntMap.add child (IntSet.add node.id parents) acc)
          acc node.children in
      Lazy.from_fun (fun () -> Array.fold_left treat_node IntMap.empty graph.nodes) in
    fun name ->
      let get_uid =
        let i = ref 0 in
        fun () -> let res = !i in incr i; res in
      let parents = Lazy.force parents in
      let get_parent id =
        IntMap.find_opt id parents
        |> Option.value ~default:IntSet.empty in
      let rec treat_ids name t ids calls acc: node IntMap.t * id  =
        let location = graph.nodes.(IntSet.min_elt ids).location in
        let id = get_uid () in
        let time = t in
        let sys_time =
          IntSet.fold (fun id -> (+.) graph.nodes.(id).sys_time) ids 0.0 in
        let allocated_bytes =
          IntSet.fold (fun id -> (+.) graph.nodes.(id).allocated_bytes) ids 0.0 in
        let distrib = Float.Array.create 0 in
        let kind = Normal in
        let landmark_id = name in
        let parents = IntSet.fold
          (fun id -> id |> get_parent |> IntSet.union)
          ids IntSet.empty in
        let parents = group_by_name graph parents in
        let loc_calls =
          IntSet.fold (fun id -> (+) graph.nodes.(id).calls) ids 0
          |> float_of_int in
        let acc, children = StringMap.fold
          (fun name srcs (acc, children) ->
            let calls = get_calls graph srcs ids in
            let t = t *. (float_of_int calls) /. loc_calls in
            let acc, child = treat_ids name t srcs calls acc in
            acc, child::children)
          parents (acc, []) in
        let node: node = {
          name;
          id;
          time;
          sys_time;
          allocated_bytes;
          distrib;
          location;
          calls;
          children;
          kind;
          landmark_id} in
        let acc = IntMap.add id node acc in
        acc, id in
      let srcs =
        Array.fold_left
          (fun acc node -> if node.name = name then IntSet.add node.id acc else acc)
          IntSet.empty graph.nodes in
      let calls = IntSet.fold  (fun id -> (+) graph.nodes.(id).calls) srcs 0  in
      let map, root = treat_ids name 1000.0 srcs calls IntMap.empty in
      let nodes = Array.init (IntMap.cardinal map) (fun i -> IntMap.find i map) in
      {nodes; root; label = name }

end

module BarChart = struct
  open Graph

  let xlmns="http://www.w3.org/2000/svg"

  let max_width = ref 1500 (** for a HD screen this is fine *)
  let update svg =
      let v =
      GlobalVariables.window
      |> Window.inner_width in
    let _ = print_endline (Format.asprintf "new_val: %i" v) in
    let _ = max_width := v - 700 in
    let _ = Element.set_attribute_n_s svg "" "width" (Format.asprintf "%i" !max_width) in
    ()
  let max_depth = 30

  let sep = 1.1
  let row_height = 30.

  let depth_size depth =
    let ratio = 0.95 in
    let rec aux acc last depth =
      if depth = 0 then
        acc
      else
        aux (acc +. last) (last *. ratio) (depth-1) in
    aux row_height (row_height *. ratio) depth

    let total_height = depth_size (max_depth +1)

    let color graph proj node =
      let intensity node = proj node /. (proj graph.nodes.(0))  in
      let rgb = Printf.sprintf "rgb(%d,%d,%d)" in
      let open Graph in
      match node.kind with
      | Normal -> begin
          let i = intensity node in
          (* Implements the bijection:
           *  [0, 1] --> [0,1]
           *               ______________
           *    i   |--> \/ 1 - (i - 1)^2
           *
           *  to "amplify" the intensity (it is a quarter of a circle).
           *)
          let i = i -. 1.0 in
          let i = i *. i in
          let i = sqrt (1.0 -. i) |> ( *. ) 255.0 |> int_of_float in
          rgb (i) 0 0
        end
      | Root -> rgb 125 125 125
      | Counter -> rgb 0 125 200
      | Sampler -> rgb 0 200 125

  let hover (graph) (node: Graph.node) svg () =
    let div = Helper.get_fixed_table () in
    let parent = Node.parent_node svg in
    let _ = Node.append_child parent div in
    let _ = Helper.removeAll div in
    let table =
      let loc_time = Graph.get_local_metric graph (fun {time; _} -> time) node.id in
      Helper.record_table
        ( [ "Name", node.name;
            "Tot Cycles", Printf.sprintf "%.0f" node.time |> Helper.format_number;
            "Fun Cycles", Printf.sprintf "%.0f" loc_time |> Helper.format_number;
            "Calls", Printf.sprintf "%d" node.calls |> Helper.format_number ]
          @ (if node.location <> "" then ["Location", node.location] else [])
          @ (if node.sys_time <> 0.0 then
              let loc_sys_time = Graph.get_local_metric graph (fun {sys_time; _} -> sys_time) node.id in
              [
                ("Tot Time", Printf.sprintf "%.0f" node.sys_time |> Helper.format_number);
                ("Fun Time", Printf.sprintf "%.0f" loc_sys_time |> Helper.format_number);
              ]
            else [])
          @ (if node.allocated_bytes <> 0.0 then
              let loc_allocated_bytes = Graph.get_local_metric graph (fun {allocated_bytes; _} -> allocated_bytes) node.id in
              [
                ("Tot Alloc bytes", Printf.sprintf "%.0f" node.allocated_bytes |> Helper.format_number);
                ("Fun Alloc bytes", Printf.sprintf "%.0f" loc_allocated_bytes |> Helper.format_number);
              ]
            else [])) in
    Node.append_child div table

  (** [print_rect x y dx dy elem] dispalays asvg rectange in [elem].
      @param x
      @param y are the coordinates of the left-upper corner of the rectangle.
      @param dx
      @param dy are the size of the rectangle.  *)
  let print_rect (x: float) (y:float) (dx: float) (dy: float) f_click f_hover elem color: unit =
    let rect = Document.create_element_n_s document xlmns "rect" in
    let _ = Element.set_attribute_n_s rect "" "x" (Format.asprintf "%.2f" x) in
    let _ = Element.set_attribute_n_s rect "" "y" (Format.asprintf "%.2f" y) in
    let _ = Element.set_attribute_n_s rect "" "width" (Format.asprintf "%.2f" (dx-.sep)) in
    let _ = Element.set_attribute_n_s rect "" "height" (Format.asprintf "%.2f" dy) in
    let _ = Element.set_attribute_n_s rect "" "fill" color in
    let _ = Element.set_attribute_n_s rect "" "class" "bar" in
    let _ = Element.set_onclick rect f_click in
    let _ = Element.set_onmouseover rect f_hover in
    Node.append_child elem rect

  let cmp (g: Graph.graph)  proj i1 i2 =
    let v1 = proj g.nodes.(i1) in
    let v2 = proj g.nodes.(i2) in
    compare v2 v1

  let rec print_fun (graph: Graph.graph) left depth root proj fun_id element =
    let node = graph.nodes.(fun_id) in
    let local = proj node in
    let total = proj graph.nodes.(root) in
    let ratio = local /. total in
    let dx = ratio *. (float_of_int !max_width) in
    if dx < 3.0 then left +. dx else
    let y = depth_size depth in
    let dy = (depth_size (depth +1)) -. y -. sep in
    let y = total_height -. (depth_size depth) in
    let on_click () =
      let _ = print_endline "click" in
      let _ = Helper.removeAll element in
      let _ = update element in
      print_fun graph 0.0 0 fun_id proj fun_id element |> ignore in
    let hover = hover graph node element in
    let color = color graph proj node in
    let _ = print_rect left y dx dy on_click hover element color in
    let r = if depth > max_depth then 0.0 else
      node.children
      |> List.sort (cmp graph proj)
      |> List.fold_left
      (fun left id -> print_fun graph left (depth+1) root proj id element)
      left  in
    max r (left +. dx)


  let print_svg (g: Graph.graph) proj elem =
    let _ = print_endline "Starting print_svg" in
    let div_table = Helper.get_fixed_table () in
    let _ = Node.append_child elem div_table in
    let svg = Document.create_element_n_s document xlmns "svg" in
    let _ = update svg in
    let _ = Element.set_attribute_n_s svg "" "width" (Format.asprintf "%i" !max_width) in
    let total_height = depth_size (max_depth +1)
      |> int_of_float in
    let _ = Element.set_attribute_n_s svg "" "height" (Format.asprintf "%i" total_height) in
    let _ = print_fun g 0. 0 0 proj 0 svg |> ignore in
    let button = Helper.create ~text:"goto root" ~class_name:"inter_button" "button" in
    let goto_root () =
      let _ = Helper.removeAll svg in
      let _ = update svg in
      print_fun g 0.0 0 0 proj 0 svg |> ignore in
    let _ = Element.set_onclick button goto_root in
    let _ = Node.append_child elem div_table in
    let _ = Node.append_child elem svg in
    let _ = Node.append_child elem @@ Helper.create "br" in
    let _ = Node.append_child elem button in
    let _ = print_endline "Chart created" in
    ()

end

module PieChart = struct
  open Graph

  let xlmns="http://www.w3.org/2000/svg"

  let max_size = ref 1000 (** for a HD screen this is fine *)
  let update svg =
    let _width = Window.inner_width GlobalVariables.window in
    let _height = Window.inner_width GlobalVariables.window in
    (* let _ = max_size := min (width - 700) (height - 200) in *)
    let _ = print_endline (Format.asprintf "new_val: %i" !max_size) in
    let _ = Element.set_attribute_n_s svg "" "width" (Format.asprintf "%i" !max_size) in
    let _ = Element.set_attribute_n_s svg "" "height" (Format.asprintf "%i" !max_size) in
    ()


  let max_depth = 20

  let lim = 2. *. Float.pi /. 400.
  let pie_height = 35.

  let radius depth =
    let ratio = 0.95 in
    let rec aux acc r depth =
      if depth = 0 then acc
      else aux (acc +. r) (r *. ratio) (depth -1) in
    aux 0. pie_height depth

  let color graph proj node =
    let intensity node = proj node /. (proj graph.nodes.(0))  in
    let rgb = Printf.sprintf "rgb(%d,%d,%d)" in
    let open Graph in
    match node.kind with
    | Normal -> begin
        let i = intensity node in
        (* Implements the bijection:
         *  [0, 1] --> [0,1]
         *               ______________
         *    i   |--> \/ 1 - (i - 1)^2
         *
         *  to "amplify" the intensity (it is a quarter of a circle).
         *)
        let i = i -. 1.0 in
        let i = i *. i in
        let i = sqrt (1.0 -. i) |> ( *. ) 255.0 |> int_of_float in
        rgb (i) 0 0
      end
    | Root -> rgb 125 125 125
    | Counter -> rgb 0 125 200
    | Sampler -> rgb 0 200 125

  let hover (graph) (node: Graph.node) svg () =
    let div = Helper.get_fixed_table () in
    let parent = Node.parent_node svg in
    let _ = Node.append_child parent div in
    let _ = Helper.removeAll div in
    let table =
      let loc_time = Graph.get_local_metric graph (fun {time; _} -> time) node.id in
      Helper.record_table
        ( [ "Name", node.name;
            "Tot Cycles", Printf.sprintf "%.0f" node.time |> Helper.format_number;
            "Fun Cycles", Printf.sprintf "%.0f" loc_time |> Helper.format_number;
            "Calls", Printf.sprintf "%d" node.calls |> Helper.format_number ]
          @ (if node.location <> "" then ["Location", node.location] else [])
          @ (if node.sys_time <> 0.0 then
              let loc_sys_time = Graph.get_local_metric graph (fun {sys_time; _} -> sys_time) node.id in
              [
                ("Tot Time", Printf.sprintf "%.0f" node.sys_time |> Helper.format_number);
                ("Fun Time", Printf.sprintf "%.0f" loc_sys_time |> Helper.format_number);
              ]
            else [])
          @ (if node.allocated_bytes <> 0.0 then
              let loc_allocated_bytes = Graph.get_local_metric graph (fun {allocated_bytes; _} -> allocated_bytes) node.id in
              [
                ("Tot Alloc bytes", Printf.sprintf "%.0f" node.allocated_bytes |> Helper.format_number);
                ("Fun Alloc bytes", Printf.sprintf "%.0f" loc_allocated_bytes |> Helper.format_number);
              ]
            else [])) in
    Node.append_child div table

  type point = {x: float; y: float}
  type svg_path_elem =
    | Move of point
    | Line of point
    | Horizontal of float
    | Vertical of float
    | Arc of float * float * float * bool * bool * point
  type svg_path = svg_path_elem list

  let pp_point fmt p = Format.fprintf fmt "%4.4f %4.4f"p.x p.y
  let pp_elem fmt = function
    | Move p -> Format.fprintf fmt "M %a" pp_point p
    | Line p -> Format.fprintf fmt "L %a" pp_point p
    | Arc (rx, ry, rot_x, l_flag, s_flag, p) ->
      Format.fprintf fmt "A %.4f %.4f %.4f %i %i %a" rx ry rot_x
        (if l_flag then 1 else 0)
        (if s_flag then 1 else 0)
        pp_point p
    | Horizontal y -> Format.fprintf fmt "H %.2f" y
    | Vertical x -> Format.fprintf fmt "V %.2f" x
  let pp_path fmt (path: svg_path) =
    let _ = Format.fprintf fmt "@[<v 0>" in
    let _ = List.iter (Format.fprintf fmt "%a @ " pp_elem) path in
    let _ = Format.fprintf fmt "@]" in
    ()

  let trans {x; y} =
    let off = (!max_size |> float_of_int) /. 2.0 in
    let x = x +. off in
    let y = off -. y in
    { x; y }

  let print_circle f_click f_hover elem color: unit =
    let r = pie_height -. 2.0
      |> Format.asprintf "%.2f" in
    let circle = Document.create_element_n_s document xlmns "circle" in
    let c = (float_of_int !max_size) /. 2.0 |> Format.asprintf "%.2f" in
    let _ = Element.set_attribute_n_s circle "" "cx" c in
    let _ = Element.set_attribute_n_s circle "" "cy" c in
    let _ = Element.set_attribute_n_s circle "" "r" r in
    let _ = Element.set_attribute_n_s circle "" "fill" color in
    let _ = Element.set_attribute_n_s circle "" "class" "bar" in
    let _ = Element.set_onclick circle f_click in
    let _ = Element.set_onmouseover circle f_hover in
    Node.append_child elem circle

  let rotate t =
    Float.pi /. 2. -. t

  (** [print_pie depth theta phi f_click c_hover elem color]
      displays a pie part starting at angle [theta] and spanning in angle [phi].
      [depth] is  *)

  let print_pie depth theta phi f_click f_hover elem color: unit =
    let r1 = radius depth in
    let r2 = (radius (depth + 1)) -. 2.0 in
    let lim = lim *. 25. /. r1 in
    let ta = theta +. lim |> rotate in
    let tb = theta +. phi -. lim |> rotate in
    let a1 = {x = r1 *. cos(ta); y = r1 *. sin(ta)} |> trans in
    let a2 = {x = r2 *. cos(ta); y = r2 *. sin(ta)} |> trans in
    let b1 = {x = r1 *. cos(tb); y = r1 *. sin(tb)} |> trans in
    let b2 = {x = r2 *. cos(tb); y = r2 *. sin(tb)} |> trans in
    let p =
      [
        Move b1;
        Arc (r1, r1, 0., phi > Float.pi, false, a1);
        Line a2;
        Arc (r2, r2, 0., phi > Float.pi, true, b2);
        Line b1;
      ] in
    let d = Format.asprintf "%a" pp_path p in
    let path = Document.create_element_n_s document xlmns "path" in
    let _ = Element.set_attribute_n_s path "" "d" d in
    let _ = Element.set_attribute_n_s path "" "fill" color in
    let _ = Element.set_attribute_n_s path "" "class" "bar" in
    let _ = Element.set_onclick path f_click in
    let _ = Element.set_onmouseover path f_hover in
    Node.append_child elem path

  let cmp (g: Graph.graph)  proj i1 i2 =
    let v1 = proj g.nodes.(i1) in
    let v2 = proj g.nodes.(i2) in
    compare v2 v1

  let rec print_fun (graph: Graph.graph) theta depth root proj fun_id element =
    let node = graph.nodes.(fun_id) in
    let local = proj node in
    let total = proj graph.nodes.(root) in
    let ratio = local /. total in
    let phi = ratio *. (2. *. Float.pi) in
    if phi < lim then theta +. phi else
    let on_click () =
      let _ = print_endline "click" in
      let _ = Helper.removeAll element in
      let _ = update element in
      print_fun graph 0.0 0 fun_id proj fun_id element |> ignore in
    let hover = hover graph node element in
    let color = color graph proj node in
    let _ =
      if depth = 0 then
        print_circle on_click hover element color
      else
      print_pie depth theta phi on_click hover element color  in
    let r = if depth > max_depth then 0.0 else
      node.children
      |> List.sort (cmp graph proj)
      |> List.fold_left
      (fun left id -> print_fun graph left (depth+1) root proj id element)
      theta  in
    max r (theta +. phi)


  let print_svg (g: Graph.graph) proj elem =
    let _ = print_endline "Starting print_svg" in
    let div_table = Helper.get_fixed_table () in
    let _ = Node.append_child elem div_table in
    let svg = Document.create_element_n_s document xlmns "svg" in
    let _ = update svg in
    let _ = Element.set_attribute_n_s svg "" "width" (Format.asprintf "%i" !max_size) in
    let _ = Element.set_attribute_n_s svg "" "height" (Format.asprintf "%i" !max_size) in
    let _ = print_fun g 0. 0 0 proj 0 svg |> ignore in
    let button = Helper.create ~text:"goto root" ~class_name:"inter_button" "button" in
    let goto_root () =
      let _ = Helper.removeAll svg in
      let _ = update svg in
      print_fun g 0.0 0 0 proj 0 svg |> ignore in
    let _ = Element.set_onclick button goto_root in
    let _ = Node.append_child elem div_table in
    let _ = Node.append_child elem svg in
    let _ = Node.append_child elem @@ Helper.create "br" in
    let _ = Node.append_child elem button in
    let _ = print_endline "PieChart created" in
    ()

end

module TreeView = struct
  open Helper

  let open_button = "[+]"
  let close_button = "[-]"

  (** [generate render expand children inside parent x] adds a list item [<li>]
      in [inside]

      @param render is a closure to render the [x] node.

      @param expand

      @param children is a continuation computing the sons of each element of
        the graph

      @param inside is an HTML element where the node should be inserted

      @param x is the node that should be generated

      @return the [<li>] node.

      {[
        <li>
          <div class="collapsible">
            <span>  /* Generated only if children x <> [] */
            </span>
          </div>
        </li>
      ]}

  *)
  let rec generate render expand children inside parent x =
    let li = create "li" in
    let div = create "div" in
    let content = render parent div x in
    Node.append_child div content;
    Node.append_child li div;
    let sons = children x in
    if sons <> [] then begin
      let span = create "span" ~text:open_button ~class_name:"collapseButton" in
      Element.set_class_name div "collapsible";
      Node.append_child div span;
      (* This list stores the list of the child of [x] when it is expanded. *)
      let expanded_state = ref [] in
      let ul = create "ul" in
      Node.append_child li ul;
      let do_expand () =
        Node.set_text_content span close_button;
        expanded_state := List.map (generate render expand children ul (Some x)) sons
      in
      if expand x then
        do_expand ();
      let onclick _ =
        if !expanded_state = [] then begin
          do_expand ()
        end else begin
          Node.set_text_content span open_button;
          List.iter (Node.remove_child ul) !expanded_state;
          expanded_state := []
        end
      in
      Element.set_onclick div onclick
    end;
    Node.append_child inside li;
    li

  let append render expand children inside root =
    let ul = create "ul" in
    Node.append_child inside ul;
    generate render expand children ul None root |> ignore

  let callgraph inside ({Graph.nodes; _} as graph) proj =
    let root =
      if Array.length nodes = 0 then
        error "callgraph: no root"
      else nodes.(0)
    in
    let intensity = Landmark.Graph.intensity ~proj graph in
    let color node =
      let rgb = Printf.sprintf "rgb(%d,%d,%d)" in
      let open Graph in
      match node.kind with
      | Normal -> begin
          let i = intensity node in
          (* Implements the bijection:
           *  [0, 1] --> [0,1]
           *               ______________
           *    i   |--> \/ 1 - (i - 1)^2
           *
           *  to "amplify" the intensity (it is a quarter of a circle).
           *)
          let i = i -. 1.0 in
          let i = i *. i in
          let i = sqrt (1.0 -. i) in
          rgb (int_of_float (255.0 *. i)) 0 0
        end
      | Root -> rgb 125 125 125
      | Counter -> rgb 0 125 200
      | Sampler -> rgb 0 200 125
    in
    let previous_info = ref None in
    let render (parent : Graph.node option) _container ({Graph.name; time = node_time; kind; calls; distrib; allocated_bytes; sys_time; location; id; _} as node) =
      let loc_time = Graph.get_local_metric graph (fun {time; _} -> time) id in
      let node_value = proj node in
      let span = create "span" ~class_name:"content" ~text:name ~style:(Printf.sprintf "color:%s" (color node)) in
      Element.set_onmouseover span (fun () ->
          (match !previous_info with Some dispose -> dispose () | None -> ());
          let table =
            Helper.record_table
              ( [ "Name", name;
                  "Tot Cycles", Printf.sprintf "%.0f" node_time |> Helper.format_number;
                  "Fun Cycles", Printf.sprintf "%.0f" loc_time |> Helper.format_number;
                  "Calls", Printf.sprintf "%d" calls |> Helper.format_number ]
                @ (if location <> "" then ["Location", location] else [])
                @ (if sys_time <> 0.0 then
                    let loc_sys_time = Graph.get_local_metric graph (fun {sys_time; _} -> sys_time) id in
                    [
                      ("Tot Time", Printf.sprintf "%.0f" sys_time |> Helper.format_number);
                      ("Fun Time", Printf.sprintf "%.0f" loc_sys_time |> Helper.format_number);
                    ]
                  else [])
                @ (if allocated_bytes <> 0.0 then
                    let loc_allocated_bytes = Graph.get_local_metric graph (fun {allocated_bytes; _} -> allocated_bytes) id in
                    [
                      ("Tot Alloc bytes", Printf.sprintf "%.0f" allocated_bytes |> Helper.format_number);
                      ("Fun Alloc bytes", Printf.sprintf "%.0f" loc_allocated_bytes |> Helper.format_number);
                    ]
                  else []))
          in
          let div = Helper.get_fixed_table () in
          Helper.removeAll div;
          Node.append_child div table);
      ( match parent, kind with
        | Some parent, Graph.Normal ->
          let parent_value = proj parent in
          let text =
            Printf.sprintf " (%2.2f%%) " (100.0 *. node_value /. parent_value)
          in
          let span_value = create ~text "span" in
          Node.append_child span span_value
        | _, Graph.Counter ->
          let text =
            Printf.sprintf " (%d calls) " calls
          in
          let span_time = create ~text "span" in
          Node.append_child span span_time
        | _, Graph.Sampler ->
          let text =
            Printf.sprintf " (%d values) " (Float.Array.length distrib)
          in
          let span_time = create ~text "span" in
          Node.append_child span span_time
        | _ -> ());
      span
    in
    let reference = Landmark.Graph.shallow_ancestor graph in
    let depth = Landmark.Graph.depth graph in
    let expand node =
      let reference = reference node in
      depth node <= 1 || proj node > 0.1 *. proj reference
    in
    let children {Graph.children; _} =
      let sons = ref [] in
      List.iter
        (fun id -> sons := nodes.(id) :: !sons)
        children;
      List.sort (fun node node' ->
          compare (proj node') (proj node)) !sons
    in
    append render expand children inside root

end

module CallerView = struct

  (** [make_tab tabs main (present, title, fill)] return a tab where the *)
  let make_tab tabs main (present, title, fill) =
    if not present then [] else begin
      let div = Helper.create "div" in
      let title = Helper.create ~text:title "li" in
      Node.append_child tabs title;
      Node.append_child main div;
      let action = Lazy.from_fun (fun _ -> fill div) in
      [title, (div, action)]
    end

  let selector graph tab create proj element =
    let inv = create graph in
    let names = Graph.get_all_name graph in
    let input_name = "fun_input"^tab in
    let list_name = "all_funs"^tab in
    let input = Helper.create "input" in
    let _ = Element.set_attribute input "list" list_name in
    let _ = Element.set_attribute input "id" input_name in
    let inlist = Helper.create "datalist" in
    let _ = Element.set_attribute inlist "id" list_name in
    let _ = Graph.StringSet.iter
      (fun name ->
        let option = Helper.create ~text:name "option" in
        let _ = Element.set_attribute option "value" name in
        Node.append_child inlist option)
      names in
    let button = Helper.create ~text:"GO" ~class_name:"inter_button" "button" in
    let div_view = Helper.create "div" in
    let _ = Node.append_child element input in
    let _ = Node.append_child element inlist in
    let _ = Node.append_child element button in
    let _ = Node.append_child element div_view in
    let view_graph g =
      let tabs_title = Helper.create "ul" ~class_name:"tabs" in
      let _ = Node.append_child div_view tabs_title in
      let tabs: (bool * string * ('a Node.t -> unit)) list = [
        true, "Bar View", BarChart.print_svg g proj;
        true, "Pie View", PieChart.print_svg g proj;
        true, "Graph View", (fun div -> Element.set_attribute div "class" "tree"; TreeView.callgraph div g proj);
      ] in
      let l = List.map (make_tab tabs_title div_view) tabs in
      let l = List.flatten l in
      Helper.tabs_logic l in
    let onclick () =
      let funname = Helper.input_of_id input_name in
      let fn = Html.Input.value funname in
      let g = inv fn in
      let _ = Helper.removeAll div_view in
      view_graph g in
    begin
      let g = inv "ROOT" in
      Element.set_onclick button onclick;
      view_graph g;
    end
end

let filename_onclick _ =
  print_endline "click";
  let filename = Helper.input_of_id "filename" in
  let file = FileList.item (Html.Input.files filename) 0 in
  let filereader = FileReader.new_file_reader () in
  match file with
  | None -> error "Unable to open file."
  | Some file ->
    print_endline "success";
    let onload _ =
      let result = FileReader.result filereader in
      match result with
      | None -> error "Error while reading file."
      | Some text ->
        let open Graph in
        let graph = graph_of_string text in
        let main = Helper.element_of_id "main" in
        Helper.removeAll main; print_endline "removed";
        Helper.show main;
        let tabs_title = Helper.create "ul" ~class_name:"tabs" in
        let _ = Node.append_child main tabs_title in
        let fill_graph tab proj =
          CallerView.selector graph tab merge_graph proj in
        let tabs = [
          (* cycle tab *)
          true, "Source Tree Cycles", fill_graph "cycle" (fun {time; _} -> time);
          (* time tab *)
          has_sys_time graph, "Source Tree Time",
          fill_graph "time" (fun {sys_time; _} -> sys_time);
          (* Gc tab *)
          has_allocated_bytes graph, "Source Tree Allocation",
          fill_graph "bytes "(fun {allocated_bytes; _} -> allocated_bytes);
          (* Aggregated table *)
          true, "Aggregated Table", Graph.aggregated_table graph;
          (* caller graph *)
          true, "Caller graph",
          CallerView.selector graph "caller" inverse_graph (fun {time; _} -> time);
        ] in
        let l = List.flatten (List.map (CallerView.make_tab tabs_title main) tabs) in
        Helper.tabs_logic l
    in
    FileReader.read_as_text filereader file;
    FileReader.set_onload filereader onload

let onload _ = begin
  let filename_button = Helper.element_of_id "filenameButton" in
  Element.set_onclick filename_button filename_onclick;
  Helper.hide (Helper.element_of_id "main")
end
let () = Window.set_onload GlobalVariables.window onload
