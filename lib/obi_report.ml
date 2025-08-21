let emit_page name page =
  let open Tyxml.Html in
  Printf.printf "Generating: %s\n" name;
  let file_handle = open_out name in
  let fmt = Format.formatter_of_out_channel file_handle in
  Format.fprintf fmt "%a@." (pp ~indent:true ()) page;
  close_out file_handle

let generate_package_report config commit compiler package =
  let open Tyxml.Html in
  let name = OpamPackage.to_string package in
  let () = OpamConsole.note "Package %s" name in
  let status = Yojson.Safe.from_file (Obi_config.work_dir_path config [ "results"; commit; compiler.Obi_builder.version; "status"; name ]) |> Obi_solution.status_of_yojson |> Result.get_ok in
  html
    (head (title (txt name)) [ link ~rel:[ `Stylesheet ] ~href:"/stylesheet.css" () ])
    (body
       (List.map
          (fun (layer : Obi_solution.layer) ->
            let style = if layer.result = 0 then "good" else "bad" in
            let content =
              let build_log = Obi_config.work_dir_path config [ layer.hash; "build.log" ] in
              if Sys.file_exists build_log then pre [ txt (Obi_os.read_from_file build_log) ]
              else
                let bad_log = Obi_config.work_dir_path config [ "results"; commit; compiler.version; "bad"; layer.package ] in
                if Sys.file_exists bad_log then pre [ txt (Obi_os.read_from_file bad_log) ] else pre []
            in
            details (summary ~a:[ a_class [ style ] ] [ txt layer.package ]) [ content ])
          status.layers))
  |> emit_page (Obi_config.work_dir_path config [ "html"; compiler.version; name ^ ".html" ])

let set_of_dir config commit compiler dir =
  Sys.readdir (Obi_config.work_dir_path config [ "results"; commit; compiler.Obi_builder.version; dir ])
  |> Array.fold_left (fun acc file -> OpamPackage.Set.add (OpamPackage.of_string file) acc) OpamPackage.Set.empty

type details = {
  compiler : Obi_builder.compiler;
  solution : OpamPackage.Set.t;
  good : OpamPackage.Set.t;
  bad : OpamPackage.Set.t;
  status : OpamPackage.Set.t;
}

type results = {
  commit : string;
  details : details list;
}

let html_list_of_set compiler s =
  let open Tyxml.Html in
  ul
    (OpamPackage.Set.to_list_map
       (fun s ->
         let name = OpamPackage.to_string s in
         li [ a ~a:[ a_href (compiler.Obi_builder.version ^ "/" ^ name ^ ".html") ] [ txt name ] ])
       s)

let rec generate_diff_loop = function
  | g0 :: g1 :: tl ->
      let open Tyxml.Html in
      [ h1 [ txt (g0.commit ^ " - " ^ g1.commit) ] ]
      @ (List.map2
           (fun d0 d1 ->
             let removed = OpamPackage.Set.diff d0.solution d1.solution in
             let added = OpamPackage.Set.diff d1.solution d0.solution in
             [ h2 [ txt "removed solution" ]; html_list_of_set d0.compiler removed; h2 [ txt "added solution" ]; html_list_of_set d0.compiler added ])
           g0.details g1.details
        |> List.flatten)
      @ (List.map2
           (fun d0 d1 ->
             let added = OpamPackage.Set.diff d1.good d0.good in
             [ h2 [ txt "added good" ]; html_list_of_set d0.compiler added ])
           g0.details g1.details
        |> List.flatten)
      @ (List.map2
           (fun d0 d1 ->
             let removed = OpamPackage.Set.diff d0.bad d1.bad in
             let added = OpamPackage.Set.diff d1.bad d0.bad in
             [ h2 [ txt "removed bad" ]; html_list_of_set d0.compiler removed; h2 [ txt "added bad" ]; html_list_of_set d0.compiler added ])
           g0.details g1.details
        |> List.flatten)
      @ generate_diff_loop (g1 :: tl)
  | [ g0 ] ->
      let open Tyxml.Html in
      [
        h1 [ txt g0.commit ];
        table
          [
            tr (List.map (fun d0 -> td [ h2 [ txt "Solutions" ]; html_list_of_set d0.compiler d0.solution ]) g0.details);
            tr (List.map (fun d0 -> td [ h2 [ txt "Good" ]; html_list_of_set d0.compiler d0.good ]) g0.details);
            tr (List.map (fun d0 -> td [ h2 [ txt "Bad" ]; html_list_of_set d0.compiler d0.bad ]) g0.details);
          ];
      ]
  | _ -> []

let generate_index_report config commits compilers =
  let results =
    List.map
      (fun commit ->
        {
          commit;
          details =
            List.map
              (fun compiler ->
                {
                  compiler;
                  solution = set_of_dir config commit compiler "solution";
                  good = set_of_dir config commit compiler "good";
                  bad = set_of_dir config commit compiler "bad";
                  status = set_of_dir config commit compiler "status";
                })
              compilers;
        })
      commits
  in
  let open Tyxml.Html in
  html (head (title (txt "index")) [ link ~rel:[ `Stylesheet ] ~href:"/stylesheet.css" () ]) (body (generate_diff_loop results))
  |> emit_page (Obi_config.work_dir_path config [ "html"; "index.html" ])