open StdLabels
open Reactjs
open Lwt.Infix
open Nodejs_high_level_lwt

module S = Yojson.Safe
module U = Yojson.Safe.Util

type mailing_list = { date_range : string;
                      articles : post list; }
and post = {author : string;
            title : string;
            content : string; }

let newsletter json = DOM.(
    make_class_spec
      (fun ~this -> let open Infix in

        make ~tag:`html []

      )|> create_class
  )

let () =
  Lwt.async (fun () ->
      let p = Nodejs_high_level.process in
      let args = p#arguments in

      if List.length args <> 3
      then begin
        "Must provide input json file of posts for HTML generation"
        |> prerr_endline;
        p#exit 1
        end;

      let posts_file = List.nth args 2 in

      let read_string ~key o = U.member key o |> U.to_string in
      Fs.read_file posts_file >|= fun (_, data) ->
      let j = S.from_string data#to_string in

      let mailing_list = {
        date_range = U.member "date_range" j |> U.to_string;
        articles = U.member "articles" j
                   |> U.to_list
                   |> List.map ~f:(fun entry ->
                       {author = read_string ~key:"author" entry;
                        title = read_string ~key:"title" entry;
                        content = read_string ~key:"content" entry;}
                     )
      }
      in

      let rendered =
        (Reactjs.react_dom_server ())
        ##renderToStaticMarkup (newsletter mailing_list
                                |> create_element_from_class)
        |> Js.to_string
      in
      print_endline rendered

    )
