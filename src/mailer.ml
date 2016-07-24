open StdLabels
open Reactjs
open Lwt.Infix
open Nodejs_high_level_lwt

let email json = DOM.(
    make_class_spec
      (fun ~this -> let open Infix in

        make ~tag:`html []

      )|> create_class
  )

type mailing_list = { date_range : string;
                      articles : post list; } [@@deriving of_yojson]
and post = {author : string;
            title : string;
            content : string; }

let () =
  Lwt.async (fun () ->
      let p = Nodejs_high_level.process in
      let args = p#arguments in

      if List.length args <> 3
      then begin
        "Must provide input file of posts for HTML generation"
        |> prerr_endline;
        p#exit 1
        end;

      let posts_file = List.nth args 2 in
      print_endline posts_file;

      Fs.read_file posts_file >|= fun (error, data) ->
      let j = Yojson.Safe.from_string data#to_string in

      Yojson.Safe.to_string j |> print_endline;

      match mailing_list_of_yojson j with
      | Result.Ok right ->
        print_endline "everything okay"
      | Result.Error left ->
        print_endline @@ "ERROR: " ^ left ;
        p#exit 1


    )
