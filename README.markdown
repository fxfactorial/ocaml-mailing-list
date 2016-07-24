OCaml mailing list
==================

This is an OCaml program that generates the OCaml mailing list using
bindings to `ReactJS` for the HTML modeling and `nodejs` for the
actual generation of HTML.

Here's the source code, should be an inspiration for OCaml bindings to
ReactJS usage and nodejs as well. 

You'll need two packages: 

```shell
$ opam pin add -y reactjs https://github.com/fxfactorial/ocaml-reactjs
$ opam pin add -y nodejs https://github.com/fxfactorial/ocaml-nodejs
```

Then a plain invocation of `make` should work and will build an
example `index.html`.

# Result 

The generated example HTML, a mobile-responsive and completely self
contained HTML email, should look like:

![img](./newsletter_generated.png)

# Source code

```ocaml
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

let newsletter_style = 
  DOM.make ~tag:`style [Text {|some_really_long_css_string_see_source|}]

let newsletter mailing_l = DOM.(
    let open Infix in
    make_class_spec
      (fun ~this ->
         let head = make ~tag:`head [
             Elem (make ~elem_spec:(object%js
                     val name = !*"viewport"
                     val content = !*"width=device-width"
                   end) ~tag:`meta []);
             Elem (make ~elem_spec:(object%js
                     val httpEquiv = !*"Content-Type"
                     val content = !*"text/html; charset=UTF-8"
                   end) ~tag:`meta []);
             Elem newsletter_style;
           ]
         in
         let mailing_list_header =
           Elem (make
                   ~elem_spec:(object%js
                     val style = (object%js
                       val textAlign = !*"center"
                     end)
                   end)
                   ~tag:`h2
                   [Text
                      (Printf.sprintf
                         "OCaml mailing list for: %s"
                         mailing_l.date_range)])
         in

         let posts =
           mailing_l.articles |> List.map ~f:(fun {author; title; content} ->
               let title_line =
                 Text (Printf.sprintf "%s announced: %s" author title)
               in
               Elem (make ~tag:`p
                       [Elem (make ~tag:`p [
                            Elem (make ~tag:`h3 [title_line]);
                            Elem (make ~tag:`br []);
                            Elem (make ~tag:`p [Text content])]
                          )])
             )
         in
         let table =
           make ~elem_spec:(object%js
             val className = !*"body-wrap"
             val style = (object%js val backgroundColor = !*"#f6f6f6" end)
           end) ~tag:`table [
             Elem (make ~tag:`tr [
                 Elem (make ~tag:`td []);
                 Elem (make ~elem_spec:(object%js
                         val className = !*"container"
                         val style = (object%js
                           val backgroundColor = !*"#FFFFFF"
                         end)
                       end)
                         ~tag:`td ([
                             mailing_list_header;
                             Elem (make ~tag:`hr []);
                           ] @ posts));

               ])
           ]
         in
         let footer_table =
           let foot_message =
             [
               Text "This mailing list was generated \
                     from json using OCaml bindings to ReactJS \
                     and Nodejs. Improve it by sending a PR to: ";
               Elem (make ~elem_spec:(object%js
                       val href = !*"http://github.com/fxfactorial/ocaml-mailing-list"
                     end) ~tag:`a [Text "ocaml-mailing-list"])
             ]
           in
           let footer_content =
             Elem (make ~tag:`table [
                 Elem (make ~tag:`tr [
                     Elem (make ~elem_spec:(object%js
                             val style = (object%js
                               val textAlign = !*"center"
                             end)
                           end) ~tag:`td [Elem (make ~tag:`p foot_message)])
                   ])
               ])
           in
           make ~elem_spec:(object%js val className = !*"footer-wrap" end)
             ~tag:`table [
             Elem (make ~tag:`tr [
                 Elem (make ~tag:`td []);
                 Elem (make
                         ~elem_spec:(object%js
                           val className = !*"container"
                         end)
                         ~tag:`td [
                         Elem (make ~elem_spec:(object%js
                                 val className = !*"content"
                               end) ~tag:`div [footer_content])
                       ])
               ])
           ]
         in
         let body =
           make
             ~elem_spec:(object%js
               val style = (object%js val backgroundColor = !*"#f6f6f6" end)
             end)
             ~tag:`body [Elem table; Elem footer_table ]
         in
         make ~tag:`html [Elem head; Elem body]
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
```


# Contributions

Please do contribute, especially if you can make it look prettier!

# Acknowledgements

I basically translated
 [this](https://github.com/leemunroe/responsive-html-email-template)
 into `React`.
