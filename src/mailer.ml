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

let newsletter_style = (DOM.make ~tag:`style [Text {|
/* -------------------------------------
    GLOBAL
------------------------------------- */
* {
  font-family: "Helvetica Neue", "Helvetica", Helvetica, Arial, sans-serif;
  font-size: 100%;
  line-height: 1.6em;
  margin: 0;
  padding: 0;
}
img {
  max-width: 600px;
  width: auto;
}
body {
  -webkit-font-smoothing: antialiased;
  height: 100%;
  -webkit-text-size-adjust: none;
  width: 100% !important;
}
/* -------------------------------------
    ELEMENTS
------------------------------------- */
a {
  color: #348eda;
}
.btn-primary {
  Margin-bottom: 10px;
  width: auto !important;
}
.btn-primary td {
  background-color: #348eda;
  border-radius: 25px;
  font-family: "Helvetica Neue", Helvetica, Arial, "Lucida Grande", sans-serif;
  font-size: 14px;
  text-align: center;
  vertical-align: top;
}
.btn-primary td a {
  background-color: #348eda;
  border: solid 1px #348eda;
  border-radius: 25px;
  border-width: 10px 20px;
  display: inline-block;
  color: #ffffff;
  cursor: pointer;
  font-weight: bold;
  line-height: 2;
  text-decoration: none;
}
.last {
  margin-bottom: 0;
}
.first {
  margin-top: 0;
}
.padding {
  padding: 10px 0;
}
/* -------------------------------------
    BODY
------------------------------------- */
table.body-wrap {
  padding: 20px;
  width: 100%;
}
table.body-wrap .container {
  border: 1px solid #f0f0f0;
}
/* -------------------------------------
    FOOTER
------------------------------------- */
table.footer-wrap {
  clear: both !important;
  width: 100%;
}
.footer-wrap .container p {
  color: #666666;
  font-size: 12px;
}
table.footer-wrap a {
  color: #999999;
}
/* -------------------------------------
    TYPOGRAPHY
------------------------------------- */
h1,
h2,
h3 {
  color: #111111;
  font-family: "Helvetica Neue", Helvetica, Arial, "Lucida Grande", sans-serif;
  font-weight: 200;
  line-height: 1.2em;
  margin: 40px 0 10px;
}
h1 {
  font-size: 36px;
}
h2 {
  font-size: 28px;
}
h3 {
  font-size: 22px;
}
p,
ul,
ol {
  font-size: 14px;
  font-weight: normal;
  margin-bottom: 10px;
}
ul li,
ol li {
  margin-left: 5px;
  list-style-position: inside;
}
/* ---------------------------------------------------
    RESPONSIVENESS
------------------------------------------------------ */
/* Set a max-width, and make it display as block so it will automatically stretch to that width, but will also shrink down on a phone or something */
.container {
  clear: both !important;
  display: block !important;
  Margin: 0 auto !important;
  max-width: 600px !important;
}
/* Set the padding on the td rather than the div for Outlook compatibility */
.body-wrap .container {
  padding: 20px;
}
/* This should also be a block element, so that it will fill 100% of the .container */
.content {
  display: block;
  margin: 0 auto;
  max-width: 600px;
}
/* Let's make sure tables in the content area are 100% wide */
.content table {
  width: 100%;
}
|}])

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
          Elem (make ~tag:`h3
                  [Text
                     (Printf.sprintf
                        "OCaml mailing list for: %s"
                        mailing_l.date_range)])
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
                        ~tag:`td [
                        mailing_list_header;
                        Elem (make ~tag:`hr []);
                        Elem (make ~tag:`br []);
                        Elem (make ~tag:`p [Text "Hello"]);
                        Elem (make ~tag:`p [Text "World"]);
                      ]);

              ])
          ]
        in
        let body =
          make
            ~elem_spec:(object%js
              val style = (object%js val backgroundColor = !*"#f6f6f6" end)
            end)
            ~tag:`body [Elem table]

        in
        make ~tag:`html [ Elem head; Elem body ]
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
