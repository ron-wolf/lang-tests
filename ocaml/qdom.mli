(** XML processing. *)

(** An XML element node (and nearby text). *)
type element = {
  tag: Xmlm.name;
  mutable attrs: Xmlm.attribute list;
  mutable child_nodes: element list;
  mutable text_before: string;        (** The text node immediately before us *)
  mutable last_text_inside: string;   (** The last text node inside us with no following element *)
  source_name: Support.filepath;             (** For error messages *)
  pos: Xmlm.pos;                      (** Location of element in XML *)
};;

(** {2 Parsing} *)

(** @raise [Safe_exception] if the XML is not well formed. *)
val parse_input : string -> Xmlm.input -> element

(** @raise [Safe_exception] if the XML is not well formed. *)
val parse_file : string -> element

(** {2 Helper functions} *)

val find : (element -> bool) -> element -> element

(** [raise_elem "Problem with " elem] raises a [Safe_exception] with the message "Problem with <element> at ..." *)
val raise_elem : string -> element -> 'a

module type NsType = sig val ns : string end

module NsQuery :
  functor (Ns : NsType) ->
    sig
      (** Get the value of the non-namespaced attribute [attr].
          Throws an exception if [elem] isn't in our namespace. *)
      val get_attribute : string -> element -> string

      val get_attribute_opt : string -> element -> string option

      val fold_left : ('a -> element -> 'a) -> 'a -> element -> string -> 'a

      (** Apply [fn] to each child node in our namespace with local name [tag] *)
      val map : (element -> 'a) -> element -> string -> 'a list

      (** Call [fn] on each child node in our namespace *)
      val iter : (element -> unit) -> element -> unit

      (** Call [fn] on each child node in our namespace with local name [tag] *)
      val iter_with_name : (element -> unit) -> element -> string -> unit

      (** Get the value of the non-namespaced attribute [attr].
          Throws an exception if [elem] isn't in our namespace. *)
      val tag : element -> string option

      (** Throws [Safe_exception] if element does not have the expected name and namespace. *)
      val check_tag : string -> element -> unit
      ;;
    end;;
