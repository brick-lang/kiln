open Basic

module type S = sig
  type state
  include Monadic.S
    with type 'a t = state -> state * 'a

  val run : (state -> state * 'result) -> state -> state * 'result
  val get : state t
  val put : state -> unit t
  val modify : (state -> state) -> unit t
  val eval : (state -> state * 'result) -> state -> 'result
  val exec : (state -> state * 'result) -> state -> state
end

module Make(K : sig type t end) : (S with type state := K.t) = struct (*  *)
  module Monad = struct
    type 'a t = K.t -> K.t * 'a
    let return a : 'a t = fun s -> (s, a)
    let bind (m:'a t) ~(f:'a -> 'b t) : 'b t = fun s ->
      let (t, a) = m s
      in (f a) t

    let map ma ~f = bind ma ~f:(fun a -> return (f a))
  end
  include Monadic.Make(Monad)

  let join maa = maa >>= fun ma -> ma
  let get : 'a t = fun s -> (s,s)
  let put s = fun _ -> (s, ())
  let modify f = get >>= fun k -> put (f k)
  let run ma k = ma k
  let eval act = snd <.> run act
  let exec act = fst <.> run act
end


