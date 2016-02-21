open Basic
       
module type S = sig
  type state
  include Monadic.S

  val run : (state -> state * 'a) -> state -> state * 'a
  val get : state t
  val put : state -> unit t
  val modify : (state -> state) -> unit t
  val eval : (state -> state * 'b) -> state -> 'b
  val exec : (state -> state * 'b) -> state -> state
end

module Make(K : sig type t end) (* : (S with type state := K.t) *) = struct (*  *)
  module Run = struct
    type 'a t = K.t -> K.t * 'a
  end
  include Run

  let run ma k = ma k

  module Monad = struct
    type 'a t = 'a Run.t
    let return (lazy a) = fun s -> (s, a)
    let bind (m : K.t -> K.t * 'a) (f : 'a -> K.t -> K.t * 'b) : K.t -> K.t * 'b = fun s ->
      let (t, a) = m s
      in run (f a) t

    let map ma ~f = bind ma (fun a -> return @@ lazy (f a))
  end
  include Monadic.Make(Monad)

  let join maa = maa >>= fun ma -> ma
  let get = fun s -> (s,s)
  let put s = fun _ -> (s, ())
  let modify f = get >>= fun k -> put (f k)
  let eval act = snd *.* run act
  let exec act = fst *.* run act
end


