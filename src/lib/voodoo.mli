val cache : Obuilder_spec.Cache.t list

module Prep : sig
  val spec : base:Spec.t -> Spec.t
end

module Odoc : sig
  val spec : base:Spec.t -> Config.t -> Spec.t
end

module OdocDriver : sig
  val spec : base:Spec.t -> odoc_pin:string -> sherlodoc_pin:string -> Spec.t
end
