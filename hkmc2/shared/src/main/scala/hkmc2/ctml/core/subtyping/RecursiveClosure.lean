import CTMLCore.Declarative.Recursive

/-!
The single-variable closure experiment. This file imports the sibling CTML mechanization;
it does not add axioms or change the declarative subtype relation.
-/

set_option autoImplicit false

namespace CTMLCore.Recursive.Closure

open Syntax

/-- Add an independent seed to the recursive body. -/
def seeded {n : Nat} (body : WFTy (n + 1)) (seed : WFTy n) : WFTy (n + 1) :=
  WFTy.union body seed.weaken

/-- The family of closed carriers, indexed by an arbitrary seed. -/
def close {n : Nat} (body : WFTy (n + 1)) (seed : WFTy n) : WFTy n :=
  mu (seeded body seed)

/-- Substitution leaves the seed independent of the recursive variable. -/
theorem seeded_instantiate {n : Nat} (body : WFTy (n + 1)) (seed t : WFTy n) :
    (seeded body seed).instantiate t = WFTy.union (body.instantiate t) seed := by
  change WFTy.union (body.instantiate t) (seed.weaken.instantiate t) = _
  rw [WFTy.weaken_instantiate_cancel]

/-- The exact scoped monotonicity premise required by the existing fold theorem. -/
def CanClose (ctx : SubtypingContext) (body : WFTy (ctx.typeDepth + 1)) : Prop :=
  ∀ seed : WFTy ctx.typeDepth,
    MonotoneAt (ctx.bindType.assume (guard (seeded body seed)))
      ((seeded body seed).liftAt 1 (by omega))

/-- Adding an independent seed preserves monotonicity. -/
theorem seeded_monotone {ctx : SubtypingContext} {body : WFTy (ctx.typeDepth + 1)}
    (mono : MonotoneAt ctx body) (seed : WFTy ctx.typeDepth) :
    MonotoneAt ctx (seeded body seed) := by
  intro s t h
  rw [seeded_instantiate, seeded_instantiate]
  exact Subtype.unionMono (mono h) .refl

/-- The seed stays outside the carrier binder when the surrounding context is extended. -/
theorem seeded_lift {n : Nat} (body : WFTy (n + 1)) (seed : WFTy n) :
    (seeded body seed).liftAt 1 (by omega) =
      seeded (body.liftAt 1 (by omega)) seed.weaken := by
  change WFTy.union (body.liftAt 1 (by omega)) (seed.weaken.liftAt 1 (by omega)) = _
  rw [WFTy.liftAt_weaken]
  rfl

/-- The premise holds for list layers with an arbitrary element type and arbitrary seed. -/
theorem lists_canClose (ctx : SubtypingContext) (element : WFTy ctx.typeDepth) :
    CanClose ctx (Lists.body element) := by
  intro seed
  have mono : MonotoneAt (ctx.bindType.assume (guard (seeded (Lists.body element) seed)))
      (Lists.body element.weaken) :=
    @Lists.monotone (ctx.bindType.assume (guard (seeded (Lists.body element) seed))) element.weaken
  have result : MonotoneAt (ctx.bindType.assume (guard (seeded (Lists.body element) seed)))
      (seeded (Lists.body element.weaken) seed.weaken) :=
    @seeded_monotone (ctx.bindType.assume (guard (seeded (Lists.body element) seed)))
      (Lists.body element.weaken) mono seed.weaken
  rw [seeded_lift, Lists.body_lift]
  exact fun {s t} h => result h

/-- Every member of the family satisfies the original lower recursive bound. -/
theorem closed {ctx : SubtypingContext} {body : WFTy (ctx.typeDepth + 1)}
    (mono : CanClose ctx body) (seed : WFTy ctx.typeDepth) :
    Subtype ctx (body.instantiate (close body seed)) (close body seed) := by
  have h := Recursive.fold (seeded body seed) (mono seed)
  rw [seeded_instantiate] at h
  exact Subtype.leUnionLeft.trans h

/-- Every seed is contained in its closure. -/
theorem seed_le {ctx : SubtypingContext} {body : WFTy (ctx.typeDepth + 1)}
    (mono : CanClose ctx body) (seed : WFTy ctx.typeDepth) :
    Subtype ctx seed (close body seed) := by
  have h := Recursive.fold (seeded body seed) (mono seed)
  rw [seeded_instantiate] at h
  exact Subtype.leUnionRight.trans h

/-- A closed type containing the seed contains its closure; monotonicity is not needed here. -/
theorem least {ctx : SubtypingContext} {body : WFTy (ctx.typeDepth + 1)}
    {seed t : WFTy ctx.typeDepth} (ht : Subtype ctx (body.instantiate t) t)
    (hs : Subtype ctx seed t) : Subtype ctx (close body seed) t := by
  apply carrier
  rw [seeded_instantiate]
  exact Subtype.unionLe ht hs

/-- Every solution is recovered by choosing that solution itself as the seed. -/
theorem recover {ctx : SubtypingContext} {body : WFTy (ctx.typeDepth + 1)}
    (mono : CanClose ctx body) {t : WFTy ctx.typeDepth}
    (ht : Subtype ctx (body.instantiate t) t) :
    Subtype ctx (close body t) t ∧ Subtype ctx t (close body t) :=
  ⟨least ht .refl, seed_le mono t⟩

/-- All remaining obligations may use the variable at either variance, provided they respect
mutual subtyping. No monotonicity of the remaining problem is assumed. -/
theorem solutions {ctx : SubtypingContext} {body : WFTy (ctx.typeDepth + 1)}
    (mono : CanClose ctx body) (remaining : WFTy ctx.typeDepth → Prop)
    (congruent : ∀ s t, Subtype ctx s t → Subtype ctx t s →
      (remaining s ↔ remaining t)) :
    (∃ t, Subtype ctx (body.instantiate t) t ∧ remaining t) ↔
      (∃ seed, remaining (close body seed)) := by
  constructor
  · rintro ⟨t, ht, rest⟩
    have eqv := recover mono ht
    exact ⟨t, (congruent t (close body t) eqv.2 eqv.1).mp rest⟩
  · rintro ⟨seed, rest⟩
    exact ⟨close body seed, closed mono seed, rest⟩

end CTMLCore.Recursive.Closure
