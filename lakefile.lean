import Lake
open Lake DSL

package amadeus {
  -- Package description
  defaultFacet := PackageFacet.oleanLib
}

require std from git "https://github.com/leanprover/std4" @ "main"

@[default_target]
lean_lib Amadeus {
  -- Library settings
  roots := #[`Amadeus]
}

@[default_target]
lean_exe amadeus {
  root := `Main
}
