type Universe =
  Labeled "hex12p1" (Fixture Hex12p_7channel) :+:
  Def_SlimPar64_1 :+:
  Def_Ultrabar_1 :+:
  Def_GigBar_1

universe :: Universe
universe = def_hex12p1 :+: def_slimPar64_1 :+: def_ultrabar_1 :+: def_gb_1
