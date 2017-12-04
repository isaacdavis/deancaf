
let walk_class n c =


let gen_offsets tree =
  tree#iter (fun n c -> walk_class c);