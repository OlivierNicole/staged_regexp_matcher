let () =
  let ic = open_in "test.txt" in
  let str = input_line ic in
  let rx = Str.regexp "\\([ABC][abc]*[ .]\\)+" in
  for i = 1 to 100000 do
    assert (
      Str.string_match rx str 0
    )
  done;
  close_in ic
