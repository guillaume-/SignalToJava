type integer;

process P = (? integer a, b; ! integer u, v, w, x, y, z;)(
|w := a + b
|x := a * w
|y := (b * x) - w
|z := x + y + u
|u := 2 * b
|v := u - 1
|)end;
